module Trace (
  fromJson
  ) where

import Dict
import open Either
import Json

type Instruction = { dasm:String, bytes:[Int] }
data Path = Instr Int | Path Int [Path]
type SubRoutine = { name:String, base:Int, instructions:[Instruction], path:Path }
type JsonDict = Dict.Dict String Json.JsonValue

type Error a = Either String a

(>>=) : Error a -> (a -> Error b) -> Error b
ea >>= a_to_eb =
  case ea of
    Right x -> a_to_eb x
    Left s  -> Left s

error obj str =
  Left <| "error: expected " ++ str ++ " at " ++ (show obj)

lookup : comparable -> JsonDict -> Error Json.JsonValue
lookup k dict =
  case (Dict.lookup k dict) of
    Just v -> Right v
    _      -> error dict <| "key=" ++ (show k)

matchArray : Json.JsonValue -> Error [Json.JsonValue]
matchArray json =
  case json of
    Json.Array xs -> Right xs
    _             -> error json "array"

matchDict : Json.JsonValue -> Error JsonDict
matchDict json =
  case json of
    Json.Object dict -> Right dict
    _                -> error json "dict"

matchInt : Json.JsonValue -> Error Int
matchInt json =
  case json of
    Json.Number f -> Right (truncate f)
    _             -> error json "number"

matchString : Json.JsonValue -> Error String
matchString json =
  case json of
    Json.String s -> Right s
    _             -> error json "string"

getString d x = (lookup x d) >>= matchString
getArray  d x = (lookup x d) >>= matchArray
getInt    d x = (lookup x d) >>= matchInt
getDict   d x = (lookup x d) >>= matchDict

makeBytes : [Json.JsonValue] -> Error [Int]
makeBytes arr =
  let
    fn b rest =
      matchInt b >>= \n ->
      rest >>= \ns ->
      Right <| n::ns
  in foldr fn (Right []) arr

makeInstruction : JsonDict -> Error Instruction
makeInstruction dict =
  let 
    gets = getString dict
    geta = getArray dict
  in
    gets "dasm" >>= \dasm ->
    geta "bytes" >>= \arr ->
    makeBytes arr >>= \bytes ->
    Right { dasm=dasm, bytes=bytes }

makeInstructions : [Json.JsonValue] -> Error [Instruction]
makeInstructions instrs =
  let
    fn json rest =
      matchDict json >>= makeInstruction >>= \i -> 
      rest >>= \is ->
      Right (i::is)
  in foldr fn (Right []) instrs

makeInstr : JsonDict -> Error Path
makeInstr dict =
  lookup "idx" dict >>= 
  matchInt >>= \n ->
  Right (Instr n)
  
makeSubPath : JsonDict -> Error Path
makeSubPath dict =
  let
    geti = getInt dict
    geta = getArray dict
  in
    geti "repeats" >>= \repeats ->
    geta "path" >>=
    makePaths >>= \paths ->
    Right (Path repeats paths)

makePaths : [Json.JsonValue] -> Error [Path]
makePaths ps =
  let
    fn json rest =
      matchDict json >>= makePath >>= \path ->
      rest >>= \paths ->
      Right (path::paths)
  in foldr fn (Right []) ps

makePath : JsonDict -> Error Path
makePath dict =
  getString dict "type" >>= \typ ->
  if typ == "path"
    then makeSubPath dict
    else makeInstr dict

makeSub : JsonDict -> Error SubRoutine
makeSub dict =
  let
    geti = getInt dict
    gets = getString dict
    geta = getArray dict
    getd = getDict dict
  in
    gets "name" >>= \name ->
    geti "base" >>= \base ->
    geta "instructions" >>= \ilist ->
    getd "path" >>= \pdict ->
    makeInstructions ilist >>= \instrs ->
    makePath pdict >>= \path ->
    Right {name=name, base=base, instructions=instrs, path=path}

makeSubs : [Json.JsonValue] -> Error [SubRoutine]
makeSubs subs = 
  let
    fn json rest =
      matchDict json >>= makeSub >>= \sub ->
      rest >>= \subs ->
      Right (sub::subs)
  in foldr fn (Right []) subs

fromJson : Json.JsonValue -> Either String [SubRoutine]
fromJson json =
  matchArray json >>= makeSubs
