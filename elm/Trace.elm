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

type Transform a = Either String a

(>>=) : Transform a -> (a -> Transform b) -> Transform b
ea >>= a_to_eb =
  case ea of
    Right x -> a_to_eb x
    Left s  -> Left s

error obj str =
  Left <| "error: expected " ++ str ++ " at " ++ (show obj)

lookup : comparable -> JsonDict -> Transform Json.JsonValue
lookup k dict =
  case (Dict.lookup k dict) of
    Just v -> Right v
    _      -> error dict ("key=" ++ (show k))

matchArray : Json.JsonValue -> Transform [Json.JsonValue]
matchArray json =
  case json of
    Json.Array xs -> Right xs
    _             -> error json "array"

matchDict : Json.JsonValue -> Transform JsonDict
matchDict json =
  case json of
    Json.Object dict -> Right dict
    _                -> error json "dict"

matchInt : Json.JsonValue -> Transform Int
matchInt json =
  case json of
    Json.Number f -> Right (truncate f)
    _             -> error json "number"

matchString : Json.JsonValue -> Transform String
matchString json =
  case json of
    Json.String s -> Right s
    _             -> error json "string"

makeBytes : [Json.JsonValue] -> Transform [Int]
makeBytes arr =
  let
    fn b rest =
      matchInt b >>= \n ->
      rest >>= \ns ->
      Right <| n::ns
  in foldr fn (Right []) arr

makeInstruction : JsonDict -> Transform Instruction
makeInstruction dict =
  let 
    get  x = lookup x dict
    gets x = get x >>= matchString
    geta x = get x >>= matchArray
  in
    gets "dasm" >>= \dasm ->
    geta "bytes" >>= \arr ->
    makeBytes arr >>= \bytes ->
    Right { dasm=dasm, bytes=bytes }

makeInstructions : [Json.JsonValue] -> Transform [Instruction]
makeInstructions instrs =
  let
    fn json rest =
      matchDict json >>= makeInstruction >>= \i -> 
      rest >>= \is ->
      Right (i::is)
  in foldr fn (Right []) instrs

makeInstr : JsonDict -> Transform Path
makeInstr dict =
  lookup "idx" dict >>= 
  matchInt >>= \n ->
  Right (Instr n)
  
makeSubPath : JsonDict -> Transform Path
makeSubPath dict =
  let
    get  x = lookup x dict
    geti x = get x >>= matchInt
    geta x = get x >>= matchArray
  in
    geti "repeats" >>= \repeats ->
    geta "path" >>=
    makePaths >>= \paths ->
    Right (Path repeats paths)

makePaths : [Json.JsonValue] -> Transform [Path]
makePaths ps =
  let
    fn json rest =
      matchDict json >>= makePath >>= \path ->
      rest >>= \paths ->
      Right (path::paths)
  in foldr fn (Right []) ps

makePath : JsonDict -> Transform Path
makePath dict =
  let
    get  x = lookup x dict
    gets x = get x >>= matchString
  in
    gets "type" >>= \typ ->
    if typ == "path"
      then makeSubPath dict
      else makeInstr dict

makeSub : JsonDict -> Transform SubRoutine
makeSub dict =
  let
    get  x = lookup x dict 
    geti x = get x >>= matchInt
    gets x = get x >>= matchString
    geta x = get x >>= matchArray
    getd x = get x >>= matchDict
  in
    gets "name" >>= \name ->
    geti "base" >>= \base ->
    geta "instructions" >>= \ilist ->
    getd "path" >>= \pdict ->
    makeInstructions ilist >>= \instrs ->
    makePath pdict >>= \path ->
    Right {name=name, base=base, instructions=instrs, path=path}

makeSubs : [Json.JsonValue] -> Transform [SubRoutine]
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
