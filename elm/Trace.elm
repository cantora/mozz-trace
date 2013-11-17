module Trace (
  fromJson
  ) where

import Dict
import open Either
import Json

type JsonDict = Dict.Dict String Json.JsonValue

type Instruction = { dasm:String, bytes:[Int] }
--data Path = Instr Int | Path Int [Path]
type SubRoutine = { name:String, base:Int, instructions:[Instruction], path:JsonDict, meta:[JsonDict] }

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

makeMetaSeq : [Json.JsonValue] -> Error [JsonDict]
makeMetaSeq marr =
  let
    fn json rest =
      matchDict json >>= \d ->
      rest >>= \ds ->
      Right (d::ds)
  in foldr fn (Right []) marr

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
    geta "meta" >>= \mlist ->
    makeInstructions ilist >>= \instrs ->
    makeMetaSeq mlist >>= \metas ->
    Right {name=name, base=base, instructions=instrs, path=pdict, meta=metas}

fromJson : Json.JsonValue -> Either String SubRoutine
fromJson json =
  matchDict json >>= makeSub
