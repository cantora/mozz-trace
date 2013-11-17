module Trace.SubRoutine (
  SubRoutine,
  Instruction,
  make
  ) where

import Trace.Json as Json
import Dict

import Trace.Error as Error
import Trace.Error ((>>=), Error)

lookup : comparable -> Json.Dict -> Error Json.Value
lookup k dict =
  case (Dict.lookup k dict) of
    Just v -> Error.ok v
    _      -> Error.report dict <| "key=" ++ (show k)

getString d x = (lookup x d) >>= Json.string
getList  d x = (lookup x d) >>= Json.list
getInt    d x = (lookup x d) >>= Json.int
getDict   d x = (lookup x d) >>= Json.dict

type Instruction = { dasm:String, bytes:[Int] }
type SubRoutine = { name:String, base:Int, instructions:[Instruction], path:Json.Dict, meta:[Json.Dict] }

makeBytes : [Json.Value] -> Error [Int]
makeBytes arr =
  let
    fn b rest =
      Json.int b >>= \n ->
      rest >>= \ns ->
      Error.ok <| n::ns
  in foldr fn (Error.ok []) arr

makeInstruction : Json.Dict -> Error Instruction
makeInstruction dict =
  let 
    gets = getString dict
    getl = getList dict
  in
    gets "dasm" >>= \dasm ->
    getl "bytes" >>= \arr ->
    makeBytes arr >>= \bytes ->
    Error.ok { dasm=dasm, bytes=bytes }

makeInstructions : [Json.Value] -> Error [Instruction]
makeInstructions instrs =
  let
    fn json rest =
      Json.dict json >>= makeInstruction >>= \i -> 
      rest >>= \is ->
      Error.ok (i::is)
  in foldr fn (Error.ok []) instrs

makeMetaSeq : [Json.Value] -> Error [Json.Dict]
makeMetaSeq marr =
  let
    fn json rest =
      Json.dict json >>= \d ->
      rest >>= \ds ->
      Error.ok (d::ds)
  in foldr fn (Error.ok []) marr

make : Json.Dict -> Error SubRoutine
make dict =
  let
    geti = getInt dict
    gets = getString dict
    getl = getList dict
    getd = getDict dict
  in
    gets "name" >>= \name ->
    geti "base" >>= \base ->
    getl "instructions" >>= \ilist ->
    getd "path" >>= \pdict ->
    getl "meta" >>= \mlist ->
    makeInstructions ilist >>= \instrs ->
    makeMetaSeq mlist >>= \metas ->
    Error.ok {name=name, base=base, instructions=instrs, path=pdict, meta=metas}
