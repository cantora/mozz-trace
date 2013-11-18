module Trace.SubRoutine (
  make,
  traverse
  ) where

import Trace.Json as Json
import Trace.Error as Error
import Trace.Error ((>>=), Error)
import Trace.Path as Path
import Trace.Util (forEachUpToR)

import Dict
import List

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
    gets = Json.lookupString dict
    getl = Json.lookupList dict
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
    geti = Json.lookupInt dict
    gets = Json.lookupString dict
    getl = Json.lookupList dict
    getd = Json.lookupDict dict
  in
    gets "name" >>= \name ->
    geti "base" >>= \base ->
    getl "instructions" >>= \ilist ->
    getd "path" >>= \pdict ->
    getl "meta" >>= \mlist ->
    makeInstructions ilist >>= \instrs ->
    makeMetaSeq mlist >>= \metas ->
    Error.ok {name=name, base=base, instructions=instrs, path=pdict, meta=metas}

type TraverseFN b = 
  b ->                -- accumulator
  Int ->              -- offset into metas
  SubRoutine ->       -- current subroutine
  Path.Path b ->      -- current path element
  b                   -- result accumulator

traverse :
  TraverseFN b ->
  b ->                -- base accumulator
  SubRoutine ->       -- base subroutine
  Error b             -- final result
traverse fn base sub =
  let
    dotraverse acc base_idx node =
      let
        proc_fn pval curr_acc =
          curr_acc >>= \(idx, real_acc) ->
          Json.dict pval >>= \pdict ->
          Path.node pdict >>= \n ->
          dotraverse real_acc (base_idx+idx) n >>= \(len, result) ->
          Error.ok (idx+len, result)
        fold_base idx = Error.ok (idx, base)
        call_fn = fn acc base_idx sub
      in
        case node of
          Path.Leaf instr_idx -> 
            Error.ok (1, call_fn <| Path.Instr instr_idx)
          Path.Node rpt subpaths -> 
            List.foldl proc_fn (fold_base 0) subpaths >>= \(len, ch_1) ->
            let
              iter_fn i curr_acc =
                curr_acc >>= \arr ->
                List.foldl proc_fn (fold_base ((i+1)*len)) subpaths >>= \(_, ch_n) ->
                Error.ok <| ch_n::arr
            in forEachUpToR rpt iter_fn (Error.ok []) >>= \ch_arr ->
            Error.ok (len*(rpt+1), call_fn <| Path.SubPath ch_1 ch_arr)
  in
    Path.node sub.path >>= \node -> 
    dotraverse base 0 node >>= \(_, result) ->
    Error.ok result
