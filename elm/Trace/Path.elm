module Trace.Path (
  node,
  Instr,
  SubPath,
  Leaf,
  Node
  ) where

import Trace.Json as Json
import Trace.Error (Error, (>>=), ok, report)

data Path a = Instr Int | SubPath a [a]
data Node   = Leaf Int | Node Int [Json.Value]

node : Json.Dict -> Error Node
node d =
  let
    gets = Json.lookupString d
    geti = Json.lookupInt d 
    getl = Json.lookupList d
  in
    gets "type" >>= \t ->
    case t of
      "instr" -> 
        geti "idx" >>= \idx ->
        ok <| Leaf idx

      "path" ->
        geti "repeats" >>= \rpt ->
        getl "path" >>= \plist ->
        ok <| Node rpt plist

      _ -> report d "type to be one of \"path\", \"instr\""
