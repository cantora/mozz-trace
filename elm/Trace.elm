module Trace (
  fromJson,
  traverse
  ) where

import open Either
import Trace.Json as Json
import Trace.SubRoutine as SubRoutine
import Trace.Error ((>>=), Error)

fromJson : Json.Value -> Error SubRoutine.SubRoutine
fromJson json =
  Json.dict json >>= SubRoutine.make

traverse : 
  SubRoutine.TraverseFN b ->   -- function to process. see Trace.SubRoutine
  b ->                         -- base accumulator
  Json.Value ->                -- data to traverse
  Error b                      -- result
traverse fn base json =
  fromJson json >>= \subroutine ->
  SubRoutine.traverse fn base subroutine
