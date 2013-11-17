module Trace (
  fromJson
  ) where

import open Either
import Trace.Json as Json
import Trace.SubRoutine as SubRoutine
import Trace.Error ((>>=), Error)

fromJson : Json.Value -> Error SubRoutine.SubRoutine
fromJson json =
  Json.dict json >>= SubRoutine.make
