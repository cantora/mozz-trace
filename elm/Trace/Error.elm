module Trace.Error (
  Error,
  (>>=),
  report,
  ok
  ) where

import open Either

type Error a = Either String a

(>>=) : Error a -> (a -> Error b) -> Error b
ea >>= a_to_eb =
  case ea of
    Right x -> a_to_eb x
    Left s  -> Left s

ok x = Right x

report obj str =
  Left <| "error: expected " ++ str ++ " at " ++ (show obj)
