module Trace.Util (
  forEachUpToR
  ) where

forEachUpToR : Int -> (Int -> b -> b) -> b -> b
forEachUpToR n fn base =
  let
    recurse i acc = 
      if i >= n
        then acc
        else fn i <| recurse (i+1) acc
  in recurse 0 base
