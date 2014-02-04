module Data.Mozz.Trace (
  fromYaml,
  traverse
  ) where

import Trace.Yaml as Yaml
import Trace.SubRoutine as SubRoutine
import Trace.Error (Error)

fromYaml : Yaml.Value -> Error SubRoutine.SubRoutine
fromYaml yaml =
  Yaml.dict yaml >>= SubRoutine.make

traverse : 
  SubRoutine.TraverseFN b ->   -- function to process. see Trace.SubRoutine
  b ->                         -- base accumulator
  Yaml.Value ->                -- data to traverse
  Error b                      -- result
traverse fn base yaml =
  fromYaml yaml >>= \subroutine ->
  SubRoutine.traverse fn base subroutine
