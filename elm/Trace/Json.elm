module Trace.Json (
  dict,
  array,
  int,
  string
  ) where

import Json
import Dict as DictMod
import Trace.Error (Error, ok, report)
import open Either

type Value = Json.JsonValue
type Dict = DictMod.Dict String Value

array : Value -> Error [Value]
array json =
  case json of
    Json.Array xs -> ok xs
    _             -> report json "array"

dict : Value -> Error Dict
dict json =
  case json of
    Json.Object d -> ok d
    _             -> report json "dict"

int : Value -> Error Int
int json =
  case json of
    Json.Number f -> ok (truncate f)
    _             -> report json "number"

string : Value -> Error String
string json =
  case json of
    Json.String s -> ok s
    _             -> report json "string"

