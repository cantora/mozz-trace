module Trace.Json (
  dict,
  list,
  int,
  string,
  lookupString,
  lookupInt,
  lookupDict,
  lookupList
  ) where

import Json
import Dict as DictMod
import Trace.Error (Error, ok, report, (>>=))
import open Either

type Value = Json.JsonValue
type Dict = DictMod.Dict String Value

list : Value -> Error [Value]
list json =
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

lookup : comparable -> Dict -> Error Value
lookup k d =
  case (DictMod.lookup k d) of
    Just v -> ok v
    _      -> report d <| "key=" ++ (show k)

lookupString d x = (lookup x d) >>= string
lookupList   d x = (lookup x d) >>= list
lookupInt    d x = (lookup x d) >>= int
lookupDict   d x = (lookup x d) >>= dict

