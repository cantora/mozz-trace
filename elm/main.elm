import Http
import Json
import JavaScript as JS
import Char
import Graphics.Element
import Color
import open Either
import String
import List

import Trace
import Trace.Error as Error
import Trace.SubRoutine as SubRoutine
import Trace.SubRoutine (SubRoutine)
import Trace.Path (Path, Instr, SubPath)

foreign import jsevent "loadtrace" (Json.toJSObject Json.Null)
  traceData : Signal JS.JSObject

traceJson = Json.fromJSObject <~ traceData

layout trace = flow down [
  header,
  body trace,
  footer
  ]

main = layout <~ traceJson

header = [markdown|
# queen baby legs @(>_<)@

...etc
|]

{-
process : String -> Int -> SubRoutine -> Path String -> String
process str m_idx _ path = 
  String.append str <| case path of
    Instr ii ->
      "i " ++ (show ii) ++ "\n"
    SubPath first [] ->
      first
    SubPath first _ ->
      "__\n" ++ first ++ "^^\n"
-}

process : [Element] -> Int -> SubRoutine -> Path [Element] -> [Element]
process arr m_idx sub path = 
  let
    new_el = case path of
      Instr ii ->
        plainText <| sub.instructions
      SubPath first [] ->
        flow down first
      SubPath first _ ->
        flow down <| [plainText "__\n"] ++ first ++ [plainText "^^\n"]
  in arr ++ [new_el]

thing : Json.JsonValue -> Element
thing trace = 
  let
    fail s = asText <| "failed to process input: " ++ s
    go = Trace.traverse process []
  in Error.try (go trace) (flow down) fail

body trace = flow down [
  thing trace,
  asText "",
  asText trace
  ]

footer = [markdown|
yes... and im the catman ="_"=
|]
