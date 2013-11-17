import Http
import Json
import JavaScript as JS
import Char
import Graphics.Element
import Color
import open Either

import Trace
import Trace.Error as Error

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

thing : Json.JsonValue -> Element
thing trace = 
  let
    fail s = asText <| "failed to process input: " ++ s
    success sub = asText sub
  in Error.try (Trace.fromJson trace) success fail

body trace = flow down [
  thing trace,
  asText "",
  asText trace
  ]

footer = [markdown|
yes... and im the catman ="_"=
|]
