import Http
import Json
import JavaScript as JS
import Char
import Graphics.Element
import Color
import open Either

import Trace

foreign import jsevent "loadtrace" (Json.toJSObject Json.Null)
  traceData : Signal JS.JSObject

traceJson = Json.fromJSObject <~ traceData

tree : Json.JsonValue -> Element
tree json =
  case (Trace.fromJson json) of
    Right x  -> asText x
    Left err -> asText err

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

body trace = flow down [
  tree trace,
  asText "",
  asText trace
  ]

footer = [markdown|
yes... and im the catman ="_"=
|]
