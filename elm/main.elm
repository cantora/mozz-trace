import Http
import Json
import JavaScript as JS
import Char
import Graphics.Element
import Color
import open Either

import Trace

-- foreign import jsevent "loadtrace" (JS.fromString "no data")
--  traceData : Signal JS.JSString

--defaultTrace = maybe Json.fromString "{}"
foreign import jsevent "loadtrace" (Json.toJSObject Json.Null)
  traceData : Signal JS.JSObject

traceJson = Json.fromJSObject <~ traceData
--asText <~ traceJson

header = [markdown|
# queen baby legs @(>_<)@

...etc
|]

footer = [markdown|
yes... and im the catman ="_"=
|]

tree : Json.JsonValue -> Element
tree json =
  case (Trace.fromJson json) of
    Right xs -> asText xs
    Left err -> asText err  -- { name="asdf", base=0, instructions=[], path=(Instr 0) }]

layout trace = flow down (header :: (tree trace) :: (asText "") :: (asText trace) :: footer :: [])
main = layout <~ traceJson
