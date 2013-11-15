#!/bin/bash

usage() {
  echo "USAGE: $0 RUNTIME JS"
  exit 1
}

if [ ! -f "$1" ]; then
  echo "invalid runtime file $1"
  usage
fi

if [ ! -f "$2" ]; then
  echo "invalid input elm javascript file $2"
  usage
fi

addtabs="sed s/^/\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20/g"

cat <<ENDHERE
<!DOCTYPE HTML>

<html>
    <head>
        <meta charset="UTF-8">
        <title>
            $3
        </title>
        <script type="text/javascript">
ENDHERE

echo; cat $1 | $addtabs; echo

cat <<ENDHERE
        </script>
        <script type="text/javascript">
ENDHERE

echo; cat $2; echo

cat <<ENDHERE
        </script>
        <script type="text/javascript">
ENDHERE

echo
while read line; do 
  echo $line | $addtabs
done
echo

cat <<ENDHERE
        </script>
    </head>
    <body onload="init();">
        <script type="text/javascript">
            var thing = Elm.fullscreen(Elm.Main)
            thing.send('loadtrace', mozz_input)
        </script>
        <noscript>
            
        </noscript>
    </body>
</html>
ENDHERE
