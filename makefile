
index.html: resource/elm/main.js resource/elm-runtime.js resource/input.js script/trace-html.sh
	@cat resource/input.js \
		| script/trace-html.sh resource/elm-runtime.js resource/elm/main.js "title" \
		> $@

.PHONY: resource/elm/main.js #let elm determine what needs to be rebuilt
resource/elm/main.js: elm/main.elm
	elm -m --src-dir=elm/ -o $< --build-dir=resource --cache-dir=build


.PHONY: clean
clean:
	rm -f index.html
	rm -f build/elm/Trace/*
	rmdir build/elm/Trace
	rm -f build/elm/*
	rmdir build/elm
	rmdir build
