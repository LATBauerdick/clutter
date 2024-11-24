build:
	spago build

run:
	spago build --output output-es && purs-backend-es bundle-app --main Main --minify --no-build --to ./index.js

.PHONY: build run

