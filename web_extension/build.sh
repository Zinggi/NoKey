#!/bin/bash
cd ../web
elm-make src/Main.elm --output=../web_extension/build/main.js
cp ../web/setup.js ../web_extension/popup/setup.js
