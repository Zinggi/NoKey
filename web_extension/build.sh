#!/bin/bash
cd ../web
elm-make src/Main.elm --output=../web_extension/build/main.js
echo "cp ../web/setup.js ../web_extension/build/setup.js"
cp ../web/setup.js ../web_extension/build/setup.js

cd ../web_extension
elm-make MainBackground.elm --output=../web_extension/build/background.js
elm-make Popup.elm --output=../web_extension/build/popup.js
