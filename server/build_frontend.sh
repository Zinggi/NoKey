#!/bin/bash
cd ../web
elm-make src/Main.elm --output=../server/priv/static/js/main.js
cp ../web/setup.js ../server/priv/static/js/setup.js

