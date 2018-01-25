#!/bin/sh
elm-live --after-build=./buildQuick.sh elm/*.elm --output=build/apps.js &
P0=$!


parcel=./node_modules/.bin/parcel
$parcel watch content_scripts/pageUtils.js --no-hmr --no-cache &
P1=$!
$parcel watch background.js --no-hmr --no-cache &
P2=$!
$parcel watch popup/main.html --no-hmr --no-cache &
P3=$!

wait $P0 $P1 $P2 $P3
