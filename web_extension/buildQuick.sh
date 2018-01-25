#!/bin/bash
parcel=./node_modules/.bin/parcel
$parcel build content_scripts/pageUtils.js --no-cache --no-minify
$parcel build background.js --no-cache --no-minify
$parcel build popup/main.html --no-cache --no-minify
