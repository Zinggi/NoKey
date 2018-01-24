#!/bin/bash
parcel=./node_modules/.bin/parcel
$parcel build content_scripts/pageUtils.js --no-cache
$parcel build background.js --no-cache
$parcel build popup/main.html --no-cache
