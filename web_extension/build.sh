#!/bin/bash
elm-make elm/*.elm --output=build/apps.js

parcel=./node_modules/.bin/parcel
$parcel build content_scripts/pageUtils.js --no-cache
$parcel build background.js --no-cache
$parcel build popup/main.html --no-cache
$parcel build content_scripts/fillForm.html --no-cache
$parcel build content_scripts/newPassword.html --no-cache
