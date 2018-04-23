# The WebExtension for NoKey

## Setup

 * install dependencies:
    `yarn`

 * install dependencies mentioned in '../web/README.md' using `elm-github-install`


## Develop

* watch files:
    `yarn watch`
    `web-ext run -s addon/` ([web-ext](https://developer.mozilla.org/en-US/Add-ons/WebExtensions/Getting_started_with_web-ext))

* build files:
    `yarn build`

https://developer.mozilla.org/en-US/Add-ons/WebExtensions/Debugging


## create a package

 * Increment version in `manifest.json`
 * Remove 'unsafe-eval' from manifest. It's only needed because of how webpack works in dev mode
 * `yarn build`
 * `web-ext build -s addon/`

## Publish
### Firefox
 * https://addons.mozilla.org/en-US/developers/
 * `edit listing` -> `upload new version`
 * download signed version
 * (create archive of source code:)
    `cd ..`, `git archive master -o web_extension/web-ext-artifacts/nokey-<0.1.0>.source.zip`


### Chrome
 * https://chrome.google.com/webstore/developer/dashboard
 * click edit and upload new .zip
 * publish changes, takes ~60min until the store updates




