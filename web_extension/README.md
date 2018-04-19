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
 * `yarn build`
 * `web-ext build -s addon/`
 * create archive of source code:
    `cd ..`, `git archive master -o web_extension/web-ext-artifacts/nokey-<0.1.0>.source.zip`




