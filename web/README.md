# NoKey Web

This is the shared part of the password manager.
This website can later be embedded in the browser extension, the android app, etc.

## Develop

### Setup

 * Install [elm](http://elm-lang.org/)
 * Install [elm-github-install](https://github.com/gdotdesign/elm-github-install) (for [elm-phoenix](https://github.com/saschatimme/elm-phoenix))
 * install dependencies: `elm-github-install`
 * install dev dependencies: `yarn`
 * (Optional) to run tests, install [elm-test](https://github.com/elm-community/elm-test/tree/master) and [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples)

### Run

For a dev server, run `yarn dev`, then visit <http://localhost:3001/main.html>

To run tests, run `elm-verify-examples && elm-test`


## Release

 * Bump version in Data/Settings
 * Write release log in Views/ReleaseLog

