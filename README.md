# NoKey

## Source code organization

This package is organized in these folders:
    * `web/`: Contains the shared elm code + the web app
    * `web_extension/`: Contains the extension code
    * `android/`: Contains the android version
    * `server/`: Contains the elixir code for the server
The server and webextension both require the `web/` folder to be present, as they make use of the common code there.

## Developpment requirements

To run everything, you need [yarn](https://yarnpkg.com/) and [elm](http://elm-lang.org/).

More specific instructions can be found in the corresponding folders.

