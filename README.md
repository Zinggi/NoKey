# NoKey

NoKey is a distributed password manager that works without a master password.

Instead, you can unlock your passwords by confirming from another device.
E.g. if you need a password on your PC, you only have to confirm this on your phone.
No need to remember any passwords!

[nokey.xyz](https://nokey.xyz/)

## How does it work?

Your passwords are encrypted using a strong, randomly generated password, here called group password.
This group password is never stored anywhere directly. Instead, NoKey uses [Shamir's Secret Sharing](https://en.wikipedia.org/wiki/Shamir's_Secret_Sharing)
to split the group password into multiple shares (called keys in NoKey).
When you confirm you want to unlock a group on another device, the device sends its key to the one that requested it.
Then, if enough keys have been collected, the requester can recover the group password and with that decrypt your stored passwords.

For a more in depth explanation, you can check out the project report (**coming soon**).

## Source code organization

This package is organized in these folders:

  * `web/`: Contains the shared elm code + the web app
  * `web_extension/`: Contains the extension code
  * `android/`: Contains the android version
  * `server/`: Contains the elixir code for the server

The server and webextension both require the `web/` folder to be present, as they make use of the common code there.

## Development requirements

To run everything, you need [yarn](https://yarnpkg.com/) and [elm](http://elm-lang.org/).

More specific instructions can be found in the corresponding folders.

## Creating a release

  * Grep for `TODO!`s: `rg TODO!` and resolve them
  * Find stray log statements: `rg "^([^/\n]*(console.log|Log\.[de])|[^-]*Debug.log)"` and possibly remove them
  * Increment versions in `build.gradle`, `web/src/Data/Settings.elm` and `web_extension/manifest.json`
  * Go through the readmes of `web, android, web_extension`
  * Push changes to github
  * Upload the generated builds on github releases
  * Update links on website
  * Update the server

