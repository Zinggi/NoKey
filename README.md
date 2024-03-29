![NoKey](https://raw.githubusercontent.com/Zinggi/NoKey/master/web/staticFiles/imgs/logo.svg?sanitize=true)

NoKey is a distributed password manager that works without a master password.

Instead, you can unlock your passwords by confirming from another device.
E.g. if you need a password on your PC, you only have to confirm this on your phone.
No need to remember any passwords!

## No active development!

**NoKey is not actively developed anymore. I'm not adding any new features and won't be maintaining the apps on the different stores if Google ever decides to remove it for some reason.**

**I'll keep the server running and the web app alive for users that are still using it. (Including me :smile:)**  
I have no idea how many users there are, the server doesn't store anything..

## Screenshots
[![screenshot](https://i.imgur.com/2RVudMw.png)](https://imgur.com/a/sL2jkPu)
(click the image for more)

## Install
**Don't use multiple clients on the same device!**

### Android

[Play Store](https://play.google.com/store/apps/details?id=xyz.nokey.nokey)  

### Chrome Extension

[Chrome Web Store](https://chrome.google.com/webstore/detail/nokey/jfgokfcaagmicdnbebhfccjkbjkdbnjc)

### Firefox Extension

[AMO](https://addons.mozilla.org/en-US/firefox/addon/nokey/)

---

If none of these options work for you, you can try the [web app](https://nokey.xyz/main.html).    
For Safari, there is currently no option that works ([it seems to be missing a few things from the Web Crypto API](https://github.com/Zinggi/NoKey/issues/9)). 


## How does it work?

Your passwords are encrypted using [AES](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) with a randomly generated key, here called group key.
This group key is never stored anywhere directly. Instead, NoKey uses [Shamir's Secret Sharing](https://en.wikipedia.org/wiki/Shamir's_Secret_Sharing)
to split the group key into multiple key shares.
When you confirm you want to unlock a group on another device, the device sends its key to the one that requested it.
Then, if enough keys have been collected, the requester can recover the group key and with that decrypt your stored passwords.

For a more in depth explanation, you can check out the [project report](https://github.com/Zinggi/NoKey/releases/download/0.4.0-docs/Report.pdf).


## FAQ

#### Can your server read my passwords?
> **No**, this is impossible. The server only forwards messages sent between devices.
Passwords are never stored or transmitted in the clear, they always stay fully encrypted.
The only way to decrypt them is by collecting enough keys for a password group.
These keys never leave a device in the clear, they are always encrypted with the public key of the receiver,
such that only that device is able to read them.

> So all the server could do is observe how encrypted passwords and encrypted keys are exchanged, but there is no way to get to those passwords.

#### What about privacy, what information does your server collect?
> **Nothing**. The server doesn't store any information, it doesn't even have a database.
The source code of the server is [here](/server)

#### Ok, but I don't trust you to actually run the same code as available here. What could a malicous server do?
> A malicous server could record every exchanged message.
But, it still woudn't be able to collect any passwords.
It also couldn't alter any of the messages sent between devices, as each message is authenticated and integrity protected.

> However, it could read saved usernames and corresponding sites and this way create some sort of user profile.

> So, if you really don't trust my server, you're welcome to [host it yourself](https://github.com/Zinggi/NoKey/issues/28#issuecomment-396875079).

#### What do the device icons mean? Can I change them?
> See https://github.com/Zinggi/NoKey/issues/27

#### How exactely are passwords propagated and stored?
> See https://github.com/Zinggi/NoKey/issues/28#issuecomment-396868982

#### What data exactely flows through the server?
> See https://github.com/Zinggi/NoKey/issues/28#issuecomment-396872038

#### What crypto keys does NoKey handle and how are they handled?
> See https://github.com/Zinggi/NoKey/issues/28#issuecomment-396865465


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
    + Only increment Android if the Android shell code changed, otherwise, don't do anything about Android.
  * Go through the readmes of `web, android, web_extension`
  * Push changes to github
  * Upload the generated builds on github releases
  * Update links on website
  * Update the server

