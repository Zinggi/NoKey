/*// get a single random number
const getRandom32bitInt = () => {
    const randInt = new Uint32Array(1);
    crypto.getRandomValues(randInt);
    return randInt[0];
};
// Not usefull for elm, as the random library only uses 32bit anyway.
const getRandom53bitInt = () => {
    // get two cryptographically secure random ints
    const randInts = new Uint32Array(2);
    crypto.getRandomValues(randInts);


    // convert the two 32 bit ints to one 53 bit int (since that's the max number of bits for javascript ints)
    // taken from https://github.com/heap/cryptohat/blob/v1.0.1/index.js#L194
    const mask = Math.pow(2, 53 - 32) - 1;
    // NOTE: 4294967296 is Math.pow(2, 32). We inline the number to give V8 a
    //       better chance to implement the multiplication as << 32.
    return randInts[0] + (randInts[1] & mask) * 4294967296;
};*/


const copyToClipboard = require('./copyToClipboard.js');
const createObserver = require('./createObserver.js');
const { registerFileUpload } = require('./fileUpload.js');
//--------------------------------------------------------------------------------
// Util lib
//--------------------------------------------------------------------------------

// check if we run inside an extension.
const runsInsideExtension = () => {
    if (window.browser && browser.runtime && browser.runtime.id)
        return true;
    return false;
};


const jsonToUint = (json) => {
    return stringToUint(JSON.stringify(json));
};

const uintToJson = (uint) => {
    return JSON.parse(uintToString(uint));
};

const stringToUint = (input) => {
    return (new TextEncoder()).encode(input);
};

const uintToString = (uintArray) => {
    return (new TextDecoder()).decode(uintArray);
};

const arrayBufferToBase64 = (buff) => {
    return btoa(String.fromCharCode.apply(null, new Uint8Array(buff)));
};

const base64toArrayBuffer = (str) => {
    return Uint8Array.from(atob(str), c => c.charCodeAt(0));
};


//--------------------------------------------------------------------------------
// Crypto lib
//--------------------------------------------------------------------------------
const crypto = window.crypto || window.msCrypto;


// returns a promise with two keys, [encryptionKey, signKey]
// call this if we haven't stored a key yet
// TODO: if there is an error anywhere, pass error along to elm inside flag to display error message
const genKeys = () => {
    // console.log("gen RSA-OAEP key: (encryption):");
    // TODO: should use this polyfill
    // https://github.com/PeculiarVentures/webcrypto-liner
    return Promise.all([
        crypto.subtle.generateKey({
            name: "RSA-OAEP",
            modulusLength: 2048,
            publicExponent: new Uint8Array([0x03]),
            hash: { name: "SHA-256" }
        }, true, ["encrypt", "decrypt"]),
        crypto.subtle.generateKey({
            name: "RSA-PSS",
            modulusLength: 2048,
            publicExponent: new Uint8Array([0x03]),
            hash: { name: "SHA-256" },
        }, true, ["sign", "verify"])
    ]);
};

// Use the given key to encrypt the data.
// Use this via a port, to encrypt shares
// Can only encrypt small data!
const encrypt = (publicKey, data) => {
    const imported = crypto.subtle.importKey(
        "jwk", publicKey, { name: "RSA-OAEP", hash: {name: "SHA-256"} }, true, ["encrypt"]
    );
    return imported.then((key) => {
        const buffer = jsonToUint(data);
        return crypto.subtle.encrypt({ name: "RSA-OAEP" }, key, buffer).then((enc) => {
            return arrayBufferToBase64(enc);
        });
    });
};
// should be used when we want to take out our share
const decrypt = (privateKey, data) => {
    return crypto.subtle.decrypt({ name: "RSA-OAEP" }, privateKey, base64toArrayBuffer(data)).then((dec) => {
        return uintToJson(dec);
    });
};

// Sign a message with our key
// should be used in API, before we send out a message
const sign = (privateKey, data) => {
    const signedData = JSON.stringify(data);
    return crypto.subtle.sign({ name: "RSA-PSS", saltLength: 128 }, privateKey, stringToUint(signedData)).then((signature) => {
        return { signature : arrayBufferToBase64(signature), signedData: signedData };
    });
};
// should be used when we receive a message to check if it is authentic
const verify = (publicKey, signature, data) => {
    return crypto.subtle.importKey(
        "jwk", publicKey, { name: "RSA-PSS", hash: {name: "SHA-256"} }, true, ["verify"]
    ).then((key) => {
        return crypto.subtle.verify({ name: "RSA-PSS", saltLength: 128 }, key, base64toArrayBuffer(signature), stringToUint(data)).then((isAuthentic) => {
            const parsedData = JSON.parse(data);
            return { data: parsedData, isAuthentic: isAuthentic };
        });
    });
};

// store state + keys in storage
// for keys, we can't store them directely, we can only store the exported keys
const storeState = (state, keys) => {
    const forStorageKeys = {
        encryptionKey: {
            exportedPublic: keys.encryptionKey.exportedPublic,
            exportedPrivate: keys.encryptionKey.exportedPrivate
        },
        signingKey: {
            exportedPublic: keys.signingKey.exportedPublic,
            exportedPrivate: keys.signingKey.exportedPrivate
        }
    };
    if (runsInsideExtension()) {
        browser.storage.local.set({ state: state, keys: forStorageKeys });
    } else {
        window.localStorage.setItem("state", JSON.stringify(state));
        window.localStorage.setItem("keys", JSON.stringify(forStorageKeys));
    }
};

// create new keys
const genNewKeys = (onGot) => {
    return genKeys().then(([encryptionKey, signKey]) => {
        return Promise.all([
            crypto.subtle.exportKey("jwk", encryptionKey.privateKey),
            crypto.subtle.exportKey("jwk", encryptionKey.publicKey),
            crypto.subtle.exportKey("jwk", signKey.privateKey),
            crypto.subtle.exportKey("jwk", signKey.publicKey),
        ]).then(([encPr, encPu, signPr, signPu]) => {
            return {
                encryptionKey: {
                    public: encryptionKey.publicKey,
                    private: encryptionKey.privateKey,
                    exportedPublic: encPu,
                    exportedPrivate: encPr
                },
                signingKey: {
                    public: signKey.publicKey,
                    private: signKey.privateKey,
                    exportedPublic: signPu,
                    exportedPrivate: signPr
                }
            };
        });
    });
};

// gen new keys if keys === null, else import them
const genOrRestoreKeys = (keys) => {
    if (keys === null) {
        return genNewKeys();
    } else {
        return importKeys(keys);
    }
};

const importKeys = (keys) => {
    return Promise.all([
        crypto.subtle.importKey("jwk", keys.encryptionKey.exportedPublic, { name: "RSA-OAEP", hash: {name: "SHA-256"} }, true, ["encrypt"]),
        crypto.subtle.importKey("jwk", keys.encryptionKey.exportedPrivate, { name: "RSA-OAEP", hash: {name: "SHA-256"} }, true, ["decrypt"]),
        crypto.subtle.importKey("jwk", keys.signingKey.exportedPublic, { name: "RSA-PSS", hash: {name: "SHA-256"} }, true, ["verify"]),
        crypto.subtle.importKey("jwk", keys.signingKey.exportedPrivate, { name: "RSA-PSS", hash: {name: "SHA-256"} }, true, ["sign"])
    ]).then(([pubEnc, privDec, pubVeri, privSign]) => {
        return {
            encryptionKey: {
                private: privDec,
                public: pubEnc,
                exportedPublic: keys.encryptionKey.exportedPublic,
                exportedPrivate: keys.encryptionKey.exportedPrivate
            },
            signingKey: {
                private: privSign,
                public: pubVeri,
                exportedPublic: keys.signingKey.exportedPublic,
                exportedPrivate: keys.signingKey.exportedPrivate
            }
        };
    });
};


const hashPassword = (pw, salt, itterations) => {
    const encodedPw = stringToUint(pw);
    const saltBuffer = stringToUint(salt);
    return window.crypto.subtle.importKey(
        'raw', encodedPw, {name: 'PBKDF2'}, false, ['deriveBits', 'deriveKey']
    ).then((key) => {
        return window.crypto.subtle.deriveKey({
                "name": 'PBKDF2', "salt": saltBuffer,
                // TODO: how high do we go? Think about mobile...
                "iterations": itterations,
                "hash": 'SHA-256'
            }, key,
            // For this we don't actually need a cipher suite,
            // but the api requires that it must be specified..
            { "name": 'AES-CBC', "length": 256 }, true,
            [ "encrypt", "decrypt" ]
        );
    }).then((webKey) => {
        return crypto.subtle.exportKey("raw", webKey);
    }).then((buffer) => {
        return uintToString(buffer);
    });
};





// retrive state and keys or if no keys are stored yet, generate new ones
const getState = (onGot, onError) => {
    if (runsInsideExtension()) {
        browser.storage.local.get({state: null, keys: null})
            .then((state) => {
                return genOrRestoreKeys(state.keys).then((keys) => {
                    onGot(state.state, keys);
                });
            }).catch((err) => {
                if (onError) onError(err);
                // TODO: pipe that to elm
            });
    } else {
        let state = window.localStorage.getItem("state");
        let keys = window.localStorage.getItem("keys");
        if (state !== null) {
            state = JSON.parse(state);
        } if (keys !== null) {
            keys = JSON.parse(keys);
        }
        genOrRestoreKeys(keys).then((myKeys) => {
            onGot(state, myKeys);
        }).catch((err) => {
            if (onError) onError(err);
        });
    }
};

// reset storage, and store the newly provided state.
// This keeps the old keys (since the 'old' device no longer exists)
const resetStorage = (state, keys) => {
    if (runsInsideExtension()) {
        browser.storage.local.clear();
    } else {
        window.localStorage.clear();
    }
    storeState(state, keys);
};



//--------------------------------------------------------------------------------
// Random lib
//--------------------------------------------------------------------------------

const getRandomUints = (n) => {
    const randInts = new Uint32Array(n);
    crypto.getRandomValues(randInts);
    return randInts;
};

const getRandomInts = (n) => {
    const randInts = new Uint32Array(n);
    crypto.getRandomValues(randInts);
    return Array.from(randInts);
};


// Setup


const setupAndroid = (app) => {
    app.ports.scanQR.subscribe(() => {
        // console.log("scan QR called!!!");
        if (window.Android.scanQR)
            window.Android.scanQR();
    });

    window.Android.fromAndroid = (msg) => {
        if (msg.type == 'QrResult') {
            // console.log("Got QR: ", msg.data);
            app.ports.onGotQR.send(msg.data);
        }
    };
};


const setupDom = (app) => {
    // console.log("register mutation observer");
    const observer = createObserver([{
            selector: '.copy-to-clipboard',
            onMount(node) {
                // console.log("mount btn", node);
                node.addEventListener('click', () => {
                    // console.log("copy to clipboard");
                    const txt = node.getAttribute('data-txt');
                    copyToClipboard(txt);
                });
            }
            // onUnmount(node) {
            //     console.log("unmount btn", node);
            // }
        }, {
            selector: '.file-upload',
            onMount(node) {
                // console.log("should register file upload");
                registerFileUpload(node, (data) => {
                    app.ports.onFileContentRead.send(data);
                });
            }
            // onUnmount(node) {
            //     console.log("unmount file");
            // }
        }
    ]);

    return observer;
};


const setup = (startFn, onStart, onError) => {
    if (crypto === undefined) {
        throw Error("window.crypto not available");
    } else if (crypto && crypto.subtle === undefined) {
        throw Error("window.crypto.subtle not available");
    }
    getState((state, keys) => {
        // 1 + 8 32bit ints give us a generator of period (2^32)^9bits, which corresponds to (2^8)^36bit,
        // e.g. more than enough for 32 character passwords.
        const rands = getRandomInts(9);

        let deviceTypeString = "Browser";
        let androidShellVersion = null;
        if (typeof window.Android !== 'undefined') {
            deviceTypeString = "Android";
        } else if (runsInsideExtension()) {
            deviceTypeString = "WebExtension";
        }
        let deviceType = { type: deviceTypeString };
        if (deviceTypeString === "Android" && window.Android.getAndroidShellVersion) {
            deviceType.version = window.Android.getAndroidShellVersion();
        }
        // console.log("device type:", deviceType);

        const flags = {
            initialSeed: [rands[0], rands.slice(1)],
            storedState: state,
            encryptionKey: keys.encryptionKey.exportedPublic,
            signingKey: keys.signingKey.exportedPublic,
            deviceType: deviceType
        };

        // This was only for testing
        /*window.testCrypto = {
            sign: (data) => sign(keys.signingKey.private, data),
            verify: (signature, data) => verify(keys.signingKey.exportedPublic, signature, data),
            encrypt: (data) => encrypt(keys.encryptionKey.exportedPublic, data),
            decrypt: (data) => decrypt(keys.encryptionKey.private, data),
            keys: keys
        };*/

        // console.log(flags);
        const app = startFn(flags);

        app.ports.setTitle.subscribe((title) => {
            document.title = title;
        });


        app.ports.storeState.subscribe((state) => {
            // console.log("store state: ", state);
            storeState(state, keys);
        });

        app.ports.resetStorage.subscribe((state) => {
            resetStorage(state, keys);
        });


        // Crypto stuff

        // port verifyAuthenticity : { time : Time, from : String, data : Value, signature : Value, key : Value } -> Cmd msg
        // port onAuthenticatedMsg : ({ data : Value, isAuthentic : Bool, time : Time, from : String } -> msg) -> Sub msg
        app.ports.verifyAuthenticity.subscribe((msg) => {
            // console.log("verify authenticity", msg);
            verify(msg.key, msg.signature, msg.data).then((res) => {
                // console.log("verified", res);
                app.ports.onAuthenticatedMsg.send(
                    { data: res.data, isAuthentic: res.isAuthentic, time: msg.time, from: msg.from }
                );
            });
        });

        // port getSignatureForMsg : { msg : Value, otherId : String } -> Cmd msg
        // port onSignedMsg : ({ data : Value, signature : Value, otherId : String } -> msg) -> Sub msg
        app.ports.getSignatureForMsg.subscribe((data) => {
            // console.log("get signature for msg", data);
            sign(keys.signingKey.private, data.msg).then((res) => {
                // console.log("signed msg", res);
                app.ports.onSignedMsg.send({ data: res.signedData, signature: res.signature, otherId: data.otherId });
            });
        });

        // port decryptMyShares : List ( GroupId, Value ) -> Cmd msg
        // port onReceiveMyShares : (List ( GroupId, Value ) -> msg) -> Sub msg
        app.ports.decryptMyShares.subscribe((shares) => {
            // console.log("should decrypt my shares:", shares);
            const cmds = shares.map((idShare) => {
                let share = idShare[1];
                return decrypt(keys.encryptionKey.private, share.y).then((newY) => {
                    share.y = newY;
                    return [idShare[0], share];
                });
            });
            Promise.all(cmds).then((shares) => {
                // console.log("decrypted my shares:", shares);
                app.ports.onReceiveMyShares.send(shares);
            });
        });

        // port encryptNewShares : { time : Time, groupId : GroupId, shares : List ( DeviceId, ( Value, Value ) ) } -> Cmd msg
        // port onNewEncryptedShares : ({ time : Time, groupId : GroupId, shares : List ( DeviceId, Value ) } -> msg) -> Sub msg
        app.ports.encryptNewShares.subscribe((msg) => {
            // console.log("should encrypt new shares:", msg);
            const cmds = msg.shares.map((idKeyShare) => {
                let share = idKeyShare[1][1];
                return encrypt(idKeyShare[1][0], share.y).then((yEnc) => {
                    share.y = yEnc;
                    return [idKeyShare[0], share];
                });
            });
            Promise.all(cmds).then((shares) => {
                // console.log("new shares encrypted:", shares);
                app.ports.onNewEncryptedShares.send({ time: msg.time, groupId: msg.groupId, shares: shares });
            });
        });

        // port encryptShares : { shares : List (GroupId, Value), publicKey : Value, deviceId : DeviceId, reqIds : Value } -> Cmd msg
        // port onDidEncryptShares : ({ deviceId : DeviceId, encryptedShares : Value, reqIds : Value } -> msg) -> Sub msg
        app.ports.encryptShares.subscribe((msg) => {
            // console.log("should encrypt shares:", msg);

            const cmds = msg.shares.map((idShare) => {
                let share = idShare[1];
                return encrypt(msg.publicKey, share.y).then((yEnc) => {
                    share.y = yEnc;
                    return [idShare[0], share];
                });
            });
            Promise.all(cmds).then((shares) => {
                // console.log("new shares encrypted:", shares);
                app.ports.onDidEncryptShares.send({ encryptedShares: shares, deviceId: msg.deviceId, reqIds: msg.reqIds });
            });
        });

        // port decryptRequestedShares : { ids : List String, shares : Value, time : Time, otherId : DeviceId } -> Cmd msg
        // port onDidDecryptRequestedShares : ({ shares : Value, time : Time, otherId : DeviceId, ids : List String } -> msg) -> Sub msg
        app.ports.decryptRequestedShares.subscribe((msg) => {
            // console.log("should decrypt requested shares", msg);

            const cmds = msg.shares.map((idShare) => {
                let share = idShare[1];
                return decrypt(keys.encryptionKey.private, share.y).then((newY) => {
                    share.y = newY;
                    return [idShare[0], share];
                });
            });
            Promise.all(cmds).then((decShares) => {
                // console.log("requested shares decrypted", decShares);
                app.ports.onDidDecryptRequestedShares.send({ shares: decShares, time: msg.time, otherId: msg.otherId, ids: msg.ids });
            });
        });


        // port hashPwFirst : { password : String, name : String, itterations : Int } -> Cmd msg
        // port didHashPwFirst : ({ name : String, key : String, salt : String, passwordHash : String, hashSalt : String } -> msg) -> Sub msg
        app.ports.hashPwFirst.subscribe((msg) => {
            const salt = uintToString(getRandomUints(8));
            const hashSalt = uintToString(getRandomUints(8));
            Promise.all([hashPassword(msg.password, salt, msg.itterations), hashPassword(msg.password, hashSalt, msg.itterations)]).then(([key, passwordHash]) => {
                app.ports.didHashPwFirst.send({
                    name: msg.name, key: key, salt: salt, passwordHash: passwordHash, hashSalt: hashSalt, time: Date.now()
                });
            });
        });

        // port openBox : { boxId : KeyBoxId, salt: String, hashSalt: String, password : String, itterations : Int } -> Cmd msg
        // port onDidOpenBox : ({ boxId : KeyBoxId, key : String, passwordHash : String, time : Time } -> msg) -> Sub msg
        app.ports.openBox.subscribe((msg) => {
            // performance.mark("hashPw-start");
            // console.log("open box", msg);
            Promise.all([hashPassword(msg.password, msg.salt, msg.itterations), hashPassword(msg.password, msg.hashSalt, msg.itterations)])
                .then(([key, passwordHash]) => {
                    app.ports.onDidOpenBox.send({
                        boxId: msg.boxId, key: key, passwordHash: passwordHash, time: Date.now()
                    });
                    // performance.mark("hashPw-end");
                    // performance.measure(
                        // "hashPw",
                        // "hashPw-start",
                        // "hashPw-end"
                    // );
                    // console.log(performance.getEntriesByName("hashPw"));
                }
            );
        });

        // when we reconnect, ask others if there is a new version
        window.addEventListener('online', (e) => {
            // console.log('online');
            app.ports.onGotOnlineStatus.send(true);
        });
        window.addEventListener('offline', (e) => {
            // console.log('online');
            app.ports.onGotOnlineStatus.send(false);
        });

        // make copyToClipboard work.
        setupDom(app);

        if (deviceTypeString === "Android") {
            setupAndroid(app);
        }

        if (onStart) {
            onStart(app);
        }
    }, onError);
};



module.exports = {
    setup: setup,
    setupDom: setupDom,
    getRandomInts: getRandomInts,
    runsInsideExtension: runsInsideExtension
};
