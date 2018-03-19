const crypto = window.crypto || window.msCrypto;

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



const runsInsideExtension = () => {
    if (window.browser && browser.runtime && browser.runtime.id)
        return true;
    return false;
};


// returns a promise with two keys, [encryptionKey, signKey]
// TODO: call this if we haven't stored a key yet or if resetStorage is called
// send the public keys via a flag to elm and put it inside sync data.
// if there is an error anywhere, pass error along to elm inside flag to display error message
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
        window.crypto.subtle.generateKey({
            name: "RSA-PSS",
            modulusLength: 2048,
            publicExponent: new Uint8Array([0x03]),
            hash: { name: "SHA-256" },
        }, true, ["sign", "verify"])
    ]);
};
// TODO: use the given key to encrypt the data.
// Use this via a port, to encrypt shares
const encrypt = (key, data) => {};
// should be used when we want to take out our share
const decrypt = (key, data) => {};

// TODO: sign a message with our key
// should be used in API, before we send out a message
const sign = (key, data) => {};
// should be used when we receive a message to check if it is authentic
const verify = (key, data) => {};

// TODO: save key in storage
// TODO: do this everytime we store the state
const saveKey = (key) => {
    Promise.all([
        crypto.subtle.exportKey("jwk", key.publicKey),
        crypto.subtle.exportKey("jwk", key.privateKey)
    ]).then(([ePubK, ePrivK]) => {
        console.log("exported pub key: ", ePubK);
        console.log("exported priv key: ", ePrivK);
    }).catch((err) => {
        console.error(err);
    });
};
// TODO: get keys from storage
const retrieveKeys = () => {
};





const storeState = (state) => {
    if (runsInsideExtension()) {
        browser.storage.local.set({state: state});
    } else {
        window.localStorage.setItem("state", JSON.stringify(state));
    }
};

const getState = (onGot) => {
    if (runsInsideExtension()) {
        browser.storage.local.get({state: null})
            .then((state) => onGot(state.state))
            .catch(() => {
                console.error("couldn't get storage!");
            });
    } else {
        const state = window.localStorage.getItem("state");
        if (state === null) {
            onGot(null);
        } else {
            onGot(JSON.parse(state));
        }
    }
};

const resetStorage = (state) => {
    if (runsInsideExtension()) {
        browser.storage.local.clear();
    } else {
        window.localStorage.clear();
    }
    storeState(state);
    // TODO: reset keypto keys and generate new ones
};



const getRandomInts = (n) => {
    const randInts = new Uint32Array(n);
    crypto.getRandomValues(randInts);
    return Array.from(randInts);
};



const setup = (startFn, onStart) => {
    getState((state) => {
        // console.log("stored state: ", state);

        // 1 + 8 32bit ints give us a generator of period (2^32)^9bits, which corresponds to (2^8)^36bit,
        // e.g. more than enough for 32 character passwords.
        const rands = getRandomInts(9);

        const flags = {
            initialSeed: [rands[0], rands.slice(1)],
            storedState: state
        };

        // console.log(flags);
        const app = startFn(flags);

        app.ports.setTitle.subscribe((title) => {
            document.title = title;
        });


        app.ports.storeState.subscribe((state) => {
            // console.log("store state: ", state);
            storeState(state);
        });

        app.ports.resetStorage.subscribe(resetStorage);

        if (onStart) {
            onStart(app);
        }
    });
};


if (typeof module === 'undefined') {
    module = {};
}
module.exports = {
    setup: setup,
    getRandomInts: getRandomInts,
    runsInsideExtension: runsInsideExtension
};
