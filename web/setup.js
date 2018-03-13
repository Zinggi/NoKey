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
    storedState(state);
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
