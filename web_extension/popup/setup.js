const crypto = window.crypto || window.msCrypto;

const getRandomInts = (n) => {
    const randInts = new Uint32Array(n);
    crypto.getRandomValues(randInts);
    return Array.from(randInts);
};

// 1 + 8 32bit ints give us a generator of period (2^32)^9bits, which corresponds to (2^8)^36bit,
// e.g. more than enough for 32 character passwords.
const rands = getRandomInts(9);
const flags = { initialSeed: [rands[0], rands.slice(1)] };
// console.log(flags);
const app = Elm.Main.fullscreen(flags);

app.ports.setTitle.subscribe((title) => {
    document.title = title;
});
