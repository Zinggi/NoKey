'use strict';

require('./main.html');
require('./styles.css');

const handleErrors = require('./js/handleErrors.js');
handleErrors.register();
const Elm = require('./src/Main.elm');
const {setup} = require('./js/setup.js');
const registerServiceWorker = require('./js/service-worker-registration.js');

try {
registerServiceWorker();

setup(Elm.Main.fullscreen, (app) => {
        console.log("setup complete");
        // remove splash screen
        const el = document.getElementById('splash');
        el.parentNode.removeChild(el);
        // window.elmApp = app;
    }, err => onError("", "", "", "", err)
);
} catch (error) {
    // This seems useless, but only this way will we see what error
    // a 'Script error' actually produced.
    console.error(error);
    throw error;
}
