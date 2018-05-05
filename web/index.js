'use strict';

require('./main.html');
require('./styles.css');

const handleErrors = require('./js/handleErrors.js');
handleErrors.register();
const Elm = require('./src/Main.elm');
const {setup} = require('./js/setup.js');
const registerServiceWorker = require('./js/service-worker-registration.js');

try {
    registerServiceWorker(() => {
        let div = document.createElement('div');
        div.id = 'newUpdate';
        div.innerHTML = '<p>New update has been installed. Press ' +
            '<button id="newUpdateButton">Refresh</button> to use the new version.';
        document.getElementsByTagName('body')[0].appendChild(div);
        let btn = document.getElementById('newUpdateButton');
        btn.addEventListener('click', () => {
            // console.log("Refresh clicked!");
            location.reload();
        });
    });

    setup(Elm.Main.fullscreen, (app) => {
            // console.log("setup complete");
            // remove splash screen
            const el = document.getElementById('splash');
            el.parentNode.removeChild(el);
            // window.elmApp = app;
        }, err => handleErrors.onError("", "", "", "", err)
    );
} catch (error) {
    // This seems useless, but only this way will we see what error
    // a 'Script error' actually produced.
    console.error(error);
    throw error;
}
