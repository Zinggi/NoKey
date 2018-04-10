'use strict';

require('./main.html');
const Elm = require('./src/Main.elm');
const {setup} = require('./js/setup.js');
const registerServiceWorker = require('./js/service-worker-registration.js');

registerServiceWorker();

setup(Elm.Main.fullscreen, (app) => {
    // remove splash screen
    const el = document.getElementById('splash');
    el.parentNode.removeChild(el);

    // window.elmApp = app;
});

