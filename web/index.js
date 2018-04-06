'use strict';

require('./main.html');
const Elm = require('./src/Main.elm');
const {setup} = require('./js/setup.js');
const registerServiceWorker = require('./js/service-worker-registration.js');

registerServiceWorker();

setup(Elm.Main.fullscreen, (app) => {
    // window.elmApp = app;
});

