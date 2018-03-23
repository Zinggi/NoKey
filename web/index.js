/* jslint node: true */
'use strict';

const Elm = require('./src/Main.elm');
const setup = require('./js/setup.js');


setup(Elm.Main.fullscreen, (app) => {
    // window.elmApp = app;
});

