import { setup } from '../web/setup.js';
import Elm from './build/apps.js';

console.log("(background) start background.js");


setup(Elm.MainBackground.worker, (app) => {
    // console.log("(background) started", app);

    // ports are stored in this object
    let ports = {};
    let lastPort = null;

    chrome.runtime.onConnect.addListener(function(port) {
        // console.log("(background) port connected", port.name);
        ports[port.name] = port;
        lastPort = port.name;

        port.onMessage.addListener(function(msg) {
            if (msg.type === "onStateRequest") {
                // console.log("(background) onStateRequest from " + port.name);
                app.ports.onStateRequest.send(msg.data);
            } else if (msg.type === "onReceiveMsg") {
                // console.log("(background) onReceiveMsg from " + port.name, msg.data);
                app.ports.onReceiveMsg.send(msg.data);
            } else if (msg.type === "getAccountsForSite") {
                app.ports.onRequestAccountsForSite.send(msg.data);
            } else if (msg.type === "didSubmit") {
                console.log("didSubmit", msg.data);
                // TODO: ask if we want to store this, e.g. open add new site dialog in popup, then store.
            }
        });
        port.onDisconnect.addListener(() => {
            // console.log("(background) port.onDisconnect " + port.name);
            delete ports[port.name];
        });
    });

    app.ports.accountsForSite.subscribe((accounts) => {
        if (!lastPort) return;
        ports[lastPort].postMessage({
            type: "onGetAccountsForSite",
            data: accounts
        });
    });

    app.ports.sendOutNewState.subscribe((state) => {
        // console.log("(background) want to sendOutNewState", state);
        for (let key in ports) {
            if (ports[key]) {
                // console.log("(background) sent to " + key);
                ports[key].postMessage({
                    type: "onNewState",
                    data: state
                });
            }
        }
    });
});


browser.browserAction.setBadgeText({text: "test"});
browser.browserAction.setBadgeBackgroundColor({color: "green"});
