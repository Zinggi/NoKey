import { setup } from '../web/js/setup.js';
import Elm from "./elm/elm.js";
// console.log("(background) start background.js");

const sendMsgToAll = (msg, ports) => {
    console.log("send msg:", msg, "to:", ports);
    for (let key in ports) {
        if (ports[key]) {
            ports[key].postMessage(msg);
        }
    }
};


console.log(Elm.MainBackground);
setup(Elm.MainBackground.fullscreen, (app) => {
    // console.log("(background) started", app);

    // ports are stored in this object
    let ports = {};
    let lastPort = null;
    let hasPopupOpen = false;

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
                // console.log("didSubmit", msg.data);
                app.ports.onAddSiteEntry.send(msg.data);
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

    // fillForm : { login : String, site : String, password : String } -> Cmd msg
    app.ports.fillForm.subscribe((msg) => {
        // console.log("fill form:", msg);
        sendMsgToAll({ type: "fillForm", data: msg }, ports);
    });

    // close all popups
    app.ports.closePopup.subscribe((msg) => {
        sendMsgToAll({ type: "closePopup" }, ports);
    });

    app.ports.notificationCount.subscribe((count) => {
        // console.log("notificationCount: ", count);
        browser.browserAction.setBadgeBackgroundColor({color: "red"});
        if (count > 0) {
            browser.browserAction.setBadgeText({text: ""+count});
            // this is the tooltip
            browser.browserAction.setTitle({title: "NoKey: User interaction required"});

            if (hasPopupOpen) {
                return;
            }
            hasPopupOpen = true;
            const popupUrl = browser.extension.getURL("popup/main.html");
            browser.windows.create({
                url: popupUrl,
                width: 600,
                height: 300,
                type: 'popup', // TODO: test difference between:"normal" "popup" ("panel": deprecated on chrome) ("detached_panel" doesn't exist on chrome)
                allowScriptsToClose: true // TODO: make window close on esc and save and forget button: window.close
            }).then((win) => {
                chrome.windows.onRemoved.addListener((id) => {
                    console.log("on remove window");
                    if (id == win.id) {
                        hasPopupOpen = false;
                        console.log("close popup window");
                    }
                });
            }, (err) => {
                console.error("creating window failed!", err);
            });
        } else {
            browser.browserAction.setBadgeText({text: ""});
            browser.browserAction.setTitle({title: "NoKey"});
        }
    });

    app.ports.sendOutNewState.subscribe((state) => {
        sendMsgToAll({ type: "onNewState", data: state }, ports);
        // console.log("(background) want to sendOutNewState", state);
    });
});



