import { setup } from '../web/js/setup.js';
import Elm from "./elm/elm.js";
// console.log("(background) start background.js");

const sendMsgToAll = (msg, ports) => {
    // console.log("send msg:", msg, "to:", ports);
    for (let key in ports) {
        if (ports[key]) {
            try {
                ports[key].postMessage(msg);
            } catch (err) {
                // console.log("couldn't post msg to", key, "err", err);
            }
        }
    }
};


// console.log(Elm.MainBackground);
setup(Elm.MainBackground.fullscreen, (app) => {
    // console.log("(background) started", app);

    // ports are stored in this object
    let ports = {};
    let lastPort = null;
    let hasPopupOpen = false;
    let previousNotificationsCount = 0;

    // TODO: this is a very primitive way to check if we are running on firefox.
    // It stops working as soon as chrome adds this api too
    const isFirefox = (browser.contentScripts && browser.contentScripts.register) ? true : false;
    // console.log("Is Firefox:", isFirefox);

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
        sendMsgToAll({
            type: "onGetAccountsForSite",
            data: accounts
        }, ports);
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

            if (hasPopupOpen || count == previousNotificationsCount ) {
                return;
            }
            hasPopupOpen = true;
            const popupUrl = browser.extension.getURL("popup/main.html");
            browser.windows.create({
                url: popupUrl+"?popup=true",
                width: 601,
                height: 401,
                type: 'popup'
            }).then((win) => {
                // TODO: This is a workaround for this firefox bug:
                // https://discourse.mozilla.org/t/ff57-browser-windows-create-displays-blank-panel-detached-panel-popup/23644/5
                // This can hopefully be removed at some point
                browser.windows.update(win.id, { width: 600, height: 400 });

                browser.windows.onRemoved.addListener((id) => {
                    // console.log("on remove window");
                    if (id == win.id) {
                        hasPopupOpen = false;
                        // console.log("close popup window");
                    }
                });
            }, (err) => {
                console.error("creating window failed!", err);
            });
        } else {
            browser.browserAction.setBadgeText({text: ""});
            browser.browserAction.setTitle({title: "NoKey"});
            if (hasPopupOpen) {

            }
        }
        previousNotificationsCount = count;
    });

    app.ports.sendOutNewState.subscribe((state) => {
        sendMsgToAll({ type: "onNewState", data: state }, ports);
        // console.log("(background) want to sendOutNewState", state);
    });

    app.ports.openExtensionInTab.subscribe((state) => {
        const popupUrl = browser.extension.getURL("popup/main.html");
        browser.tabs.create({
            url: popupUrl+"?tab=true"
        }).then((win) => {
            // console.log("did it work?");
        }, (err) => {
            console.error("creating window failed!", err);
        });
    });
});



