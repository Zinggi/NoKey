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


const openPopup = (state) => {
    sendMsgToAll({
        type: "openMainPopup"
    }, state.ports);
};

// console.log(Elm.MainBackground);
setup(Elm.MainBackground.fullscreen, (app) => {
    // console.log("(background) started", app);

    // ports are stored in this object
    let state = {};
    state.ports = {};
    state.hasPopupOpen = false;
    state.previousNotificationsCount = 0;

    // TODO: this is a very primitive way to check if we are running on firefox.
    // It stops working as soon as chrome adds this api too
    const isFirefox = (browser.contentScripts && browser.contentScripts.register) ? true : false;
    // console.log("Is Firefox:", isFirefox);

    browser.runtime.onConnect.addListener((port) => {
        // console.log("(background) port connected", port.name);
        state.ports[port.name] = port;

        port.onMessage.addListener((msg) => {
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
            } else if (msg.type === "closePopup") {
                state.hasPopupOpen = false;
                sendMsgToAll({ type: "closePopup" }, state.ports);
            }
        });
        port.onDisconnect.addListener(() => {
            // console.log("(background) port.onDisconnect " + port.name);
            delete state.ports[port.name];
        });
    });
    browser.tabs.onUpdated.addListener((tabId, info) => {
        if (info.status === 'complete' && state.hasPopupOpen) {
            openPopup(state);
            setTimeout(() => { openPopup(state); }, 500);
            setTimeout(() => { openPopup(state); }, 1000);
        }
    });

    app.ports.accountsForSite.subscribe((accounts) => {
        sendMsgToAll({
            type: "onGetAccountsForSite",
            data: accounts
        }, state.ports);
    });

    // fillForm : { login : String, site : String, password : String } -> Cmd msg
    app.ports.fillForm.subscribe((msg) => {
        // console.log("fill form:", msg);
        sendMsgToAll({ type: "fillForm", data: msg }, state.ports);
    });

    // close all popups
    app.ports.closePopup.subscribe((msg) => {
        sendMsgToAll({ type: "closePopup" }, state.ports);
        state.hasPopupOpen = false;
    });

    app.ports.notificationCount.subscribe((count) => {
        // console.log("notificationCount: ", count);
        browser.browserAction.setBadgeBackgroundColor({color: "red"});
        if (count > 0) {
            browser.browserAction.setBadgeText({text: ""+count});
            // this is the tooltip
            browser.browserAction.setTitle({title: "NoKey: User interaction required"});

            if (state.hasPopupOpen || count <= state.previousNotificationsCount ) {
                state.previousNotificationsCount = count;
                return;
            } else {
                openPopup(state);
                state.hasPopupOpen = true;
            }
        } else {
            browser.browserAction.setBadgeText({text: ""});
            browser.browserAction.setTitle({title: "NoKey"});
        }
        state.previousNotificationsCount = count;
    });

    app.ports.sendOutNewState.subscribe((data) => {
        sendMsgToAll({ type: "onNewState", data: data }, state.ports);
        // console.log("(background) want to sendOutNewState", data);
    });

    app.ports.openExtensionInTab.subscribe((msg) => {
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



