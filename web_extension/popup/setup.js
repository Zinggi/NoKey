import Elm from '../elm/elm.js';

const app = Elm.Popup.fullscreen();
const {setupDom} = require('../../web/js/setup.js');

setupDom();

const isPopup = !window.menubar.visible;

const port = browser.runtime.connect({name: "popup" + Math.random() });
port.onMessage.addListener((msg) => {
    // console.log("(popup) got new state", state);
    if (msg.type === "onNewState") {
        // console.log("(content) got new state", state);
        app.ports.onNewState.send(msg.data);
    } else if (msg.type === "setSize" && isPopup) {
        window.document.documentElement.setAttribute("style", "");
        window.document.body.setAttribute("style", "");
    } else if (msg.type === "closePopup" && isPopup) {
        window.document.body.setAttribute("style", "width: 780px; height: 580px;");
        window.close();
    }
});

app.ports.getState.subscribe(() => {
    // console.log("(popup) getState");
    port.postMessage({type: "onStateRequest", data: {}});
});
app.ports.sendMsgToBackground.subscribe((msg) =>{
    // console.log("(popup) sendMsgToBackground", msg);
    port.postMessage({type: "onReceiveMsg", data: msg});
});

// Close popup on esc
window.addEventListener('keydown', (e) => {
    if(e.key == 'Escape' || e.key == 'Esc' || e.keyCode == 27) {
        e.preventDefault();
        window.close();
        return false;
    }
}, true);
