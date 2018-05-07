import Elm from '../elm/elm.js';

const app = Elm.Popup.fullscreen();
const {setupDom} = require('../../web/js/setup.js');

setupDom();

const getParams = () => {
    const query = window.location.search;
    if (!query) return { };
    return (/^[?#]/.test(query) ? query.slice(1) : query)
        .split('&')
        .reduce((params, param) => {
            let [ key, value ] = param.split('=');
            params[key] = value ? decodeURIComponent(value.replace(/\+/g, ' ')) : '';
            return params;
        }, { });
};
const isPopup = (() => {
    const p = getParams();
    if (p.popup === 'true')
        return true;
    return false;
})();


const port = browser.runtime.connect({name: "popup" + Math.random() });
port.onMessage.addListener((msg) => {
    // console.log("(popup) got new msg", msg);
    if (msg.type === "onNewState") {
        app.ports.onNewState.send(msg.data);
    } else if (msg.type === "closePopup" && isPopup) {
        window.close();
    }
});

if (isPopup) {
    window.document.documentElement.setAttribute("style", "");
    window.document.body.setAttribute("style", "");
}

app.ports.getState.subscribe(() => {
    port.postMessage({type: "onStateRequest", data: {}});
});

app.ports.sendMsgToBackground.subscribe((msg) =>{
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
