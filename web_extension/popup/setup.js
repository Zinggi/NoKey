const app = Elm.Popup.fullscreen();

const port = browser.runtime.connect({name: "popup"});
port.onMessage.addListener(function(state) {
    // console.log("(popup) got new state", state);
    app.ports.onNewState.send(state);
});

app.ports.getState.subscribe(() => {
    // console.log("(popup) getState");
    port.postMessage({type: "onStateRequest", data: {}});
});
app.ports.sendMsgToBackground.subscribe((msg) =>{
    // console.log("(popup) sendMsgToBackground", msg);
    port.postMessage({type: "onReceiveMsg", data: msg});
});
