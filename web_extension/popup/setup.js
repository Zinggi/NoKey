const app = Elm.Popup.fullscreen();
const bgWindow = browser.extension.getBackgroundPage();
const backgroundApp = bgWindow.backgroundApp;

backgroundApp.onSendOutNewState = (state) => {
    // console.log("got new state, sending to app:", state);
    app.ports.onNewState.send(state);
};

app.ports.getState.subscribe(() => {
    // console.log("ask background for its state");
    backgroundApp.ports.onStateRequest.send({});
});
app.ports.sendMsgToBackground.subscribe((msg) => {
    // console.log("send msg to background:", msg);
    backgroundApp.ports.onReceiveMsg.send(msg);
});

