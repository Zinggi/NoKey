const pw_fields = document.querySelectorAll('input[type=password]');
console.log("candidates: ");
console.log(pw_fields);
for (let ele of pw_fields) {
    console.log("element:");
    console.log(ele);
    ele.style.background = "blueviolet";

    const container = document.createElement('div');
    container.style.position = "absolute";
    const rect = ele.getBoundingClientRect();
    container.style.top = (rect.top+window.scrollY-30)+"px";
    container.style.left = (rect.left+window.scrollX)+"px";
    container.style.background = "white";
    container.style.zIndex = "99";
    container.style.boxShadow = "rgba(0, 0, 0, 0.48) 0px 0px 3px 2px";
    container.style.padding = "5px";
    document.body.appendChild(container);

    const elmNode = document.createElement('div');
    container.appendChild(elmNode);

    // start elm
    const app = Elm.Popup.embed(elmNode);

    // TODO: this doesn't work in a content script, we need to use
    // TODO: since we need to rewrite this anyway, also refactor the popup to use the same method
    // https://developer.chrome.com/extensions/messaging
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

}
