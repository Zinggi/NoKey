
const main = () => {
    for (let ele of findPwFields()) {
        console.log("element:");
        console.log(ele);

        // ele.style.background = "blueviolet";
        const rect = ele.getBoundingClientRect();

        const container = makeContainer(rect.top, rect.left);
        document.body.appendChild(container);

        const elmNode = document.createElement('div');
        container.appendChild(elmNode);

        // start elm
        const app = Elm.Popup.embed(elmNode);

        // wire up the ports
        const port = browser.runtime.connect({name: window.location.href + Math.random() });
        port.onMessage.addListener(function(state) {
            // console.log("(content) got new state", state);
            app.ports.onNewState.send(state);
        });

        app.ports.getState.subscribe(() => {
            // console.log("(content) getState");
            port.postMessage({type: "onStateRequest", data: {}});
        });
        app.ports.sendMsgToBackground.subscribe((msg) =>{
            // console.log("(content) sendMsgToBackground", msg);
            port.postMessage({type: "onReceiveMsg", data: msg});
        });
    }
};

const findPwFields = () => {
    const pw_fields = document.querySelectorAll('input[type=password]');
    console.log("candidates: ");
    console.log(pw_fields);
    return pw_fields;
};

const makeContainer = (top, left) => {
    const container = document.createElement('div');
    container.style.position = "absolute";
    container.style.top = (top+window.scrollY-30)+"px";
    container.style.left = (left+window.scrollX)+"px";
    container.style.background = "white";
    container.style.zIndex = "99";
    container.style.boxShadow = "rgba(0, 0, 0, 0.48) 0px 0px 3px 2px";
    container.style.padding = "5px";
    return container;
}

// main();




