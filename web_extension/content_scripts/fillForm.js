import Elm from "../elm/elm.js";
import setup from '../../web/js/setup.js';
import setupElm from './popupHelper.js';

const main = () => {
    const [port, div] = setupElm("fillForm");

    const app = Elm.FillLogin.embed(div);
    port.onMessage.addListener((msg) => {
        if (msg.type == "onNewState") {
            // console.log("(content) got new state", state);
            app.ports.onNewState.send(msg.data);
        }
    });

    app.ports.getState.subscribe(() => {
        // console.log("(content, fillLogin) getState");
        port.postMessage({type: "onStateRequest", data: {}});
    });
    app.ports.sendMsgToBackground.subscribe((msg) =>{
        // console.log("(content) sendMsgToBackground", msg);
        port.postMessage({type: "onReceiveMsg", data: msg});
    });
    return app;
};

main();
