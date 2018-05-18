import Elm from "../elm/elm.js";
import setup from '../../web/js/setup.js';
import setupElm from './popupHelper.js';


const main = () => {
    const [port, div] = setupElm("newPassword");

    const rands = setup.getRandomInts(9);
    const flags = {
        initialSeed: [rands[0], rands.slice(1)]
    };

    const app = Elm.GeneratePassword.embed(div, flags);
    app.ports.onAcceptPw.subscribe((pw) => {
        // console.log("On accept: ", pw);
        window.parent.postMessage({ type: "closePopup" }, "*");
        window.parent.postMessage({ type: "fillPassword", data: pw }, "*");
    });
};

main();
