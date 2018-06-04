
const sendSize = (id) => () => {
    const firstDiv = document.body.getElementsByTagName('div')[0];
    if (!firstDiv) return;

    // console.log(firstDiv);
    const width = firstDiv.scrollWidth;
    const height = firstDiv.scrollHeight;
    // console.log(id, width, height);
    // Send height back to parent page
    window.parent.postMessage({ type: "onSizeChanged", data: { height: height, width: width, id: id } }, "*");
};

const createCloseButton = (div) => {
    const c = document.createElement("div");
    c.style.position = "absolute";
    c.style.top = "0px";
    c.style.right = "0px";
    c.style.background = "#ff00001a none repeat scroll 0% 0%;";
    c.style.zIndex = "99";
    c.style.width = "32px";
    c.style.height = "24px";
    c.style.fontSize = "20px";
    c.style.textAlign = "center";
    c.style.borderRadius = "3px";
    c.style.border = "#2400004d";
    c.style.borderWidth = "1px";
    c.style.borderStyle = "solid";
    c.style.padding = "1px";
    c.style.margin = "4px";
    c.style.cursor = "pointer";


    c.setAttribute("role", "button");
    c.setAttribute("title", "Close popup and keep it closed for 20 seconds.");
    c.addEventListener("click", () => {
        window.parent.postMessage({ type: "onCloseClicked" }, "*");
    });

    const x = document.createTextNode("X");
    c.appendChild(x);
    div.parentElement.appendChild(c);
};

const setupElm = (id) => {
    const port = browser.runtime.connect({name: window.location.href + Math.random() });

    const observer = new MutationObserver(() => {
        // just to be save, try various timings
        // without this, the height and width can be in an uninitialized state.
        window.requestAnimationFrame(sendSize(id));
        [0, 5, 10, 20, 50, 100, 250, 500].forEach((t) => {
            window.setTimeout(sendSize(id), t);
        });
    });
    // Start observing the target node for configured mutations
    observer.observe(document.body, { childList: true, subtree: true, attributes: true });
    // Just in case we missed a size change..
    window.setInterval(sendSize(id), 1000);

    const div = document.createElement("div");
    div.style.position = "absolute";
    div.style.minWidth = "300px";
    document.body.appendChild(div);

    createCloseButton(div);

    return [port, div];
};

module.exports = setupElm;
