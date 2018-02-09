
const sendSize = (id) => () => {
    const firstDiv = document.body.children[1];
    if (!firstDiv) return;

    // console.log(firstDiv);
    const width = firstDiv.scrollWidth;
    const height = firstDiv.scrollHeight;
    // console.log(id, width, height);
    // Send height back to parent page
    window.parent.postMessage({ type: "onSizeChanged", data: { height: height, width: width, id: id } }, "*");
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
    document.body.appendChild(div);

    return [port, div];
};

module.exports = setupElm;
