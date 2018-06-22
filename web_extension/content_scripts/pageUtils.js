import ActionOutside from 'action-outside';
import pwLib from '../js/passwordManagerLib.js';


/*
 * parseUrl("https://gist.github.com/jlong/2428561?foo=bar#test")
 *      =>
 *  {
 *      hash: "#test",
 *      search: "?foo=bar",
 *      pathname: "/jlong/2428561",
 *      port: "",
 *      hostname: "gist.github.com",
 *      host: "gist.github.com",
 *      password: "",
 *      username: "",
 *      protocol: "https:",
 *      origin: "https://gist.github.com",
 *      href: "https://gist.github.com/jlong/2428561?foo=bar#test"
 *  }
 *
 */
const parseUrl = (url) => {
    const link = new URL(url);
    return link;
};


const injectIcon = (isPw, isSignUp, accounts, groupKey) => {
    return (input) => {
        if (typeof input.noPass_injected !== "undefined") return;
        input.noPass_injected = true;
        // TODO: what if the group changes, after a new dom mutation???
        input.noPass_groupKey = groupKey;

        // debug colors
        if (isSignUp === true) {
            // if (isPw) {
            //     input.style.background = "limegreen";
            // } else {
            //     input.style.background = "green";
            // }
        } else if (isSignUp === false) {
            // if (isPw) {
            //     input.style.background = "orange";
            // } else {
            //     input.style.background = "pink";
            // }
        } else {
            // console.log("we don't know if this is a sign up or login page!", input);
            return;
        }

        const addIcon = true; //isSignUp || (accounts.length !== 0);

        if (addIcon) {
            const iconPath = browser.extension.getURL('icons/logo.svg');
            input.style.backgroundRepeat = "no-repeat";
            input.style.backgroundAttachment = "scroll";
            input.style.backgroundSize = "16px 16px";
            input.style.backgroundPosition = "calc(100% - 4px) 50%";
            input.style.backgroundImage = "url('" + iconPath + "')";
            // // Might be usefull:
            // input.addEventListener("mouseout", (e) => {
            //   if (e.target !== popup_target) resetIcon(e.target);
            // });
            // input.addEventListener("mousemove", onIconHover);
            input.addEventListener("click", (event) => {
                // console.log("on icon click", event);
                openPopup(event.target, isPw, isSignUp, accounts);
            });
            input.addEventListener("focus", (event) => {
                openPopup(event.target, isPw, isSignUp, accounts);
            });
        }
    };
};



const getFormData = (group) => {
    // TODO: extract all login values, not just one
    const login = group.login.value;
    const pw = group.password.value;
    return { password: pw, login: login, site: getCurrentSite(), securityLevel: 2 };
};

const onNodeAdded = (accounts) => () => {
    // console.log("onNodeAdded");

    groups = pwLib.classifyForms();
    // console.log("groups:", groups);

    for (const key in groups) {
        const group = groups[key];

        // skip if we already injected the icon
        if (typeof group.form.noPass_injected !== "undefined") continue;
        group.form.noPass_injected = true;

        // console.log("group", group);

        injectIcon(false, group.isSignUp, accounts, key)(group.login);
        injectIcon(true, group.isSignUp, accounts, key)(group.password);
    }
};

let popupLoaded = false;
let popupDisabled = false;
let popupContainer = null;
let actionOutsidePopup = null;
let currentInput = null;
let port = null;
let groups = null;
let currentGroup = null;

const fillCurrentInput = (content) => {
    if (!currentInput) return;

    fillInput(content, currentInput);
};

const fillPassword = (pw) => {
    if (!groups || !currentGroup) {
        fillCurrentInput(pw);
    } else {
        const group = groups[currentGroup];
        group.pws.forEach((elem) => { fillInput(pw, elem); });
    }
};

const fillInput = (txt, elem) => {
    // console.log("XXXX");
    // fill
    elem.value = txt;
    // TODO: we might need to simulate button presses
    // pwLib.simulateEvent('click', elem);
    pwLib.simulateEvent('change', elem);
    pwLib.simulateEvent('mousedown', elem);
    pwLib.simulateKeydown('a');
};

const fillCurrentForm = (msg) => {
    if (!groups || !currentGroup || msg.site !== getCurrentSite()) return;

    closePopup();
    const group = groups[currentGroup];
    group.pws.forEach((elem) => { fillInput(msg.password, elem); });
    group.logins.forEach((elem) => { fillInput(msg.login, elem); });
};

const saveEnteredForm = () => {
    if (!groups || !currentGroup) return;
    const group = groups[currentGroup];
    const entry = getFormData(group);
    const data = { entry: entry, isSignUp: group.isSignUp };
    postMsg({ type: "didSubmit", data: data });
};


const closePopup = () => {
    popupContainer.style.display = 'none';
    Array.from(popupContainer.children).forEach((c) => { c.style.display = 'none'; });
    actionOutsidePopup.listen(false);
};

const closePopupFor = (time) => {
    closePopup();
    disablePopup();
    setTimeout(() => { enablePopup(); }, time);
};

const disablePopup = () => {
    popupDisabled = true;
};
const enablePopup = () => {
    popupDisabled = false;
};


const openPopup = (elem, isPw, isSignUp, accounts) => {
    if (!popupLoaded || popupDisabled) return;

    // Don't allow to immediately close again:
    setTimeout(() => { actionOutsidePopup.listen(true); }, 1000);
    showContainer(elem, popupContainer, isPw, isSignUp, accounts);

    const rect = elem.getBoundingClientRect();
    moveContainer(rect.bottom, rect.left);
};

const showContainer = (elem, popupContainer, isPw, isSignUp, accounts) => {
    popupContainer.style.display = '';
    popupContainer.style.position = "absolute";
    popupContainer.style.right = "";
    popupContainer.style.width = "";
    popupContainer.style.height = "";
    popupContainer.classList.remove("fade-in-no-key");

    const selectElement = () => {
        if (isSignUp && isPw) {
            return "content_scripts/newPassword";
        } else if (!isSignUp && accounts.length > 0) {
            return "content_scripts/fillForm";
        }
    };
    const elementToShow = selectElement();
    let didShow = false;
    for (let child of popupContainer.children) {
        if (elementToShow === child.myId) {
            child.style.display = '';
            didShow = true;
        } else {
            child.style.display = 'none';
        }
    }
    if (!didShow) {
        popupContainer.style.boxShadow = "";
    } else {
        popupContainer.style.boxShadow = "rgba(0, 0, 0, 0.48) 0px 0px 3px 2px";
    }

    currentInput = elem;
    // TODO: if group changes, what happens here?
    currentGroup = elem.noPass_groupKey;
};

const moveContainer = (bottom, left) => {
    popupContainer.style.top = (bottom+window.scrollY+10)+"px";
    popupContainer.style.left = (left+window.scrollX)+"px";
};

const adjustPopupSize = (size) => {
    for (let child of popupContainer.children) {
        if (size.id === child.myId) {
            if (size.width === 0 || size.height === 0) continue;
            child.style.height = size.height + "px";
            child.style.width = size.width + "px";
        }
    }
};

const makeContainer = () => {
    const container = document.createElement('div');
    container.style.position = "absolute";
    // container.style.background = "white";
    container.style.lineHeight = 0;
    container.style.zIndex = "2147483647"; // OVER 9000!!!!
    container.style.boxShadow = "rgba(0, 0, 0, 0.48) 0px 0px 3px 2px";

    // we 'hide' the container, so that the iframes can already preload and figure out their sizes.
    // we can't set display 'none', as then our iframes won't know their size
    container.style.left = "-5000px";
    container.style.top = "-5000px";

    const elmNodes = [["content_scripts/fillForm", ""], ["content_scripts/newPassword", ""], ["popup/main", "?popup=true"]].map((f) => {
        const iframe = document.createElement('iframe');
        iframe.src = browser.extension.getURL(f[0]+".html"+f[1]);
        iframe.frameBorder = 0;
        iframe.style.width = "100%";
        iframe.style.height = "100%";

        container.appendChild(iframe);

        iframe.myId = f[0];

        return iframe;
    });
    // wire up ports to fill form
    // fillForm : { login : String, site : String, password : String } -> Cmd msg
    port.onMessage.addListener((msg) => {
        if (msg.type === "fillForm") {
            // console.log("fill form with:", msg);
            fillCurrentForm(msg.data);
        }
    });

    // return iframe;
    return [container, elmNodes];
};

const openMainPopup = () => {
    popupContainer.style.display = '';
    setTimeout(() => { actionOutsidePopup.listen(true); }, 5000);
    popupContainer.style.top = "30px";
    popupContainer.style.right = "30px";
    popupContainer.style.left = "";
    popupContainer.style.width = "600px";
    popupContainer.style.height = "400px";
    popupContainer.style.position = "fixed";
    popupContainer.classList.add("fade-in-no-key");
    for (let child of popupContainer.children) {
        if ("popup/main" === child.myId) {
            child.style.display = '';
        } else {
            child.style.display = 'none';
        }
    }
    popupContainer.style.boxShadow = "rgba(0, 0, 0, 0.48) 0px 0px 3px 2px";
};

const getCurrentSite = () => {
    return document.location.hostname;
};

const postMsg = (msg) => {
    if (!port) return;
    try {
        port.postMessage(msg);
    } catch (err) {
        // console.log("tried to post msg, but couldn't. msg", msg, "err", err);
    }
};

const onWindowLoad = () => {
    port = browser.runtime.connect({name: window.location.href + Math.random() });
    window.addEventListener("focus", (event) => {
        // console.log("onfocus");
        postMsg({type: "getAccountsForSite", data: getCurrentSite()});
    }, false);
    port.onMessage.addListener((msg) => {
        // console.log("got msg:", msg);
        if (msg.type == "onGetAccountsForSite") {
            const accounts = msg.data;
            // console.log("known accounts:", accounts);
            var obs = new MutationObserver(onNodeAdded(accounts));
            obs.observe(document, { childList: true, subtree: true });
            onNodeAdded(accounts)();

            //listen to messages from iframes
            window.addEventListener("message", (event) => {
                const isExpected = Array.from(popupContainer.children).some((frame) => {
                    return frame.contentWindow === event.source;
                });
                if (!isExpected) return;

                // reply to msg:
                //      event.source.postMessage({}, event.origin);
                const msg = event.data;
                // console.log("msg from iframe:", msg);
                if (msg.type === "closePopup") {
                    closePopup();
                } else if (msg.type === "fillPassword") {
                    fillPassword(msg.data);
                } else if (msg.type === "onSizeChanged") {
                    // console.log("on size change", msg);
                    adjustPopupSize(msg.data);
                } else if (msg.type === "onCloseClicked") {
                    closePopupFor(1000*20);
                } else {
                    console.error("msg type not recognised", msg);
                }

            }, false);
            if (!popupLoaded) {
                // console.log("first");
                popupLoaded = true;
                let [container, elmNodes] = makeContainer();
                popupContainer = container;
                actionOutsidePopup = new ActionOutside(popupContainer, () => {
                    postMsg({type: "closePopup"});
                    closePopup();
                });
                document.body.appendChild(popupContainer);
            }
        } else if (msg.type == "openMainPopup") {
            openMainPopup();
        } else if (msg.type == "closePopup") {
            closePopup();
        }
    });
    postMsg({type: "getAccountsForSite", data: getCurrentSite()});
};


if (document.readyState === 'complete') {
    onWindowLoad();
} else {
    window.addEventListener('load', onWindowLoad, false);
}
window.addEventListener('beforeunload', () => {
    saveEnteredForm();
}, false);


