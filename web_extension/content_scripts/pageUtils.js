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

        const addIcon = isSignUp || (accounts.length !== 0);

        if (addIcon) {
            const iconPath = browser.extension.getURL('icons/library.svg');
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
                openPopup(event.target, isPw, isSignUp);
            });
        }
    };
};



const getFormData = (group) => {
    // TODO: extract all login values, not just one
    const login = group.mainLogin.value;
    const pw = group.mainPw.value;
    return { password: pw, login: login, site: getCurrentSite(), securityLevel: 2 };
};

const onNodeAdded = (accounts) => () => {
    // console.log("onNodeAdded");

    groups = pwLib.classifyForms();
    // console.log("groups:", groups);

    const hijackedOnSubmit = (group) => (event) => {
        const entry = getFormData(group);
        const data = { entry: entry, isSignUp: group.isSignUp };
        port.postMessage({ type: "didSubmit", data: data });
    };


    for (const key in groups) {
        const group = groups[key];

        // skip if we already injected the icon
        if (typeof group.form.noPass_injected !== "undefined") continue;
        group.form.noPass_injected = true;

        // console.log("group", group);

        group.logins.forEach(injectIcon(false, group.isSignUp, accounts, key));
        group.pws.forEach(injectIcon(true, group.isSignUp, accounts, key));

        group.form.addEventListener('submit', hijackedOnSubmit(group), false);
        for (const b of group.submitButtons) {
            b.addEventListener('click', hijackedOnSubmit(group), false);
        }
        // TODO: what if we press enter inside the form?
    }
};

let popupLoaded = false;
let popupContainer = null;
let actionOutsidePopup = null;
let currentInput = null;
let port = null;
let groups = null;
let currentGroup = null;

const fillCurrentInput = (content) => {
    if (!currentInput) return;

    currentInput.value = content;
};

const fillCurrentForm = (msg) => {
    if (!groups || !currentGroup || msg.site !== getCurrentSite()) return;

    closePopup();
    const group = groups[currentGroup];
    group.pws.forEach((elem) => elem.value = msg.password);
    group.logins.forEach((elem) => elem.value = msg.login);
};

const closePopup = () => {
    popupContainer.style.display = 'none';
    Array.from(popupContainer.children).forEach((c) => { c.style.display = 'none'; });
    actionOutsidePopup.listen(false);
};

const openPopup = (elem, isPw, isSignUp) => {
    if (!popupLoaded) return;

    actionOutsidePopup.listen(true);
    showContainer(elem, popupContainer, isPw, isSignUp);

    const rect = elem.getBoundingClientRect();
    moveContainer(rect.bottom, rect.left);
};

const showContainer = (elem, popupContainer, isPw, isSignUp) => {
    popupContainer.style.display = '';

    const selectElement = () => {
        if (isSignUp && isPw) {
            return "newPassword";
        } else if (!isSignUp) {
            return "fillForm";
        }
    };
    const elementToShow = selectElement();
    for (let child of popupContainer.children) {
        if (elementToShow === child.myId) {
            child.style.display = '';
        } else {
            child.style.display = 'none';
        }
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

    const elmNodes = ["fillForm", "newPassword"].map((f) => {
        const iframe = document.createElement('iframe');
        iframe.src = browser.extension.getURL("dist/"+f+".html");
        iframe.frameBorder = 0;
        iframe.style.width = "100%";
        iframe.style.height = "100%";

        container.appendChild(iframe);

        iframe.myId = f;

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

const getCurrentSite = () => {
    return document.location.hostname;
};

const onWindowLoad = () => {
    port = browser.runtime.connect({name: window.location.href + Math.random() });
    port.postMessage({type: "getAccountsForSite", data: getCurrentSite()});
    port.onMessage.addListener((msg) => {
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
                } else if (msg.type === "fillCurrentInput") {
                    fillCurrentInput(msg.data);
                } else if (msg.type === "onSizeChanged") {
                    adjustPopupSize(msg.data);
                } else {
                    console.error("msg type not recognised", msg);
                }

            }, false);
            if (!popupLoaded) {
                // console.log("first");
                popupLoaded = true;
                let [container, elmNodes] = makeContainer();
                popupContainer = container;
                actionOutsidePopup = new ActionOutside(popupContainer, closePopup);
                document.body.appendChild(popupContainer);
            }
        }
    });
};


// TODO: isolate styles:
//  iframes:
//      https://github.com/anderspitman/octopress-blog/blob/dbd21b2a76ea57cf4e967fd44a204c610b35325f/source/_posts/2014-08-04-chrome-extension-content-script-stylesheet-isolation.markdown


if (document.readyState === 'complete') {
    onWindowLoad();
} else {
    window.onload = onWindowLoad;
}

