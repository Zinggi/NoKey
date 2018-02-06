// This file has been adapted from https://github.com/perfectapi/CKP/blob/develop/keepass.js
// All credit to the authers.
import ActionOutside from 'action-outside';
import Elm from '../build/apps.js';
import setup from '../../web/setup.js';

const loginInputTypes = ['text', 'email', 'tel'];
const config_passwordInputNames = ['passwd','password','pass'];
const config_loginInputNames = ['login','user','mail','email','username','opt_login','log','usr_name'];
const config_buttonSignUpNames = ['signup', 'sign up', 'register', 'create'];
const config_buttonLogInNames = ['login', 'log in'];


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


const readInputNames = (input) => {
    return [input.name,input.getAttribute('autocomplete'),input.id];
};

const isGoodName = (name, goodNames) => {
    if (!name) return false;
    let nm = name.toLowerCase();
    return goodNames.some((n) => { return nm.indexOf(n.toLowerCase()) >= 0; });
};

const hasGoodName = (fieldNames, goodFieldNames) => {
    return fieldNames.some((fn) => { return isGoodName(fn, goodFieldNames); });
};

const isPasswordInput = (input) => {
    if (input.type === 'password') {
        return true;
    } else if (input.type === 'text') {
        return hasGoodName(readInputNames(input), config_passwordInputNames);
    }
    return false;
};

const isLoginInput = (input) => {
    return (loginInputTypes.indexOf(input.type) >= 0 &&
        hasGoodName(readInputNames(input), config_loginInputNames));
};

const getLoginInputs = () => {
    return [].filter.call(document.getElementsByTagName('input'), isLoginInput);
};

const getPasswordInputs = () => {
    return [].filter.call(document.getElementsByTagName('input'), isPasswordInput);
};

const injectIcon = (isPw, isSignUp, accounts) => {
    return (input) => {
        if (typeof input.noPass_injected !== "undefined")
            return;

        // TODO: remove colors
        if (isSignUp === true) {
            if (isPw) {
                input.style.background = "limegreen";
            } else {
                input.style.background = "green";
            }
        } else if (isSignUp === false) {
            if (isPw) {
                input.style.background = "orange";
            } else {
                input.style.background = "pink";
            }
        } else {
            console.log("we don't know if this is a sign up or login page!");
            return;
        }
        console.log("Inject icon: ", input.id || input.name, "  is pw: ", isPw);
        input.noPass_injected = true;

        const addIcon = isSignUp || (accounts.length !== 0);

        if (addIcon) {
            const iconPath = browser.extension.getURL('icons/library.svg');
            input.style.backgroundRepeat = "no-repeat";
            input.style.backgroundAttachment = "scroll";
            input.style.backgroundSize = "16px 16px";
            input.style.backgroundPosition = "calc(100% - 4px) 50%";
            input.style.backgroundImage = "url('" + iconPath + "')";
            // input.addEventListener("mouseout", (e) => {
            //   if (e.target !== popup_target) resetIcon(e.target);
            // });
            // input.addEventListener("mousemove", onIconHover);
            input.addEventListener("click", (event) => {
                console.log("on icon click", event);
                openPopup(event.target, isPw, isSignUp);
            });
        }
    };
};

const findForms = (logins, pws) => {
    let groups = { forms: [] };
    let i = 0;
    const pushElem = (elem, isLogin, obj) => {
        if (isLogin) {
            obj.logins.push(elem);
        } else {
            obj.pws.push(elem);
        }
    };
    const addFromList = (list, isLogin) => {
        for (const elem of list) {
            const form = elem.form;
            const ind = groups.forms.indexOf(form);
            if (ind != -1) {
                pushElem(elem, isLogin, groups[ind]);
                // groups[ind].push(elem);
            } else {
                // groups[i] = [elem];
                groups[i] = { logins: [], pws: [], form: form };
                pushElem(elem, isLogin, groups[i]);

                groups.forms.push(form);
                i++;
            }
        }
    };
    addFromList(logins, true);
    addFromList(pws, false);
    return groups;
};

const readButtonNames = (input) => {
    return [input.name,input.value,input.id];
};

const isSignUpButton = (button) => {
    return hasGoodName(readButtonNames(button), config_buttonSignUpNames);
};

const isLogInButton = (button) => {
    return hasGoodName(readButtonNames(button), config_buttonLogInNames);
};

const getSubmitButtons = (form) => {
    const submitInputs = form.querySelectorAll("input[type=submit]");
    const submitButtons = form.querySelectorAll("button[type=submit]");
    const otherInputs = form.querySelectorAll("input[type=button]");
    const otherButtons = form.querySelectorAll("button");
    const buttonLikes = form.querySelectorAll("[role=button]");
    const classifyList = (list) => {
        for (const b of list) {
            if (isLogInButton(b)) {
                return [false, b];
            } else if (isSignUpButton(b)) {
                return [true, b];
            }
        }
        return null;
    };
    const collect = (list, fn) => {
        let btns = [];
        let isSignUps = [];
        for (const l of list) {
            const ret = fn(l);
            if (ret !== null) {
                const [isSignUp, btn] = ret;
                btns.push(btn);
                isSignUps.push(isSignUp);
            }
        }
        if (btns.length === 0) {
            return null;
        } else {
            return [isSignUps[0], btns];
        }
    };
    return collect([submitInputs, submitButtons, otherInputs, otherButtons, buttonLikes], classifyList);
};

const classifyGroups = (groups) => {
    for (let i = 0; i < groups.forms.length; i++) {
        const [isSignUp, submitButtons] = getSubmitButtons(groups.forms[i]);
        groups[i].isSignUp = isSignUp;
        groups[i].submitButtons = submitButtons;
        groups[i].mainLogin = groups[i].logins[0];
        groups[i].mainPw = groups[i].pws[0];
    }
    delete groups.forms;
    return groups;
};


const getFormData = (group) => {
    const login = group.mainLogin.value;
    const pw = group.mainPw.value;
    return { password: pw, login: login, site: getCurrentSite(), securityLevel: 2 };
};

const onNodeAdded = (accounts) => () => {
    const logins = getLoginInputs();
    const pws = getPasswordInputs();
    const groups = classifyGroups(findForms(logins, pws));

    const hijackedOnSubmit = (group) => (event) => {
        const data = getFormData(group);
        port.postMessage({ type: "didSubmit", data: data });
    };

    for (const key in groups) {
        const group = groups[key];

        // skip if we already injected the icon
        if (typeof group.form.noPass_injected !== "undefined") continue;
        group.form.noPass_injected = true;

        console.log("group", group);

        group.logins.forEach(injectIcon(false, group.isSignUp, accounts));
        group.pws.forEach(injectIcon(true, group.isSignUp, accounts));

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

const fillCurrentInput = (content) => {
    if (!currentInput)
        return;
    currentInput.value = content;
};

const closePopup = () => {
    popupContainer.style.display = 'none';
    Array.from(popupContainer.children).forEach((c) => { c.style.display = 'none'; });
    actionOutsidePopup.listen(false);
};

const openPopup = (elem, isPw, isSignUp) => {
    if (!popupLoaded) {
        console.log("first");
        popupLoaded = true;
        let [container, elmNodes] = makeContainer();
        popupContainer = container;
        actionOutsidePopup = new ActionOutside(popupContainer, closePopup);
        document.body.appendChild(popupContainer);
        startElm(elmNodes);
    }
    console.log("always???");


    actionOutsidePopup.listen(true);
    showContainer(elem, popupContainer, isPw, isSignUp);

    const rect = elem.getBoundingClientRect();
    moveContainer(rect.bottom, rect.left);
};

const showContainer = (elem, popupContainer, isPw, isSignUp) => {
    popupContainer.style.display = '';
    console.log(popupContainer.children);
    const elementToShow = popupContainer.children[(+isPw)*2 + (+isSignUp)];
    elementToShow.style.display = '';
    currentInput = elem;
};

const startElm = (elmNodes) => {
    console.log(Elm);
    const popup = (node) => {
        const app = Elm.Popup.embed(node);
        // wire up the ports
        port.onMessage.addListener((msg) => {
            if (msg.type == "onNewState") {
                // console.log("(content) got new state", state);
                app.ports.onNewState.send(msg.data);
            }
        });

        app.ports.getState.subscribe(() => {
            // console.log("(content) getState");
            port.postMessage({type: "onStateRequest", data: {}});
        });
        app.ports.sendMsgToBackground.subscribe((msg) =>{
            // console.log("(content) sendMsgToBackground", msg);
            port.postMessage({type: "onReceiveMsg", data: msg});
        });
        return app;
    };
    const pw = (node) => {
        const rands = setup.getRandomInts(9);
        const flags = {
            initialSeed: [rands[0], rands.slice(1)]
        };

        const app = Elm.GeneratePassword.embed(node, flags);
        app.ports.onAcceptPw.subscribe((pw) => {
            console.log("On accept: ", pw);
            closePopup();
            fillCurrentInput(pw);
        });

        return app;
    };
    for (let i = 0; i < elmNodes.length; i++) {
        const app = [popup, popup, popup, pw][i](elmNodes[i]);
    }
};

const moveContainer = (bottom, left) => {
    popupContainer.style.top = (bottom+window.scrollY+10)+"px";
    popupContainer.style.left = (left+window.scrollX)+"px";
};

const makeContainer = () => {
    const container = document.createElement('div');
    container.style.position = "absolute";
    container.style.background = "white";
    container.style.zIndex = "2147483647"; // OVER 9000!!!!
    container.style.boxShadow = "rgba(0, 0, 0, 0.48) 0px 0px 3px 2px";

    const elmNodes = [0,1,2,3].map(() => document.createElement('div'));
    elmNodes.forEach((node) => {
        node.style.display = 'none';
        container.appendChild(node);
    });
    // container.appendChild(elmNode);

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
            console.log("known accounts:", accounts);
            var obs = new MutationObserver(onNodeAdded(accounts));
            obs.observe(document, { childList: true, subtree: true });
            onNodeAdded(accounts)();
        }
    });
};


// TODO: isolate styles:
//  iframes:
//      https://github.com/anderspitman/octopress-blog/blob/dbd21b2a76ea57cf4e967fd44a204c610b35325f/source/_posts/2014-08-04-chrome-extension-content-script-stylesheet-isolation.markdown
//      https://www.sitepoint.com/chrome-extensions-bridging-the-gap-between-layers/
//      https://stackoverflow.com/questions/12783217/how-to-really-isolate-stylesheets-in-the-google-chrome-extension#20241247
//  js lib:
//      https://github.com/liviavinci/Boundary


if (document.readyState === 'complete') {
    onWindowLoad();
} else {
    window.onload = onWindowLoad;
}

