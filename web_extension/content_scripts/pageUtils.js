// This file has been adapted from https://github.com/perfectapi/CKP/blob/develop/keepass.js
// All credit to the authers.
import ActionOutside from 'action-outside';
import Elm from '../Popup.elm';

const loginInputTypes = ['text', 'email', 'tel'];
const config_passwordInputNames = ['passwd','password','pass'];
const config_loginInputNames = ['login','user','mail','email','username','opt_login','log','usr_name'];
const config_buttonSignUpNames = ['signup', 'sign up', 'register', 'create'];
const config_buttonLogInNames = ['login', 'log in'];

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

const injectIcon = (isPw, isSignUp) => {
    return (input) => {
        if (typeof input.noPass_injected !== "undefined") {
            return;
        }
        console.log("Inject icon: ", input.id || input.name, "  is pw: ", isPw);
        input.noPass_injected = true;
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
        }

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
        input.addEventListener("click", onIconClick);
    };
};

const onIconClick = (event) => {
    console.log("on icon click", event);
    openPopup(event.target);
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
                groups[i] = { logins: [], pws: [] };
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

const isSignUp = (form) => {
    const submitInputs = form.querySelectorAll("input[type=submit]");
    const submitButtons = form.querySelectorAll("button[type=submit]");
    const otherInputs = form.querySelectorAll("input[type=button]");
    const otherButtons = form.querySelectorAll("button");
    const buttonLikes = form.querySelectorAll("[role=button]");
    const classifyList = (list) => {
        for (const b of list) {
            if (isLogInButton(b)) {
                return false;
            } else if (isSignUpButton(b)) {
                return true;
            }
        }
        return null;
    };
    const firstNonNull = (list, fn) => {
        for (const l of list) {
            const ret = fn(l);
            if (ret !== null) {
                return ret;
            }
        }
        return null;
    };
    return firstNonNull([submitInputs, submitButtons, otherInputs, otherButtons, buttonLikes], classifyList);
};

const classifyGroups = (groups) => {
    for (let i = 0; i < groups.forms.length; i++) {
        groups[i].isSignUp = isSignUp(groups.forms[i]);
    }
    delete groups.forms;
    return groups;
};

const onNodeAdded = () => {
    const logins = getLoginInputs();
    const pws = getPasswordInputs();
    const groups = classifyGroups(findForms(logins, pws));

    for (const key in groups) {
        const group = groups[key];
        group.logins.forEach(injectIcon(false, group.isSignUp));
        group.pws.forEach(injectIcon(true, group.isSignUp));
    }

};

let popupLoaded = false;
let popupVisible = false;
let popupContainer = null;
let actionOutsidePopup = null;

const openPopup = (elem) => {
    if (!popupLoaded) {
        popupLoaded = true;
        let [container, elmNode] = makeContainer();
        popupContainer = container;
        actionOutsidePopup = new ActionOutside(popupContainer, () => {
            popupVisible = false;
            popupContainer.style.display = 'none';
            actionOutsidePopup.listen(false);
        });
        document.body.appendChild(popupContainer);
        startElm(elmNode);
    }

    popupVisible = true;
    actionOutsidePopup.listen(true);
    popupContainer.style.display = '';

    const rect = elem.getBoundingClientRect();
    moveContainer(rect.bottom, rect.left);
};

const startElm = (elmNode) => {
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
    container.style.padding = "5px";

    const elmNode = document.createElement('div');
    container.appendChild(elmNode);

    return [container, elmNode];
};

const onWindowLoad = () => {
    var obs = new MutationObserver(onNodeAdded);
    obs.observe(document, { childList: true, subtree: true });
    onNodeAdded();
};


if (document.readyState === 'complete') {
    onWindowLoad();
} else {
    window.onload = onWindowLoad;
}

