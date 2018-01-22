// This file has been adapted from https://github.com/perfectapi/CKP/blob/develop/keepass.js
// All credit to the authers.
var loginInputTypes = ['text', 'email', 'tel'];
var config_passwordInputNames = ['passwd','password','pass'];
var config_loginInputNames = ['login','user','mail','email','username','opt_login','log','usr_name'];
function readInputNames(input) {
    return [input.name,input.getAttribute('autocomplete'),input.id];
}

function isGoodName(name, goodNames) {
    if (!name) return false;
    let nm = name.toLowerCase();
    return goodNames.some((n) => { return nm.indexOf(n.toLowerCase()) >= 0; });
}

function hasGoodName(fieldNames, goodFieldNames) {
    return fieldNames.some((fn) => { return isGoodName(fn, goodFieldNames); });
}

function isPasswordInput(input) {
    if (input.type === 'password') {
        return true;
    } else if (input.type === 'text') {
        return hasGoodName(readInputNames(input), config_passwordInputNames)
    }
    return false;
}

function isLoginInput(input) {
    return (loginInputTypes.indexOf(input.type) >= 0 &&
        hasGoodName(readInputNames(input), config_loginInputNames));
}

function getLoginInputs() {
    return [].filter.call(document.getElementsByTagName('input'), isLoginInput);
}

function getPasswordInputs() {
    return [].filter.call(document.getElementsByTagName('input'), isPasswordInput);
}

function injectIcon(isPw) {
    return (input) => {
        if (typeof input.passff_injected !== "undefined") {
            return;
        }
        console.log("Inject icon: ", input.id || input.name, "  is pw: ", isPw);
        input.passff_injected = true;
        if (isPw) {
            input.style.background = "limegreen";
        } else {
            input.style.background = "pink";
        }
        // input.style.backgroundRepeat = "no-repeat";
        // input.style.backgroundAttachment = "scroll";
        // input.style.backgroundSize = "16px 16px";
        // input.style.backgroundPosition = "calc(100% - 4px) 50%";
        // input.style.backgroundImage = "url('" + passff_icon_light + "')";
        // input.addEventListener("mouseout", (e) => {
        //   if (e.target !== popup_target) resetIcon(e.target);
        // });
        // input.addEventListener("mousemove", onIconHover);
        // input.addEventListener("click", onIconClick);
    }
}

function onNodeAdded() {
    getLoginInputs().forEach(injectIcon(false));
    getPasswordInputs().forEach(injectIcon(true));
}

function onWindowLoad() {
    var obs = new MutationObserver(onNodeAdded);
    obs.observe(document, { childList: true, subtree: true });
    onNodeAdded();
}


if (document.readyState === 'complete') {
    onWindowLoad();
} else {
    window.onload = onWindowLoad;
}
