/*
 * This library hels when writing password managers.
 * It provides a set of functions to identify and classify input fields.
 *
 * This file has been inspired from https://github.com/perfectapi/CKP/blob/develop/keepass.js
 * All credit to the authers.
 *
 * TODO: take from:
 * https://github.com/bitwarden/browser/blob/master/src/content/autofill.js
 */


const loginInputTypes = ['text', 'email', 'tel'];
const config_passwordInputNames = ['passwd','password','pass'];
const config_loginInputNames = ['login','user','mail','email','username','opt_login','log','usr_name', 'benutzer'];
const config_buttonSignUpNames = ['signup', 'sign up', 'register', 'create', 'join', 'get started'];
const config_buttonLogInNames = ['login', 'log in'];



//--------------------------------------------------------------------------------

const readInputNames = (input) => {
    return [input.name, input.getAttribute('autocomplete'), input.id, input.className];
};

const readButtonNames = (input) => {
    return [input.innerText, input.name, input.value, input.id, input.className];
};

const readFormNames = (form) => {
    return [form.name, form.id, form.className];
};
//--------------------------------------------------------------------------------

/*
 * creates a bool with a confidence level.
 * smaller is more confident than high.
 * -1 means absolute certain
*/
const boolWithConfidence = (bool, confidence) => {
    return [bool, confidence];
};
const getBool = (bWithConfidence) => {
    return bWithConfidence[0];
};
const moreLikelyBool = (b1, b2) => {
    if (b1[1] > b2[1]) {
        return b1;
    } else if (b1[1] < b2[1]) {
        return b2;
    } else {
        // same, so lets bias towards the first argument
        return b1;
    }
};


//--------------------------------------------------------------------------------

const isGoodName = (name, goodNames) => {
    if (!name) return false;
    let nm = name.toLowerCase();
    return goodNames.some((n) => { return nm.indexOf(n.toLowerCase()) >= 0; });
};

const hasGoodName = (fieldNames, goodFieldNames) => {
    for (let i = 0; i < fieldNames.length; i++) {
        if (isGoodName(fieldNames[i], goodFieldNames)) {
            return boolWithConfidence(true, i);
        }
    }
    return boolWithConfidence(false, 10);
};

const isPasswordInput = (input) => {
    if (input.type === 'password') {
        return boolWithConfidence(true, -1);
    } else if (input.type === 'text') {
        return hasGoodName(readInputNames(input), config_passwordInputNames);
    }
    return boolWithConfidence(false, -1);
};

const isLoginInput = (input) => {
    if (loginInputTypes.indexOf(input.type) >= 0) {
        return hasGoodName(readInputNames(input), config_loginInputNames);
    }
    return boolWithConfidence(false, -1);
};


const isSignUpButton = (button) => {
    return hasGoodName(readButtonNames(button), config_buttonSignUpNames);
};

const isLogInButton = (button) => {
    return hasGoodName(readButtonNames(button), config_buttonLogInNames);
};

const isSignUpForm = (form) => {
    // if it has two password fields, it's a sign up.
    const allPasswordInputs = form.querySelectorAll('input[type="password"]');
    if (allPasswordInputs.length === 2) {
        return boolWithConfidence(true, -1);
    }

    // Sign up forms usually contain more fields than sign in pages.
    // if it has multiple fields, it might be a sign up field, but we aren't too certain
    let isSign = boolWithConfidence(false, 100);
    if (form.elements.length > 2) {
        isSign = boolWithConfidence(true, 9);
    } else if (form.elements.length > 3) {
        isSign = boolWithConfidence(true, 2);
    }
    const otherGuess = hasGoodName(readFormNames(form), config_buttonSignUpNames);

    return moreLikelyBool(otherGuess, isSign);
};

//--------------------------------------------------------------------------------

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
            if (!form) {
                // console.log("not in a form:", elem);
                continue;
            }
            const ind = groups.forms.indexOf(form);
            if (ind != -1) {
                pushElem(elem, isLogin, groups[ind]);
            } else {
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

const isSignUpGroup = (group) => {
    const [isSignUp, submitButtons] = getSubmitButtons(group.form);
    if (group.mainPw) {
        const auto = group.mainPw.getAttribute("autocomplete");
        // console.log("auto", auto, "pw", group.mainPw);
        if (auto.indexOf("new-password") !== -1) {
            return [true, submitButtons];
        } else if (auto.indexOf("current-password") !== -1) {
            return [false, submitButtons];
        }
    }
    if (isSignUp === null) {
        const isSignUpF = isSignUpForm(group.form);
        return [isSignUpF && isSignUpF[0], submitButtons];
    } else {
        return [isSignUp, submitButtons];
    }
};



const classifyGroups = (groups) => {
    let newGroups = [];
    for (let i = 0; i < groups.forms.length; i++) {
        let g = {
            login: groups[i].logins[0],
            password: groups[i].pws[0],
            form: groups.forms[i],
            pws: groups[i].pws,
            logins: groups[i].logins
        };
        if (!(g.login && g.password)) {
            continue;
        }

        const [isSignUp, submitButtons] = isSignUpGroup(groups[i]);
        g.isSignUp = isSignUp;
        g.submitButtons = submitButtons;
        // groups[i].isSignUp = isSignUp;
        // groups[i].submitButtons = submitButtons;
        newGroups.push(g);
    }
    // delete groups.forms;

    console.log("newGroups", newGroups);
    return newGroups;
};


//--------------------------------------------------------------------------------
// public interface
//--------------------------------------------------------------------------------

const getLoginInputs = () => {
    return [].filter.call(document.getElementsByTagName('input'), (i) => { return getBool(isLoginInput(i)); });
};

const getPasswordInputs = () => {
    return [].filter.call(document.getElementsByTagName('input'), (i) => { return getBool(isPasswordInput(i)); });
};

/*
 * Given a form element, returns all buttons inside the form that appear to act as a submit button.
 * Returns [Bool isSignUp, Array buttons] | null
 *
 * TODO: If we aren't sure what it is yet, try to find the closeset title to
 *          check if it says something with sign up or login.
 * TODO: I probably should rename this, e.g. classify form
 */
const getSubmitButtons = (form) => {
    const submitInputs = form.querySelectorAll("input[type=submit]");
    const submitButtons = form.querySelectorAll("button[type=submit]");
    const otherInputs = form.querySelectorAll("input[type=button]");
    const otherButtons = form.querySelectorAll("button");
    const buttonLikes = form.querySelectorAll("[role=button]");
    const classifyList = (list) => {
        for (const b of list) {
            const isLogin = isLogInButton(b);
            const isSignUp = isSignUpButton(b);
            // console.log("button", b, "isLogin", isLogin, "isSignUp", isSignUp);
            if (isSignUp[1] < isLogin[1]) {
                if (isSignUp[0]) {
                    return [true, b];
                } else if (isLogin[0]) { // definitely not a sign up, but is it really a login?
                    return [false, b];
                } // no sign up but also no login!
            } else if (isLogin[1] < isSignUp[1]) {
                if (isLogin[0]) {
                    return [false, b];
                } else if (isSignUp[0]) {
                    return [true, b];
                } // no login and no sign up!
            } else { // same confidence, now they hopefully don't say conflicting things..
                if (isSignUp[0] && !isLogin[0]) {
                    return [true, b];
                } else if (isLogin[0] && !isSignUp[0]) {
                    return [false, b];
                } // they conflict, we don't know what to do..
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
            return [null, []];
        } else {
            return [isSignUps[0], btns];
        }
    };
    return collect([submitInputs, submitButtons, otherInputs, otherButtons, buttonLikes], classifyList);
};


/*
 * finds all login and sign in forms of the page and classifies them
 *
 * returns [{
 *  isSignUp: Bool,
 *  submitButtons: Array DomElement,
 *  login: DomElement,
 *  password: DomElement,
 *  form: DomElement,
 * }]
 */
const classifyForms = () => {
    const logins = getLoginInputs();
    const pws = getPasswordInputs();
    const forms = findForms(logins, pws);
    // console.log("logins, pws, forms", logins, pws, forms);
    return classifyGroups(forms);
};


//--------------------------------------------------------------------------------

// https://stackoverflow.com/questions/6157929/how-to-simulate-a-mouse-click-using-javascript/6158050#6158050<Paste>
const simulate = (eventName, element) => {
    var options = extend(defaultOptions, arguments[2] || {});
    var oEvent, eventType = null;
    // console.log("??", eventName);

    for (var name in eventMatchers)
    {
        if (eventMatchers[name].test(eventName)) { eventType = name; break; }
    }

    if (!eventType)
        throw new SyntaxError('Only HTMLEvents and MouseEvents interfaces are supported');

    if (document.createEvent)
    {
        oEvent = document.createEvent(eventType);
        if (eventType == 'HTMLEvents')
        {
            oEvent.initEvent(eventName, options.bubbles, options.cancelable);
        }
        else
        {
            oEvent.initMouseEvent(eventName, options.bubbles, options.cancelable, document.defaultView,
            options.button, options.pointerX, options.pointerY, options.pointerX, options.pointerY,
            options.ctrlKey, options.altKey, options.shiftKey, options.metaKey, options.button, element);
        }
        element.dispatchEvent(oEvent);
    }
    else
    {
        options.clientX = options.pointerX;
        options.clientY = options.pointerY;
        var evt = document.createEventObject();
        oEvent = extend(evt, options);
        element.fireEvent('on' + eventName, oEvent);
    }
    return element;
};

const simulateKeydown = (keycode) => {
    var e = new KeyboardEvent( "keydown", { bubbles:true, cancelable:true, char:String.fromCharCode(keycode), key:String.fromCharCode(keycode), shiftKey:false, ctrlKey:false, altKey:false } );
    Object.defineProperty(e, 'keyCode', {get : function() { return this.keyCodeVal; } });
    e.keyCodeVal = keycode;
    document.dispatchEvent(e);
};
const extend = (destination, source) => {
    for (var property in source)
      destination[property] = source[property];
    return destination;
};

const eventMatchers = {
    'HTMLEvents': /^(?:load|unload|abort|error|select|change|submit|reset|focus|blur|resize|scroll)$/,
    'MouseEvents': /^(?:click|dblclick|mouse(?:down|up|over|move|out))$/
};
const defaultOptions = {
    pointerX: 0,
    pointerY: 0,
    button: 0,
    ctrlKey: false,
    altKey: false,
    shiftKey: false,
    metaKey: false,
    bubbles: true,
    cancelable: true
};

//
module.exports = {
    getLoginInputs: getLoginInputs,
    getPasswordInputs: getPasswordInputs,
    getSubmitButtons: getSubmitButtons,
    classifyForms: classifyForms,
    simulateEvent: simulate,
    simulateKeydown: simulateKeydown
};

