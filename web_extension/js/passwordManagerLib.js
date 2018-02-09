/*
 * This library hels when writing password managers.
 * It provides a set of functions to identify and classify input fields.
 *
 * This file has been inspired from https://github.com/perfectapi/CKP/blob/develop/keepass.js
 * All credit to the authers.
 */


const loginInputTypes = ['text', 'email', 'tel'];
const config_passwordInputNames = ['passwd','password','pass'];
const config_loginInputNames = ['login','user','mail','email','username','opt_login','log','usr_name'];
const config_buttonSignUpNames = ['signup', 'sign up', 'register', 'create', 'join'];
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
    return boolWithConfidence(false, -1);
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
    return hasGoodName(readFormNames(form), config_buttonSignUpNames);
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
                console.log("not in a form:", elem);
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
        if (auto === "new-password") {
            return [true, submitButtons];
        } else if (auto === "current-password") {
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
    for (let i = 0; i < groups.forms.length; i++) {
        groups[i].mainLogin = groups[i].logins[0];
        groups[i].mainPw = groups[i].pws[0];

        const [isSignUp, submitButtons] = isSignUpGroup(groups[i]);
        groups[i].isSignUp = isSignUp;
        groups[i].submitButtons = submitButtons;
    }
    delete groups.forms;
    return groups;
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
 * returns {
 *  isSignUp: Bool,
 *  submitButtons: Array DomElement,
 *  mainLogin: DomElement,
 *  mainPw: DomElement,
 *  form: DomElement,
 *  logins: Array DomElement,
 *  pws: Array DomElement
 * }
 */
const classifyForms = () => {
    const logins = getLoginInputs();
    const pws = getPasswordInputs();
    const forms = findForms(logins, pws);
    return classifyGroups(forms);
};


//--------------------------------------------------------------------------------

module.exports = {
    getLoginInputs: getLoginInputs,
    getPasswordInputs: getPasswordInputs,
    getSubmitButtons: getSubmitButtons,
    classifyForms: classifyForms
};

