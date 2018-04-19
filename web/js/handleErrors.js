
let stringifyOnce = function(obj, replacer, indent) {
    var printedObjects = [];
    var printedObjectKeys = [];

    function printOnceReplacer(key, value){
        if ( printedObjects.length > 2000){ // browsers will not print more than 20K, I don't see the point to allow 2K.. algorithm will not be fast anyway if we have too many objects
        return 'object too long';
        }
        var printedObjIndex = false;
        printedObjects.forEach(function(obj, index){
            if(obj===value){
                printedObjIndex = index;
            }
        });

        if ( key == ''){ //root element
             printedObjects.push(obj);
            printedObjectKeys.push("root");
             return value;
        }
        if (value instanceof Error) {
            var error = {};

            Object.getOwnPropertyNames(value).forEach(function (key) {
                error[key] = value[key];
            });
            return error;
        }

        else if(printedObjIndex+"" != "false" && typeof(value)=="object"){
            if ( printedObjectKeys[printedObjIndex] == "root"){
                return "(pointer to root)";
            }else{
                return "(see " + ((!!value && !!value.constructor) ? value.constructor.name.toLowerCase()  : typeof(value)) + " with key " + printedObjectKeys[printedObjIndex] + ")";
            }
        }else{

            var qualifiedKey = key || "(empty key)";
            printedObjects.push(value);
            printedObjectKeys.push(qualifiedKey);
            if(replacer){
                return replacer(key, value);
            }else{
                return value;
            }
        }
    }
    return JSON.stringify(obj, printOnceReplacer, indent);
};



const onError = (message, url, line, column, error) => {
    document.body.innerHTML = '';
    let txt = '';
    try {
        txt = stringifyOnce(
            { message: message, url: url, line: line, column: column, error: error, log: log, errors: logE,
              nav: window.navigator.userAgent, other: message.message
            }
            , null, 2
        );
    } catch (error) {
        txt = error.toString();
    }

    const txtN = document.createTextNode(txt);
    let pre = document.createElement('pre');
    pre.appendChild(txtN);
    document.body.appendChild(pre);
    console.error(message, url, line, column, error, log, logE);
    return false;
}


let log = [];
let logE = [];

const register = () => {
    // overwrite log and error
    if (window.console && console.log && console.error){
        var old = console.log;
        var oldE = console.error;
        window.console.log = function() {
            old.apply(this, arguments);
            log.push(arguments);
        };
        window.console.error = function() {
            oldE.apply(this, arguments);
            logE.push(arguments);
        };
    }
    // handel errors
    window.onerror = onError;
    window.onunhandledrejection = (event) => {
        // console.error('Unhandled rejection (promise: ', event.promise, ', reason: ', event.reason, ').');
        onError('Unhandled rejection', '', '', '', event);
    };
};


module.exports = {
    register: register
};

