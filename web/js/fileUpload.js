
const registerFileUpload = (node, onDone) => {
    if (node === null) {
        return;
    }
    node.addEventListener("change", (event) => {
        if (node.files.length === 0) return;

        // Lets only do one file for now
        var file = node.files[0];
        // console.log("the file", file);
        var reader = new FileReader();

        // onload fires once the file is ready
        reader.onload = (event) => {
            var content = event.target.result;
            var data = {
                contents: content,
                filename: file.name
            };

            if (onDone) {
                onDone(data);
            }
        };

        // Chose the text option of the reader
        // reader.readAsDataURL(file);
        reader.readAsText(file);
    }, false);
};

module.exports = {
    registerFileUpload: registerFileUpload
};
