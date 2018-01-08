const pw_fields = document.querySelectorAll('input[type=password]');
console.log("candidates: ");
console.log(pw_fields);
for (let ele of pw_fields) {
    console.log("element:");
    console.log(ele);
    ele.style.background = "blueviolet";
}
