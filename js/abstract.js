function showAbstract(key) {
    var ele = document.getElementById(key);
    if(ele.style.display == "block") {
        ele.classList.remove("fadeIn");
        ele.classList.add("fadeOut");
        setTimeout(function() {ele.style.display = "none"; ele.classList.remove("animated","fadeOut");}, 500);
    }
    else {
        ele.style.display = "block";
        ele.classList.add("animated","fadeIn");
    }
}
