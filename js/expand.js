window.onload = function() {
    var more = document.getElementById("more");
    more.onclick = function() {
      var ele = document.getElementById("expand");
      ele.style.display = "block";
      ele.classList.add("animated","fadeIn");
      more.style.display = "none";
    };
}
