var disqus_shortname = 'axiomatic';
(function () {
var s = document.createElement('script'); s.async = true;
s.type = 'text/javascript';
s.src = 'https://' + disqus_shortname + '.disqus.com/count.js';
(document.getElementsByTagName('HEAD')[0] || document.getElementsByTagName('BODY')[0]).appendChild(s);
}());

window.onload = function() {
    var more = document.getElementById("more");
    more.onclick = function() {
      var ele = document.getElementById("expand");
      ele.style.display = "block";
      ele.classList.add("animated","fadeIn");
      more.style.display = "none";
    };
}
