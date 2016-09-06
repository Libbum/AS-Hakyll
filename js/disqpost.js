 var disqus_shortname = 'axiomatic';
 var url = document.getElementById("url").innerText;
 var disqus_identifier = url;
 var disqus_url = 'https://axiomatic.neophilus.net' + url;
 var disqus_title = document.getElementById("title").innerText;
 (function() {
     var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
     dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
     (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
 })();
