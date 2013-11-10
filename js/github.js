//Quick and easy recent github activity without jQuery. 
//Uses Github's v3 API and JSON-P
//Author: Tim DuBois 10/11/13

window.repolist = function(response) {
    var data = response.data
    var i = 0, fragment = '<ul>';

    for (i; i < 4; i++) {
        fragment += '<li><a href="'+data[i].html_url+'">'+data[i].name+'</a> ('+data[i].language+')<br>'+data[i].description+'</li>';
    }

    document.getElementById("github").innerHTML = fragment+'</ul>';
};


