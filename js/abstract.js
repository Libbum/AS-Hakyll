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

document.addEventListener( "DOMContentLoaded", ready, false );

function ready() {
  document.getElementById('rvmm').onclick = function () { showAbstract('1606.08681'); };
  document.getElementById('tddelocal').onclick = function () { showAbstract('1508.05204'); };
  document.getElementById('pop2016').onclick = function () { showAbstract('10.1063/1.4948424'); };
  document.getElementById('molsim2016').onclick = function () { showAbstract('10.1080/08927022.2015.1068941'); };
  document.getElementById('pre2015').onclick = function () { showAbstract('10.1103/PhysRevE.92.023025'); };
  document.getElementById('njp2015').onclick = function () { showAbstract('10.1088/1367-2630/17/2/023017'); };
  document.getElementById('jatm2013').onclick = function () { showAbstract('10.1175/JAS-D-12-0268.1'); };
  document.getElementById('prl2013').onclick = function () { showAbstract('10.1103/PhysRevLett.110.077002'); };
  document.getElementById('atmenv2012').onclick = function () { showAbstract('10.1016/j.atmosenv.2012.02.049'); };
  document.getElementById('pre2010').onclick = function () { showAbstract('10.1103/PhysRevE.82.056304'); };

  document.getElementById('phda').onclick = function () { showAbstract('phd'); };
  document.getElementById('honoursa').onclick = function () { showAbstract('honours'); };
  document.getElementById('undergrada').onclick = function () { showAbstract('undergrad'); };
}
