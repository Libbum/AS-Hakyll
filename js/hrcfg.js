if ((skel.isActive('desktop')) && (!skel.isActive('1000px'))) {
  var headerElement = document.querySelector(".head");
  var headroom  = new Headroom(headerElement);
  headroom.init();
  headroom.offset = 200;
}
