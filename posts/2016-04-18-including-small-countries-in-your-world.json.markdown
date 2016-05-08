---
title: Including Small Countries in Your world.json
byline: The little guy is often the most important.
tags: JavaScript, D3.js
---

In the process of building [Odyssey](https://odyssen.neophilus.net), I came across an omission in most of the world structures on [Bl.ocks](http://bl.ocks.org/). Pretty much everyone nowadays grabs the [Natural Earth](http://www.naturalearthdata.com/) `ne_110m_admin_0_countries` dataset, converts it to [TopoJSON](https://github.com/mbostock/topojson), throws the result to a projection such as

``` javascript
   d3.geo.orthographic().translate([width / 2, height / 2]).scale(width / 2 - 20).clipAngle(90)
```

and then move on with their lives.

But what if you care about information on this projection? <!--BLURB--> What if Mike Bostock's [World Tour](http://bl.ocks.org/mbostock/4183330) gave you this grandiose idea of an interactive globe for your website that needs to pinpoint tiny countries like Singapore? Singapore and other small countries like it are not included in the main 1:110m scale data as they are not large enough to be polytopes. Instead, they are relegated to `ne_110m_admin_0_tiny_countries` as point information and usually omitted from D3 1:110m globe implementations.  

Including this data set adds a small amount of complexity to the build and a bit of finessing of the finished product, but nothing over the top. I'll assume you have TopoJSON setup already and know how to use it. If not, checkout [Let's Make A Map](https://bost.ocks.org/mike/map/) for a good overview. For the lazy ones you can grab the end result of the following steps from [here](/assets/world.json).

Get the latest data from Natural Earth. This can be whatever you want to combine; I'll be using just [countries](http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip)
and [tiny country points](http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_tiny_countries.zip), but it'd also be possible to use the sovereignty data in place of countries (for example) if you want to drill down a little deeper. Using the 1:50m or 1:10m sets is also fine, however I find that this is too precise (read: too much info) to animate smoothly on your grandma's computer or mobile devices.

Extract both sets into a new directory and convert the shape data to GeoJSON:
```
ogr2ogr -f GeoJSON countries.json ne_110m_admin_0_countries.shp
ogr2ogr -f GeoJSON tinycountries.json ne_110m_admin_0_tiny_countries.shp
```

We now need to manually merge these files. AFAIK, `ogr2ogr` isn't happy merging poly and non-poly geometry, and the `TopoJSON` command line tool doesn't merge stuff at all. It's possible to merge this data on the client side each load, but this seems pretty inefficient. One could write a simple shell script for this, but as we're only doing it once I see no need for automation. The GeoJSON format we have output with our two files above follows a simple syntax:
``` json
   { "type": "FeatureCollection", "features": [ ... ] }
```
with a plethora of `Feature` elements filling the array. To merge the data one must simply add a comma (`,`) to the last entry of the `countries.json` `features` array, then append the contents of the same array from `tinycountries.json`. For this example we'll save that file as `world.geo.json`.

Now, our newly created file can be converted to TopoJSON via
```
    topojson -o world.json --id-property su_a3 --properties name -- countries=world.geo.json
```
and loaded using a somewhat normal D3 map routine. The important part is the `d3.json` function. Here we don't use a mesh load but rather a path append:
``` javascript
d3.json("/assets/world.json", function(error, world) {
    if (error) throw error;

   var land = topojson.feature(world, world.objects.countries).features;

   svg.insert("g", ".graticule").attr("id", "countries");
   d3.selectAll("#countries").selectAll("path").data(land)
      .enter().append("path").attr("class", "land")
      .attr("id", function(d, i) { return d.id; }).attr("d", path);
 });
```
This, along with a little boiler plate (which you can view [here](https://bl.ocks.org/Libbum/e8cda20eea9d401f642357c4f46281e4)), gives us the following globe:
<div id="map"></div>
So what's going on with all of those dots everywhere? Well; they're our small countries, which are not represented by paths but rather just points. On this map they stick out like a sore thumb and it's for this reason they are separated out in the first place. We don't really want to see them either: just be able to reference them and zoom to their location.

To do this one can modify the `.data(land)` item to include a filter
``` javascript
.data(land.filter(function(d) { return d.geometry.type !== 'Point'; }))
```
which will give us back our normal looking globe. However, we now have a complete list of individual countries which we can reference via their `su_a3` id or name and in-turn, center the viewport on a country even if it isn't visible on this scale. The map below is filtering out ONLY tiny countries and giving you a world tour. Full codebase for this globe is [here](http://bl.ocks.org/Libbum/ec6a8df2049c6084106512e962788aa5).
<div id="countryName"></div>
<div id="map2"></div>
For a better demo of what can be done with this, checkout my photoblog [Odyssey](https://odyssen.neophilus.net).

<style>
.fill {
  fill: #fff;
}
.graticule {
  fill: none;
  stroke: #777;
  stroke-width: .5px;
  stroke-opacity: .5;
}
.land {
  fill: #222;
}
#map, #map2, #countryName {
   text-align: center;
}
.cselect {
   fill: #81a2be;
}
</style>
<script src="//d3js.org/d3.v3.min.js"></script>
<script src="//d3js.org/topojson.v1.min.js"></script>
<script>

var width = 500,
    height = 500;

var projection = d3.geo.orthographic()
    .translate([width / 2, height / 2])
    .scale(width / 2 - 20)
    .clipAngle(90)
    .precision(0.6)
    .rotate([-40, -30]);

var path = d3.geo.path()
    .projection(projection);

var graticule = d3.geo.graticule();

var title = d3.select("#countryName");

var svg = d3.select("#map").append("svg")
    .attr("width", width)
    .attr("height", height);

svg.append("defs").append("path")
    .datum({type: "Sphere"})
    .attr("id", "sphere")
    .attr("d", path);

svg.append("use")
    .attr("class", "stroke")
    .attr("xlink:href", "#sphere");

svg.append("use")
    .attr("class", "fill")
    .attr("xlink:href", "#sphere");

svg.append("path")
    .datum(graticule)
    .attr("class", "graticule")
    .attr("d", path);

var projection2 = d3.geo.orthographic()
  .translate([width / 2, height / 2])
  .scale(width / 2 - 20)
  .clipAngle(90)
  .precision(0.6)
  .rotate([-40, -30]);

var path2 = d3.geo.path()
  .projection(projection2);

var svg2 = d3.select("#map2").append("svg")
  .attr("width", width)
  .attr("height", height);

svg2.append("defs").append("path")
  .datum({type: "Sphere"})
  .attr("id", "sphere2")
  .attr("d", path2);

svg2.append("use")
  .attr("class", "stroke")
  .attr("xlink:href", "#sphere2");

svg2.append("use")
  .attr("class", "fill")
  .attr("xlink:href", "#sphere2");

svg2.append("path")
  .datum(graticule)
  .attr("class", "graticule")
  .attr("d", path2);

d3.json("/assets/world.json", function(error, world) {
    if (error) throw error;

   var land = topojson.feature(world, world.objects.countries).features,
   small = land.filter(function(d) { return d.geometry.type === 'Point'; }),
   i = 0,
   n  = small.length;

   svg.insert("g", ".graticule").attr("id", "countries");
   d3.selectAll("#countries").selectAll("path").data(land)
      .enter().append("path").attr("class", "land").attr("id", function(d, i) { return d.id; }).attr("d", path);

   svg2.insert("g", ".graticule2").attr("id", "countries2");
   d3.selectAll("#countries2").selectAll("path").data(land.filter(function(d) { return d.geometry.type !== 'Point'; }))
      .enter().append("path").attr("class", "land").attr("id", function(d, i) { return d.id; }).attr("d", path2);

   (function transition() {
       d3.transition()
           .duration(2000)
           .each("start", function() {
              d3.selectAll(".cselect").remove();
              title.text(small[i = (i + 1) % n].properties.name);
           })
           .tween("rotate", function() {
            var p = d3.geo.centroid(small[i]),
                r = d3.interpolate(projection2.rotate(), [-p[0], -p[1]]);
            return function(t) {
              svg2.insert("path", ".graticule2").datum({ type: "Point", coordinates: [p[0], p[1]] })
                           .attr("class", "cselect").attr("d", path2);
              projection2.rotate(r(t));
              d3.select("#map2").selectAll("path").attr("d", d3.geo.path().projection(projection2));
            };
           })
        .transition()
           .each("end", transition);
    })();
 });
</script>
