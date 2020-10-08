
var restriction_type = ['National', 'Local'];
var restriction_colours = ['#ffb400', '#9762a2'];

var ltla_restriction_colour_func = d3.scaleOrdinal()
  .domain(restriction_type)
  .range(restriction_colours)

var width_map = document.getElementById("content_size").offsetWidth;

// Add AJAX request for data
var ltla_restrictions = $.ajax({
  url:"./Outputs/ltla_covid_restrictions_hcl_latest.geojson",
  dataType: "json",
  success: console.log("LTLA boundary for restrictions successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText)
  }
})

var tileUrl = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png';
var attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br> Contains Ordnance Survey data © Crown copyright and database right 2020.<br>Contains Parliamentary information licensed under the Open Parliament Licence v3.0.<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons. Click on an area to find out more.';

function restriction_ltla_colour(d) {
     return d === restriction_type[0] ? restriction_colours[0] :
            d === restriction_type[1] ? restriction_colours[1] :
           '#feebe2';
 }

 function style_restriction(feature) {
     return {
         fillColor: restriction_ltla_colour(feature.properties.Restriction_type),
         weight: 1,
         opacity: .6,
         color: 'white',
         dashArray: '3',
         fillOpacity: 1
     };
 }

$.when(ltla_restrictions).done(function() {

var ltla_map_restrictions = L.map('ltla_restrictions_map');

var basemap_restriction = L.tileLayer(tileUrl, {
  attribution,
  minZoom: 6,
})
    .addTo(ltla_map_restrictions);

var ltla_restrictions_hcl = L.geoJSON(ltla_restrictions.responseJSON,
      {style: style_restriction})
      .addTo(ltla_map_restrictions)
      .bindPopup(function (layer) {
        return layer.feature.properties.label});

ltla_map_restrictions
.fitBounds(ltla_restrictions_hcl.getBounds());

});


function key_restrictions() {

  restriction_type.forEach(function(item, index) {
    var list = document.createElement("li");
    list.innerHTML = item.replace('Local','Local and national');
    list.className = 'key_list_restrictions';
    list.style.borderColor = ltla_restriction_colour_func(index);
      var tt = document.createElement('div');
    tt.className = 'side_tt';
    tt.style.borderColor = ltla_restriction_colour_func(index);
    var tt_h3_1 = document.createElement('h3');
    tt_h3_1.innerHTML = item.Cause;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("ltla_restrictions_key");
    div.appendChild(list);
    })
}

key_restrictions();
