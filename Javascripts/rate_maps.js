var tileUrl = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png';
var attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br> Contains Ordnance Survey data © Crown copyright and database right 2020.<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons. Click on an area to find out more';

function UTLA_rate_colour(d) {
    return d === utla_rate_bins[0] ? decile_colours[0] :
           d === utla_rate_bins[1] ? decile_colours[1] :
           d === utla_rate_bins[2] ? decile_colours[2] :
           d === utla_rate_bins[3] ? decile_colours[3] :
           d === utla_rate_bins[4] ? decile_colours[4] :
           d === utla_rate_bins[5] ? decile_colours[5] :
           d === utla_rate_bins[6] ? decile_colours[6] :
           d === utla_rate_bins[7] ? decile_colours[7] :
           d === utla_rate_bins[8] ? decile_colours[8] :
           d === utla_rate_bins[9] ? decile_colours[9] :
                    '#feebe2';
}

function style(feature) {
    return {
        fillColor: UTLA_rate_colour(feature.properties.bins),
        weight: 2,
        opacity: 1,
        color: 'white',
        dashArray: '3',
        fillOpacity: 1
    };
}

$.when(utla).done(function() {

var utla_map = L.map('rate_map_1');

var basemap = L.tileLayer(tileUrl, {
  attribution,
  minZoom: 6,
})
    .addTo(utla_map);

var utla_rate_boundary = L.geoJSON(utla.responseJSON,
      {style: style})
      .addTo(utla_map)
      .bindPopup(function (layer) {
        return layer.feature.properties.Label_1});

utla_map
.fitBounds(utla_rate_boundary.getBounds());

});





//
// // Legend
// var utla_rate_legend = L.control({position: 'bottomright'});
//
// utla_rate_legend.onAdd = function (utla_map) {
//     var div = L.DomUtil.create('div', 'legend'),
//         grades = [0, 2000, 4000, 6000, 8000, 10000, 12000, 14000];
//
//         // add title
//       div.innerHTML += "<p><b>Decile of cumulative<br>rate per 100,000</b></p>";
//
// // loop through our density intervals and generate a label with a colored square for each interval
//   for (var i = 0; i < grades.length; i++) {
//     div.innerHTML +=
//     '<i style="background:' + get_density_colour(grades[i]) + '"></i> ' +
//     d3.format(',.0f')(grades[i]) + (grades[i + 1] ? '&ndash;' + d3.format(',.0f')(grades[i + 1] -1) + '<br>' : '+');
//     }
//
//     return div;
// };
//
// utla_rate_legend
// .addTo(utla_map);

function LTLA_rate_colour(d) {
    return d === ltla_rate_bins[0] ? decile_colours[0] :
           d === ltla_rate_bins[1] ? decile_colours[1] :
           d === ltla_rate_bins[2] ? decile_colours[2] :
           d === ltla_rate_bins[3] ? decile_colours[3] :
           d === ltla_rate_bins[4] ? decile_colours[4] :
           d === ltla_rate_bins[5] ? decile_colours[5] :
           d === ltla_rate_bins[6] ? decile_colours[6] :
           d === ltla_rate_bins[7] ? decile_colours[7] :
           d === ltla_rate_bins[8] ? decile_colours[8] :
           d === ltla_rate_bins[9] ? decile_colours[9] :
                    '#feebe2';
}

function style_ltla(feature) {
    return {
        fillColor: LTLA_rate_colour(feature.properties.bins),
        weight: 2,
        opacity: 1,
        color: 'white',
        dashArray: '3',
        fillOpacity: 1
    };
}

$.when(ltla).done(function() {

var ltla_map = L.map('rate_map_2');

var basemap = L.tileLayer(tileUrl, {
  attribution,
  minZoom: 6,
})
    .addTo(ltla_map);

var ltla_rate_boundary = L.geoJSON(ltla.responseJSON,
      {style: style_ltla})
      .addTo(ltla_map)
      .bindPopup(function (layer) {
        return layer.feature.properties.Label_1});

ltla_map
.fitBounds(ltla_rate_boundary.getBounds());

});
