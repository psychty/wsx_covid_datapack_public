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
        fillColor: UTLA_rate_colour(feature.properties.cumulative_bins),
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
        return layer.feature.properties.Label_1 + '<br><br>' + layer.feature.properties.Label_2});

utla_map
.fitBounds(utla_rate_boundary.getBounds());

});

function key_utla_rate_deciles() {
  utla_rate_bins.forEach(function(item, index) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = 'key_list_rate';
    list.style.borderColor = utla_decile_colour_func(index);
      var tt = document.createElement('div');
    tt.className = 'side_tt';
    tt.style.borderColor = utla_decile_colour_func(index);
    var tt_h3_1 = document.createElement('h3');
    tt_h3_1.innerHTML = item.Cause;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("utla_decile_key");
    div.appendChild(list);
    })
}

key_utla_rate_deciles();

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


function key_ltla_rate_deciles() {
  ltla_rate_bins.forEach(function(item, index) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = 'key_list_rate';
    list.style.borderColor = ltla_decile_colour_func(index);
      var tt = document.createElement('div');
    tt.className = 'side_tt';
    tt.style.borderColor = ltla_decile_colour_func(index);
    var tt_h3_1 = document.createElement('h3');
    tt_h3_1.innerHTML = item.Cause;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("ltla_decile_key");
    div.appendChild(list);
    })
}

key_ltla_rate_deciles();
