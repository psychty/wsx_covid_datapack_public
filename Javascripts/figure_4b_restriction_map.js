// var restriction_type = ['Medium', 'High', 'Very High'];
// var restriction_colours = ['#ffb400', '#9762a2', '#374776'];

// var ltla_restriction_colour_func = d3.scaleOrdinal()
//   .domain(restriction_type)
//   .range(restriction_colours)

// var width_map = document.getElementById("content_size").offsetWidth;

// // Add AJAX request for data
// var ltla_restrictions = $.ajax({
//   url:"./Outputs/ltla_covid_restrictions_hcl_latest.geojson",
//   dataType: "json",
//   success: console.log("LTLA boundary for restrictions successfully loaded."),
//   error: function (xhr) {
//     alert(xhr.statusText)
//   }
// })

// var tileUrl = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png';
// var attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br> Contains Ordnance Survey data © Crown copyright and database right 2020.<br>Contains Parliamentary information licensed under the Open Parliament Licence v3.0.<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons. Click on an area to find out more.';

// function restriction_ltla_colour(d) {
//      return d === restriction_type[0] ? restriction_colours[0] :
//             d === restriction_type[1] ? restriction_colours[1] :
//             d === restriction_type[2] ? restriction_colours[2] :
//            '#feebe2';
//  }

//  function style_restriction(feature) {
//      return {
//          fillColor: restriction_ltla_colour(feature.properties.l_tier),
//          weight: 1,
//          opacity: .6,
//          color: 'white',
//          dashArray: '3',
//          fillOpacity: 1
//      };
//  }

// $.when(ltla_restrictions).done(function() {

// var ltla_map_restrictions = L.map('ltla_restrictions_map');

// var basemap_restriction = L.tileLayer(tileUrl, {
//   attribution,
//   minZoom: 6,
// })
//     .addTo(ltla_map_restrictions);

// var ltla_restrictions_hcl = L.geoJSON(ltla_restrictions.responseJSON,
//       {style: style_restriction})
//       .addTo(ltla_map_restrictions)
//       .bindPopup(function (layer) {
//         return layer.feature.properties.label});

// ltla_map_restrictions
// .fitBounds(ltla_restrictions_hcl.getBounds());

// });

// function key_restrictions() {

//   restriction_type.forEach(function(item, index) {
//     var list = document.createElement("li");
//     list.innerHTML = item.replace('Local','Local and national');
//     list.className = 'key_list_restrictions';
//     list.style.borderColor = ltla_restriction_colour_func(index);
//       var tt = document.createElement('div');
//     tt.className = 'side_tt';
//     tt.style.borderColor = ltla_restriction_colour_func(index);
//     var tt_h3_1 = document.createElement('h3');
//     tt_h3_1.innerHTML = item.Cause;

//     tt.appendChild(tt_h3_1);
//     var div = document.getElementById("ltla_restrictions_key");
//     div.appendChild(list);
//     })
// }

// key_restrictions();

// ! ltla restrictions

var restriction_type = [
  "Tier 1 (medium)",
  "Tier 2 (high)",
  "Tier 3 (very high)",
  "Tier 4 (stay at home)",
];
// var restriction_colours = ["#ffb400", "#9762a2", "#374776"];
var restriction_colours = ["#ffb400", "#6A9BC3", "#2A5783", "#5c3776"];

var restriction_colour_func = d3
  .scaleOrdinal()
  .domain(restriction_type)
  .range(restriction_colours);

// Add AJAX request for data
var ltla_restrictions = $.ajax({
  url: "./Outputs/ltla_covid_latest.geojson",
  dataType: "json",
  success: console.log("LTLA boundary for restrictions successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText);
  },
});

var tileUrl = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png";
var attribution =
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br>Click on an area to find out more.';

function restriction_ltla_colour(d) {
  return d === restriction_type[0]
    ? restriction_colours[0]
    : d === restriction_type[1]
    ? restriction_colours[1]
    : d === restriction_type[2]
    ? restriction_colours[2]
    : d === restriction_type[3]
    ? restriction_colours[3]
    : "#feebe2";
}

function style_restriction(feature) {
  return {
    fillColor: restriction_ltla_colour(feature.properties.Tier),
    weight: 1,
    opacity: 0.6,
    color: "white",
    dashArray: "3",
    fillOpacity: 1,
  };
}

$.when(ltla_restrictions).done(function () {
  var ltla_map_restrictions = L.map("ltla_restrictions_map");

  var basemap_restriction = L.tileLayer(tileUrl, {
    attribution,
    minZoom: 5,
  }).addTo(ltla_map_restrictions);

  var ltla_restrictions_hcl = L.geoJSON(ltla_restrictions.responseJSON, {
    style: style_restriction,
  })
    .addTo(ltla_map_restrictions)
    .bindPopup(function (layer) {
      return (
        "<b>" +
        layer.feature.properties.Name +
        "</b><br>" +
        layer.feature.properties.Tier
      );
    });

  ltla_map_restrictions.fitBounds(ltla_restrictions_hcl.getBounds());
});

// ! primary schools

var open_type = ["All pupils", "Restricted"];

var primary_school_colours = ["#74cce1", "#ff0c0c"];

var primary_school_colour_func = d3
  .scaleOrdinal()
  .domain(open_type)
  .range(primary_school_colours);

var primary_school_label_func = d3
  .scaleOrdinal()
  .domain(open_type)
  .range(
    "All primary school pupils are expected to return as normal.",
    "Primary schools should restrict on site education to vulnerable children and children of critical workers."
  );

// Add AJAX request for data
var ltla_primary = $.ajax({
  url: "./Outputs/ltla_covid_latest.geojson",
  dataType: "json",
  success: console.log(
    "LTLA boundary for primary schools opening successfully loaded."
  ),
  error: function (xhr) {
    alert(xhr.statusText);
  },
});

function primary_ltla_colour(d) {
  return d === open_type[0]
    ? primary_school_colours[0]
    : d === open_type[1]
    ? primary_school_colours[1]
    : "#feebe2";
}

function style_primary(feature) {
  return {
    fillColor: primary_ltla_colour(feature.properties.Primary_schools),
    weight: 1,
    opacity: 0.6,
    color: "white",
    dashArray: "3",
    fillOpacity: 1,
  };
}

$.when(ltla_primary).done(function () {
  var ltla_map_primary = L.map("ltla_primary_schools_map");

  var basemap_primary = L.tileLayer(tileUrl, {
    attribution,
    minZoom: 5,
  }).addTo(ltla_map_primary);

  var ltla_primary_hcl = L.geoJSON(ltla_primary.responseJSON, {
    style: style_primary,
  })
    .addTo(ltla_map_primary)
    .bindPopup(function (layer) {
      return (
        "<b>" +
        layer.feature.properties.Name +
        "</b><br>" +
        layer.feature.properties.Primary_schools
      );
    });

  ltla_map_primary.fitBounds(ltla_primary_hcl.getBounds());
});

// ! Icons
