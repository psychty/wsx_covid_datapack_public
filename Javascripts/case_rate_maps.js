// Maps
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/ltla_cumulative_rate_bins.json", false);
request.send(null);

var ltla_cumulative_rate_bins = JSON.parse(request.responseText);

// Maps
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/ltla_rolling_rate_bins.json", false);
request.send(null);

var ltla_rolling_rate_bins = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/utla_cumulative_rate_bins.json", false);
request.send(null);

var utla_cumulative_rate_bins = JSON.parse(request.responseText);
var decile_colours = [
  "#a50026",
  "#d73027",
  "#f46d43",
  "#fdae61",
  "#fee090",
  "#e0f3f8",
  "#abd9e9",
  "#74add1",
  "#4575b4",
  "#313695",
];

var utla_decile_cumulative_colour_func = d3
  .scaleOrdinal()
  .domain(utla_cumulative_rate_bins)
  .range(decile_colours);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/utla_rolling_rate_bins.json", false);
request.send(null);

var utla_rolling_rate_bins = JSON.parse(request.responseText);

var utla_decile_rolling_colour_func = d3
  .scaleOrdinal()
  .domain(utla_rolling_rate_bins)
  .range(decile_colours);

var ltla_decile_cumulative_colour_func = d3
  .scaleOrdinal()
  .domain(ltla_cumulative_rate_bins)
  .range(decile_colours);

var ltla_decile_rolling_colour_func = d3
  .scaleOrdinal()
  .domain(ltla_rolling_rate_bins)
  .range(decile_colours);

var width_map = document.getElementById("content_size").offsetWidth;

// Add AJAX request for data
var utla = $.ajax({
  url: "./Outputs/utla_covid_rate_latest.geojson",
  dataType: "json",
  success: console.log("UTLA boundary data successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText);
  },
});

// Add AJAX request for data
var ltla = $.ajax({
  url: "./Outputs/ltla_covid_cumulative_rate_latest.geojson",
  dataType: "json",
  success: console.log("LTLA boundary data successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText);
  },
});

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/percentage_change_bins.json", false);
request.send(null);

var percentage_change_bins = JSON.parse(request.responseText);
var change_colours = [
  "#276419",
  "#4d9221",
  "#7fbc41",
  "#b8e186",
  "#e6f5d0",
  "#fde0ef",
  "#f1b6da",
  "#de77ae",
  "#c51b7d",
  "#8e0152",
  "#313695",
];
perc_change_colour_func = d3
  .scaleOrdinal()
  .domain(percentage_change_bins)
  .range(change_colours);

var tileUrl = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png";
var attribution =
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br> Contains Ordnance Survey data © Crown copyright and database right 2020.<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons. Click on an area to find out more';

function perc_change_rate_colour(d) {
  return d === percentage_change_bins[0]
    ? change_colours[0]
    : d === percentage_change_bins[1]
    ? change_colours[1]
    : d === percentage_change_bins[2]
    ? change_colours[2]
    : d === percentage_change_bins[3]
    ? change_colours[3]
    : d === percentage_change_bins[4]
    ? change_colours[4]
    : d === percentage_change_bins[5]
    ? change_colours[5]
    : d === percentage_change_bins[6]
    ? change_colours[6]
    : d === percentage_change_bins[7]
    ? change_colours[7]
    : d === percentage_change_bins[8]
    ? change_colours[8]
    : d === percentage_change_bins[9]
    ? change_colours[9]
    : d === percentage_change_bins[10]
    ? change_colours[10]
    : "#feebe2";
}

function style_change(feature) {
  return {
    fillColor: perc_change_rate_colour(
      feature.properties.Perc_change_on_rolling_7_days_tidy
    ),
    weight: 2,
    opacity: 1,
    color: "white",
    dashArray: "3",
    fillOpacity: 1,
  };
}

function UTLA_cumulative_rate_colour(d) {
  return d === utla_cumulative_rate_bins[0]
    ? decile_colours[0]
    : d === utla_cumulative_rate_bins[1]
    ? decile_colours[1]
    : d === utla_cumulative_rate_bins[2]
    ? decile_colours[2]
    : d === utla_cumulative_rate_bins[3]
    ? decile_colours[3]
    : d === utla_cumulative_rate_bins[4]
    ? decile_colours[4]
    : d === utla_cumulative_rate_bins[5]
    ? decile_colours[5]
    : d === utla_cumulative_rate_bins[6]
    ? decile_colours[6]
    : d === utla_cumulative_rate_bins[7]
    ? decile_colours[7]
    : d === utla_cumulative_rate_bins[8]
    ? decile_colours[8]
    : d === utla_cumulative_rate_bins[9]
    ? decile_colours[9]
    : "#feebe2";
}

function style_cumulative(feature) {
  return {
    fillColor: UTLA_cumulative_rate_colour(feature.properties.cumulative_bins),
    weight: 2,
    opacity: 1,
    color: "white",
    dashArray: "3",
    fillOpacity: 0.8,
  };
}

function UTLA_rolling_rate_colour(d) {
  return d === utla_rolling_rate_bins[0]
    ? decile_colours[0]
    : d === utla_rolling_rate_bins[1]
    ? decile_colours[1]
    : d === utla_rolling_rate_bins[2]
    ? decile_colours[2]
    : d === utla_rolling_rate_bins[3]
    ? decile_colours[3]
    : d === utla_rolling_rate_bins[4]
    ? decile_colours[4]
    : d === utla_rolling_rate_bins[5]
    ? decile_colours[5]
    : d === utla_rolling_rate_bins[6]
    ? decile_colours[6]
    : d === utla_rolling_rate_bins[7]
    ? decile_colours[7]
    : d === utla_rolling_rate_bins[8]
    ? decile_colours[8]
    : d === utla_rolling_rate_bins[9]
    ? decile_colours[9]
    : "#feebe2";
}

function style_rolling(feature) {
  return {
    fillColor: UTLA_rolling_rate_colour(feature.properties.rolling_bins),
    weight: 2,
    opacity: 1,
    color: "white",
    dashArray: "3",
    fillOpacity: 0.8,
  };
}

$.when(utla).done(function () {
  var utla_map = L.map("rate_map_1");

  var utla_basemap = L.tileLayer(tileUrl, {
    attribution,
    minZoom: 6,
  }).addTo(utla_map);

  var utla_rate_boundary_cumulative = L.geoJSON(utla.responseJSON, {
    style: style_cumulative,
  })
    // .addTo(utla_map)
    .bindPopup(function (layer) {
      return (
        "<p style = 'font-size: .8rem'>" +
        layer.feature.properties.Label_1 +
        "</p><p style = 'font-size: .8rem'>" +
        layer.feature.properties.Label_2 +
        "</p>"
      );
    });

  var utla_rate_boundary_rolling = L.geoJSON(utla.responseJSON, {
    style: style_rolling,
  })
    .addTo(utla_map)
    .bindPopup(function (layer) {
      return (
        "<b>" +
        layer.feature.properties.Name +
        "</b><p style = 'font-size: .8rem'>" +
        layer.feature.properties.Label_2 +
        "</p>"
      );
    });

  var utla_rate_boundary_change = L.geoJSON(utla.responseJSON, {
    style: style_change,
  })
    // .addTo(utla_map)
    .bindPopup(function (layer) {
      return (
        "<b>" +
        layer.feature.properties.Name +
        "</b><p style = 'font-size: .8rem'>" +
        layer.feature.properties.Label_3 +
        "</p>"
      );
    });

  var baseMaps = {
    "Rolling 7-day rate per 100,000": utla_rate_boundary_rolling,
    "Percentage change rolling 7-day cases": utla_rate_boundary_change,
    "Cumulative rate per 100,000": utla_rate_boundary_cumulative,
  };

  L.control.layers(baseMaps, null, { collapsed: false }).addTo(utla_map);

  utla_map.fitBounds(utla_rate_boundary_cumulative.getBounds());

  utla_map.on("baselayerchange", function (ev) {
    console.log("Base layer changes");
    var selected_base_layer = ev.name;

    if (selected_base_layer === "Cumulative rate per 100,000") {
      key_cumulative_utla_rate_deciles();
    }
    if (selected_base_layer === "Rolling 7-day rate per 100,000") {
      key_rolling_utla_rate_deciles();
    }
    if (selected_base_layer === "Percentage change rolling 7-day cases") {
      key_change_utla_rate_keys();
    }
  });
});

function key_cumulative_utla_rate_deciles() {
  $(".key_list_rate_utla").remove();

  d3.select("#summary_utla_rate_title").html(function (d) {
    return "Cumulative rate of confirmed COVID-19 cases per 100,000 population (all ages); Upper Tier Local Authorities;";
  });

  d3.select("#summary_utla_rate_subtitle").html(function (d) {
    return "Pillar 1 and 2 confirmed cases; as at " + data_refreshed_date;
  });

  d3.select("#selected_utla_key_title").html(function (d) {
    return "<b>Upper Tier Decile Key for cumulative rate per 100,000</b> (range of values given in brackets)";
  });

  utla_cumulative_rate_bins.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item.replace(")", " cases per 100,000)");
    list.className = "key_list_rate_utla";
    list.style.borderColor = utla_decile_cumulative_colour_func(index);
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = utla_decile_cumulative_colour_func(index);
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item.Cause;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("utla_map_key");
    div.appendChild(list);
  });
}

function key_rolling_utla_rate_deciles() {
  $(".key_list_rate_utla").remove();

  d3.select("#summary_utla_rate_title").html(function (d) {
    return "Latest confirmed COVID-19 case rate per 100,000 population (all ages); Upper Tier Local Authorities;";
  });

  d3.select("#summary_utla_rate_subtitle").html(function (d) {
    return (
      "Pillar 1 and 2 confirmed cases; seven days to " + complete_date + ";"
    );
  });

  d3.select("#selected_utla_key_title").html(function (d) {
    return "<b>Upper Tier Decile Key for rolling rate per 100,000</b> (range of values given in brackets)";
  });

  utla_rolling_rate_bins.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item.replace(")", " new cases per 100,000)");
    list.className = "key_list_rate_utla";
    list.style.borderColor = utla_decile_rolling_colour_func(index);
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = utla_decile_rolling_colour_func(index);
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item.Cause;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("utla_map_key");
    div.appendChild(list);
  });
}

function key_change_utla_rate_keys() {
  $(".key_list_rate_utla").remove();

  d3.select("#summary_utla_rate_title").html(function (d) {
    return "Percentage change in recent confirmed COVID-19 case rate cases per 100,000 population (all ages); Upper Tier Local Authorities;";
  });

  d3.select("#summary_utla_rate_subtitle").html(function (d) {
    return (
      "Pillar 1 and 2 confirmed cases; seven days to " +
      complete_date +
      " compared to seven days leading to " +
      rolling_seven_days_ago +
      ";"
    );
  });

  d3.select("#selected_utla_key_title").html(function (d) {
    return "<b>Colour key</b>";
  });

  percentage_change_bins.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = "key_list_rate_utla";
    list.style.borderColor = perc_change_colour_func(index);
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = perc_change_colour_func(index);
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item.Cause;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("utla_map_key");
    div.appendChild(list);
  });
}

key_rolling_utla_rate_deciles();

// Lower tiers

function LTLA_cumulative_rate_colour(d) {
  return d === ltla_cumulative_rate_bins[0]
    ? decile_colours[0]
    : d === ltla_cumulative_rate_bins[1]
    ? decile_colours[1]
    : d === ltla_cumulative_rate_bins[2]
    ? decile_colours[2]
    : d === ltla_cumulative_rate_bins[3]
    ? decile_colours[3]
    : d === ltla_cumulative_rate_bins[4]
    ? decile_colours[4]
    : d === ltla_cumulative_rate_bins[5]
    ? decile_colours[5]
    : d === ltla_cumulative_rate_bins[6]
    ? decile_colours[6]
    : d === ltla_cumulative_rate_bins[7]
    ? decile_colours[7]
    : d === ltla_cumulative_rate_bins[8]
    ? decile_colours[8]
    : d === ltla_cumulative_rate_bins[9]
    ? decile_colours[9]
    : "#feebe2";
}

function style_cumulative_ltla(feature) {
  return {
    fillColor: LTLA_cumulative_rate_colour(feature.properties.cumulative_bins),
    weight: 2,
    opacity: 1,
    color: "white",
    dashArray: "3",
    fillOpacity: 0.8,
  };
}

function LTLA_rolling_rate_colour(d) {
  return d === ltla_rolling_rate_bins[0]
    ? decile_colours[0]
    : d === ltla_rolling_rate_bins[1]
    ? decile_colours[1]
    : d === ltla_rolling_rate_bins[2]
    ? decile_colours[2]
    : d === ltla_rolling_rate_bins[3]
    ? decile_colours[3]
    : d === ltla_rolling_rate_bins[4]
    ? decile_colours[4]
    : d === ltla_rolling_rate_bins[5]
    ? decile_colours[5]
    : d === ltla_rolling_rate_bins[6]
    ? decile_colours[6]
    : d === ltla_rolling_rate_bins[7]
    ? decile_colours[7]
    : d === ltla_rolling_rate_bins[8]
    ? decile_colours[8]
    : d === ltla_rolling_rate_bins[9]
    ? decile_colours[9]
    : "#feebe2";
}

function style_rolling_ltla(feature) {
  return {
    fillColor: LTLA_rolling_rate_colour(feature.properties.rolling_bins),
    weight: 2,
    opacity: 1,
    color: "white",
    dashArray: "3",
    fillOpacity: 0.8,
  };
}

$.when(ltla).done(function () {
  var ltla_map = L.map("rate_map_2");

  var ltla_basemap = L.tileLayer(tileUrl, {
    attribution,
    minZoom: 6,
  }).addTo(ltla_map);

  var ltla_rate_boundary_cumulative = L.geoJSON(ltla.responseJSON, {
    style: style_cumulative_ltla,
  }).bindPopup(function (layer) {
    return (
      "<p  style = 'font-size: .8rem'>" +
      layer.feature.properties.Label_1 +
      "</p><p style = 'font-size: .8rem'" +
      layer.feature.properties.Label_2 +
      "</p>"
    );
  });

  var ltla_rate_boundary_rolling = L.geoJSON(ltla.responseJSON, {
    style: style_rolling_ltla,
  })
    .addTo(ltla_map)
    .bindPopup(function (layer) {
      return (
        "<p style = 'font-size: .8rem'>" +
        layer.feature.properties.Name +
        "</p><p style = 'font-size: .8rem'>" +
        layer.feature.properties.Label_2 +
        "</p>"
      );
    });

  var ltla_rate_boundary_change = L.geoJSON(ltla.responseJSON, {
    style: style_change,
  }).bindPopup(function (layer) {
    return (
      "<p style = 'font-size: .8rem'>" +
      layer.feature.properties.Name +
      "</p><p style = 'font-size: .8rem'>" +
      layer.feature.properties.Label_3 +
      "</p>"
    );
  });

  var baseMaps = {
    "Rolling 7-day rate per 100,000": ltla_rate_boundary_rolling,
    "Percentage change rolling 7-day cases": ltla_rate_boundary_change,
    "Cumulative rate per 100,000": ltla_rate_boundary_cumulative,
  };

  L.control.layers(baseMaps, null, { collapsed: false }).addTo(ltla_map);

  ltla_map.fitBounds(ltla_rate_boundary_cumulative.getBounds());

  ltla_map.on("baselayerchange", function (ev) {
    console.log("Base layer changes");
    var selected_base_layer = ev.name;

    if (selected_base_layer === "Cumulative rate per 100,000") {
      key_cumulative_ltla_rate_deciles();
    }
    if (selected_base_layer === "Rolling 7-day rate per 100,000") {
      key_rolling_ltla_rate_deciles();
    }
    if (selected_base_layer === "Percentage change rolling 7-day cases") {
      key_change_ltla_rate_keys();
    }
  });
});

function key_change_ltla_rate_keys() {
  $(".key_list_rate_ltla").remove();

  d3.select("#summary_ltla_rate_title").html(function (d) {
    return "Percentage change in recent confirmed COVID-19 case rate cases per 100,000 population (all ages); Lower Tier Local Authorities;";
  });

  d3.select("#summary_ltla_rate_subtitle").html(function (d) {
    return (
      "Pillar 1 and 2 confirmed cases; seven days to " +
      complete_date +
      " compared to seven days leading to " +
      rolling_seven_days_ago +
      ";"
    );
  });

  d3.select("#selected_ltla_key_title").html(function (d) {
    return "<b>Colour key</b>";
  });

  percentage_change_bins.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = "key_list_rate_ltla";
    list.style.borderColor = perc_change_colour_func(index);
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = perc_change_colour_func(index);
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item.Cause;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("ltla_map_key");
    div.appendChild(list);
  });
}

function key_cumulative_ltla_rate_deciles() {
  $(".key_list_rate_ltla").remove();

  d3.select("#summary_ltla_rate_title").html(function (d) {
    return "Cumulative rate of confirmed COVID-19 cases per 100,000 population (all ages); Lower Tier Local Authorities;";
  });

  d3.select("#summary_ltla_rate_subtitle").html(function (d) {
    return "Pillar 1 and 2 confirmed cases; as at " + data_refreshed_date;
  });

  d3.select("#selected_ltla_key_title").html(function (d) {
    return "<b>Lower Tier Decile Key for cumulative rate per 100,000</b> (range of values given in brackets)";
  });

  ltla_cumulative_rate_bins.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item.replace(")", " cases per 100,000)");
    list.className = "key_list_rate_ltla";
    list.style.borderColor = ltla_decile_cumulative_colour_func(index);
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = ltla_decile_cumulative_colour_func(index);
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item.Cause;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("ltla_map_key");
    div.appendChild(list);
  });
}

function key_rolling_ltla_rate_deciles() {
  $(".key_list_rate_ltla").remove();

  d3.select("#summary_ltla_rate_title").html(function (d) {
    return "Latest confirmed COVID-19 case rate per 100,000 population (all ages); Lower Tier Local Authorities;";
  });

  d3.select("#summary_ltla_rate_subtitle").html(function (d) {
    return (
      "Pillar 1 and 2 confirmed cases; seven days to " + complete_date + ";"
    );
  });

  d3.select("#selected_ltla_key_title").html(function (d) {
    return "<b>Lower Tier Decile Key for rolling rate per 100,000</b> (range of values given in brackets)";
  });

  ltla_rolling_rate_bins.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item.replace(")", " new cases per 100,000)");
    list.className = "key_list_rate_ltla";
    list.style.borderColor = ltla_decile_rolling_colour_func(index);
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = ltla_decile_rolling_colour_func(index);
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item.Cause;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("ltla_map_key");
    div.appendChild(list);
  });
}

key_rolling_ltla_rate_deciles();
