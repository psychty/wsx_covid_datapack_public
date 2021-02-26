var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_update_date.json", false);
request.send(null);
var vaccine_update_date = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_administered_date.json", false);
request.send(null);
var vaccine_administered_date = JSON.parse(request.responseText);

d3.select("#latest_vaccine_publication_date").html(function (d) {
  return (
    "The vaccination data for local areas was last updated on " +
    vaccine_update_date +
    " and includes vaccines administered from " +
    vaccine_administered_date +
    "."
  );
});

// ! Table

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_at_a_glance.json", false);
request.send(null);
var vaccine_at_a_glance = JSON.parse(request.responseText);

window.onload = () => {
  loadTable_ltla_vaccine(vaccine_at_a_glance);
};

function loadTable_ltla_vaccine(vaccine_at_a_glance) {
  const tableBody = document.getElementById("vaccine_table_1");
  var dataHTML = "";

  for (let item of vaccine_at_a_glance) {
    dataHTML += `<tr><td>${item.Name}</td><td>${d3.format(",.0f")(
      item.Total_where_age_known
    )}</td><td>${d3.format(",.1f")(
      item.Rate_age_known_per_100000
    )}</td><td>${d3.format(",.0f")(
      item.Individuals_70_plus
    )}</td><td>${d3.format(".0%")(item.Proportion_70_plus)}</td></tr>`;
  }

  tableBody.innerHTML = dataHTML;
}

// Maps

var msoa_covid_vaccines_raw = [
  "Up to 1,999",
  "2,000-2,999",
  "3,000-3,999",
  "4,000-4,999",
  "5,000-5,999",
  "6,000-6,9999",
  "7,000+",
];

var msoa_covid_vaccines_colours = [
  "#ffffcc",
  "#c7e9b4",
  "#7fcdbb",
  "#41b6c4",
  "#1d91c0",
  "#225ea8",
  "#0c2c84",
];

var msoa_covid_vaccines_colour_func = d3
  .scaleOrdinal()
  .domain(msoa_covid_vaccines_raw)
  .range(msoa_covid_vaccines_colours);

// Add AJAX request for data
var msoa_vaccine_total = $.ajax({
  url: "./Outputs/msoa_covid_vaccine_latest.geojson",
  dataType: "json",
  success: console.log("MSOA boundary data successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText);
  },
});

var tileUrl = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png";
var attribution =
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br> Contains Ordnance Survey data © Crown copyright and database right 2020.<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons. Click on an area to find out more';

function vaccine_msoa_colour(d) {
  return d === msoa_covid_vaccines_raw[0]
    ? msoa_covid_vaccines_colours[0]
    : d === msoa_covid_vaccines_raw[1]
    ? msoa_covid_vaccines_colours[1]
    : d === msoa_covid_vaccines_raw[2]
    ? msoa_covid_vaccines_colours[2]
    : d === msoa_covid_vaccines_raw[3]
    ? msoa_covid_vaccines_colours[3]
    : d === msoa_covid_vaccines_raw[4]
    ? msoa_covid_vaccines_colours[4]
    : d === msoa_covid_vaccines_raw[5]
    ? msoa_covid_vaccines_colours[5]
    : d === msoa_covid_vaccines_raw[6]
    ? msoa_covid_vaccines_colours[6]
    : "#feebe2";
}

function style_msoa_vaccine_total(feature) {
  return {
    fillColor: vaccine_msoa_colour(feature.properties.Total_banded),
    weight: 1,
    opacity: 1,
    color: "#c9c9c9",
    // dashArray: "3",
    fillOpacity: 1,
  };
}

// msoa_map_vaccine;

$.when(msoa_vaccine_total).done(function () {
  var msoa_map_vaccine_leaf = L.map("msoa_map_vaccine");

  var basemap_msoa_vaccine = L.tileLayer(tileUrl, {
    attribution,
    minZoom: 8,
  }).addTo(msoa_map_vaccine_leaf);

  var msoa_vaccine_total_hcl = L.geoJSON(msoa_vaccine_total.responseJSON, {
    style: style_msoa_vaccine_total,
  })
    .addTo(msoa_map_vaccine_leaf)
    .bindPopup(function (layer) {
      return (
        "<p><b>" +
        layer.feature.properties.msoa11nm +
        " (" +
        layer.feature.properties.LTLA_name +
        ")</b></p><p>A total of <b>" +
        d3.format(",.0f")(layer.feature.properties.Total_where_age_known) +
        "</b> people have received at least one dose of a COVID-19 vaccine.</p><p>Data correct as at " +
        vaccine_update_date
      );
    });

  msoa_map_vaccine_leaf.fitBounds(msoa_vaccine_total_hcl.getBounds());
});

function key_msoa_vaccines() {
  msoa_covid_vaccines_raw.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = "key_list";
    list.style.borderColor = msoa_covid_vaccines_colour_func(index);
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = msoa_covid_vaccines_colour_func(index);
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("msoa_vaccine_key");
    div.appendChild(list);
  });
}

key_msoa_vaccines();
