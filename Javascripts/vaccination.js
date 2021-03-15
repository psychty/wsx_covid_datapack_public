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
    )}</td><td>${d3.format(".1%")(
      item.Proportion_age_known
    )}</td><td>${d3.format(",.0f")(item.Age_65_and_over)}</td><td>${d3.format(
      ".1%"
    )(item.Proportion_65_plus)}</td></tr>`;
  }

  tableBody.innerHTML = dataHTML;
}

// Parameters

var msoa_covid_vaccines_raw = [
  "Up to 1,999",
  "2,000-2,999",
  "3,000-3,999",
  "4,000-4,999",
  "5,000-5,999",
  "6,000-6,999",
  "7,000+",
];

var msoa_covid_vaccines_all_age_colours = [
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
  .range(msoa_covid_vaccines_all_age_colours);

var msoa_covid_vaccines_all_age_proportion_raw = [
  "Less than 10%",
  "10-19%",
  "20-29%",
  "30-39%",
  "40-49%",
  "50-59%",
  "60-69%",
  "70-79%",
  "80-89%",
  "90-99%",
  "100% of estimated population",
];

var msoa_covid_vaccines_all_age_proportion_colours = [
  "#FDE725",
  "#BBDF27",
  "#7AD151",
  "#43BF71",
  "#22A884",
  "#21908C",
  "#2A788E",
  "#35608D",
  "#414487",
  "#482576",
  "#440154",
];

var msoa_covid_vaccines_colour_proportions_func = d3
  .scaleOrdinal()
  .domain(msoa_covid_vaccines_all_age_proportion_raw)
  .range(msoa_covid_vaccines_all_age_proportion_colours);

var msoa_covid_vaccines_ages_currently_eligible_raw = [
  "Up to 999",
  "1,000-1,999",
  "2,000-2,999",
  "3,000-3,999",
  "4,000-4,999",
  "5,000+",
];

var msoa_covid_vaccines_ages_currently_eligible_colours = [
  "#ffffb2",
  "#fed976",
  "#feb24c",
  "#fd8d3c",
  "#f03b20",
  "#bd0026",
];

var msoa_covid_vaccines_colour_ages_currently_eligible_func = d3
  .scaleOrdinal()
  .domain(msoa_covid_vaccines_ages_currently_eligible_raw)
  .range(msoa_covid_vaccines_ages_currently_eligible_colours);

var msoa_covid_vaccines_ages_currently_eligible_proportion_raw = [
  "Less than 70%",
  "70-74%",
  "75-79%",
  "80-84%",
  "85-89%",
  "90-94%",
  "95-99%",
  "100% of estimated population",
];

var msoa_covid_vaccines_ages_currently_eligible_proportion_colours = [
  "#F0F921",
  "#FEBC2A",
  "#F48849",
  "#DB5C68",
  "#B93289",
  "#8B0AA5",
  "#5402A3",
  "#0D0887",
];

var msoa_covid_vaccines_ages_currently_eligible_colour_proportions_func = d3
  .scaleOrdinal()
  .domain(msoa_covid_vaccines_ages_currently_eligible_proportion_raw)
  .range(msoa_covid_vaccines_ages_currently_eligible_proportion_colours);

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

// All age count

function vaccine_msoa_colour_all_age_count(d) {
  return d === msoa_covid_vaccines_raw[0]
    ? msoa_covid_vaccines_all_age_colours[0]
    : d === msoa_covid_vaccines_raw[1]
    ? msoa_covid_vaccines_all_age_colours[1]
    : d === msoa_covid_vaccines_raw[2]
    ? msoa_covid_vaccines_all_age_colours[2]
    : d === msoa_covid_vaccines_raw[3]
    ? msoa_covid_vaccines_all_age_colours[3]
    : d === msoa_covid_vaccines_raw[4]
    ? msoa_covid_vaccines_all_age_colours[4]
    : d === msoa_covid_vaccines_raw[5]
    ? msoa_covid_vaccines_all_age_colours[5]
    : d === msoa_covid_vaccines_raw[6]
    ? msoa_covid_vaccines_all_age_colours[6]
    : "#feebe2";
}

function style_msoa_vaccine_total(feature) {
  return {
    fillColor: vaccine_msoa_colour_all_age_count(
      feature.properties.Total_banded
    ),
    weight: 0.5,
    opacity: 1,
    color: "#000000",
    // dashArray: "3",
    fillOpacity: 1,
  };
}

// All age proportion
function vaccine_msoa_colour_all_age_proportion(d) {
  return d === msoa_covid_vaccines_all_age_proportion_raw[0]
    ? msoa_covid_vaccines_all_age_proportion_colours[0]
    : d === msoa_covid_vaccines_all_age_proportion_raw[1]
    ? msoa_covid_vaccines_all_age_proportion_colours[1]
    : d === msoa_covid_vaccines_all_age_proportion_raw[2]
    ? msoa_covid_vaccines_all_age_proportion_colours[2]
    : d === msoa_covid_vaccines_all_age_proportion_raw[3]
    ? msoa_covid_vaccines_all_age_proportion_colours[3]
    : d === msoa_covid_vaccines_all_age_proportion_raw[4]
    ? msoa_covid_vaccines_all_age_proportion_colours[4]
    : d === msoa_covid_vaccines_all_age_proportion_raw[5]
    ? msoa_covid_vaccines_all_age_proportion_colours[5]
    : d === msoa_covid_vaccines_all_age_proportion_raw[6]
    ? msoa_covid_vaccines_all_age_proportion_colours[6]
    : d === msoa_covid_vaccines_all_age_proportion_raw[7]
    ? msoa_covid_vaccines_all_age_proportion_colours[7]
    : d === msoa_covid_vaccines_all_age_proportion_raw[8]
    ? msoa_covid_vaccines_all_age_proportion_colours[8]
    : d === msoa_covid_vaccines_all_age_proportion_raw[9]
    ? msoa_covid_vaccines_all_age_proportion_colours[9]
    : d === msoa_covid_vaccines_all_age_proportion_raw[10]
    ? msoa_covid_vaccines_all_age_proportion_colours[10]
    : "#feebe2";
}

function style_msoa_vaccine_total_proportion(feature) {
  return {
    fillColor: vaccine_msoa_colour_all_age_proportion(
      feature.properties.Proportion_age_known_banded
    ),
    weight: 0.5,
    opacity: 1,
    color: "#000000",
    // dashArray: "3",
    fillOpacity: 1,
  };
}

// ages_currently_eligible styles count
function vaccine_msoa_colour_ages_currently_eligible_count(d) {
  return d === msoa_covid_vaccines_ages_currently_eligible_raw[0]
    ? msoa_covid_vaccines_ages_currently_eligible_colours[0]
    : d === msoa_covid_vaccines_ages_currently_eligible_raw[1]
    ? msoa_covid_vaccines_ages_currently_eligible_colours[1]
    : d === msoa_covid_vaccines_ages_currently_eligible_raw[2]
    ? msoa_covid_vaccines_ages_currently_eligible_colours[2]
    : d === msoa_covid_vaccines_ages_currently_eligible_raw[3]
    ? msoa_covid_vaccines_ages_currently_eligible_colours[3]
    : d === msoa_covid_vaccines_ages_currently_eligible_raw[4]
    ? msoa_covid_vaccines_ages_currently_eligible_colours[4]
    : d === msoa_covid_vaccines_ages_currently_eligible_raw[5]
    ? msoa_covid_vaccines_ages_currently_eligible_colours[5]
    : "#feebe2";
}

function style_msoa_vaccine_count_ages_currently_eligible(feature) {
  return {
    fillColor: vaccine_msoa_colour_ages_currently_eligible_count(
      feature.properties.Total_age_65_banded
    ),
    weight: 0.5,
    opacity: 1,
    color: "#000000",
    // dashArray: "3",
    fillOpacity: 1,
  };
}

// Ages currently eligible proportion
function vaccine_msoa_colour_ages_currently_eligible_proportion(d) {
  return d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[0]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[0]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[1]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[1]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[2]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[2]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[3]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[3]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[4]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[4]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[5]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[5]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[6]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[6]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[7]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[7]
    : "#feebe2";
}

function style_msoa_vaccine_ages_currently_eligible_proportion(feature) {
  return {
    fillColor: vaccine_msoa_colour_ages_currently_eligible_proportion(
      feature.properties.Proportion_65_plus_banded
    ),
    weight: 0.5,
    opacity: 1,
    color: "#000000",
    // dashArray: "3",
    fillOpacity: 1,
  };
}

// msoa_map_vaccine;
$.when(msoa_vaccine_total).done(function () {
  var msoa_map_vaccine_leaf = L.map("msoa_map_vaccine");

  var msoa_vaccine_all_age_2_proportion_map_layer = L.geoJSON(
    msoa_vaccine_total.responseJSON,
    {
      style: style_msoa_vaccine_total_proportion,
    }
  ).bindPopup(function (layer) {
    return (
      "<p><b>" +
      layer.feature.properties.msoa11nm +
      " (" +
      layer.feature.properties.msoa11cd +
      ")</b></p><p>A total of <b>" +
      d3.format(",.0f")(layer.feature.properties.Total_where_age_known) +
      "</b> people aged 16+ have received at least one dose of a COVID-19 vaccine. This is <b>" +
      d3.format(".1%")(layer.feature.properties.Proportion_age_known) +
      " </b>of the estimated population in this area.</p><p>A total of <b>" +
      d3.format(",.0f")(layer.feature.properties.Age_65_and_over) +
      " </b>people aged 65+ have received at least one dose (<b>" +
      d3.format(".1%")(layer.feature.properties.Proportion_65_plus) +
      "</b>).</p><p>Data correct as at " +
      vaccine_update_date +
      ".</p>"
    );
  });

  var msoa_vaccine_all_age_1_count_map_layer = L.geoJSON(
    msoa_vaccine_total.responseJSON,
    {
      style: style_msoa_vaccine_total,
    }
  )
    .addTo(msoa_map_vaccine_leaf)
    .bindPopup(function (layer) {
      return (
        "<p><b>" +
        layer.feature.properties.msoa11nm +
        " (" +
        layer.feature.properties.msoa11cd +
        ")</b></p><p>A total of <b>" +
        d3.format(",.0f")(layer.feature.properties.Total_where_age_known) +
        "</b> people aged 16+ have received at least one dose of a COVID-19 vaccine. This is <b>" +
        d3.format(".1%")(layer.feature.properties.Proportion_age_known) +
        " </b>of the estimated population in this area.</p><p>A total of <b>" +
        d3.format(",.0f")(layer.feature.properties.Age_65_and_over) +
        " </b>people aged 65+ have received at least one dose (<b>" +
        d3.format(".1%")(layer.feature.properties.Proportion_65_plus) +
        "</b>).</p><p>Data correct as at " +
        vaccine_update_date +
        ".</p>"
      );
    });

  var baseMaps_all_age = {
    "Number of individuals": msoa_vaccine_all_age_1_count_map_layer,
    "Proportion of estimated population aged 16+": msoa_vaccine_all_age_2_proportion_map_layer,
  };

  var basemap_msoa_vaccine = L.tileLayer(tileUrl, {
    attribution,
    minZoom: 8,
  }).addTo(msoa_map_vaccine_leaf);

  L.control
    .layers(baseMaps_all_age, null, { collapsed: false })
    .addTo(msoa_map_vaccine_leaf);

  msoa_map_vaccine_leaf.fitBounds(
    msoa_vaccine_all_age_1_count_map_layer.getBounds()
  );

  msoa_map_vaccine_leaf.on("baselayerchange", function (ev) {
    console.log("Base layer changes");
    var selected_base_layer = ev.name;
    if (selected_base_layer === "Proportion of estimated population aged 16+") {
      key_msoa_vaccines_proportion();
    }
    if (selected_base_layer === "Number of individuals") {
      key_msoa_vaccines();
    }
  });

  // ! Over 65s

  var msoa_map_ages_currently_eligible_vaccine_leaf = L.map(
    "msoa_map_vaccine_ages_currently_eligible"
  );

  var msoa_vaccine_older_age_2_proportion_map_layer = L.geoJSON(
    msoa_vaccine_total.responseJSON,
    {
      style: style_msoa_vaccine_ages_currently_eligible_proportion,
    }
  ).bindPopup(function (layer) {
    return (
      "<p><b>" +
      layer.feature.properties.msoa11nm +
      " (" +
      layer.feature.properties.msoa11cd +
      ")</b></p><p>A total of <b>" +
      d3.format(",.0f")(layer.feature.properties.Age_65_and_over) +
      "</b> people aged 65+ have received at least one dose of a COVID-19 vaccine. This is <b>" +
      d3.format(".1%")(layer.feature.properties.Proportion_65_plus) +
      " </b>of the estimated population in this area.</p><p>Data correct as at " +
      vaccine_update_date +
      ".</p>"
    );
  });

  var msoa_vaccine_ages_currently_eligible_1_count_map_layer = L.geoJSON(
    msoa_vaccine_total.responseJSON,
    {
      style: style_msoa_vaccine_count_ages_currently_eligible,
    }
  )
    .addTo(msoa_map_ages_currently_eligible_vaccine_leaf)
    .bindPopup(function (layer) {
      return (
        "<p><b>" +
        layer.feature.properties.msoa11nm +
        " (" +
        layer.feature.properties.msoa11cd +
        ")</b></p><p>A total of <b>" +
        d3.format(",.0f")(layer.feature.properties.Age_65_and_over) +
        "</b> people aged 65+ have received at least one dose of a COVID-19 vaccine. This is <b>" +
        d3.format(".1%")(layer.feature.properties.Proportion_65_plus) +
        " </b>of the estimated population in this area.</p><p>Data correct as at " +
        vaccine_update_date +
        ".</p>"
      );
    });

  var baseMaps_age_currently_eligible = {
    "Number of individuals aged 65+": msoa_vaccine_ages_currently_eligible_1_count_map_layer,
    "Proportion of estimated population aged 65+": msoa_vaccine_older_age_2_proportion_map_layer,
  };

  var basemap_msoa_ages_currently_eligible_vaccine = L.tileLayer(tileUrl, {
    attribution,
    minZoom: 8,
  }).addTo(msoa_map_ages_currently_eligible_vaccine_leaf);

  L.control
    .layers(baseMaps_age_currently_eligible, null, { collapsed: false })
    .addTo(msoa_map_ages_currently_eligible_vaccine_leaf);

  msoa_map_ages_currently_eligible_vaccine_leaf.fitBounds(
    msoa_vaccine_ages_currently_eligible_1_count_map_layer.getBounds()
  );

  msoa_map_ages_currently_eligible_vaccine_leaf.on(
    "baselayerchange",
    function (ev) {
      console.log("Base layer changes");
      var selected_base_layer = ev.name;
      if (
        selected_base_layer === "Proportion of estimated population aged 65+"
      ) {
        key_msoa_vaccines_ages_currently_eligible_proportion();
      }
      if (selected_base_layer === "Number of individuals aged 65+") {
        key_msoa_ages_currently_eligible_vaccines();
      }
    }
  );
});

// ! Keys

function key_msoa_vaccines() {
  $(".key_list_vaccine_all").remove();

  d3.select("#msoa_map_vaccine_all_title").html(function (d) {
    return "Cumulative number of individuals receiving at least one Covid-19 vaccination dose; Sussex MSOAs;";
  });

  d3.select("#all_age_msoa_map_key_title").html(function (d) {
    return "Number of people aged 65+ receiving at least one dose";
  });

  msoa_covid_vaccines_raw.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = "key_list_vaccine_all";
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

function key_msoa_vaccines_proportion() {
  $(".key_list_vaccine_all").remove();

  d3.select("#msoa_map_vaccine_all_title").html(function (d) {
    return "Proportion of individuals (aged 16+) receiving at least one Covid-19 vaccination dose; Sussex MSOAs;";
  });

  d3.select("#all_age_msoa_map_key_title").html(function (d) {
    return "Proportion of people aged 16+ receiving at least one dose";
  });

  msoa_covid_vaccines_all_age_proportion_raw.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = "key_list_vaccine_all";
    list.style.borderColor = msoa_covid_vaccines_colour_proportions_func(index);
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = msoa_covid_vaccines_colour_proportions_func(index);
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("msoa_vaccine_key");
    div.appendChild(list);
  });
}

key_msoa_vaccines();

function key_msoa_ages_currently_eligible_vaccines() {
  $(".key_list_vaccine_ages_currently_eligible").remove();

  d3.select("#msoa_map_vaccine_ages_currently_eligible_title").html(function (
    d
  ) {
    return "Cumulative number of individuals aged 65+ receiving at least one Covid-19 vaccination dose; Sussex MSOAs;";
  });

  d3.select("#ages_currently_eligible_msoa_map_key_title").html(function (d) {
    return "Number of people receiving at least one dose";
  });

  msoa_covid_vaccines_ages_currently_eligible_raw.forEach(function (
    item,
    index
  ) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = "key_list_vaccine_ages_currently_eligible";
    list.style.borderColor = msoa_covid_vaccines_colour_ages_currently_eligible_func(
      index
    );
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = msoa_covid_vaccines_colour_ages_currently_eligible_func(
      index
    );
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById(
      "msoa_vaccine_ages_currently_eligible_key"
    );
    div.appendChild(list);
  });
}

function key_msoa_vaccines_ages_currently_eligible_proportion() {
  $(".key_list_vaccine_ages_currently_eligible").remove();

  d3.select("#msoa_map_vaccine_ages_currently_eligible_title").html(function (
    d
  ) {
    return "Proportion of individuals (aged 65+) receiving at least one Covid-19 vaccination dose; Sussex MSOAs;";
  });

  d3.select("#ages_currently_eligible_msoa_map_key_title").html(function (d) {
    return "Proportion of people aged 65+ receiving at least one dose";
  });

  msoa_covid_vaccines_ages_currently_eligible_proportion_raw.forEach(function (
    item,
    index
  ) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = "key_list_vaccine_ages_currently_eligible";
    list.style.borderColor = msoa_covid_vaccines_ages_currently_eligible_colour_proportions_func(
      index
    );
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = msoa_covid_vaccines_ages_currently_eligible_colour_proportions_func(
      index
    );
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById(
      "msoa_vaccine_ages_currently_eligible_key"
    );
    div.appendChild(list);
  });
}

key_msoa_ages_currently_eligible_vaccines();
