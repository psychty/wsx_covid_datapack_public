var case_key_msoa = [
  "0-2 cases",
  "3-5 cases",
  "6-10 cases",
  "11-15 cases",
  "More than 15 cases",
];
var case_key_msoa_colours = [
  "#f1eef6",
  "#d7b5d8",
  "#df65b0",
  "#dd1c77",
  "#980043",
];

var msoa_case_colour_func = d3
  .scaleOrdinal()
  .domain(case_key_msoa)
  .range(case_key_msoa_colours);

// var width_msoa_map = document.getElementById("content_size").offsetWidth;

// Add AJAX request for data
var msoa_map_data = $.ajax({
  url: "./Outputs/msoa_covid_rate_latest.geojson",
  dataType: "json",
  success: console.log("MSOA boundary for cases successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText);
  },
});

var tileUrl = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png";
var attribution =
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br> Contains Ordnance Survey data © Crown copyright and database right 2020.<br>Contains Parliamentary information licensed under the Open Parliament Licence v3.0.<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons. Click on an area to find out more.';

function msoa_case_colour(d) {
  return d === case_key_msoa[0]
    ? case_key_msoa_colours[0]
    : d === case_key_msoa[1]
    ? case_key_msoa_colours[1]
    : d === case_key_msoa[2]
    ? case_key_msoa_colours[2]
    : d === case_key_msoa[3]
    ? case_key_msoa_colours[3]
    : d === case_key_msoa[4]
    ? case_key_msoa_colours[4]
    : "#feebe2";
}

function style_msoa_cases(feature) {
  return {
    fillColor: msoa_case_colour(feature.properties.Case_key),
    weight: 1,
    opacity: 0.6,
    color: "white",
    dashArray: "3",
    fillOpacity: 1,
  };
}

$.when(msoa_map_data).done(function () {
  var msoa_map = L.map("msoa_map_place");

  var msoa_basemap = L.tileLayer(tileUrl, {
    attribution,
    minZoom: 10,
  }).addTo(msoa_map);

  var msoa_map_hcl = L.geoJSON(msoa_map_data.responseJSON, {
    style: style_msoa_cases,
  })
    .addTo(msoa_map)
    .bindPopup(function (layer) {
      return (
        "<p style = 'font-size: .8rem'>" +
        layer.feature.properties.Label +
        "</p>"
      );
    });

  msoa_map.fitBounds(msoa_map_hcl.getBounds());

  var marker_chosen = L.marker([0, 0]).addTo(msoa_map_hcl);

  //search event
  $(document).on("click", "#btnPostcode", function () {
    var input = $("#txtPostcode").val();
    var url = "https://api.postcodes.io/postcodes/" + input;

    post(url).done(function (postcode) {
      var chosen_msoa = postcode["result"]["msoa"];
      var chosen_lat = postcode["result"]["latitude"];
      var chosen_long = postcode["result"]["longitude"];

      console.log(chosen_msoa, chosen_lat, chosen_long);
      // L.marker([chosen_lat, chosen_long]).addTo(msoa_map);
      add_chosen_ps();

      marker_chosen.setLatLng([chosen_lat, chosen_long]);
    });
  });

  //enter event - search
  $("#txtPostcode").keypress(function (e) {
    if (e.which === 13) {
      $("#btnPostcode").click();
    }
  });

  //ajax call
  function post(url) {
    return $.ajax({
      url: url,
      success: function () {
        //woop
      },
      error: function (desc, err) {
        $("#result_text").html("Details: " + desc.responseText);

        console.log("The postcode you entered does not seem to be valid");
      },
    });
  }
});

function key_msoa_cases() {
  case_key_msoa.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item + " in 7 days to " + complete_date;
    list.className = "key_list_msoa_cases";
    list.style.borderColor = msoa_case_colour_func(index);
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = msoa_case_colour_func(index);
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item.Case_key;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("msoa_case_key");
    div.appendChild(list);
  });
}

key_msoa_cases();

function add_chosen_ps() {
  console.log("working yo");
}
