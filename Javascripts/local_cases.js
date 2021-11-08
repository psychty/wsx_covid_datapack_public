var case_key_msoa = [
  "0-2 cases",
  "3-5 cases",
  "6-10 cases",
  "11-20 cases",
  "21-30 cases",
  "31-40 cases",
  "41-50 cases",
  "More than 50 cases",
];
var case_key_msoa_colours = [
  "#feebe2",
  "#fcc5c0",
  "#fa9fb5",
  "#f768a1",
  "#dd3497",
  "#ae017e",
  "#7a0177",
  "#30002f",
];

var msoa_case_colour_func = d3
  .scaleOrdinal()
  .domain(case_key_msoa)
  .range(case_key_msoa_colours);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/msoa_summary.json", false);
request.send(null);
var msoa_summary_data = JSON.parse(request.responseText); // parse the fetched json data into a variable

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
    : d === case_key_msoa[5]
    ? case_key_msoa_colours[5]
    : d === case_key_msoa[6]
    ? case_key_msoa_colours[6]
    : d === case_key_msoa[7]
    ? case_key_msoa_colours[7]
    : "#feebe2";
}

function style_msoa_cases(feature) {
  return {
    fillColor: msoa_case_colour(feature.properties.Case_key),
    weight: 1,
    opacity: 0.6,
    color: "white",
    dashArray: "3",
    fillOpacity: 0.7,
  };
}

$.when(msoa_map_data).done(function () {
  var msoa_map = L.map("msoa_map_place");

  var msoa_basemap = L.tileLayer(tileUrl, {
    attribution,
    minZoom: 6,
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

      marker_chosen.setLatLng([chosen_lat, chosen_long]);
      msoa_map.setView([chosen_lat, chosen_long], 11);

      var msoa_summary_data_chosen = msoa_summary_data.filter(function (d) {
        return d.MSOA11NM == chosen_msoa;
      });

      console.log(msoa_summary_data_chosen);

      d3.select("#local_case_summary_1")
        .data(msoa_summary_data_chosen)
        .html(function (d) {
          return d.MSOA11NM + " (" + d.msoa11hclnm + ")";
        });

      if (msoa_summary_data_chosen[0]["Latest_rate"] == "No rate available") {
        d3.select("#local_case_summary_2")
          .data(msoa_summary_data_chosen)
          .html(function (d) {
            return (
              "<b class = 'case_latest'>" +
              d.This_week +
              "</b> cases in the seven days to " +
              complete_date
            );
          });
      }

      if (msoa_summary_data_chosen[0]["Latest_rate"] != "No rate available") {
        d3.select("#local_case_summary_2")
          .data(msoa_summary_data_chosen)
          .html(function (d) {
            return (
              "<b class = 'case_latest'>" +
              d.This_week +
              "</b> cases in the seven days to " +
              complete_date +
              ". This is <b class = 'case_latest'>" +
              d.Latest_rate +
              "</b> cases per 100,000 population."
            );
          });
      }

      d3.select("#local_case_summary_3")
        .data(msoa_summary_data_chosen)
        .html(function (d) {
          return d.Change_label;
        });
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

        d3.select("#local_case_summary_1").html(function (d) {
          return "The postcode you entered does not seem to be valid, please check and try again.";
        });
        d3.select("#local_case_summary_2").html(function (d) {
          return "This could be because there is a problem with the postcode look up tool we are using.";
        });
        d3.select("#local_case_summary_3").html(function (d) {
          return "";
        });
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

d3.select("#local_case_map_title").html(function (d) {
  return (
    "Number of confirmed COVID-19 cases in the seven days to " +
    complete_date +
    "; MSOAs in England"
  );
});
