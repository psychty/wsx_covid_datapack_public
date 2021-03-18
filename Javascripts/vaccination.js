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

wsx_overall_cumulative = vaccine_at_a_glance.filter(function (d) {
  return d.Name === "West Sussex";
});

wsx_number_vaccinated = wsx_overall_cumulative[0].Total_where_age_known;
wsx_proportion_vaccinated = wsx_overall_cumulative[0].Proportion_age_known;
wsx_estimated_population = wsx_overall_cumulative[0].Population_16_and_over;

d3.select("#wsx_so_far").html(function (d) {
  return (
    "<b>The total number of people in West Sussex, recorded as having received at least one dose of a COVID-19 vaccination as of the " +
    vaccine_update_date +
    " was " +
    d3.format(",.0f")(wsx_number_vaccinated) +
    ". This is " +
    d3.format(".1%")(wsx_proportion_vaccinated) +
    " of the estimated population of people aged 16 and over.</b>"
  );
});

// ! Percentage visual

d3.select("#select_guage_area_button")
  .selectAll("myOptions")
  .data([
    "West Sussex",
    "Adur",
    "Arun",
    "Chichester",
    "Crawley",
    "Horsham",
    "Mid Sussex",
    "Worthing",
  ])
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

var width_guage = 250;
var height_guage = width_guage;
var innerR = width_guage * 0.3;
var outerR = width_guage * 0.4;
var twoPi = 2 * Math.PI;

var svg_overall_vaccinated = d3
  .select("#overall_guage_1")
  .append("svg")
  .attr("width", width_guage)
  .attr("height", height_guage)
  .append("g")
  .attr(
    "transform",
    "translate(" + width_guage / 2 + "," + height_guage / 2 + ")"
  )
  .attr("class", "percentage_guage");

// Retrieve the selected area name
var selected_vaccine_area = d3
  .select("#select_guage_area_button")
  .property("value");

overall_cumulative = vaccine_at_a_glance.filter(function (d) {
  return d.Name === selected_vaccine_area;
});

number_vaccinated = overall_cumulative[0].Total_where_age_known;
proportion_vaccinated = overall_cumulative[0].Proportion_age_known;
estimated_population = overall_cumulative[0].Population_16_and_over;

var arc_vaccine_overall = d3
  .arc()
  .startAngle(0)
  .innerRadius(innerR)
  .outerRadius(outerR);

svg_overall_vaccinated
  .append("path")
  .attr("class", "background")
  .attr("d", arc_vaccine_overall.endAngle(twoPi));

var foreground_vaccinated = svg_overall_vaccinated
  .append("path")
  .attr("class", "foreground");

var Percent_vaccinated_1 = svg_overall_vaccinated
  .append("text")
  .attr("id", "vaccine_overall_perc")
  .attr("text-anchor", "middle")
  .attr("class", "percent-vaccine")
  .attr("dy", "-0.25em");

svg_overall_vaccinated
  .append("text")
  .attr("text-anchor", "middle")
  .attr("id", "vaccinated_label_1")
  .attr("class", "description")
  .attr("dy", "0.5em")
  .text(
    d3.format(",.0f")(number_vaccinated) +
      " / " +
      d3.format(",.0f")(estimated_population)
  );

svg_overall_vaccinated
  .append("text")
  .attr("text-anchor", "middle")
  .attr("id", "deaths_label_2")
  .attr("class", "description")
  .attr("dy", "1.5em")
  .text("aged 16+ received");

svg_overall_vaccinated
  .append("text")
  .attr("text-anchor", "middle")
  .attr("id", "deaths_label_3")
  .attr("class", "description")
  .attr("dy", "2.5em")
  .text("at least one dose");

var i_vaccinated_prop = d3.interpolate(0, proportion_vaccinated);

svg_overall_vaccinated
  .transition()
  .duration(3000)
  .tween("vaccinated", function () {
    return function (t) {
      vaccinated = i_vaccinated_prop(t);
      foreground_vaccinated
        .attr("d", arc_vaccine_overall.endAngle(twoPi * vaccinated))
        .attr("fill", "#ff4f03");
      Percent_vaccinated_1.text(d3.format(".1%")(vaccinated));
    };
  });

function update_vaccine_guage(selected_vaccine_area) {
  var old_number_vaccinated = number_vaccinated;

  if (number_vaccinated === undefined) {
    old_number_vaccinated = 0.001;
  }

  var old_vaccine_percentage = proportion_vaccinated;

  if (proportion_vaccinated === undefined) {
    old_vaccine_percentage = 0.001;
  }

  var selected_vaccine_area = d3
    .select("#select_guage_area_button")
    .property("value");

  overall_cumulative = vaccine_at_a_glance.filter(function (d) {
    return d.Name === selected_vaccine_area;
  });

  number_vaccinated = overall_cumulative[0].Total_where_age_known;
  proportion_vaccinated = overall_cumulative[0].Proportion_age_known;
  estimated_population = overall_cumulative[0].Population_16_and_over;

  var i_vaccinated_prop = d3.interpolate(
    old_vaccine_percentage,
    proportion_vaccinated
  );

  svg_overall_vaccinated
    .selectAll("#vaccinated_label_1")
    .transition()
    .duration(750)
    .style("opacity", 0);

  svg_overall_vaccinated
    .transition()
    .duration(3000)
    .tween("vaccinated", function () {
      return function (t) {
        vaccinated = i_vaccinated_prop(t);
        foreground_vaccinated
          .attr("d", arc_vaccine_overall.endAngle(twoPi * vaccinated))
          .attr("fill", "#ff4f03");
        Percent_vaccinated_1.text(d3.format(".1%")(vaccinated));
      };
    });

  svg_overall_vaccinated
    .append("text")
    .attr("text-anchor", "middle")
    .attr("id", "vaccinated_label_1")
    .attr("class", "description")
    .attr("dy", "0.5em")
    .text(
      d3.format(",.0f")(number_vaccinated) +
        " / " +
        d3.format(",.0f")(estimated_population)
    )
    .style("opacity", 0)
    .transition()
    .duration(500)
    .style("opacity", 1);
}

d3.select("#select_guage_area_button").on("change", function (d) {
  var selected_vaccine_area = d3
    .select("#select_guage_area_button")
    .property("value");
  update_vaccine_guage(selected_vaccine_area);
});

// ! LTLA Age

var height_bars = height_line * 1;

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_ltla_age.json", false);
request.send(null);
var vaccine_ltla_age = JSON.parse(request.responseText);

var vaccine_ages = d3
  .map(vaccine_ltla_age, function (d) {
    return d.Age_group;
  })
  .keys();

// Perhaps the solution is in identifying whether to include individuals not yet vaccinated [as a tick box, off by default], if the user selects it then scale and bars are redrawn
var vaccine_status = ["At_least_one_dose", "Individuals_not_vaccinated"];
var vaccine_status = ["At_least_one_dose"];

console.log(
  d3
    .map(vaccine_status, function (d) {
      return d;
    })
    .keys()
);

var colour_vaccinated = d3
  .scaleOrdinal()
  .domain(vaccine_status)
  .range(["#ff4f03", "#e6e7e8"]);

var selected_vaccine_area = d3
  .select("#select_guage_area_button")
  .property("value");

var chosen_vaccine_age_area = vaccine_ltla_age.filter(function (d) {
  return d.Name === selected_vaccine_area;
});

var stackedData_vaccine_1 = d3.stack().keys(vaccine_status)(
  chosen_vaccine_age_area
);

// Use the stacked data to find the max height (length) of the bars
var max_vaccine_limit = d3.max(stackedData_vaccine_1, function (d) {
  return d[0][1];
});

// Create a tooltip for the lines and functions for displaying the tooltips as well as highlighting certain lines.
var tooltip_vaccine_age = d3
  .select("#vaccine_uptake_by_age")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip_class")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("padding", "10px");

var type_individual_vaccine_label = d3
  .scaleOrdinal()
  .domain(["At_least_one_dose", "Individuals_not_vaccinated"])
  .range([
    " people with at least one does of a COVID-19 vaccination.",
    " people who have not yet received a single dose of a COVID-19 vaccination (based on NIMS population estimates).",
  ]);

var showTooltip_vaccine_age = function (d, i) {
  var TypeName = d3.select(this.parentNode).datum().key;
  var TypeValue = d.data[TypeName];

  tooltip_vaccine_age
    .html(
      "<h5>" +
        d.data.Name +
        '</h5><p class = "side"><b>' +
        d.data.Age_group +
        "</b></p><p><b>" +
        d3.format(",.0f")(TypeValue) +
        "</b>" +
        type_individual_vaccine_label(TypeName) +
        '</p><p class = "side">This excludes a small number of individuals where the age was not recorded.</p>'
    )
    .style("opacity", 1)
    .attr("visibility", "visible")
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("visibility", "visible");
};

var mouseleave_vaccine_age = function (d) {
  tooltip_vaccine_age.style("visibility", "hidden");
};

// append the svg object to the body of the page
var svg_vaccine_age_1 = d3
  .select("#vaccine_uptake_by_age")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_bars + 30)
  .append("g")
  .attr("transform", "translate(" + 120 + "," + 30 + ")");

// x axis
var x_vaccine_ages = d3
  .scaleLinear()
  // .domain([
  //   d3.max(chosen_vaccine_age_area, function (d) {
  //     return +d.At_least_one_dose;
  //   }),
  //   0,
  // ])
  .domain([max_vaccine_limit, 0])
  .range([width_hm - 150, 0])
  .nice();

var xAxis_vaccine_ages = svg_vaccine_age_1
  .append("g")
  .attr("transform", "translate(0," + (height_bars - 30) + ")")
  .call(d3.axisBottom(x_vaccine_ages).tickFormat(d3.format(",.0f")));

xAxis_vaccine_ages.selectAll("text").style("font-size", ".8rem");

// y axis
var y_vaccine_ages = d3
  .scaleBand()
  .domain(vaccine_ages)
  .range([height_bars, 0])
  .padding([0.2]);

var yAxis_vaccine_ages = svg_vaccine_age_1
  .append("g")
  .attr("transform", "translate(0,-30)")
  .call(d3.axisLeft(y_vaccine_ages));

yAxis_vaccine_ages
  .selectAll("text")
  .attr("transform", "translate(0,0)")
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

var bars_vaccine_age = svg_vaccine_age_1
  .append("g")
  .selectAll("g")
  .data(stackedData_vaccine_1)
  .enter()
  .append("g")
  .attr("fill", function (d) {
    return colour_vaccinated(d.key);
  })
  .selectAll("rect")
  .data(function (d) {
    return d;
  })
  .enter()
  .append("rect")
  .attr("id", "bars_vaccine_age")
  .attr("x", function (d) {
    return x_vaccine_ages(d[0]);
  })
  .attr("height", y_vaccine_ages.bandwidth())
  .attr("y", function (d) {
    return y_vaccine_ages(d.data.Age_group) - y_vaccine_ages.bandwidth();
  })
  .attr("width", function (d) {
    return x_vaccine_ages(d[1]) - x_vaccine_ages(d[0]);
  })
  .on("mousemove", showTooltip_vaccine_age)
  .on("mouseout", mouseleave_vaccine_age);

// ! Map

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
