width_hm_mort = width_hm + 100;
height_line_mort = height_line 

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/ons_weekly_mortality_dates.json", false);
request.send(null);
var ons_mortality_figures_dates = JSON.parse(request.responseText);

d3.select("#ons_dates_mortality")
  .data(ons_mortality_figures_dates)
  .html(function (d) {
    return (
      "The tables include deaths that occurred up to Friday " +
      d.Occurring_week_ending +
      " but were registered up to " +
      d.Reported_week_ending +
      ". Figures by place of death may differ to previously published figures due to improvements in the way we code place of death."
    );
  });

var covid_causes = ["Not attributed to Covid-19", "Covid-19"];

var attribute_label = d3
  .scaleOrdinal()
  .domain(["Not attributed to Covid-19", "Covid-19"])
  .range([
    "not attributed to Covid-19",
    "where Covid-19 was mentioned as an underlying or contributing factor",
  ]);

var colour_covid_non_covid_all_settings = d3
  .scaleOrdinal()
  .domain(covid_causes)
  .range(["#BDD7EE", "#2F5597"]);

var colour_covid_non_covid_carehomes = d3
  .scaleOrdinal()
  .domain(covid_causes)
  .range(["#FFD966", "#ED7D31"]);

/////////////////////////////////
// x = week = cause category //
/////////////////////////////////

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/deaths_all_settings.json", false);
request.send(null);
var deaths_by_week_all = JSON.parse(request.responseText); // parse the fetched json data into a variable

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/deaths_carehomes.json", false);
request.send(null);
var deaths_by_week_ch = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/deaths_limits_by_area.json", false);
request.send(null);
var deaths_limits_by_area = JSON.parse(request.responseText);

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_mortality_1_area_button")
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

// Retrieve the selected area name
var chosen_m1_area = d3
  .select("#select_mortality_1_area_button")
  .property("value");

var chosen_m1_df = deaths_by_week_all.filter(function (d) {
  return d.Name === chosen_m1_area;
});

var chosen_latest = chosen_m1_df.filter(function (d) {
  return d.Week_number === ons_mortality_figures_dates[0]["Week_number"];
});

// var chosen_latest = chosen_m1_df.filter(function (d) {
//   return (
//     d.Week_number ===
//     d3.max(chosen_m1_df, function (d) {
//       return d.Week_number;
//     })
//   );
// });

var chosen_limits = deaths_limits_by_area.filter(function (d) {
  return d.Name === chosen_m1_area;
});

// Create a tooltip for the lines and functions for displaying the tooltips as well as highlighting certain lines.
var tooltip_m1 = d3
  .select("#covid_non_covid_mortality_all_settings")
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

var showTooltip_m1 = function (d, i) {
  var causeName = d3.select(this.parentNode).datum().key;
  var causeValue = d.data[causeName];

  tooltip_m1
    .html(
      "<h5>" +
        d.data.Name +
        '</h5><p class = "side">Week number ' +
        d.data.Week_number +
        " - " +
        d.data.Date_label +
        "</p><p><b>" +
        causeName +
        "</b></p><p><b>" +
        d3.format(",.0f")(causeValue) +
        " deaths </b>" +
        attribute_label(causeName) +
        '</p><p class = "side">' +
        d.data.Deaths_in_week_label +
        '</p><p class = "side">' +
        d.data.Cumulative_deaths_label +
        "</p>"
    )
    .style("opacity", 1)
    .attr("visibility", "visible")
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("visibility", "visible");
};

var mouseleave_m1 = function (d) {
  tooltip_m1.style("visibility", "hidden");
};

var stackedData_m1 = d3.stack().keys(covid_causes)(chosen_m1_df);

weeks = chosen_m1_df.map(function (d) {
  return d.Date_label;
});

// append the svg object to the body of the page
var svg_fg_mortality_1 = d3
  .select("#covid_non_covid_mortality_all_settings")
  .append("svg")
  .attr("width", width_hm_mort)
  .attr("height", height_line_mort)
  .append("g")
  .attr("transform", "translate(" + 50 + "," + 20 + ")");

var x_m1 = d3
  .scaleBand()
  .domain(weeks)
  .range([0, width_hm_mort * 0.6]) // this is the 50 that was pushed over from the left plus another 10 so that the chart does not get cut off
  .padding([0.2]);

var xAxis_mortality_1 = svg_fg_mortality_1
  .append("g")
  .attr("transform", "translate(0," + (height_line_mort - 120) + ")")
  .call(d3.axisBottom(x_m1).tickSizeOuter(0));

xAxis_mortality_1
  .selectAll("text")
  .attr("transform", "translate(-10,10)rotate(-90)")
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

var first_mortality_period = weeks[0];
var last_mortality_period = weeks[weeks.length - 1];

// if (width_hm < 1050) {
  xAxis_mortality_1.call(
    d3
      .axisBottom(x_m1)
      .tickValues([first_mortality_period, 'w/e 2nd Apr 21', last_mortality_period])
  );

// "w/e 7th Jan 22"
  
  xAxis_mortality_1
    .selectAll("text")
    .attr("transform", "rotate(0)")
    .style("text-anchor", function (d, i) {
      return i % 2 ? "end" : "start";
    })
    .style("font-size", ".8rem");
// }

var y_m1_ts = d3
  .scaleLinear()
  .domain([0, chosen_limits[0].Limit])
  .range([height_line_mort - 120, 0])
  .nice();

var y_m1_ts_axis = svg_fg_mortality_1
  .append("g")
  .attr("transform", "translate(0,0)")
  .call(d3.axisLeft(y_m1_ts).tickFormat(d3.format(",.0f")));

// y_m1_ts_axis
// selectAll('text')
// .style("font-size", ".8rem");

var bars_m1 = svg_fg_mortality_1
  .append("g")
  .selectAll("g")
  .data(stackedData_m1)
  .enter()
  .append("g")
  .attr("z-index", "1")
  .attr("fill", function (d) {
    return colour_covid_non_covid_all_settings(d.key);
  })
  .selectAll("rect")
  .data(function (d) {
    return d;
  })
  .enter()
  .append("rect")
  .attr("id", "bars1")
  .attr("x", function (d) {
    return x_m1(d.data.Date_label);
  })
  .attr("y", function (d) {
    return y_m1_ts(d[1]);
  })
  .attr("height", function (d) {
    return y_m1_ts(d[0]) - y_m1_ts(d[1]);
  })
  .attr("width", x_m1.bandwidth())
  .on("mousemove", showTooltip_m1)
  .on("mouseout", mouseleave_m1);

svg_fg_mortality_1
  .append("text")
  .attr("x", width_hm_mort * 0.65)
  .attr("y", 40)
  .attr("id", "m1_chosen_area")
  .attr("text-anchor", "start")
  .text(chosen_m1_area)
  .style("font-weight", "bold")
  .style("font-size", "18px");

svg_fg_mortality_1
  .append("text")
  .attr("x", width_hm_mort * 0.65)
  .attr("y", 80)
  .attr("id", "m1_selected_cumulative_covid")
  .attr("text-anchor", "start")
  .text(d3.format(",.0f")(chosen_latest[0]["Cumulative_covid_deaths"]))
  .style("font-weight", "bold")
  .style("font-size", "22px");

svg_fg_mortality_1
  .append("text")
  .attr("id", "m1_place_1")
  .attr("x", function (d) {
    if (chosen_latest[0]["Cumulative_covid_deaths"] > 10000) {
      return width_hm_mort * 0.65 + 75;
    } else if (chosen_latest[0]["Cumulative_covid_deaths"] > 1000) {
      return width_hm_mort * 0.65 + 65;
    } else if (chosen_latest[0]["Cumulative_covid_deaths"] > 100) {
      return width_hm_mort * 0.65 + 45;
    } else {
      return width_hm_mort * 0.65 + 35;
    }
  })
  .attr("y", 70)
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("deaths attributed to Covid-19");

svg_fg_mortality_1
  .append("text")
  .attr("id", "m1_place_2")
  .attr("x", function (d) {
    if (chosen_latest[0]["Cumulative_covid_deaths"] > 10000) {
      return width_hm_mort * 0.65 + 75;
    } else if (chosen_latest[0]["Cumulative_covid_deaths"] > 1000) {
      return width_hm_mort * 0.65 + 65;
    } else if (chosen_latest[0]["Cumulative_covid_deaths"] > 100) {
      return width_hm_mort * 0.65 + 45;
    } else {
      return width_hm_mort * 0.65 + 35;
    }
  })
  .attr("y", 85)
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("so far up to " + ons_mortality_figures_dates[0].Occurring_week_ending);

svg_fg_mortality_1
  .append("text")
  .attr("id", "m1_place_3")
  .attr("x", width_hm_mort * 0.64)
  .attr("y", 112)
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("out of ");

svg_fg_mortality_1
  .append("text")
  .attr("x", width_hm_mort * 0.65 + 32)
  .attr("y", 120)
  .attr("id", "m1_selected_cumulative_all")
  .attr("text-anchor", "start")
  .text(d3.format(",.0f")(chosen_latest[0]["Cumulative_deaths_all_cause"]))
  .style("font-weight", "bold")
  .style("font-size", "22px");

svg_fg_mortality_1
  .append("text")
  .attr("x", function (d) {
    if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 100000) {
      return width_hm_mort * 0.65 + 115;
    } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 10000) {
      return width_hm_mort * 0.65 + 105;
    } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 1000) {
      return width_hm_mort * 0.65 + 95;
    } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 100) {
      return width_hm_mort * 0.65 + 75;
    } else {
      return width_hm_mort * 0.65 + 65;
    }
  })
  .attr("y", 110)
  .attr("id", "m1_selected_cumulative_all_text_1")
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("total deaths registered up");

svg_fg_mortality_1
  .append("text")
  .attr("x", function (d) {
    if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 100000) {
      return width_hm_mort * 0.65 + 115;
    } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 10000) {
      return width_hm_mort * 0.65 + 105;
    } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 1000) {
      return width_hm_mort * 0.65 + 95;
    } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 100) {
      return width_hm_mort * 0.65 + 75;
    } else {
      return width_hm_mort * 0.65 + 65;
    }
  })
  .attr("y", 125)
  .attr("id", "m1_selected_cumulative_all_text_2")
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("to " + ons_mortality_figures_dates[0].Reported_week_ending);

svg_fg_mortality_1
  .append("text")
  .attr("x", width_hm_mort * 0.65)
  .attr("y", 145)
  .attr("id", "m1_selected_cumulative_all_text_3")
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("The total deaths attributed to Covid-19");

svg_fg_mortality_1
  .append("text")
  .attr("x", width_hm_mort * 0.65)
  .attr("y", 160)
  .attr("id", "m1_selected_cumulative_all_text_4")
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text(
    "represent " +
      d3.format("0.1%")(
        chosen_latest[0]["Cumulative_covid_deaths"] /
          chosen_latest[0]["Cumulative_deaths_all_cause"]
      ) +
      " of all deaths."
  );

  var lines_m1 = svg_fg_mortality_1
  .append("path")
  .datum(chosen_m1_df)
  .attr("fill", "none")
  .attr("stroke", "#000000")
  .attr("id", "lines1")
  .style("stroke-dasharray", ("5, 1"))
  .attr("stroke-width", 2)
  .attr(
    "d",
    d3
      .line()
      .defined((d) => !isNaN(d.Expected_deaths))
      .x(function (d) {
        return x_m1(d.Date_label) + x_m1.bandwidth() / 2;
     })
      .y(function (d) {
      return y_m1_ts(d.Expected_deaths);
     })
  );

//! Figure 2

var chosen_m2_df = deaths_by_week_ch.filter(function (d) {
  return d.Name === chosen_m1_area;
});

var chosen_latest_ch = chosen_m2_df.filter(function (d) {
  return d.Week_number === ons_mortality_figures_dates[0]["Week_number"];
});

// Create a tooltip for the lines and functions for displaying the tooltips as well as highlighting certain lines.
var tooltip_m2 = d3
  .select("#covid_non_covid_mortality_carehomes")
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

var showTooltip_m2 = function (d, i) {
  var causeName = d3.select(this.parentNode).datum().key;
  var causeValue = d.data[causeName];

  tooltip_m2
    .html(
      "<h5>" +
        d.data.Name +
        '</h5><p class = "side">Week number ' +
        d.data.Week_number +
        " - " +
        d.data.Date_label +
        "</p><p><b>" +
        causeName +
        "</b></p><p> <b>" +
        d3.format(",.0f")(causeValue) +
        " deaths in care homes </b>" +
        attribute_label(causeName) +
        '</p><p class = "side">' +
        d.data.Deaths_in_week_label +
        '</p><p class = "side">' +
        d.data.Cumulative_deaths_label +
        "</p>"
    )
    .style("opacity", 1)
    .attr("visibility", "visible")
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("visibility", "visible");
};

var mouseleave_m2 = function (d) {
  tooltip_m2.style("visibility", "hidden");
};

var stackedData_m2 = d3.stack().keys(covid_causes)(chosen_m2_df);

// append the svg object to the body of the page
var svg_fg_mortality_2 = d3
  .select("#covid_non_covid_mortality_carehomes")
  .append("svg")
  .attr("width", width_hm_mort)
  .attr("height", height_line_mort)
  .append("g")
  .attr("transform", "translate(" + 50 + "," + 20 + ")");

var x_m2 = d3
  .scaleBand()
  .domain(weeks)
  .range([0, width_hm_mort * 0.6]) // this is the 50 that was pushed over from the left plus another 10 so that the chart does not get cut off
  .padding([0.2]);

var xAxis_mortality_2 = svg_fg_mortality_2
  .append("g")
  .attr("transform", "translate(0," + (height_line_mort - 120) + ")")
  .call(d3.axisBottom(x_m2).tickSizeOuter(0));

xAxis_mortality_2
  .selectAll("text")
  .attr("transform", "translate(-10,10)rotate(-90)")
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

// if (width_hm < 750) {
  xAxis_mortality_2.call(
    d3
      .axisBottom(x_m2)
      .tickValues([first_mortality_period, 'w/e 2nd Apr 21', last_mortality_period])
  );

  xAxis_mortality_2
    .selectAll("text")
    .attr("transform", "rotate(0)")
    .style("text-anchor", function (d, i) {
      return i % 2 ? "end" : "start";
    })
    .style("font-size", ".8rem");
// }

var y_m2_ts = d3
  .scaleLinear()
  .domain([0, chosen_limits[0].Limit])
  .range([height_line_mort - 120, 0])
  .nice();

var y_m2_ts_axis = svg_fg_mortality_2
  .append("g")
  .attr("transform", "translate(0,0)")
  .call(d3.axisLeft(y_m2_ts).tickFormat(d3.format(",.0f")));

var bars_m2 = svg_fg_mortality_2
  .append("g")
  .selectAll("g")
  .data(stackedData_m2)
  .enter()
  .append("g")
  .attr("fill", function (d) {
    return colour_covid_non_covid_carehomes(d.key);
  })
  .selectAll("rect")
  .data(function (d) {
    return d;
  })
  .enter()
  .append("rect")
  .attr("id", "bars2")
  .attr("x", function (d) {
    return x_m2(d.data.Date_label);
  })
  .attr("y", function (d) {
    return y_m2_ts(d[1]);
  })
  .attr("height", function (d) {
    return y_m2_ts(d[0]) - y_m2_ts(d[1]);
  })
  .attr("width", x_m2.bandwidth())
  .on("mousemove", showTooltip_m2)
  .on("mouseout", mouseleave_m2);

  
  var lines_m2 = svg_fg_mortality_2
  .append("path")
  .datum(chosen_m2_df)
  .attr("fill", "none")
  .attr("stroke", "maroon")
  .attr("id", "lines2")
  .style("stroke-dasharray", ("5, 1"))
  .attr("stroke-width", 2)
  .attr(
    "d",
    d3
      .line()
      .defined((d) => !isNaN(d.Expected_deaths))
      .x(function (d) {
        return x_m2(d.Date_label) + x_m2.bandwidth() / 2;
     })
      .y(function (d) {
      return y_m2_ts(d.Expected_deaths);
     })
  );


svg_fg_mortality_2
  .append("text")
  .attr("x", width_hm_mort * 0.65 - 20)
  .attr("y", 40)
  .attr("id", "m1_chosen_area")
  .attr("text-anchor", "start")
  .text(chosen_m1_area)
  .style("font-weight", "bold")
  .style("font-size", "18px");

svg_fg_mortality_2
  .append("circle")
  .attr("id", "covid_dot_m2")
  .attr("cx", width_hm_mort * 0.65 - 20)
  .attr("cy", 72)
  .attr("r", 10)
  .attr("fill", function (d) {
    return colour_covid_non_covid_carehomes("Covid-19");
  });

svg_fg_mortality_2
  .append("text")
  .attr("x", width_hm_mort * 0.65)
  .attr("y", 80)
  .attr("id", "m2_selected_cumulative_covid")
  .attr("text-anchor", "start")
  .text(d3.format(",.0f")(chosen_latest_ch[0]["Cumulative_covid_deaths"]))
  .style("font-weight", "bold")
  .style("font-size", "22px");

svg_fg_mortality_2
  .append("text")
  .attr("id", "m2_place_1")
  .attr("x", function (d) {
    if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 10000) {
      return width_hm_mort * 0.65 + 75;
    } else if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 1000) {
      return width_hm_mort * 0.65 + 65;
    } else if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 100) {
      return width_hm_mort * 0.65 + 45;
    } else {
      return width_hm_mort * 0.65 + 35;
    }
  })
  .attr("y", 70)
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("care home deaths attributed to Covid-19");

svg_fg_mortality_2
  .append("text")
  .attr("id", "m2_place_2")
  .attr("x", function (d) {
    if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 10000) {
      return width_hm_mort * 0.65 + 75;
    } else if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 1000) {
      return width_hm_mort * 0.65 + 65;
    } else if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 100) {
      return width_hm_mort * 0.65 + 45;
    } else {
      return width_hm_mort * 0.65 + 35;
    }
  })
  .attr("y", 85)
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("so far up to " + ons_mortality_figures_dates[0].Occurring_week_ending);

svg_fg_mortality_2
  .append("text")
  .attr("id", "m2_place_3")
  .attr("x", width_hm_mort * 0.64)
  .attr("y", 112)
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("out of ");

svg_fg_mortality_2
  .append("text")
  .attr("x", width_hm_mort * 0.65 + 32)
  .attr("y", 120)
  .attr("id", "m2_selected_cumulative_all")
  .attr("text-anchor", "start")
  .text(d3.format(",.0f")(chosen_latest_ch[0]["Cumulative_deaths_all_cause"]))
  .style("font-weight", "bold")
  .style("font-size", "22px");

svg_fg_mortality_2
  .append("text")
  .attr("x", function (d) {
    if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 100000) {
      return width_hm_mort * 0.65 + 115;
    } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 10000) {
      return width_hm_mort * 0.65 + 105;
    } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 1000) {
      return width_hm_mort * 0.65 + 95;
    } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 100) {
      return width_hm_mort * 0.65 + 75;
    } else {
      return width_hm_mort * 0.65 + 65;
    }
  })
  .attr("y", 110)
  .attr("id", "m2_selected_cumulative_all_text_1")
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("total deaths registered up");

svg_fg_mortality_2
  .append("text")
  .attr("x", function (d) {
    if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 100000) {
      return width_hm_mort * 0.65 + 115;
    } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 10000) {
      return width_hm_mort * 0.65 + 105;
    } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 1000) {
      return width_hm_mort * 0.65 + 95;
    } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 100) {
      return width_hm_mort * 0.65 + 75;
    } else {
      return width_hm_mort * 0.65 + 65;
    }
  })
  .attr("y", 125)
  .attr("id", "m2_selected_cumulative_all_text_2")
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("to " + ons_mortality_figures_dates[0].Reported_week_ending);

svg_fg_mortality_2
  .append("text")
  .attr("x", width_hm_mort * 0.65)
  .attr("y", 145)
  .attr("id", "m2_selected_cumulative_all_text_3")
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text("The total deaths attributed to Covid-19");

svg_fg_mortality_2
  .append("text")
  .attr("x", width_hm_mort * 0.65)
  .attr("y", 160)
  .attr("id", "m2_selected_cumulative_all_text_4")
  .attr("text-anchor", "start")
  .style("font-size", ".8rem")
  .text(
    "represent " +
      d3.format("0.1%")(
        chosen_latest_ch[0]["Cumulative_covid_deaths"] /
          chosen_latest_ch[0]["Cumulative_deaths_all_cause"]
      ) +
      " of all deaths."
  );

function update_m12() {
  var chosen_m1_area = d3
    .select("#select_mortality_1_area_button")
    .property("value");

  var chosen_m1_df = deaths_by_week_all.filter(function (d) {
    return d.Name === chosen_m1_area;
  });

  var chosen_latest = chosen_m1_df.filter(function (d) {
    return d.Week_number === ons_mortality_figures_dates[0]["Week_number"];
  });

  var chosen_m2_df = deaths_by_week_ch.filter(function (d) {
    return d.Name === chosen_m1_area;
  });

  var chosen_latest_ch = chosen_m2_df.filter(function (d) {
    return d.Week_number === ons_mortality_figures_dates[0]["Week_number"];
  });

  var chosen_limits = deaths_limits_by_area.filter(function (d) {
    return d.Name === chosen_m1_area;
  });

  d3.select("#selected_m1_title").html(function (d) {
    return (
      "Weekly deaths (all ages, all settings); " +
      chosen_m1_area +
      "; " +
      chosen_m1_df[0].Date_label +
      " - " +
      chosen_m1_df[chosen_m1_df.length - 1].Date_label
    );
  });

  d3.select("#selected_m1_subtitle").html(function (d) {
    return (
      "By week of occurrence and by COVID-19 mentioned on death certificate for deaths registered by " +
      ons_mortality_figures_dates[0].Reported_week_ending
    );
  });

  var stackedData_m1 = d3.stack().keys(covid_causes)(chosen_m1_df);

  var stackedData_m2 = d3.stack().keys(covid_causes)(chosen_m2_df);

  y_m1_ts
    .domain([0, chosen_limits[0].Limit])
    .range([height_line_mort - 120, 0])
    .nice();

  y_m1_ts_axis
    .transition()
    .duration(1000)
    .call(d3.axisLeft(y_m1_ts).tickFormat(d3.format(",.0f")));

  y_m1_ts_axis.selectAll("text").style("font-size", ".8rem");

  svg_fg_mortality_1.selectAll("#bars1").remove();
  svg_fg_mortality_1.selectAll('#lines1').remove()

  

  var bars_m1 = svg_fg_mortality_1
    .append("g")
    .selectAll("g")
    .data(stackedData_m1)
    .enter()
    .append("g")
    .attr("fill", function (d) {
      return colour_covid_non_covid_all_settings(d.key);
    })
    .selectAll("rect")
    .data(function (d) {
      return d;
    })
    .enter()
    .append("rect")
    .attr("id", "bars1")
    .attr("x", function (d) {
      return x_m1(d.data.Date_label);
    })
    .attr("y", function (d) {
      return y_m1_ts(d[1]);
    })
    .attr("height", function (d) {
      return y_m1_ts(d[0]) - y_m1_ts(d[1]);
    })
    .attr("width", x_m1.bandwidth());

   
  bars_m1.on("mousemove", showTooltip_m1).on("mouseout", mouseleave_m1);

  svg_fg_mortality_1.selectAll("#m1_chosen_area").remove();

  svg_fg_mortality_1.selectAll("#covid_dot_m1").remove();

  svg_fg_mortality_1.selectAll("#m1_selected_cumulative_covid").remove();

  svg_fg_mortality_1.selectAll("#m1_place_1").remove();

  svg_fg_mortality_1.selectAll("#m1_place_2").remove();

  svg_fg_mortality_1.selectAll("#m1_place_3").remove();

  svg_fg_mortality_1.selectAll("#m1_selected_cumulative_all").remove();

  svg_fg_mortality_1.selectAll("#m1_selected_cumulative_all_text_1").remove();

  svg_fg_mortality_1.selectAll("#m1_selected_cumulative_all_text_2").remove();

  svg_fg_mortality_1.selectAll("#m1_selected_cumulative_all_text_3").remove();

  svg_fg_mortality_1.selectAll("#m1_selected_cumulative_all_text_4").remove();

  svg_fg_mortality_1.selectAll("#m1_selected_cumulative_all").remove();

  svg_fg_mortality_1.selectAll("#m1_selected_cumulative_all_text_5").remove();

  svg_fg_mortality_1
    .append("text")
    .attr("x", width_hm_mort * 0.65 - 25)
    .attr("y", 40)
    .attr("id", "m1_chosen_area")
    .attr("text-anchor", "start")
    .text(chosen_m1_area)
    .style("font-weight", "bold")
    .style("font-size", "18px");

  svg_fg_mortality_1
    .append("circle")
    .attr("id", "covid_dot_m1")
    .attr("cx", width_hm_mort * 0.65 - 20)
    .attr("cy", 72)
    .attr("r", 10)
    .attr("fill", function (d) {
      return colour_covid_non_covid_all_settings("Covid-19");
    });

  svg_fg_mortality_1
    .append("text")
    .attr("x", width_hm_mort * 0.65)
    .attr("y", 80)
    .attr("id", "m1_selected_cumulative_covid")
    .attr("text-anchor", "start")
    .text(d3.format(",.0f")(chosen_latest[0]["Cumulative_covid_deaths"]))
    .style("font-weight", "bold")
    .style("font-size", "22px");

  svg_fg_mortality_1
    .append("text")
    .attr("id", "m1_place_1")
    .attr("x", function (d) {
      if (chosen_latest[0]["Cumulative_covid_deaths"] > 10000) {
        return width_hm_mort * 0.65 + 75;
      } else if (chosen_latest[0]["Cumulative_covid_deaths"] > 1000) {
        return width_hm_mort * 0.65 + 65;
      } else if (chosen_latest[0]["Cumulative_covid_deaths"] > 100) {
        return width_hm_mort * 0.65 + 45;
      } else {
        return width_hm_mort * 0.65 + 35;
      }
    })
    .attr("y", 70)
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("deaths attributed to Covid-19");

  svg_fg_mortality_1
    .append("text")
    .attr("id", "m1_place_2")
    .attr("x", function (d) {
      if (chosen_latest[0]["Cumulative_covid_deaths"] > 10000) {
        return width_hm_mort * 0.65 + 75;
      } else if (chosen_latest[0]["Cumulative_covid_deaths"] > 1000) {
        return width_hm_mort * 0.65 + 65;
      } else if (chosen_latest[0]["Cumulative_covid_deaths"] > 100) {
        return width_hm_mort * 0.65 + 45;
      } else {
        return width_hm_mort * 0.65 + 35;
      }
    })
    .attr("y", 85)
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text(
      "so far up to " + ons_mortality_figures_dates[0].Occurring_week_ending
    );

  svg_fg_mortality_1
    .append("text")
    .attr("id", "m1_place_3")
    .attr("x", width_hm_mort * 0.64)
    .attr("y", 112)
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("out of ");

  svg_fg_mortality_1
    .append("text")
    .attr("x", width_hm_mort * 0.65 + 30)
    .attr("y", 120)
    .attr("id", "m1_selected_cumulative_all")
    .attr("text-anchor", "start")
    .text(d3.format(",.0f")(chosen_latest[0]["Cumulative_deaths_all_cause"]))
    .style("font-weight", "bold")
    .style("font-size", "22px");

  svg_fg_mortality_1
    .append("text")
    .attr("x", function (d) {
      if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 100000) {
        return width_hm_mort * 0.65 + 115;
      } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 10000) {
        return width_hm_mort * 0.65 + 105;
      } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 1000) {
        return width_hm_mort * 0.65 + 95;
      } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 100) {
        return width_hm_mort * 0.65 + 75;
      } else {
        return width_hm_mort * 0.65 + 65;
      }
    })
    .attr("y", 110)
    .attr("id", "m1_selected_cumulative_all_text_1")
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("total deaths registered up");

  svg_fg_mortality_1
    .append("text")
    .attr("x", function (d) {
      if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 100000) {
        return width_hm_mort * 0.65 + 115;
      } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 10000) {
        return width_hm_mort * 0.65 + 105;
      } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 1000) {
        return width_hm_mort * 0.65 + 95;
      } else if (chosen_latest[0]["Cumulative_deaths_all_cause"] > 100) {
        return width_hm_mort * 0.65 + 75;
      } else {
        return width_hm_mort * 0.65 + 65;
      }
    })
    .attr("y", 125)
    .attr("id", "m1_selected_cumulative_all_text_2")
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("to " + ons_mortality_figures_dates[0].Reported_week_ending);

  svg_fg_mortality_1
    .append("text")
    .attr("x", width_hm_mort * 0.65)
    .attr("y", 145)
    .attr("id", "m1_selected_cumulative_all_text_3")
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("The total deaths attributed to Covid-19");

  svg_fg_mortality_1
    .append("text")
    .attr("x", width_hm_mort * 0.65)
    .attr("y", 160)
    .attr("id", "m1_selected_cumulative_all_text_4")
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text(
      "represent " +
        d3.format("0.1%")(
          chosen_latest[0]["Cumulative_covid_deaths"] /
            chosen_latest[0]["Cumulative_deaths_all_cause"]
        ) +
        " of all deaths."
    );

    // lines_m1
    // .datum(chosen_m1_df)
    // .transition()
    // .duration(500)
    //    .attr(
    //      "d",
    //      d3
    //        .line()
    //        .defined((d) => !isNaN(d.Expected_deaths))
    //        .x(function (d) {
    //          return x_m1(d.Date_label) + x_m1.bandwidth() / 2;
    //       })
    //        .y(function (d) {
    //        return y_m1_ts(d.Expected_deaths);
    //       })
    //    );

    var lines_m1 = svg_fg_mortality_1
    .append("path")
    .datum(chosen_m1_df)
    .attr("fill", "none")
    .attr("stroke", "maroon")
    .attr("id", "lines1")
    .style("stroke-dasharray", ("5, 1"))
    .attr("stroke-width", 2)
    .attr(
      "d",
      d3
        .line()
        .defined((d) => !isNaN(d.Expected_deaths))
        .x(function (d) {
          return x_m1(d.Date_label) + x_m1.bandwidth() / 2;
       })
        .y(function (d) {
        return y_m1_ts(d.Expected_deaths);
       })
    );
  
  d3.select("#selected_m2_title").html(function (d) {
    return (
      "Weekly deaths (all ages, care home settings); " +
      chosen_m1_area +
      "; " +
      chosen_m2_df[0].Date_label +
      " - " +
      chosen_m2_df[chosen_m1_df.length - 1].Date_label
    );
  });

  d3.select("#selected_m2_subtitle").html(function (d) {
    return (
      "By week of occurrence and by COVID-19 mentioned on death certificate for deaths registered by " +
      ons_mortality_figures_dates[0].Reported_week_ending
    );
  });

  svg_fg_mortality_2.selectAll("#bars2").remove();
  svg_fg_mortality_2.selectAll("#lines2").remove();

  var y_m2_ts = d3
    .scaleLinear()
    .domain([0, chosen_limits[0].Limit])
    .range([height_line_mort - 120, 0])
    .nice();

  y_m2_ts_axis
    .transition()
    .duration(1000)
    .call(d3.axisLeft(y_m2_ts).tickFormat(d3.format(",.0f")));

  y_m2_ts_axis.selectAll("text").style("font-size", ".8rem");

  var bars_m2 = svg_fg_mortality_2
    .append("g")
    .selectAll("g")
    .data(stackedData_m2)
    .enter()
    .append("g")
    .attr("fill", function (d) {
      return colour_covid_non_covid_carehomes(d.key);
    })
    .selectAll("rect")
    .data(function (d) {
      return d;
    })
    .enter()
    .append("rect")
    .attr("id", "bars2")
    .attr("x", function (d) {
      return x_m2(d.data.Date_label);
    })
    .attr("y", function (d) {
      return y_m2_ts(d[1]);
    })
    .attr("height", function (d) {
      return y_m2_ts(d[0]) - y_m2_ts(d[1]);
    })
    .attr("width", x_m2.bandwidth());

  bars_m2.on("mousemove", showTooltip_m2).on("mouseout", mouseleave_m2);

  
  var lines_m2 = svg_fg_mortality_2
  .append("path")
  .datum(chosen_m2_df)
  .attr("fill", "none")
  .attr("stroke", "maroon")
  .attr("id", "lines2")
  .style("stroke-dasharray", ("5, 1"))
  .attr("stroke-width", 2)
  .attr(
    "d",
    d3
      .line()
      .defined((d) => !isNaN(d.Expected_deaths))
      .x(function (d) {
        return x_m2(d.Date_label) + x_m2.bandwidth() / 2;
     })
      .y(function (d) {
      return y_m2_ts(d.Expected_deaths);
     })
  );


  svg_fg_mortality_2.selectAll("#m1_chosen_area").remove();
  svg_fg_mortality_2.selectAll("#covid_dot_m2").remove();
  svg_fg_mortality_2.selectAll("#m2_selected_cumulative_covid").remove();
  svg_fg_mortality_2.selectAll("#m2_place_1").remove();
  svg_fg_mortality_2.selectAll("#m2_place_2").remove();
  svg_fg_mortality_2.selectAll("#m2_place_3").remove();
  svg_fg_mortality_2.selectAll("#m2_selected_cumulative_all").remove();
  svg_fg_mortality_2.selectAll("#m2_selected_cumulative_all_text_1").remove();
  svg_fg_mortality_2.selectAll("#m2_selected_cumulative_all_text_2").remove();
  svg_fg_mortality_2.selectAll("#m2_selected_cumulative_all_text_3").remove();
  svg_fg_mortality_2.selectAll("#m2_selected_cumulative_all_text_4").remove();
  svg_fg_mortality_2.selectAll("#m2_selected_cumulative_all").remove();
  svg_fg_mortality_2.selectAll("#m2_selected_cumulative_all_text_5").remove();

  svg_fg_mortality_2
    .append("text")
    .attr("x", width_hm_mort * 0.65 - 25)
    .attr("y", 40)
    .attr("id", "m1_chosen_area")
    .attr("text-anchor", "start")
    .text(chosen_m1_area)
    .style("font-weight", "bold")
    .style("font-size", "18px");

  svg_fg_mortality_2
    .append("circle")
    .attr("id", "covid_dot_m2")
    .attr("cx", width_hm_mort * 0.65 - 20)
    .attr("cy", 72)
    .attr("r", 10)
    .attr("fill", function (d) {
      return colour_covid_non_covid_carehomes("Covid-19");
    });

  svg_fg_mortality_2
    .append("text")
    .attr("x", width_hm_mort * 0.65)
    .attr("y", 80)
    .attr("id", "m2_selected_cumulative_covid")
    .attr("text-anchor", "start")
    .text(d3.format(",.0f")(chosen_latest_ch[0]["Cumulative_covid_deaths"]))
    .style("font-weight", "bold")
    .style("font-size", "22px");

  svg_fg_mortality_2
    .append("text")
    .attr("id", "m2_place_1")
    .attr("x", function (d) {
      if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 10000) {
        return width_hm_mort * 0.65 + 75;
      } else if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 1000) {
        return width_hm_mort * 0.65 + 65;
      } else if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 100) {
        return width_hm_mort * 0.65 + 45;
      } else {
        return width_hm_mort * 0.65 + 35;
      }
    })
    .attr("y", 70)
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("care home deaths attributed to Covid-19");

  svg_fg_mortality_2
    .append("text")
    .attr("id", "m2_place_2")
    .attr("x", function (d) {
      if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 10000) {
        return width_hm_mort * 0.65 + 75;
      } else if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 1000) {
        return width_hm_mort * 0.65 + 65;
      } else if (chosen_latest_ch[0]["Cumulative_covid_deaths"] > 100) {
        return width_hm_mort * 0.65 + 45;
      } else {
        return width_hm_mort * 0.65 + 35;
      }
    })
    .attr("y", 85)
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text(
      "so far up to " + ons_mortality_figures_dates[0].Occurring_week_ending
    );

  svg_fg_mortality_2
    .append("text")
    .attr("id", "m2_place_3")
    .attr("x", width_hm_mort * 0.64)
    .attr("y", 112)
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("out of ");

  svg_fg_mortality_2
    .append("text")
    .attr("x", width_hm_mort * 0.65 + 30)
    .attr("y", 120)
    .attr("id", "m2_selected_cumulative_all")
    .attr("text-anchor", "start")
    .text(d3.format(",.0f")(chosen_latest_ch[0]["Cumulative_deaths_all_cause"]))
    .style("font-weight", "bold")
    .style("font-size", "22px");

  svg_fg_mortality_2
    .append("text")
    .attr("x", function (d) {
      if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 100000) {
        return width_hm_mort * 0.65 + 115;
      } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 10000) {
        return width_hm_mort * 0.65 + 105;
      } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 1000) {
        return width_hm_mort * 0.65 + 95;
      } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 100) {
        return width_hm_mort * 0.65 + 75;
      } else {
        return width_hm_mort * 0.65 + 65;
      }
    })
    .attr("y", 110)
    .attr("id", "m2_selected_cumulative_all_text_1")
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("total deaths registered up");

  svg_fg_mortality_2
    .append("text")
    .attr("x", function (d) {
      if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 100000) {
        return width_hm_mort * 0.65 + 115;
      } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 10000) {
        return width_hm_mort * 0.65 + 105;
      } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 1000) {
        return width_hm_mort * 0.65 + 95;
      } else if (chosen_latest_ch[0]["Cumulative_deaths_all_cause"] > 100) {
        return width_hm_mort * 0.65 + 75;
      } else {
        return width_hm_mort * 0.65 + 65;
      }
    })
    .attr("y", 125)
    .attr("id", "m2_selected_cumulative_all_text_2")
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("to " + ons_mortality_figures_dates[0].Reported_week_ending);

  svg_fg_mortality_2
    .append("text")
    .attr("x", width_hm_mort * 0.65)
    .attr("y", 145)
    .attr("id", "m2_selected_cumulative_all_text_3")
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("The total deaths attributed to Covid-19");

  svg_fg_mortality_2
    .append("text")
    .attr("x", width_hm_mort * 0.65)
    .attr("y", 160)
    .attr("id", "m2_selected_cumulative_all_text_4")
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text(
      "represent " +
        d3.format("0.1%")(
          chosen_latest_ch[0]["Cumulative_covid_deaths"] /
            chosen_latest_ch[0]["Cumulative_deaths_all_cause"]
        ) +
        " of all deaths."
    );
}

update_m12();

d3.select("#select_mortality_1_area_button").on("change", function (d) {
  var chosen_m1_area = d3
    .select("#select_mortality_1_area_button")
    .property("value");
  update_m12();
});
