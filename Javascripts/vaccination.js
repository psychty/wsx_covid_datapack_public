var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_update_date.json", false);
request.send(null);
var vaccine_update_date = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_administered_date.json", false);
request.send(null);
var vaccine_administered_date = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_latest_dose_date.json", false);
request.send(null);
var vaccine_latest_dose_date = JSON.parse(request.responseText)[0]["Date"];

d3.select("#latest_vaccine_publication_date").html(function (d) {
  return (
    "The data in this section come from NHS England and are collected through the National Immunisation Management Service (NIMS). Data at lower tier local authority level are updated every day and so the overall coverage data includes data as at " +
    data_refreshed_date +
    " for <b>vaccinations given up to " +
    vaccine_latest_dose_date +
    "</b>."
  );
});

d3.select("#latest_local_vaccine_publication_date").html(function (d) {
  return (
    "Data at district and borough level are updated every day. However, data at small area level (Middle Super Output Area) are currently updated once per week, on a Thursday, with data up to the previous Sunday. The data are broken down for all adults aged 12 and over who are eligible for their first vaccination appointment. This local area data was last updated on <b>" +
    vaccine_update_date +
    " and includes vaccines administered from " +
    vaccine_administered_date +
    "</b>."
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
      item["Number of individuals aged 12 and over"]
    )}</td><td>${d3.format(".1%")(
      item["Proportion (12 and over)"]
    )}</td><td>${d3.format(",.0f")(
      item["Number of individuals aged 16-64 years"]
    )}</td><td>${d3.format(".1%")(
      item["Proportion (16-64 years)"]
    )}</td><td>${d3.format(",.0f")(
      item["Number of individuals aged 65 and over"]
    )}</td><td>${d3.format(".1%")(item["Proportion (65 and over)"])}</td></tr>`;
  }

  tableBody.innerHTML = dataHTML;
}

wsx_overall_cumulative = vaccine_at_a_glance.filter(function (d) {
  return d.Name === "West Sussex";
});

wsx_number_vaccinated =
  wsx_overall_cumulative[0].Total_first_dose_where_age_known;
wsx_proportion_vaccinated =
  wsx_overall_cumulative[0].First_dose_proportion_age_known;
wsx_estimated_population = wsx_overall_cumulative[0].Population_12_and_over;

d3.select("#wsx_so_far").html(function (d) {
  return (
    "<b>The total number of people in West Sussex, recorded as having received at least one dose of a COVID-19 vaccination as of the " +
    vaccine_update_date +
    " was " +
    d3.format(",.0f")(wsx_number_vaccinated) +
    ". This is " +
    d3.format(".1%")(wsx_proportion_vaccinated) +
    " of the estimated population of people aged 12 and over.</b>"
  );
});

// ! Over time

var svg_vaccine_uptake_by_dose_timeseries = d3
  .select("#vaccine_uptake_by_dose_timeseries")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line + 30)
  .append("g")
  .attr("transform", "translate(" + 120 + "," + 30 + ")");

d3.select("#vaccine_uptake_by_dose_timeseries_area_select")
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
    "South East",
    "England",
  ])
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccination_timeseries_date_labels.json", false);
request.send(null);
var vaccination_timeseries_date_labels = JSON.parse(request.responseText).map(
  function (d) {
    return d.Date_label;
  }
);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccination_timeseries_overall.json", false);
request.send(null);
var vaccination_timeseries_data = JSON.parse(request.responseText);

// get the button
var type_vac_ts_scale = document.getElementsByName("toggle_vac_ts_rate");

// Retrieve the selected area name
var vaccine_uptake_by_dose_timeseries_area_option = d3
  .select("#vaccine_uptake_by_dose_timeseries_area_select")
  .property("value");

var vaccination_timeseries_dates = d3
  .map(vaccination_timeseries_data, function (d) {
    return d.Date_label;
  })
  .keys();

var vaccine_timeseries_chosen = vaccination_timeseries_data.filter(function (
  d
) {
  return d.Name === vaccine_uptake_by_dose_timeseries_area_option;
});

// Group the data
var vaccine_timeseries_chosen_group = d3
  .nest() // nest function allows to group the calculation per level of a factor
  .key(function (d) {
    return d.Dose_number;
  })
  .entries(vaccine_timeseries_chosen);

var x_vaccine_ts_1 = d3
  .scaleBand()
  .domain(vaccination_timeseries_dates)
  .range([0, width_hm - 120]);

var xAxis_vaccine_ts_1 = svg_vaccine_uptake_by_dose_timeseries
  .append("g")
  .attr("transform", "translate(0," + (height_line - 80) + ")")
  .call(
    d3.axisBottom(x_vaccine_ts_1).tickValues(vaccination_timeseries_date_labels)
  );

xAxis_vaccine_ts_1
  .selectAll("text")
  .attr(
    "transform",
    "translate(-" + (x_vaccine_ts_1.bandwidth() + 15) + ",10)rotate(-90)"
  )
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

var tooltip_vaccine_ts_1 = d3
  .select("#vaccine_uptake_by_dose_timeseries")
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

var dose_number = ["Dose_1", "Dose_2"];

var dose_number_colours = d3
  .scaleOrdinal()
  .domain(dose_number)
  .range(["#fa8800", "#00563f"]);

var dose_number_label = d3
  .scaleOrdinal()
  .domain(dose_number)
  .range(["First dose", "Second dose"]);

// We need to create two different functions for when a user hovers over the dots and when they hover over the lines
// Lines first
var hover_vaccine_ts_1_lines = function (d) {
  var highlighted_key = d.key;

  lines_vaccine_ts_1
    .transition()
    .duration(300)
    .style("stroke", function (d) {
      if (d.key === highlighted_key) {
        return dose_number_colours(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .attr("stroke-width", function (d) {
      if (d.key === highlighted_key) {
        return 2;
      } else {
        return 0.25;
      }
    });

  dots_vaccine_ts_1
    .transition()
    .duration(300)
    .style("fill", function (d) {
      if (d.Dose_number === highlighted_key) {
        return dose_number_colours(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .style("stroke", function (d) {
      if (d.Dose_number === highlighted_key) {
        return dose_number_colours(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .attr("r", function (d) {
      if (d.Dose_number === highlighted_key) {
        return 2;
      } else {
        return 0;
      }
    });
};

var hover_vaccine_ts_1_dots = function (d) {
  var highlighted_key = d.Dose_number;

  tooltip_vaccine_ts_1
    .html(
      "<h4>" +
        d.Name +
        '</h4><p class = "side">The number of doses among those living in ' +
        d.Name +
        " in the seven days to " +
        d.Date_label +
        " is <b>" +
        d3.format(",.0f")(d.Seven_day_rolling_vaccinations) +
        "</b>. This is " +
        d3.format(",.0f")(d.Seven_day_rolling_rate_vaccinations) +
        ' per 100,000 population.</p><p class = "side">' +
        // 'So far, there have been a total of ' +
        //   d3.format(",.0f")(d.Cumulative_cases) +
        //   " cases among this age group in " +
        //   d.Name +
        ".</p>"
    )
    .style("opacity", 1)
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("visibility", "visible");

  lines_vaccine_ts_1
    .transition()
    .duration(300)
    .style("stroke", function (d) {
      if (d.key === highlighted_key) {
        return dose_number_colours(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .attr("stroke-width", function (d) {
      if (d.key === highlighted_key) {
        return 2;
      } else {
        return 0.25;
      }
    });

  dots_vaccine_ts_1
    .transition()
    .duration(300)
    .style("fill", function (d) {
      if (d.Dose_number === highlighted_key) {
        return dose_number_colours(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .style("stroke", function (d) {
      if (d.Dose_number === highlighted_key) {
        return dose_number_colours(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .attr("r", function (d) {
      if (d.Dose_number === highlighted_key) {
        return 2;
      } else {
        return 0;
      }
    });
};

// No matter which function was called, on mouseleave restore everything back to the way it was.
var mouseleave_vaccine_ts_1 = function (d) {
  tooltip_vaccine_ts_1.style("visibility", "hidden");

  lines_vaccine_ts_1
    .transition()
    .delay(500)
    .duration(500)
    .style("stroke", function (d) {
      return dose_number_colours(d.key);
    })
    .style("visibility", "visible")
    .attr("stroke-width", 2);

  dots_vaccine_ts_1
    .transition()
    .delay(500)
    .duration(500)
    .style("fill", function (d) {
      return dose_number_colours(d.Dose_number);
    })
    .style("stroke", function (d) {
      return dose_number_colours(d.Dose_number);
    })
    .attr("r", 2)
    .style("visibility", "visible");
};

// Things specific to doses
if (type_vac_ts_scale[0].checked) {
  console.log("User selected actual dose numbers");

  // Update text based on selected area
  d3.select("#vaccine_uptake_by_dose_timeseries_title").html(function (d) {
    return (
      "Rolling 7 day number of Covid-19 vaccinations received; " +
      vaccine_uptake_by_dose_timeseries_area_option +
      "; as at " +
      data_refreshed_date
    );
  });

  // y scale for doses
  y_vaccine_ts_1 = d3
    .scaleLinear()
    .domain([
      0,
      d3.max(vaccine_timeseries_chosen, function (d) {
        return +d.Seven_day_rolling_vaccinations;
      }),
    ])
    .range([height_line - 80, 0])
    .nice();

  var y_vaccine_ts_1_axis = svg_vaccine_uptake_by_dose_timeseries
    .append("g")
    .attr("transform", "translate(0,0)")
    .call(d3.axisLeft(y_vaccine_ts_1).tickFormat(d3.format(",.0f")));

  y_vaccine_ts_1_axis.selectAll("text").style("font-size", ".8rem");

  // Lines
  var lines_vaccine_ts_1 = svg_vaccine_uptake_by_dose_timeseries
    .selectAll(".line")
    .data(vaccine_timeseries_chosen_group)
    .enter()
    .append("path")
    .attr("id", "c3_lines")
    .attr("class", "c3_all_lines")
    .attr("stroke", function (d) {
      return dose_number_colours(d.key);
    })
    .attr("d", function (d) {
      return d3
        .line()
        .x(function (d) {
          return x_vaccine_ts_1(d.Date_label);
        })
        .y(function (d) {
          return y_vaccine_ts_1(+d.Seven_day_rolling_vaccinations);
        })(d.values);
    })
    .style("stroke-width", 2)
    .on("mouseover", hover_vaccine_ts_1_lines)
    .on("mouseout", mouseleave_vaccine_ts_1);

  // Points
  var dots_vaccine_ts_1 = svg_vaccine_uptake_by_dose_timeseries
    .selectAll("circles")
    .data(vaccine_timeseries_chosen)
    .enter()
    .append("circle")
    .attr("cx", function (d) {
      return x_vaccine_ts_1(d.Date_label);
    })
    .attr("cy", function (d) {
      return y_vaccine_ts_1(+d.Seven_day_rolling_vaccinations);
    })
    .style("fill", function (d) {
      return dose_number_colours(d.Dose_number);
    })
    .attr("stroke", function (d) {
      return dose_number_colours(d.Dose_number);
    })
    .attr("r", 2)
    .on("mousemove", hover_vaccine_ts_1_dots)
    .on("mouseout", mouseleave_vaccine_ts_1);
}

// Things specific to rates
if (type_vac_ts_scale[1].checked) {
  console.log("User selected rates of doses");

  // Update text based on selected area
  d3.select("#vaccine_uptake_by_dose_timeseries_title").html(function (d) {
    return (
      "Rolling 7 day rate per 100,000 population Covid-19 vaccinations received; " +
      vaccine_uptake_by_dose_timeseries_area_option +
      "; as at " +
      data_refreshed_date
    );
  });

  // y scale for dose rates
  y_vaccine_ts_1 = d3
    .scaleLinear()
    .domain([
      0,
      d3.max(vaccine_timeseries_chosen, function (d) {
        return +d.Seven_day_rolling_rate_vaccinations;
      }),
    ])
    .range([height_line - 80, 0])
    .nice();

  var y_vaccine_ts_1_axis = svg_vaccine_uptake_by_dose_timeseries
    .append("g")
    .attr("transform", "translate(0,0)")
    .call(d3.axisLeft(y_vaccine_ts_1).tickFormat(d3.format(",.0f")));

  y_vaccine_ts_1_axis.selectAll("text").style("font-size", ".8rem");

  // Lines
  var lines_vaccine_ts_1 = svg_vaccine_uptake_by_dose_timeseries
    .selectAll(".line")
    .data(vaccine_timeseries_chosen_group)
    .enter()
    .append("path")
    .attr("id", "c3_lines")
    .attr("class", "c3_all_lines")
    .attr("stroke", function (d) {
      return dose_number_colours(d.key);
    })
    .attr("d", function (d) {
      return d3
        .line()
        .x(function (d) {
          return x_vaccine_ts_1(d.Date_label);
        })
        .y(function (d) {
          return y_vaccine_ts_1(+d.Seven_day_rolling_rate_vaccinations);
        })(d.values);
    })
    .style("stroke-width", 2)
    .on("mouseover", hover_vaccine_ts_1_lines)
    .on("mouseout", mouseleave_vaccine_ts_1);

  var dots_vaccine_ts_1 = svg_vaccine_uptake_by_dose_timeseries
    .selectAll("circles")
    .data(vaccine_timeseries_chosen)
    .enter()
    .append("circle")
    .attr("cx", function (d) {
      return x_vaccine_ts_1(d.Date_label);
    })
    .attr("cy", function (d) {
      return y_vaccine_ts_1(+d.Seven_day_rolling_rate_vaccinations);
    })
    .style("fill", function (d) {
      return dose_number_colours(d.Dose_number);
    })
    .attr("stroke", function (d) {
      return dose_number_colours(d.Dose_number);
    })
    .attr("r", 2)
    .on("mousemove", hover_vaccine_ts_1_dots)
    .on("mouseout", mouseleave_vaccine_ts_1);
}

function update_vaccine_ts_1() {
  // Retrieve the selected area name
  var vaccine_uptake_by_dose_timeseries_area_option = d3
    .select("#vaccine_uptake_by_dose_timeseries_area_select")
    .property("value");

  if (type_vac_ts_scale[0].checked) {
    // Update text based on selected area
    d3.select("#vaccine_uptake_by_dose_timeseries_title").html(function (d) {
      return (
        "Rolling 7 day number of Covid-19 vaccinations received; " +
        vaccine_uptake_by_dose_timeseries_area_option +
        "; as at " +
        data_refreshed_date
      );
    });

    var vaccine_timeseries_chosen = vaccination_timeseries_data.filter(
      function (d) {
        return d.Name === vaccine_uptake_by_dose_timeseries_area_option;
      }
    );

    // Group the new data
    var vaccine_timeseries_chosen_group = d3
      .nest() // nest function allows to group the calculation per level of a factor
      .key(function (d) {
        return d.Dose_number;
      })
      .entries(vaccine_timeseries_chosen);

    y_vaccine_ts_1
      .domain([
        0,
        d3.max(vaccine_timeseries_chosen, function (d) {
          return +d.Seven_day_rolling_vaccinations;
        }),
      ])
      .nice();

    y_vaccine_ts_1_axis
      .transition()
      .duration(1000)
      .call(d3.axisLeft(y_vaccine_ts_1));

    y_vaccine_ts_1_axis.selectAll("text").style("font-size", ".8rem");

    dots_vaccine_ts_1
      .data(vaccine_timeseries_chosen)
      .transition()
      .duration(1000)
      .attr("cx", function (d) {
        return x_vaccine_ts_1(d.Date_label);
      })
      .attr("cy", function (d) {
        return y_vaccine_ts_1(+d.Seven_day_rolling_vaccinations);
      });

    lines_vaccine_ts_1
      .data(vaccine_timeseries_chosen_group)
      .transition()
      .duration(1000)
      .attr("d", function (d) {
        return d3
          .line()
          .x(function (d) {
            return x_vaccine_ts_1(d.Date_label);
          })
          .y(function (d) {
            return y_vaccine_ts_1(+d.Seven_day_rolling_vaccinations);
          })(d.values);
      });
  }

  if (type_vac_ts_scale[1].checked) {
    // Update text based on selected area
    d3.select("#vaccine_uptake_by_dose_timeseries_title").html(function (d) {
      return (
        "Rolling 7 day rate per 100,000 population Covid-19 vaccinations received; " +
        vaccine_uptake_by_dose_timeseries_area_option +
        "; as at " +
        data_refreshed_date
      );
    });

    var vaccine_timeseries_chosen = vaccination_timeseries_data.filter(
      function (d) {
        return d.Name === vaccine_uptake_by_dose_timeseries_area_option;
      }
    );

    // Group the new data
    var vaccine_timeseries_chosen_group = d3
      .nest() // nest function allows to group the calculation per level of a factor
      .key(function (d) {
        return d.Dose_number;
      })
      .entries(vaccine_timeseries_chosen);

    y_vaccine_ts_1
      .domain([
        0,
        d3.max(vaccine_timeseries_chosen, function (d) {
          return +d.Seven_day_rolling_rate_vaccinations;
        }),
      ])
      .nice();

    y_vaccine_ts_1_axis
      .transition()
      .duration(1000)
      .call(d3.axisLeft(y_vaccine_ts_1));

    y_vaccine_ts_1_axis.selectAll("text").style("font-size", ".8rem");

    dots_vaccine_ts_1
      .data(vaccine_timeseries_chosen)
      .transition()
      .duration(1000)
      .attr("cx", function (d) {
        return x_vaccine_ts_1(d.Date_label);
      })
      .attr("cy", function (d) {
        return y_vaccine_ts_1(+d.Seven_day_rolling_rate_vaccinations);
      });

    lines_vaccine_ts_1
      .data(vaccine_timeseries_chosen_group)
      .transition()
      .duration(1000)
      .attr("d", function (d) {
        return d3
          .line()
          .x(function (d) {
            return x_vaccine_ts_1(d.Date_label);
          })
          .y(function (d) {
            return y_vaccine_ts_1(+d.Seven_day_rolling_rate_vaccinations);
          })(d.values);
      });
  }
}

d3.select("#vaccine_uptake_by_dose_timeseries_area_select").on(
  "change",
  function (d) {
    var vaccine_uptake_by_dose_timeseries_area_option = d3
      .select("#vaccine_uptake_by_dose_timeseries_area_select")
      .property("value");
    update_vaccine_ts_1();
  }
);

var toggle_vac_ts_rate_func = function (d) {
  console.log("ooooo yur, all this gets done for whatever is toggled");
  update_vaccine_ts_1();
};

dose_number.forEach(function (d, i) {
  var list = document.createElement("li");
  list.innerHTML = dose_number_label(d);
  list.className = "key_list";
  list.style.borderColor = dose_number_colours(i);
  var tt = document.createElement("div");
  tt.className = "side_tt";
  tt.style.borderColor = dose_number_colours(i);
  var tt_h3_asr = document.createElement("h3");
  tt_h3_asr.innerHTML = d;
  tt.appendChild(tt_h3_asr);
  var div = document.getElementById("dose_number_key_figure");
  div.appendChild(list);
});

// ! vaccine ts 2 - age stacked

var svg_first_dose_vaccine_uptake_by_dose_timeseries = d3
  .select("#first_dose_vaccine_uptake_by_dose_timeseries")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line + 30)
  .append("g")
  .attr("transform", "translate(" + 120 + "," + 30 + ")");

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccination_timeseries_age.json", false);
request.send(null);
var vaccination_timeseries_age_data = JSON.parse(request.responseText);

// get the button
var type_vac_age_ts_2_scale = document.getElementsByName(
  "toggle_vac_ts_2_age_rate"
);

var vaccine_ages_public = d3
  .map(vaccination_timeseries_age_data, function (d) {
    return d.Age_group;
  })
  .keys();

var vaccine_ages_public_colour_vaccinated = d3
  .scaleOrdinal()
  .domain(vaccine_ages_public)
  .range([
    "#f4dbb0",
    "#f4dbb0",
    "#F3E55C",
    "#FAC127",
    "#FB9E07",
    "#F57D15",
    "#E8602D",
    "#D44842",
    "#BB3754",
    "#9F2A63",
    "#82206C",
    "#65156E",
    "#480B6A",
    "#280B54",
    "#0D082A",
    "#000004",
    "#000004",
  ]);

d3.select("#first_dose_vaccine_uptake_by_age_timeseries_area_select")
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
    "South East",
    "England",
  ])
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

var first_dose_vaccine_uptake_by_age_timeseries_area_option = d3
  .select("#first_dose_vaccine_uptake_by_age_timeseries_area_select")
  .property("value");

var chosen_vaccine_age_ts = vaccination_timeseries_age_data.filter(function (
  d
) {
  return d.Name === first_dose_vaccine_uptake_by_age_timeseries_area_option;
});

// Group the data
var chosen_vaccine_age_ts_group = d3
  .nest() // nest function allows to group the calculation per level of a factor
  .key(function (d) {
    return d.Age_group;
  })
  .entries(chosen_vaccine_age_ts);

// Create a tooltip for the lines and functions for displaying the tooltips as well as highlighting certain lines.
var tooltip_vaccine_age_ts_2 = d3
  .select("#first_dose_vaccine_uptake_by_dose_timeseries")
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

var x_vaccine_age_ts_2 = d3
  .scaleBand()
  .domain(vaccination_timeseries_dates)
  .range([0, width_hm - 120]);

var xAxis_vaccine_ts_2 = svg_first_dose_vaccine_uptake_by_dose_timeseries
  .append("g")
  .attr("transform", "translate(0," + (height_line - 80) + ")")
  .call(
    d3
      .axisBottom(x_vaccine_age_ts_2)
      .tickValues(vaccination_timeseries_date_labels)
  );

xAxis_vaccine_ts_2
  .selectAll("text")
  .attr(
    "transform",
    "translate(-" + (x_vaccine_age_ts_2.bandwidth() + 15) + ",10)rotate(-90)"
  )
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

// We need to create two different functions for when a user hovers over the dots and when they hover over the lines
// Lines first
var hover_vaccine_age_ts_2_lines = function (d) {
  var highlighted_key = d.key;

  lines_vaccine_age_ts_2
    .transition()
    .duration(300)
    .style("stroke", function (d) {
      if (d.key === highlighted_key) {
        return vaccine_ages_public_colour_vaccinated(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .attr("stroke-width", function (d) {
      if (d.key === highlighted_key) {
        return 2;
      } else {
        return 0.25;
      }
    });

  dots_vaccine_age_ts_2
    .transition()
    .duration(300)
    .style("fill", function (d) {
      if (d.Age_group === highlighted_key) {
        return vaccine_ages_public_colour_vaccinated(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .style("stroke", function (d) {
      if (d.Age_group === highlighted_key) {
        return vaccine_ages_public_colour_vaccinated(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .attr("r", function (d) {
      if (d.Age_group === highlighted_key) {
        return 2;
      } else {
        return 0;
      }
    });
};

var hover_vaccine_age_ts_2_dots = function (d) {
  var highlighted_key = d.Age_group;

  tooltip_vaccine_age_ts_2
    .html(
      "<h4>" +
        d.Name +
        '</h4><p class = "side">The number of first doses among those aged ' +
        highlighted_key +
        " living in " +
        d.Name +
        " in the seven days to " +
        d.Date_label +
        " is <b>" +
        d3.format(",.0f")(d.Seven_day_sum_dose_1) +
        "</b>. This is " +
        d3.format(",.0f")(d.Rolling_age_specific_first_dose_rate_per_100000) +
        " per 100,000 population aged " +
        highlighted_key +
        '.</p > <p class="side">' +
        // 'So far, there have been a total of ' +
        //   d3.format(",.0f")(d.Cumulative_cases) +
        //   " cases among this age group in " +
        //   d.Name +
        ".</p>"
    )
    .style("opacity", 1)
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("visibility", "visible");

  lines_vaccine_age_ts_2
    .transition()
    .duration(300)
    .style("stroke", function (d) {
      if (d.key === highlighted_key) {
        return vaccine_ages_public_colour_vaccinated(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .attr("stroke-width", function (d) {
      if (d.key === highlighted_key) {
        return 2;
      } else {
        return 0.25;
      }
    });

  dots_vaccine_age_ts_2
    .transition()
    .duration(300)
    .style("fill", function (d) {
      if (d.Age_group === highlighted_key) {
        return vaccine_ages_public_colour_vaccinated(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .style("stroke", function (d) {
      if (d.Age_group === highlighted_key) {
        return vaccine_ages_public_colour_vaccinated(highlighted_key);
      } else {
        return "#d2d2d2";
      }
    })
    .attr("r", function (d) {
      if (d.Age_group === highlighted_key) {
        return 2;
      } else {
        return 0;
      }
    });
};

// No matter which function was called, on mouseleave restore everything back to the way it was.
var mouseleave_vaccine_age_ts_2 = function (d) {
  tooltip_vaccine_age_ts_2.style("visibility", "hidden");

  lines_vaccine_age_ts_2
    .transition()
    .delay(500)
    .duration(500)
    .style("stroke", function (d) {
      return vaccine_ages_public_colour_vaccinated(d.key);
    })
    .style("visibility", "visible")
    .attr("stroke-width", 2);

  dots_vaccine_age_ts_2
    .transition()
    .delay(500)
    .duration(500)
    .style("fill", function (d) {
      return vaccine_ages_public_colour_vaccinated(d.Age_group);
    })
    .style("stroke", function (d) {
      return vaccine_ages_public_colour_vaccinated(d.Age_group);
    })
    .attr("r", 2)
    .style("visibility", "visible");
};

// Things specific to doses
if (type_vac_age_ts_2_scale[0].checked) {
  console.log("User selected actual dose numbers");

  // Update text based on selected area
  d3.select("#first_dose_vaccine_uptake_by_age_timeseries_title").html(
    function (d) {
      return (
        "Rolling 7 day first dose Covid-19 vaccinations received; by age; " +
        first_dose_vaccine_uptake_by_age_timeseries_area_option +
        "; as at " +
        data_refreshed_date
      );
    }
  );

  // y scale for doses
  y_vaccine_age_ts_2 = d3
    .scaleLinear()
    .domain([
      0,
      d3.max(chosen_vaccine_age_ts, function (d) {
        return +d.Seven_day_sum_dose_1;
      }),
    ])
    .range([height_line - 80, 0])
    .nice();

  var y_vaccine_age_ts_2_axis = svg_first_dose_vaccine_uptake_by_dose_timeseries
    .append("g")
    .attr("transform", "translate(0,0)")
    .call(d3.axisLeft(y_vaccine_age_ts_2).tickFormat(d3.format(",.0f")));

  y_vaccine_age_ts_2_axis.selectAll("text").style("font-size", ".8rem");

  // Lines
  var lines_vaccine_age_ts_2 = svg_first_dose_vaccine_uptake_by_dose_timeseries
    .selectAll(".line")
    .data(chosen_vaccine_age_ts_group)
    .enter()
    .append("path")
    .attr("id", "c3_lines")
    .attr("class", "c3_all_lines")
    .attr("stroke", function (d) {
      return vaccine_ages_public_colour_vaccinated(d.key);
    })
    .attr("d", function (d) {
      return d3
        .line()
        .x(function (d) {
          return x_vaccine_age_ts_2(d.Date_label);
        })
        .y(function (d) {
          return y_vaccine_age_ts_2(+d.Seven_day_sum_dose_1);
        })(d.values);
    })
    .style("stroke-width", 2)
    .on("mouseover", hover_vaccine_age_ts_2_lines)
    .on("mouseout", mouseleave_vaccine_age_ts_2);

  // Points
  var dots_vaccine_age_ts_2 = svg_first_dose_vaccine_uptake_by_dose_timeseries
    .selectAll("circles")
    .data(chosen_vaccine_age_ts)
    .enter()
    .append("circle")
    .attr("cx", function (d) {
      return x_vaccine_age_ts_2(d.Date_label);
    })
    .attr("cy", function (d) {
      return y_vaccine_age_ts_2(+d.Seven_day_sum_dose_1);
    })
    .style("fill", function (d) {
      return vaccine_ages_public_colour_vaccinated(d.Age_group);
    })
    .attr("stroke", function (d) {
      return vaccine_ages_public_colour_vaccinated(d.Age_group);
    })
    .attr("r", 2)
    .on("mousemove", hover_vaccine_age_ts_2_dots)
    .on("mouseout", mouseleave_vaccine_age_ts_2);
}

// Things specific to rates
if (type_vac_age_ts_2_scale[1].checked) {
  console.log("User selected rates of doses");

  // Update text based on selected area
  d3.select("#first_dose_vaccine_uptake_by_age_timeseries_title").html(
    function (d) {
      return (
        "Rolling 7 day rate per 100,000 population first dose Covid-19 vaccinations received by age; " +
        first_dose_vaccine_uptake_by_age_timeseries_area_option +
        "; as at " +
        data_refreshed_date
      );
    }
  );

  // y scale for dose rates
  y_vaccine_age_ts_2 = d3
    .scaleLinear()
    .domain([
      0,
      d3.max(chosen_vaccine_age_ts, function (d) {
        return +d.Rolling_age_specific_first_dose_rate_per_100000;
      }),
    ])
    .range([height_line - 80, 0])
    .nice();

  var y_vaccine_age_ts_2_axis = svg_first_dose_vaccine_uptake_by_dose_timeseries
    .append("g")
    .attr("transform", "translate(0,0)")
    .call(d3.axisLeft(y_vaccine_age_ts_2).tickFormat(d3.format(",.0f")));

  y_vaccine_age_ts_2_axis.selectAll("text").style("font-size", ".8rem");

  // Lines
  var lines_vaccine_age_ts_2 = svg_first_dose_vaccine_uptake_by_dose_timeseries
    .selectAll(".line")
    .data(chosen_vaccine_age_ts_group)
    .enter()
    .append("path")
    .attr("id", "c3_lines")
    .attr("class", "c3_all_lines")
    .attr("stroke", function (d) {
      return vaccine_ages_public_colour_vaccinated(d.key);
    })
    .attr("d", function (d) {
      return d3
        .line()
        .x(function (d) {
          return x_vaccine_age_ts_2(d.Date_label);
        })
        .y(function (d) {
          return y_vaccine_age_ts_2(
            +d.Rolling_age_specific_first_dose_rate_per_100000
          );
        })(d.values);
    })
    .style("stroke-width", 2)
    .on("mouseover", hover_vaccine_age_ts_2_lines)
    .on("mouseout", mouseleave_vaccine_age_ts_2);

  var dots_vaccine_age_ts_2 = svg_first_dose_vaccine_uptake_by_dose_timeseries
    .selectAll("circles")
    .data(chosen_vaccine_age_ts)
    .enter()
    .append("circle")
    .attr("cx", function (d) {
      return x_vaccine_age_ts_2(d.Date_label);
    })
    .attr("cy", function (d) {
      return y_vaccine_age_ts_2(
        +d.Rolling_age_specific_first_dose_rate_per_100000
      );
    })
    .style("fill", function (d) {
      return vaccine_ages_public_colour_vaccinated(d.Age_group);
    })
    .attr("stroke", function (d) {
      return vaccine_ages_public_colour_vaccinated(d.Age_group);
    })
    .attr("r", 2)
    .on("mousemove", hover_vaccine_age_ts_2_dots)
    .on("mouseout", mouseleave_vaccine_age_ts_2);
}

function update_vaccine_age_ts_2() {
  var first_dose_vaccine_uptake_by_age_timeseries_area_option = d3
    .select("#first_dose_vaccine_uptake_by_age_timeseries_area_select")
    .property("value");

  if (type_vac_age_ts_2_scale[0].checked) {
    // Update text based on selected area
    d3.select("#first_dose_vaccine_uptake_by_age_timeseries_title").html(
      function (d) {
        return (
          "Rolling 7 day first dose Covid-19 vaccinations received by age; " +
          first_dose_vaccine_uptake_by_age_timeseries_area_option +
          "; as at " +
          data_refreshed_date
        );
      }
    );

    var chosen_vaccine_age_ts = vaccination_timeseries_age_data.filter(
      function (d) {
        return (
          d.Name === first_dose_vaccine_uptake_by_age_timeseries_area_option
        );
      }
    );

    // Group the data
    var chosen_vaccine_age_ts_group = d3
      .nest() // nest function allows to group the calculation per level of a factor
      .key(function (d) {
        return d.Age_group;
      })
      .entries(chosen_vaccine_age_ts);

    y_vaccine_age_ts_2
      .domain([
        0,
        d3.max(chosen_vaccine_age_ts, function (d) {
          return +d.Seven_day_sum_dose_1;
        }),
      ])
      .nice();

    y_vaccine_age_ts_2_axis
      .transition()
      .duration(1000)
      .call(d3.axisLeft(y_vaccine_age_ts_2));

    y_vaccine_age_ts_2_axis.selectAll("text").style("font-size", ".8rem");

    dots_vaccine_age_ts_2
      .data(chosen_vaccine_age_ts)
      .transition()
      .duration(1000)
      .attr("cx", function (d) {
        return x_vaccine_age_ts_2(d.Date_label);
      })
      .attr("cy", function (d) {
        return y_vaccine_age_ts_2(+d.Seven_day_sum_dose_1);
      });

    lines_vaccine_age_ts_2
      .data(chosen_vaccine_age_ts_group)
      .transition()
      .duration(1000)
      .attr("d", function (d) {
        return d3
          .line()
          .x(function (d) {
            return x_vaccine_age_ts_2(d.Date_label);
          })
          .y(function (d) {
            return y_vaccine_age_ts_2(+d.Seven_day_sum_dose_1);
          })(d.values);
      });
  }

  if (type_vac_age_ts_2_scale[1].checked) {
    // Update text based on selected area
    d3.select("#first_dose_vaccine_uptake_by_age_timeseries_title").html(
      function (d) {
        return (
          "Rolling 7 day rate per 100,000 population first dose Covid-19 vaccinations received; " +
          first_dose_vaccine_uptake_by_age_timeseries_area_option +
          "; as at " +
          data_refreshed_date
        );
      }
    );

    var chosen_vaccine_age_ts = vaccination_timeseries_age_data.filter(
      function (d) {
        return (
          d.Name === first_dose_vaccine_uptake_by_age_timeseries_area_option
        );
      }
    );

    // Group the data
    var chosen_vaccine_age_ts_group = d3
      .nest() // nest function allows to group the calculation per level of a factor
      .key(function (d) {
        return d.Age_group;
      })
      .entries(chosen_vaccine_age_ts);

    y_vaccine_age_ts_2
      .domain([
        0,
        d3.max(chosen_vaccine_age_ts, function (d) {
          return +d.Rolling_age_specific_first_dose_rate_per_100000;
        }),
      ])
      .nice();

    y_vaccine_age_ts_2_axis
      .transition()
      .duration(1000)
      .call(d3.axisLeft(y_vaccine_age_ts_2));

    y_vaccine_age_ts_2_axis.selectAll("text").style("font-size", ".8rem");

    dots_vaccine_age_ts_2
      .data(chosen_vaccine_age_ts)
      .transition()
      .duration(1000)
      .attr("cx", function (d) {
        return x_vaccine_age_ts_2(d.Date_label);
      })
      .attr("cy", function (d) {
        return y_vaccine_age_ts_2(
          +d.Rolling_age_specific_first_dose_rate_per_100000
        );
      });

    lines_vaccine_age_ts_2
      .data(chosen_vaccine_age_ts_group)
      .transition()
      .duration(1000)
      .attr("d", function (d) {
        return d3
          .line()
          .x(function (d) {
            return x_vaccine_age_ts_2(d.Date_label);
          })
          .y(function (d) {
            return y_vaccine_age_ts_2(
              +d.Rolling_age_specific_first_dose_rate_per_100000
            );
          })(d.values);
      });
  }
}

d3.select("#first_dose_vaccine_uptake_by_age_timeseries_area_select").on(
  "change",
  function (d) {
    var first_dose_vaccine_uptake_by_age_timeseries_area_option = d3
      .select("#first_dose_vaccine_uptake_by_age_timeseries_area_select")
      .property("value");
    update_vaccine_age_ts_2();
  }
);

var toggle_vac_ts_2_age_rate_func = function (d) {
  console.log("ooooo yur");
  update_vaccine_age_ts_2();
};

d3.select("#first_dose_vaccine_uptake_by_age_timeseries_title").html(function (
  d
) {
  return (
    "Rolling 7 day first dose Covid-19 vaccinations received by age; " +
    first_dose_vaccine_uptake_by_age_timeseries_area_option +
    "; as at " +
    data_refreshed_date
  );
});

d3.select("#first_dose_vaccine_uptake_by_age_timeseries_area_select").on(
  "change",
  function (d) {
    update_vaccine_age_ts_2();
  }
);

vaccine_ages_public.forEach(function (d, i) {
  var list = document.createElement("li");
  list.innerHTML = d;
  list.className = "key_list";
  list.style.borderColor = vaccine_ages_public_colour_vaccinated(i);
  var tt = document.createElement("div");
  tt.className = "side_tt";
  tt.style.borderColor = vaccine_ages_public_colour_vaccinated(i);
  var tt_h3_asr = document.createElement("h3");
  tt_h3_asr.innerHTML = d;
  tt.appendChild(tt_h3_asr);
  var div = document.getElementById("age_group_vaccine_key_figure");
  div.appendChild(list);
});

// jcvi dates  timelines
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/jcvi_cohort_key_dates.json", false);
request.send(null);
var jcvi_cohort_key_dates = JSON.parse(request.responseText);

jcvi_cohort_ages = [
  "Care home residents",
  "90+ years",
  "85-89 year olds",
  "80-84 year olds",
  "75-79 year olds",
  "70-74 year olds",
  "65-69 year olds",
  "16-64 year olds*",
  "64 year olds",
  "60-64 year olds",
  "55-59 year olds",
  "50-54 year olds",
  "45-49 year olds",
  "40-44 year olds",
  "38-39 year olds",
  "36-37 year olds",
  "34-35 year olds",
  "32-33 year olds",
  "30-31 year olds",
  "25-29 year olds",
  "18-24 year olds",
  "16-17 year olds",
  "12-15 year olds",
];

var vaccine_jcvi_ages_public_colour = d3
  .scaleOrdinal()
  .domain(jcvi_cohort_ages)
  .range([
    "#857e7e",
    "#857e7e",
    "#857e7e",
    "#65156E",
    "#82206C",
    "#89dbfb",
    "#9F2A63",
    "#9F2A63",
    "#BB3754",
    "#D44842",
    "#E8602D",
    "#F57D15",
    "#FB9E07",
    "#FB9E07",
    "#FAC127",
    "#FAC127",
    "#FAC127",
    "#F3E55C",
    "#f4dbb0",
    "#f4dbb0",
    "#f4dbb0",
  ]);

var tooltip_jcvi_key_dates = d3
  .select("#first_dose_vaccine_uptake_by_dose_timeseries")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip_class")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("font-size", ".7rem")
  .style("padding", ".10px");

var showTooltip_jcvi_key_dates = function (d) {
  tooltip_jcvi_key_dates
    .html(
      "<h4>" +
        d.Date_label +
        "</h4><p><b>Adding " +
        d.Age_group +
        "</b></p><p class = 'tt_class'>From this date it was announced that those who were " +
        d.Cohort_name +
        " would now be eligible to book a COVID-19 vaccine appointment.</p>"
    )
    .style("opacity", 1)
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("opacity", 1)
    .style("visibility", "visible");
};

var mouseleave_jcvi_key_dates = function (d) {
  tooltip_jcvi_key_dates.style("opacity", 0).style("visibility", "hidden");
};

// We can treat this as a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_first_dose_vaccine_uptake_by_dose_timeseries
  .selectAll("jcvi_timeline")
  .data(jcvi_cohort_key_dates)
  .enter()
  .append("rect")
  .attr("class", "jcvi_notes")
  .attr("x", function (d) {
    return (
      x_vaccine_age_ts_2(d.Date_label) + x_vaccine_age_ts_2.bandwidth() / 2
    );
  })
  .attr("y", 0)
  .attr("width", 1)
  .attr("height", function (d) {
    return height_line - 80;
  })
  .style("fill", function (d) {
    return vaccine_jcvi_ages_public_colour(d.Age_group);
  });

// We can treat this like a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_first_dose_vaccine_uptake_by_dose_timeseries
  .selectAll("jcvi_timeline")
  .data(jcvi_cohort_key_dates)
  .enter()
  .append("circle")
  .attr("class", "jcvi_notes")
  .attr("cx", function (d) {
    return (
      x_vaccine_age_ts_2(d.Date_label) + x_vaccine_age_ts_2.bandwidth() / 2
    );
  })
  .attr("y", 0)
  .attr("r", 6)
  .style("fill", function (d) {
    return vaccine_jcvi_ages_public_colour(d.Age_group);
  })
  .on("mousemove", showTooltip_jcvi_key_dates)
  .on("mouseout", mouseleave_jcvi_key_dates);

// This function is gonna change the opacity and size of selected and unselected circles
function update_annotations_vac_f2() {
  // For each check box:
  d3.selectAll(".checkbox").each(function (d) {
    cb = d3.select(this);
    grp = cb.property("value");

    console.log(grp);

    // If the box is check, show the notes
    if (cb.property("checked")) {
      console.log("it is checked");
      svg_first_dose_vaccine_uptake_by_dose_timeseries
        .selectAll(".jcvi_notes")
        .transition()
        .duration(1000)
        .style("opacity", 1);
    } else {
      console.log("it is not checked");

      svg_first_dose_vaccine_uptake_by_dose_timeseries
        .selectAll(".jcvi_notes")
        .transition()
        .duration(1000)
        .style("opacity", 0);
    }
  });
}

// When a button change, I run the update function
d3.selectAll(".checkbox").on("change", update_annotations_vac_f2);

update_annotations_vac_f2();

// ! vaccine timeseries 3 - cumulative uptake

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/cumulative_vaccine_age_data.json", false);
request.send(null);
var cumulative_vaccine_age_data = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_age_denominators.json", false);
request.send(null);
var vaccine_age_denominator_data = JSON.parse(request.responseText);

var vaccination_timeseries_cumulative_ages = d3
  .map(cumulative_vaccine_age_data, function (d) {
    return d.Age_group;
  })
  .keys();

d3.select("#vaccine_cumulative_uptake_by_dose_timeseries_area_select")
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
    "South East",
    "England",
  ])
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

var cumulative_vac_area_chosen = d3
  .select("#vaccine_cumulative_uptake_by_dose_timeseries_area_select")
  .property("value");

d3.select("#vaccine_cumulative_uptake_by_dose_timeseries_age_select")
  .selectAll("myOptions")
  .data(vaccination_timeseries_cumulative_ages)
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

var cumulative_vac_age_chosen = d3
  .select("#vaccine_cumulative_uptake_by_dose_timeseries_age_select")
  .property("value");

d3.select("#cumulative_doses_vaccine_uptake_by_age_timeseries_title").html(
  function (d) {
    return (
      "Cumulative vaccinations (first and second doses); " +
      cumulative_vac_area_chosen +
      "; those aged " +
      cumulative_vac_age_chosen +
      "; as at " +
      data_refreshed_date
    );
  }
);

var svg_cumulative_vaccine_uptake_by_dose_timeseries = d3
  .select("#vaccine_cumulative_uptake_by_dose_timeseries")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line + 30)
  .append("g")
  .attr("transform", "translate(" + 120 + "," + 30 + ")");

// Filtering

chosen_cumulative_vac_data = cumulative_vaccine_age_data.filter(function (d) {
  return (
    d.Name === cumulative_vac_area_chosen &&
    d.Age_group === cumulative_vac_age_chosen
  );
});

chosen_vaccine_age_denominator = vaccine_age_denominator_data.filter(function (
  d
) {
  return (
    d.Name === cumulative_vac_area_chosen &&
    d.Age_group === cumulative_vac_age_chosen
  );
})[0]["Denominator"];

chosen_dose_1_so_far = d3.max(chosen_cumulative_vac_data, function (d) {
  return +d.Cumulative_dose_1;
});

chosen_dose_2_so_far = d3.max(chosen_cumulative_vac_data, function (d) {
  return +d.Cumulative_dose_2;
});

var x_vaccine_cumulative_ts_1 = d3
  .scaleBand()
  .domain(vaccination_timeseries_dates)
  .range([0, width_hm - 120]);

var xAxis_vaccine_cumulative_ts_1 =
  svg_cumulative_vaccine_uptake_by_dose_timeseries
    .append("g")
    .attr("transform", "translate(0," + (height_line - 80) + ")")
    .call(
      d3
        .axisBottom(x_vaccine_cumulative_ts_1)
        .tickValues(vaccination_timeseries_date_labels)
    );

xAxis_vaccine_cumulative_ts_1
  .selectAll("text")
  .attr(
    "transform",
    "translate(-" +
      (x_vaccine_cumulative_ts_1.bandwidth() + 15) +
      ",10)rotate(-90)"
  )
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

var tooltip_vaccine_cumulative_ts_1 = d3
  .select("#vaccine_cumulative_uptake_by_dose_timeseries")
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

// var hover_vaccine_cumulative_ts_1_lines = function (d) {
//   tooltip_vaccine_cumulative_ts_1
//     .html(
//       "<h4>" +
//         cumulative_vac_age_chosen +
//         '</h4><p class = "side">The number of first doses among those living in ' +
//         d.Name +
//         " so far as at " +
//         d.Date_label +
//         " was <b>" +
//         d3.format(",.0f")(d.Cumulative_dose_1) +
//         "</b>. This is " +
//         d3.format(".1%")(d.Cumulative_dose_1 / chosen_vaccine_age_denominator) +
//         " of the estimated population aged " +
//         cumulative_vac_age_chosen +
//         " (" +
//         d3.format(",.0f")(chosen_vaccine_age_denominator) +
//         ').</p > <p class="side">' +
//         "So far, " +
//         d3.format(",.0f")(d.Cumulative_dose_2) +
//         " individuals have received both doses.</p>"
//     )
//     .style("opacity", 1)
//     .style("top", event.pageY - 10 + "px")
//     .style("left", event.pageX + 10 + "px")
//     .style("visibility", "visible");
// };

// // No matter which function was called, on mouseleave restore everything back to the way it was.
// var mouseleave_vaccine_cumulative_ts_1 = function (d) {
//   tooltip_vaccine_cumulative_ts_1.style("visibility", "hidden");
// };

// y scale for doses
y_vaccine_cumulative_ts_1 = d3
  .scaleLinear()
  .domain([0, chosen_vaccine_age_denominator])
  .range([height_line - 80, 0])
  .nice();

var y_vaccine_cumulative_ts_1_axis =
  svg_cumulative_vaccine_uptake_by_dose_timeseries
    .append("g")
    .attr("transform", "translate(0,0)")
    .call(d3.axisLeft(y_vaccine_cumulative_ts_1).tickFormat(d3.format(",.0f")));

y_vaccine_cumulative_ts_1_axis.selectAll("text").style("font-size", ".8rem");

// Lines
var lines_vaccine_cumulative_ts_1 =
  svg_cumulative_vaccine_uptake_by_dose_timeseries
    .datum(chosen_cumulative_vac_data)
    .append("path")
    .attr("stroke", "#fa8800")
    .attr("fill", "none")
    .attr(
      "d",
      d3
        .line()
        .x(function (d) {
          return x_vaccine_cumulative_ts_1(d.Date_label);
        })
        .y(function (d) {
          return y_vaccine_cumulative_ts_1(d.Cumulative_dose_1);
        })
    )
    .style("stroke-width", 2);
// .on("mouseover", hover_vaccine_cumulative_ts_1_lines)
// .on("mouseout", mouseleave_vaccine_cumulative_ts_1);

var lines_vaccine_cumulative_ts_2 =
  svg_cumulative_vaccine_uptake_by_dose_timeseries
    .datum(chosen_cumulative_vac_data)
    .append("path")
    .attr("stroke", "#00563f")
    .attr("fill", "none")
    .attr(
      "d",
      d3
        .line()
        .x(function (d) {
          return x_vaccine_cumulative_ts_1(d.Date_label);
        })
        .y(function (d) {
          return y_vaccine_cumulative_ts_1(d.Cumulative_dose_2);
        })
    )
    .style("stroke-width", 2);
// .on("mouseover", hover_vaccine_cumulative_ts_1_lines)
// .on("mouseout", mouseleave_vaccine_cumulative_ts_1);

var lines_vaccine_cumulative_ts_3 =
  svg_cumulative_vaccine_uptake_by_dose_timeseries
    .datum(chosen_cumulative_vac_data)
    .append("path")
    .attr("stroke", "#000000")
    .attr("fill", "none")
    .attr(
      "d",
      d3
        .line()
        .x(function (d) {
          return x_vaccine_cumulative_ts_1(d.Date_label);
        })
        .y(function (d) {
          return y_vaccine_cumulative_ts_1(chosen_vaccine_age_denominator);
        })
    )
    .style("stroke-width", 2);

svg_cumulative_vaccine_uptake_by_dose_timeseries
  .append("text")
  .attr("x", 30)
  .attr("y", function (d) {
    return y_vaccine_cumulative_ts_1(chosen_vaccine_age_denominator) + 15;
  })
  .attr("id", "denominator_vac_x")
  .attr("text-anchor", "start")
  .style("font-size", "12px")
  .style("font-weight", "bold")
  .text(
    "Estimated population: " + d3.format(",.0f")(chosen_vaccine_age_denominator)
  );

svg_cumulative_vaccine_uptake_by_dose_timeseries
  .append("text")
  .attr("x", 30)
  .attr("y", function (d) {
    return y_vaccine_cumulative_ts_1(chosen_vaccine_age_denominator) + 30;
  })
  .attr("id", "denominator_vac_x_dose_1")
  .attr("text-anchor", "start")
  .style("fill", "#fa8800")
  .style("font-weight", "bold")
  .style("font-size", "12px")
  .text("First doses so far: " + d3.format(",.0f")(chosen_dose_1_so_far));

svg_cumulative_vaccine_uptake_by_dose_timeseries
  .append("text")
  .attr("x", 30)
  .attr("y", function (d) {
    return y_vaccine_cumulative_ts_1(chosen_vaccine_age_denominator) + 45;
  })
  .attr("id", "denominator_vac_x_dose_2")
  .attr("text-anchor", "start")
  .style("fill", "#00563f")
  .style("font-weight", "bold")
  .style("font-size", "12px")
  .text("Second doses so far: " + d3.format(",.0f")(chosen_dose_2_so_far));

function update_vaccine_cumulative_ts_1() {
  // Retrieve the selected area name
  var cumulative_vac_area_chosen = d3
    .select("#vaccine_cumulative_uptake_by_dose_timeseries_area_select")
    .property("value");

  var cumulative_vac_age_chosen = d3
    .select("#vaccine_cumulative_uptake_by_dose_timeseries_age_select")
    .property("value");

  // Update text based on selected area
  d3.select("#cumulative_doses_vaccine_uptake_by_age_timeseries_title").html(
    function (d) {
      return (
        "Cumulative vaccinations (first and second doses); " +
        cumulative_vac_area_chosen +
        "; those aged " +
        cumulative_vac_age_chosen +
        "; as at " +
        data_refreshed_date
      );
    }
  );

  chosen_cumulative_vac_data = cumulative_vaccine_age_data.filter(function (d) {
    return (
      d.Name === cumulative_vac_area_chosen &&
      d.Age_group === cumulative_vac_age_chosen
    );
  });

  chosen_vaccine_age_denominator = vaccine_age_denominator_data.filter(
    function (d) {
      return (
        d.Name === cumulative_vac_area_chosen &&
        d.Age_group === cumulative_vac_age_chosen
      );
    }
  )[0]["Denominator"];

  chosen_dose_1_so_far = d3.max(chosen_cumulative_vac_data, function (d) {
    return +d.Cumulative_dose_1;
  });

  chosen_dose_2_so_far = d3.max(chosen_cumulative_vac_data, function (d) {
    return +d.Cumulative_dose_2;
  });

  y_vaccine_cumulative_ts_1.domain([0, chosen_vaccine_age_denominator]).nice();

  y_vaccine_cumulative_ts_1_axis
    .transition()
    .duration(1000)
    .call(d3.axisLeft(y_vaccine_cumulative_ts_1));

  y_vaccine_cumulative_ts_1_axis.selectAll("text").style("font-size", ".8rem");

  lines_vaccine_cumulative_ts_1
    .datum(chosen_cumulative_vac_data)
    .transition()
    .duration(1000)
    .attr(
      "d",
      d3
        .line()
        .x(function (d) {
          return x_vaccine_cumulative_ts_1(d.Date_label);
        })
        .y(function (d) {
          return y_vaccine_cumulative_ts_1(d.Cumulative_dose_1);
        })
    );

  lines_vaccine_cumulative_ts_2
    .datum(chosen_cumulative_vac_data)
    .transition()
    .duration(1000)
    .attr(
      "d",
      d3
        .line()
        .x(function (d) {
          return x_vaccine_cumulative_ts_1(d.Date_label);
        })
        .y(function (d) {
          return y_vaccine_cumulative_ts_1(d.Cumulative_dose_2);
        })
    );

  lines_vaccine_cumulative_ts_3
    .datum(chosen_cumulative_vac_data)
    .transition()
    .duration(1000)
    .attr(
      "d",
      d3
        .line()
        .x(function (d) {
          return x_vaccine_cumulative_ts_1(d.Date_label);
        })
        .y(function (d) {
          return y_vaccine_cumulative_ts_1(chosen_vaccine_age_denominator);
        })
    );

  svg_cumulative_vaccine_uptake_by_dose_timeseries
    .selectAll("#denominator_vac_x")
    .remove();
  svg_cumulative_vaccine_uptake_by_dose_timeseries
    .selectAll("#denominator_vac_x_dose_1")
    .remove();
  svg_cumulative_vaccine_uptake_by_dose_timeseries
    .selectAll("#denominator_vac_x_dose_2")
    .remove();

  svg_cumulative_vaccine_uptake_by_dose_timeseries
    .append("text")
    .attr("x", 30)
    .attr("y", function (d) {
      return y_vaccine_cumulative_ts_1(chosen_vaccine_age_denominator) + 15;
    })
    .attr("id", "denominator_vac_x")
    .attr("text-anchor", "start")
    .style("font-size", "12px")
    .style("font-weight", "bold")
    .text(
      "Estimated population: " +
        d3.format(",.0f")(chosen_vaccine_age_denominator)
    );

  svg_cumulative_vaccine_uptake_by_dose_timeseries
    .append("text")
    .attr("x", 30)
    .attr("y", function (d) {
      return y_vaccine_cumulative_ts_1(chosen_vaccine_age_denominator) + 30;
    })
    .attr("id", "denominator_vac_x_dose_1")
    .attr("text-anchor", "start")
    .style("fill", "#fa8800")
    .style("font-weight", "bold")
    .style("font-size", "12px")
    .text("First doses so far: " + d3.format(",.0f")(chosen_dose_1_so_far));

  svg_cumulative_vaccine_uptake_by_dose_timeseries
    .append("text")
    .attr("x", 30)
    .attr("y", function (d) {
      return y_vaccine_cumulative_ts_1(chosen_vaccine_age_denominator) + 45;
    })
    .attr("id", "denominator_vac_x_dose_2")
    .attr("text-anchor", "start")
    .style("fill", "#00563f")
    .style("font-weight", "bold")
    .style("font-size", "12px")
    .text("Second doses so far: " + d3.format(",.0f")(chosen_dose_2_so_far));
}

d3.select("#vaccine_cumulative_uptake_by_dose_timeseries_area_select").on(
  "change",
  function (d) {
    var cumulative_vac_area_chosen = d3
      .select("#vaccine_cumulative_uptake_by_dose_timeseries_area_select")
      .property("value");

    var cumulative_vac_age_chosen = d3
      .select("#vaccine_cumulative_uptake_by_dose_timeseries_age_select")
      .property("value");

    update_vaccine_cumulative_ts_1();
  }
);

d3.select("#vaccine_cumulative_uptake_by_dose_timeseries_age_select").on(
  "change",
  function (d) {
    var cumulative_vac_area_chosen = d3
      .select("#vaccine_cumulative_uptake_by_dose_timeseries_area_select")
      .property("value");

    var cumulative_vac_age_chosen = d3
      .select("#vaccine_cumulative_uptake_by_dose_timeseries_age_select")
      .property("value");

    update_vaccine_cumulative_ts_1();
  }
);

// var toggle_vac_ts_rate_func = function (d) {
//   console.log("ooooo yur, all this gets done for whatever is toggled");
//   update_vaccine_ts_1();
// };

dose_number.forEach(function (d, i) {
  var list = document.createElement("li");
  list.innerHTML = dose_number_label(d);
  list.className = "key_list";
  list.style.borderColor = dose_number_colours(i);
  var tt = document.createElement("div");
  tt.className = "side_tt";
  tt.style.borderColor = dose_number_colours(i);
  var tt_h3_asr = document.createElement("h3");
  tt_h3_asr.innerHTML = d;
  tt.appendChild(tt_h3_asr);
  var div = document.getElementById("dose_number_key_figure_2");
  div.appendChild(list);
});

// ! Map

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

var lad_boundaries = $.ajax({
  url: "./Outputs/lad_boundary_export.geojson",
  dataType: "json",
  success: console.log("LAD boundary data successfully loaded."),
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
      feature.properties.Age_12_and_over_banded
    ),
    weight: 0.5,
    opacity: 1,
    color: "#000000",
    // dashArray: "3",
    fillOpacity: 1,
  };
}

function style_lad_boundary(feature) {
  return {
    weight: 1.5,
    opacity: 1,
    color: "#000000",
    // dashArray: "3",
    fillOpacity: 0,
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
      feature.properties.Proportion_12_and_over_banded
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
      feature.properties.Age_12_and_over_banded
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
      feature.properties.Proportion_12_and_over_banded
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
      layer.feature.properties.msoa11hclnm +
      " (" +
      layer.feature.properties.msoa11cd +
      ")</b></p><p>A total of <b>" +
      d3.format(",.0f")(layer.feature.properties.Age_12_and_over) +
      "</b> people aged 12+ have received at least one dose of a COVID-19 vaccine. This is <b>" +
      d3.format(".1%")(layer.feature.properties.Proportion_12_and_over) +
      " </b>of the estimated population in this area.</p><p>A total of <b>" +
      d3.format(",.0f")(layer.feature.properties.Age_16_and_over) +
      " </b>people aged 16+ have received at least one dose (<b>" +
      d3.format(".1%")(layer.feature.properties.Proportion_16_and_over) +
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
        layer.feature.properties.msoa11hclnm +
        " (" +
        layer.feature.properties.msoa11cd +
        ")</b></p><p>A total of <b>" +
        d3.format(",.0f")(layer.feature.properties.Age_12_and_over) +
        "</b> people aged 12+ have received at least one dose of a COVID-19 vaccine. This is <b>" +
        d3.format(".1%")(layer.feature.properties.Proportion_12_and_over) +
        " </b>of the estimated population in this area.</p><p>A total of <b>" +
        d3.format(",.0f")(layer.feature.properties.Age_16_and_over) +
        " </b>people aged 16+ have received at least one dose (<b>" +
        d3.format(".1%")(layer.feature.properties.Proportion_16_and_over) +
        "</b>).</p><p>Data correct as at " +
        vaccine_update_date +
        ".</p>"
      );
    });

  var baseMaps_all_age = {
    "Number of individuals": msoa_vaccine_all_age_1_count_map_layer,
    "Proportion of estimated population aged 12+":
      msoa_vaccine_all_age_2_proportion_map_layer,
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
    if (selected_base_layer === "Proportion of estimated population aged 12+") {
      key_msoa_vaccines_proportion();
    }
    if (selected_base_layer === "Number of individuals") {
      key_msoa_vaccines();
    }
  });
});

// ! Keys

function key_msoa_vaccines() {
  $(".key_list_vaccine_all").remove();

  d3.select("#msoa_map_vaccine_all_title").html(function (d) {
    return (
      "Cumulative number of individuals receiving at least one Covid-19 vaccination dose; Sussex MSOAs; vaccinations reported as at " +
      vaccine_update_date
    );
  });

  d3.select("#all_age_msoa_map_key_title").html(function (d) {
    return "Number of people aged 12+ receiving at least one dose";
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
    return "Proportion of individuals (aged 12+) receiving at least one Covid-19 vaccination dose; Sussex MSOAs;";
  });

  d3.select("#all_age_msoa_map_key_title").html(function (d) {
    return "Proportion of people aged 12+ receiving at least one dose";
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

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_wk_by_wk_age.json", false);
request.send(null);
var vaccine_wk_by_wk_age = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_wk_by_wk_age_headings.json", false);
request.send(null);
var vaccination_wk_wk_heading_1 = JSON.parse(request.responseText)[2];
var vaccination_wk_wk_heading_2 = JSON.parse(request.responseText)[3];
var vaccination_wk_wk_heading_3 = JSON.parse(request.responseText)[4];
var vaccination_wk_wk_heading_4 = JSON.parse(request.responseText)[5];
var vaccination_wk_wk_heading_5 = JSON.parse(request.responseText)[6];
var vaccination_wk_wk_heading_6 = JSON.parse(request.responseText)[7];

// Update text based on selected area
d3.select("#wk_wk_table_vaccines_title").html(function (d) {
  return (
    "Covid-19 vaccinations received in the last three complete weeks (Monday-Sunday) by area and age group; as at " +
    data_refreshed_date
  );
});

// ! Week by week table
column_names = [
  "Area",
  "Age group",
  vaccination_wk_wk_heading_1,
  vaccination_wk_wk_heading_2,
  vaccination_wk_wk_heading_3,
  vaccination_wk_wk_heading_4,
  vaccination_wk_wk_heading_5,
  vaccination_wk_wk_heading_6,
];

var clicks = {
  Name: 0,
  Age_group: 0,
  First_dose_week_minus_3: 0,
  First_dose_week_minus_2: 0,
  First_dose_week_minus_1: 0,
  Second_dose_week_minus_3: 0,
  Second_dose_week_minus_2: 0,
  Second_dose_week_minus_1: 0,
};

// draw the table
d3.select("#container_area_search_wk_wk")
  .append("div")
  .attr("class", "SearchBar")
  .append("p")
  .attr("class", "SearchBar")
  .text("Search by area:");

d3.select(".SearchBar")
  .append("input")
  .attr("class", "SearchBar")
  .attr("id", "search_ltla")
  .attr("type", "text")
  .attr("placeholder", "West Sussex");

d3.select("#container_search_age_wk_wk")
  .append("div")
  .attr("class", "SearchBar1")
  .append("p")
  .attr("class", "SearchBar")
  .text("Search by age group:");

d3.select(".SearchBar1")
  .append("input")
  .attr("class", "SearchBar")
  .attr("id", "search_age")
  .attr("type", "text")
  .attr("placeholder", "All");

var wk_wk_table = d3
  .select("#wk_wk_table_vaccines")
  .append("table")
  .attr("class", "wk_wk_table");

wk_wk_table.append("thead").append("tr").attr("class", "wk_wk_table");

var headers = wk_wk_table
  .select("tr")
  .selectAll("th")
  .data(column_names)
  .enter()
  .append("th")
  .text(function (d) {
    return d;
  });

var rows, row_entries, row_entries_no_anchor, row_entries_with_anchor;

// draw table body with rows
wk_wk_table.append("tbody").attr("class", "wk_wk_table");

// data bind
rows = wk_wk_table
  .select("tbody")
  .selectAll("tr")
  .attr("class", "wk_wk_table")
  .data(vaccine_wk_by_wk_age, function (d) {
    return d.Label;
  });

// enter the rows
rows.enter().append("tr").attr("class", "wk_wk_table");

// enter td's in each row
row_entries = rows
  .selectAll("td")
  .attr("class", "wk_wk_table")
  .data(function (d) {
    var arr = [];
    for (var k in d) {
      if (d.hasOwnProperty(k)) {
        arr.push(d[k]);
      }
    }
    return [
      arr[0],
      arr[1],
      d3.format(",.0f")(arr[2]),
      d3.format(",.0f")(arr[3]),
      d3.format(",.0f")(arr[4]),
      d3.format(",.0f")(arr[5]),
      d3.format(",.0f")(arr[6]),
      d3.format(",.0f")(arr[7]),
    ];
  })
  .enter()
  .append("td");

// draw row entries with no anchor
row_entries_no_anchor = row_entries.filter(function (d) {
  return /https?:\/\//.test(d) == false;
});
row_entries_no_anchor.text(function (d) {
  return d;
});

// draw row entries with anchor
row_entries_with_anchor = row_entries.filter(function (d) {
  return /https?:\/\//.test(d) == true;
});
row_entries_with_anchor
  .append("a")
  .attr("href", function (d) {
    return d;
  })
  .attr("target", "_blank")
  .text(function (d) {
    return d;
  });

function initial_table_load() {
  var searched_data = vaccine_wk_by_wk_age,
    text = "West Sussex";

  var searchResults = searched_data.map(function (r) {
    var regex = new RegExp("^" + text + ".*", "i");
    if (regex.test(r.Name)) {
      // if there are any results
      return regex.exec(r.Name)[0]; // return them to searchResults
    }
  });

  // filter blank entries from searchResults
  searchResults = searchResults.filter(function (r) {
    return r != undefined;
  });

  // filter dataset with searchResults
  searched_data = searchResults.map(function (r) {
    return vaccine_wk_by_wk_age.filter(function (p) {
      return p.Name.indexOf(r) != -1;
    });
  });

  // flatten array
  searched_data = [].concat.apply([], searched_data);

  // data bind with new data
  rows = wk_wk_table
    .select("tbody")
    .selectAll("tr")
    .data(searched_data, function (d) {
      return d.Label;
    });

  // enter the rows
  rows.enter().append("tr");

  // enter td's in each row
  row_entries = rows
    .selectAll("td")
    .data(function (d) {
      var arr = [];
      for (var k in d) {
        if (d.hasOwnProperty(k)) {
          arr.push(d[k]);
        }
      }
      return [
        arr[0],
        arr[1],
        d3.format(",.0f")(arr[2]),
        d3.format(",.0f")(arr[3]),
        d3.format(",.0f")(arr[4]),
        d3.format(",.0f")(arr[5]),
        d3.format(",.0f")(arr[6]),
        d3.format(",.0f")(arr[7]),
      ];
    })
    .enter()
    .append("td");

  // draw row entries with no anchor
  row_entries_no_anchor = row_entries.filter(function (d) {
    return /https?:\/\//.test(d) == false;
  });
  row_entries_no_anchor.text(function (d) {
    return d;
  });

  // draw row entries with anchor
  row_entries_with_anchor = row_entries.filter(function (d) {
    return /https?:\/\//.test(d) == true;
  });
  row_entries_with_anchor
    .append("a")
    .attr("href", function (d) {
      return d;
    })
    .attr("target", "_blank")
    .text(function (d) {
      return d;
    });

  // exit
  rows.exit().remove();
}

initial_table_load();

// ! search functionality

d3.select("#search_ltla").on("keyup", function () {
  // filter according to key pressed
  var searched_data = vaccine_wk_by_wk_age,
    text = this.value.trim();

  var searchResults = searched_data.map(function (r) {
    var regex = new RegExp("^" + text + ".*", "i");
    if (regex.test(r.Name)) {
      // if there are any results
      return regex.exec(r.Name)[0]; // return them to searchResults
    }
  });

  // filter blank entries from searchResults
  searchResults = searchResults.filter(function (r) {
    return r != undefined;
  });

  // filter dataset with searchResults
  searched_data = searchResults.map(function (r) {
    return vaccine_wk_by_wk_age.filter(function (p) {
      return p.Name.indexOf(r) != -1;
    });
  });

  // flatten array
  searched_data = [].concat.apply([], searched_data);

  // data bind with new data
  rows = wk_wk_table
    .select("tbody")
    .selectAll("tr")
    .data(searched_data, function (d) {
      return d.Label;
    });

  // enter the rows
  rows.enter().append("tr");

  // enter td's in each row
  row_entries = rows
    .selectAll("td")
    .data(function (d) {
      var arr = [];
      for (var k in d) {
        if (d.hasOwnProperty(k)) {
          arr.push(d[k]);
        }
      }
      return [
        arr[0],
        arr[1],
        d3.format(",.0f")(arr[2]),
        d3.format(",.0f")(arr[3]),
        d3.format(",.0f")(arr[4]),
        d3.format(",.0f")(arr[5]),
        d3.format(",.0f")(arr[6]),
        d3.format(",.0f")(arr[7]),
      ];
    })
    .enter()
    .append("td");

  // draw row entries with no anchor
  row_entries_no_anchor = row_entries.filter(function (d) {
    return /https?:\/\//.test(d) == false;
  });
  row_entries_no_anchor.text(function (d) {
    return d;
  });

  // draw row entries with anchor
  row_entries_with_anchor = row_entries.filter(function (d) {
    return /https?:\/\//.test(d) == true;
  });
  row_entries_with_anchor
    .append("a")
    .attr("href", function (d) {
      return d;
    })
    .attr("target", "_blank")
    .text(function (d) {
      return d;
    });

  // exit
  rows.exit().remove();
});

d3.select("#search_age").on("keyup", function () {
  // filter according to key pressed
  var searched_data = vaccine_wk_by_wk_age,
    text = this.value.trim();

  var searchResults = searched_data.map(function (r) {
    var regex = new RegExp("^" + text + ".*", "i");
    if (regex.test(r.Age_group)) {
      // if there are any results
      return regex.exec(r.Age_group)[0]; // return them to searchResults
    }
  });

  // filter blank entries from searchResults
  searchResults = searchResults.filter(function (r) {
    return r != undefined;
  });

  // filter dataset with searchResults
  searched_data = searchResults.map(function (r) {
    return vaccine_wk_by_wk_age.filter(function (p) {
      return p.Age_group.indexOf(r) != -1;
    });
  });

  // flatten array
  searched_data = [].concat.apply([], searched_data);

  // data bind with new data
  rows = wk_wk_table
    .select("tbody")
    .selectAll("tr")
    .data(searched_data, function (d) {
      return d.Label;
    });

  // enter the rows
  rows.enter().append("tr");

  // enter td's in each row
  row_entries = rows
    .selectAll("td")
    .data(function (d) {
      var arr = [];
      for (var k in d) {
        if (d.hasOwnProperty(k)) {
          arr.push(d[k]);
        }
      }
      return [
        arr[0],
        arr[1],
        d3.format(",.0f")(arr[2]),
        d3.format(",.0f")(arr[3]),
        d3.format(",.0f")(arr[4]),
        d3.format(",.0f")(arr[5]),
        d3.format(",.0f")(arr[6]),
        d3.format(",.0f")(arr[7]),
      ];
    })
    .enter()
    .append("td");

  // draw row entries with no anchor
  row_entries_no_anchor = row_entries.filter(function (d) {
    return /https?:\/\//.test(d) == false;
  });
  row_entries_no_anchor.text(function (d) {
    return d;
  });

  // draw row entries with anchor
  row_entries_with_anchor = row_entries.filter(function (d) {
    return /https?:\/\//.test(d) == true;
  });
  row_entries_with_anchor
    .append("a")
    .attr("href", function (d) {
      return d;
    })
    .attr("target", "_blank")
    .text(function (d) {
      return d;
    });

  // exit
  rows.exit().remove();
});

// ! Create sort function for every column you want to be sortable - the code will depend on the type of data
headers.on("click", function (d) {
  if (d == "Age group") {
    clicks.Age_group++;
    // even number of clicks
    if (clicks.Age_group % 2 == 0) {
      // sort ascending: alphabetically
      rows.sort(function (a, b) {
        if (a.Age_group.toUpperCase() < b.Age_group.toUpperCase()) {
          return -1;
        } else if (a.Age_group.toUpperCase() > b.Age_group.toUpperCase()) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Age_group % 2 != 0) {
      // sort descending: alphabetically
      rows.sort(function (a, b) {
        if (a.Age_group.toUpperCase() < b.Age_group.toUpperCase()) {
          return 1;
        } else if (a.Age_group.toUpperCase() > b.Age_group.toUpperCase()) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == vaccination_wk_wk_heading_1) {
    clicks.First_dose_week_minus_3++;
    // even number of clicks
    if (clicks.First_dose_week_minus_3 % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.First_dose_week_minus_3 < +b.First_dose_week_minus_3) {
          return -1;
        } else if (+a.First_dose_week_minus_3 > +b.First_dose_week_minus_3) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.First_dose_week_minus_3 % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.First_dose_week_minus_3 < +b.First_dose_week_minus_3) {
          return 1;
        } else if (+a.First_dose_week_minus_3 > +b.First_dose_week_minus_3) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == vaccination_wk_wk_heading_2) {
    clicks.First_dose_week_minus_2++;
    // even number of clicks
    if (clicks.First_dose_week_minus_2 % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.First_dose_week_minus_2 < +b.First_dose_week_minus_2) {
          return -1;
        } else if (+a.First_dose_week_minus_2 > +b.First_dose_week_minus_2) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.First_dose_week_minus_2 % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.First_dose_week_minus_2 < +b.First_dose_week_minus_2) {
          return 1;
        } else if (+a.First_dose_week_minus_2 > +b.First_dose_week_minus_2) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == vaccination_wk_wk_heading_3) {
    clicks.First_dose_week_minus_1++;
    // even number of clicks
    if (clicks.First_dose_week_minus_1 % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.First_dose_week_minus_1 < +b.First_dose_week_minus_1) {
          return -1;
        } else if (+a.First_dose_week_minus_1 > +b.First_dose_week_minus_1) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.First_dose_week_minus_1 % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.First_dose_week_minus_1 < +b.First_dose_week_minus_1) {
          return 1;
        } else if (+a.First_dose_week_minus_1 > +b.First_dose_week_minus_1) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == vaccination_wk_wk_heading_4) {
    clicks.Second_dose_week_minus_3++;
    // even number of clicks
    if (clicks.Second_dose_week_minus_3 % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.Second_dose_week_minus_3 < +b.Second_dose_week_minus_3) {
          return -1;
        } else if (+a.Second_dose_week_minus_3 > +b.Second_dose_week_minus_3) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Second_dose_week_minus_3 % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.Second_dose_week_minus_3 < +b.Second_dose_week_minus_3) {
          return 1;
        } else if (+a.Second_dose_week_minus_3 > +b.Second_dose_week_minus_3) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == vaccination_wk_wk_heading_5) {
    clicks.Second_dose_week_minus_2++;
    // even number of clicks
    if (clicks.Second_dose_week_minus_2 % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.Second_dose_week_minus_2 < +b.Second_dose_week_minus_2) {
          return -1;
        } else if (+a.Second_dose_week_minus_2 > +b.Second_dose_week_minus_2) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Second_dose_week_minus_2 % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.Second_dose_week_minus_2 < +b.Second_dose_week_minus_2) {
          return 1;
        } else if (+a.Second_dose_week_minus_2 > +b.Second_dose_week_minus_2) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == vaccination_wk_wk_heading_6) {
    clicks.Second_dose_week_minus_1++;
    // even number of clicks
    if (clicks.Second_dose_week_minus_1 % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.Second_dose_week_minus_1 < +b.Second_dose_week_minus_1) {
          return -1;
        } else if (+a.Second_dose_week_minus_1 > +b.Second_dose_week_minus_1) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Second_dose_week_minus_1 % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.Second_dose_week_minus_1 < +b.Second_dose_week_minus_1) {
          return 1;
        } else if (+a.Second_dose_week_minus_1 > +b.Second_dose_week_minus_1) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
}); // end of click listeners
