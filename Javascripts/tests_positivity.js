// ! Table

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/positivity_at_a_glance.json", false);
request.send(null);
var p_at_a_glance_all_ages = JSON.parse(request.responseText);

window.onload = () => {
  loadTable_positivity(p_at_a_glance_all_ages);
};

var formatPercent = d3.format(".0%");

d3.select("#positivity_note").html(function (d) {
  return (
    "Note - test positivity and number of tests conducted are not currently available for the latest complete date ( " +
    complete_date +
    ") and this has recently been rolled back to around 8 days (" +
    complete_date_minus_3 +
    ") behind the publication date. We hope to restore the usual data as soon as possible."
  );
});

d3.select("#positivity_text_1").html(function (d) {
  return (
    "The table below shows the number of individuals tested (using PCR tests) and the PCR positivity for West Sussex districts and the regional and national comparision for the seven days to " +
    complete_date_minus_3 +
    ". It also shows the number of LFD tests conducted in the same time period. However, it should be noted that the LFD tests are not included in the measure of individuals tested or seven day positivity."
  );
});

d3.select("#positivity_date_heading_1").html(function (d) {
  return (
    "Number of people receiving a PCR (Polymerase chain reaction) test in the seven days to " +
    complete_date_minus_3
  );
});

d3.select("#positivity_date_heading_2").html(function (d) {
  return (
    "Number of LFD (Lateral flow device) tests in the seven days to " +
    complete_date_minus_3
  );
});

// d3.select("#positivity_text_1").html(function (d) {
//   return (
//     "The table below shows the number of individuals tested (using PCR tests) and the PCR positivity for West Sussex districts and the regional and national comparision for the seven days to " +
//     complete_date +
//     ". It also shows the number of LFD tests conducted in the same time period. However, it should be noted that the LFD tests are not included in the measure of individuals tested or seven day positivity."
//   );
// });

// d3.select("#positivity_date_heading_1").html(function (d) {
//   return (
//     "Number of people receiving a PCR (Polymerase chain reaction) test in the seven days to " +
//     complete_date
//   );
// });

// d3.select("#positivity_date_heading_2").html(function (d) {
//   return (
//     "Number of LFD (Lateral flow device) tests in the seven days to " +
//     complete_date
//   );
// });

function loadTable_positivity(p_at_a_glance_all_ages) {
  const tableBody = document.getElementById("positivity_table_1");
  var dataHTML = "";

  for (let item of p_at_a_glance_all_ages) {
    dataHTML += `<tr><td>${item.Name}</td><td>${item.uniquePeopleTestedBySpecimenDateRollingSum}</td><td>${item.uniqueCasePositivityBySpecimenDateRollingSum}</td><td>${item.LFD_7_day_tests}</td></tr>`;
  }

  tableBody.innerHTML = dataHTML;
}

// ! Figure number of people tested

// This will be to highlight a particular line on the figure (and show some key figures)
d3.select("#select_line_tested_button")
  .selectAll("myOptions")
  .data(areas_1a)
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

// Create svgs
var svg_pcr_tested_figure = d3
  .select("#pcr_tested_figure")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .append("g")
  .attr("transform", "translate(" + 80 + "," + 10 + ")");

// Create svgs
var svg_pcr_positivity_figure = d3
  .select("#pcr_positivity_figure")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .append("g")
  .attr("transform", "translate(" + 80 + "," + 10 + ")");

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/positivity_df.json", false);
request.send(null);
var test_df = JSON.parse(request.responseText); // parse the fetched json data into a variable

var pcr_tested_dates = d3
  .map(test_df, function (d) {
    return d.Date_label;
  })
  .keys();

var first_test_period = pcr_tested_dates[0];

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/test_dates.json", false);
request.send(null);
var test_data_dates = JSON.parse(request.responseText).map(function (d) {
  return d.Date_label;
});

// Retrieve the selected area name
var selected_pcr_tested_area = d3
  .select("#select_line_tested_button")
  .property("value");

// Update text based on selected area
d3.select("#selected_pcr_tested_bars_1_compare_title").html(function (d) {
  return (
    "Number of people receiving a PCR (Polymerase chain reaction) test in the previous 7 days; " +
    selected_pcr_tested_area +
    "; up to " +
    complete_date_minus_3 +
    " as at " +
    data_refreshed_date
  );
});

var bars_7_day_pcr_chosen = test_df.filter(function (d) {
  return d.Name === selected_pcr_tested_area;
});

incomplete_bars_7_day_pcr_chosen = bars_7_day_pcr_chosen.filter(function (d) {
  if (isNaN(d.Seven_day_PCR_positivity)) {
    return true;
  }
  return false;
});

var pcr_tested_dates_dots_to_hide = d3
  .map(incomplete_bars_7_day_pcr_chosen, function (d) {
    return d.Date_label;
  })
  .keys();

var x_pcr_tested = d3
  .scaleBand()
  .domain(pcr_tested_dates)
  .range([0, width_hm - 80]);

var xAxis_pcr_tested = svg_pcr_tested_figure
  .append("g")
  .attr("transform", "translate(0," + (height_line - 80) + ")")
  .call(d3.axisBottom(x_pcr_tested).tickValues(test_data_dates));

xAxis_pcr_tested
  .selectAll("text")
  .attr(
    "transform",
    "translate(-" + (x_pcr_tested.bandwidth() + 15) + ",10)rotate(-90)"
  )
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

if (width_hm < 750) {
  xAxis_pcr_tested.call(
    d3
      .axisBottom(x_pcr_tested)
      .tickValues([first_test_period, last_pcr_test_period])
  );

  xAxis_pcr_tested
    .selectAll("text")
    .attr(
      "transform",
      "translate(-" + x_pcr_tested.bandwidth() + ",10)rotate(0)"
    )
    .style("text-anchor", function (d, i) {
      return i % 2 ? "end" : "start";
    })
    .style("font-size", ".8rem");
}

var y_pcr_tested = d3
  .scaleLinear()
  .domain([
    0,
    d3.max(bars_7_day_pcr_chosen, function (d) {
      return +d.Seven_day_PCR_tested_individuals;
    }),
  ])
  .range([height_line - 80, 0])
  .nice();

var yAxis_pcr_tested = svg_pcr_tested_figure
  .append("g")
  .call(d3.axisLeft(y_pcr_tested));

yAxis_pcr_tested.selectAll("text").style("font-size", ".8rem");

var tooltip_tested_pcr = d3
  .select("#pcr_tested_figure")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip_class")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("padding", ".7rem");

var showTooltip_tested_pcr = function (d) {
  tooltip_tested_pcr
    .html(
      "<h5>" +
        d.Name +
        " " +
        d.Date_label +
        "</h5><p class = 'tt_text'>In the seven days to " +
        d.Date_label +
        ", " +
        d3.format(",.0f")(d.Seven_day_PCR_tested_individuals) +
        " people were tested for COVID-19 using the PCR testing.</p><p class = 'tt_text'>Of these, " +
        d.Seven_day_PCR_positivity +
        "% of those tested received a positive result.</p>"
    )
    .style("opacity", 1)
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("opacity", 1)
    .style("visibility", "visible");
};

var mouseleave_tested_pcr = function (d) {
  tooltip_tested_pcr.style("opacity", 0).style("visibility", "hidden");
};

var weekly_pcr_tested_bars = svg_pcr_tested_figure
  .selectAll("mybar")
  .data(bars_7_day_pcr_chosen)
  .enter()
  .append("rect")
  .attr("x", function (d) {
    return x_pcr_tested(d.Date_label);
  })
  .attr("y", function (d) {
    return y_pcr_tested(d.Seven_day_PCR_tested_individuals);
  })
  .attr("width", x_pcr_tested.bandwidth())
  .attr("height", function (d) {
    return height_line - 80 - y_pcr_tested(d.Seven_day_PCR_tested_individuals);
  })
  .attr("fill", function (d) {
    return "#a35112";
  })
  .style("opacity", 1)
  .on("mousemove", showTooltip_tested_pcr)
  .on("mouseout", mouseleave_tested_pcr);

d3.select("#selected_pcr_positivity_1_compare_title").html(function (d) {
  return (
    "Proportion of people receiving a positive PCR (Polymerase chain reaction) result in the previous 7 days; " +
    selected_pcr_tested_area +
    "; up to " +
    complete_date_minus_3 +
    " as at " +
    data_refreshed_date
  );
});

var x_pcr_positivity = d3
  .scaleBand()
  .domain(pcr_tested_dates)
  .range([0, width_hm - 80]);

var xAxis_pcr_positivity = svg_pcr_positivity_figure
  .append("g")
  .attr("transform", "translate(0," + (height_line - 80) + ")")
  .call(d3.axisBottom(x_pcr_positivity).tickValues(test_data_dates));

xAxis_pcr_positivity
  .selectAll("text")
  .attr(
    "transform",
    "translate(-" + (x_pcr_positivity.bandwidth() + 15) + ",10)rotate(-90)"
  )
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

if (width_hm < 750) {
  xAxis_pcr_positivity.call(
    d3
      .axisBottom(x_pcr_positivity)
      .tickValues([first_test_period, last_pcr_test_period])
  );

  xAxis_pcr_positivity
    .selectAll("text")
    .attr(
      "transform",
      "translate(-" + x_pcr_positivity.bandwidth() + ",10)rotate(0)"
    )
    .style("text-anchor", function (d, i) {
      return i % 2 ? "end" : "start";
    })
    .style("font-size", ".8rem");
}

var y_pcr_positivity = d3
  .scaleLinear()
  .domain([
    0,
    d3.max(bars_7_day_pcr_chosen, function (d) {
      return +d.Seven_day_PCR_positivity;
    }),
  ])
  .range([height_line - 80, 0])
  .nice();

var yAxis_pcr_positivity = svg_pcr_positivity_figure
  .append("g")
  .call(d3.axisLeft(y_pcr_positivity).tickFormat(formatPercent));

yAxis_pcr_positivity.selectAll("text").style("font-size", ".8rem");

var tooltip_pcr_positivity = d3
  .select("#pcr_positivity_figure")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip_class")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("padding", ".7rem");

var showTooltip_pcr_positivity = function (d) {
  tooltip_pcr_positivity
    .html(
      "<h5>" +
        d.Name +
        " " +
        d.Date_label +
        "</h5><p class = 'tt_text'>In the seven days to " +
        d.Date_label +
        ", " +
        d3.format(".1%")(d.Seven_day_PCR_positivity) +
        " of people tested for COVID-19 using the PCR testing received at least one positive result.</p><p class = 'tt_text'>The number of individuals tested overall was  " +
        d3.format(",.0f")(d.Seven_day_PCR_tested_individuals) +
        "</p>"
    )
    .style("opacity", 1)
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("opacity", 1)
    .style("visibility", "visible");
};

var mouseleave_pcr_positivity = function (d) {
  tooltip_pcr_positivity.style("opacity", 0).style("visibility", "hidden");
};

var lines_pcr_positivity_1 = svg_pcr_positivity_figure
  .append("path")
  .datum(bars_7_day_pcr_chosen)
  .attr("fill", "none")
  .attr("stroke", "#000000")
  .attr("stroke-width", 2)
  .attr(
    "d",
    d3
      .line()
      .defined((d) => !isNaN(d.Seven_day_PCR_positivity))
      .x(function (d) {
        return (
          x_pcr_positivity(d.Date_label) + x_pcr_positivity.bandwidth() / 2
        );
      })
      .y(function (d) {
        return y_pcr_positivity(d.Seven_day_PCR_positivity);
      })
  );

var dots_pcr_positivity_1 = svg_pcr_positivity_figure
  .selectAll("myCircles")
  .data(bars_7_day_pcr_chosen)
  .enter()
  .append("circle")
  .attr("cx", function (d) {
    return x_pcr_positivity(d.Date_label) + x_pcr_positivity.bandwidth() / 2;
  })
  .attr("cy", function (d) {
    return y_pcr_positivity(+d.Seven_day_PCR_positivity);
  })
  // .attr("r", 1)
  .attr("r", function (d) {
    if (pcr_tested_dates_dots_to_hide.indexOf(d.Date_label) >= 0) {
      return 0;
    } else {
      return 1;
    }
  })
  .style("fill", function (d) {
    return "#000";
  })
  .attr("stroke", function (d) {
    return "#000";
  })
  .on("mousemove", showTooltip_pcr_positivity)
  .on("mouseout", mouseleave_pcr_positivity);

// ! on change
function update_pcr_tested() {
  var selected_pcr_tested_area = d3
    .select("#select_line_tested_button")
    .property("value");

  // Update text based on selected area
  d3.select("#selected_pcr_tested_bars_1_compare_title").html(function (d) {
    return (
      "Number of people receiving a PCR (Polymerase chain reaction) test in the previous 7 days; " +
      selected_pcr_tested_area +
      "; up to " +
      complete_date_minus_3 +
      " as at " +
      data_refreshed_date
    );
  });

  d3.select("#selected_pcr_positivity_1_compare_title").html(function (d) {
    return (
      "Proportion of people receiving a positive PCR (Polymerase chain reaction) result in the previous 7 days; " +
      selected_pcr_tested_area +
      "; up to " +
      complete_date_minus_3 +
      " as at " +
      data_refreshed_date
    );
  });

  var bars_7_day_pcr_chosen = test_df.filter(function (d) {
    return d.Name === selected_pcr_tested_area;
  });

  incomplete_bars_7_day_pcr_chosen = bars_7_day_pcr_chosen.filter(function (d) {
    if (isNaN(d.Seven_day_PCR_positivity)) {
      return true;
    }
    return false;
  });

  var pcr_tested_dates_dots_to_hide = d3
    .map(incomplete_bars_7_day_pcr_chosen, function (d) {
      return d.Date_label;
    })
    .keys();

  y_pcr_tested
    .domain([
      0,
      d3.max(bars_7_day_pcr_chosen, function (d) {
        return +d.Seven_day_PCR_tested_individuals;
      }),
    ])
    .nice();

  yAxis_pcr_tested.transition().duration(1000).call(d3.axisLeft(y_pcr_tested));

  yAxis_pcr_tested.selectAll("text").style("font-size", ".8rem");

  weekly_pcr_tested_bars
    .data(bars_7_day_pcr_chosen)
    .transition()
    .duration(1000)
    .attr("x", function (d) {
      return x_pcr_tested(d.Date_label);
    })
    .attr("y", function (d) {
      return y_pcr_tested(d.Seven_day_PCR_tested_individuals);
    })
    .attr("width", x_pcr_tested.bandwidth())
    .attr("height", function (d) {
      return (
        height_line - 80 - y_pcr_tested(d.Seven_day_PCR_tested_individuals)
      );
    })
    .attr("fill", function (d) {
      return "#a35112";
    })
    .style("opacity", 1);

  y_pcr_positivity
    .domain([
      0,
      d3.max(bars_7_day_pcr_chosen, function (d) {
        return +d.Seven_day_PCR_positivity;
      }),
    ])
    .nice();

  yAxis_pcr_positivity
    .transition()
    .duration(1000)
    .call(d3.axisLeft(y_pcr_positivity).tickFormat(formatPercent));

  yAxis_pcr_positivity.selectAll("text").style("font-size", ".8rem");

  lines_pcr_positivity_1
    .datum(bars_7_day_pcr_chosen)
    .transition()
    .duration(1000)
    .attr(
      "d",
      d3
        .line()
        .defined((d) => !isNaN(d.Seven_day_PCR_positivity))
        .x(function (d) {
          return (
            x_pcr_positivity(d.Date_label) + x_pcr_positivity.bandwidth() / 2
          );
        })
        .y(function (d) {
          return y_pcr_positivity(d.Seven_day_PCR_positivity);
        })
    );

  dots_pcr_positivity_1
    .data(bars_7_day_pcr_chosen)
    .transition()
    .duration(1000)
    .attr("r", function (d) {
      if (pcr_tested_dates_dots_to_hide.indexOf(d.Date_label) >= 0) {
        return 0;
      } else {
        return 1;
      }
    })
    .attr("cx", function (d) {
      return x_pcr_positivity(d.Date_label);
    })
    .attr("cy", function (d) {
      return y_pcr_positivity(+d.Seven_day_PCR_positivity);
    });
}

d3.select("#select_line_tested_button").on("change", function (d) {
  var selected_pcr_tested_area = d3
    .select("#select_line_tested_button")
    .property("value");
  update_pcr_tested();
});

// ! LFD tests

// This will be to highlight a particular line on the figure (and show some key figures)
d3.select("#select_line_lfd_tests_button")
  .selectAll("myOptions")
  .data(areas_1a)
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

// Create svgs
var svg_lfd_tests_figure = d3
  .select("#lfd_tests_figure")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .append("g")
  .attr("transform", "translate(" + 80 + "," + 10 + ")");

// var request = new XMLHttpRequest();
// request.open("GET", "./Outputs/lfd_df.json", false);
// request.send(null);
// var lfd_test_df = JSON.parse(request.responseText); // parse the fetched json data into a variable

// var lfd_tests_dates = d3
//   .map(lfd_test_df, function (d) {
//     return d.Date_label;
//   })
//   .keys();

// var request = new XMLHttpRequest();
// request.open("GET", "./Outputs/lfd_test_dates.json", false);
// request.send(null);
// var lfd_test_data_dates = JSON.parse(request.responseText).map(function (d) {
//   return d.Date_label;
// });

// Retrieve the selected area name
var selected_lfd_tests_area = d3
  .select("#select_line_lfd_tests_button")
  .property("value");

// Update text based on selected area
d3.select("#selected_lfd_tests_bars_1_compare_title").html(function (d) {
  return (
    "Number of LFD (Lateral flow device) tests in the previous 7 days; " +
    selected_lfd_tests_area +
    "; up to " +
    latest_date +
    " as at " +
    data_refreshed_date
  );
});

var bars_7_day_lfd_chosen = test_df.filter(function (d) {
  return d.Name === selected_lfd_tests_area;
});

var x_lfd_tests = d3
  .scaleBand()
  .domain(pcr_tested_dates)
  .range([0, width_hm - 90]);

var xAxis_lfd_tests = svg_lfd_tests_figure
  .append("g")
  .attr("transform", "translate(0," + (height_line - 80) + ")")
  .call(d3.axisBottom(x_lfd_tests).tickValues(test_data_dates));

xAxis_lfd_tests
  .selectAll("text")
  .attr(
    "transform",
    "translate(-" + (x_lfd_tests.bandwidth() + 15) + ",10)rotate(-90)"
  )
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

if (width_hm < 750) {
  xAxis_lfd_tests.call(
    d3
      .axisBottom(x_lfd_tests)
      .tickValues([first_test_period, last_lfd_test_period])
  );

  xAxis_lfd_tests
    .selectAll("text")
    .attr(
      "transform",
      "translate(-" + x_lfd_tests.bandwidth() + ",10)rotate(0)"
    )
    .style("text-anchor", function (d, i) {
      return i % 2 ? "end" : "start";
    })
    .style("font-size", ".8rem");
}

var y_lfd_tests = d3
  .scaleLinear()
  .domain([
    0,
    d3.max(bars_7_day_lfd_chosen, function (d) {
      return +d.LFD_7_day_tests;
    }),
  ])
  .range([height_line - 80, 0])
  .nice();

var yAxis_lfd_tests = svg_lfd_tests_figure
  .append("g")
  .call(d3.axisLeft(y_lfd_tests));

yAxis_lfd_tests.selectAll("text").style("font-size", ".8rem");

var tooltip_lfd_tests = d3
  .select("#lfd_tests_figure")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip_class")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("padding", ".7rem");

var showTooltip_lfd_tests = function (d) {
  tooltip_lfd_tests
    .html(
      "<h5>" +
        d.Name +
        " " +
        d.Date_label +
        "</h5><p class = 'tt_text'>In the seven days to " +
        d.Date_label +
        ", there were " +
        d3.format(",.0f")(d.LFD_7_day_tests) +
        " lateral flow device tests for COVID-19.</p>"
    )
    .style("opacity", 1)
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("opacity", 1)
    .style("visibility", "visible");
};

var mouseleave_lfd_tests = function (d) {
  tooltip_lfd_tests.style("opacity", 0).style("visibility", "hidden");
};

var weekly_lfd_tests_bars = svg_lfd_tests_figure
  .selectAll("mybar")
  .data(bars_7_day_lfd_chosen)
  .enter()
  .append("rect")
  .attr("x", function (d) {
    return x_lfd_tests(d.Date_label);
  })
  .attr("y", function (d) {
    return y_lfd_tests(d.LFD_7_day_tests);
  })
  .attr("width", x_lfd_tests.bandwidth())
  .attr("height", function (d) {
    return height_line - 80 - y_lfd_tests(d.LFD_7_day_tests);
  })
  .attr("fill", function (d) {
    return "#0a75ad";
  })
  .style("opacity", 1)
  .on("mousemove", showTooltip_lfd_tests)
  .on("mouseout", mouseleave_lfd_tests);

// ! on change
function update_lfd_tests() {
  // Retrieve the selected area name
  var selected_lfd_tests_area = d3
    .select("#select_line_lfd_tests_button")
    .property("value");

  // Update text based on selected area
  d3.select("#selected_lfd_tests_bars_1_compare_title").html(function (d) {
    return (
      "Number of LFD (Lateral flow device) tests in the previous 7 days; " +
      selected_lfd_tests_area +
      "; up to " +
      latest_date +
      " as at " +
      data_refreshed_date
    );
  });

  var bars_7_day_lfd_chosen = test_df.filter(function (d) {
    return d.Name === selected_lfd_tests_area;
  });

  y_lfd_tests
    .domain([
      0,
      d3.max(bars_7_day_lfd_chosen, function (d) {
        return +d.LFD_7_day_tests;
      }),
    ])
    .nice();

  yAxis_lfd_tests.transition().duration(1000).call(d3.axisLeft(y_lfd_tests));

  yAxis_lfd_tests.selectAll("text").style("font-size", ".8rem");

  weekly_lfd_tests_bars
    .data(bars_7_day_lfd_chosen)
    .transition()
    .duration(1000)
    .attr("x", function (d) {
      return x_lfd_tests(d.Date_label);
    })
    .attr("y", function (d) {
      return y_lfd_tests(d.LFD_7_day_tests);
    })
    .attr("width", x_lfd_tests.bandwidth())
    .attr("height", function (d) {
      return height_line - 80 - y_lfd_tests(d.LFD_7_day_tests);
    })
    .attr("fill", function (d) {
      return "#0a75ad";
    })
    .style("opacity", 1);
}

d3.select("#select_line_lfd_tests_button").on("change", function (d) {
  var selected_lfd_tests_area = d3
    .select("#select_line_lfd_tests_button")
    .property("value");
  update_lfd_tests();
});
