var width_hm = document.getElementById("content_size").offsetWidth,
  height_hm = 25,
  height_hm_title = 45,
  height_hm_explainer = 15,
  height_sm = 220,
  incomplete_colour = "#999999",
  height_line = 410;

var width_sm = width_hm / 2 - 10;

// We dont want the small plots to be less than 325 pixels wide so this says if the
if (width_sm < 300) {
  width_sm = 300;
}

var areas = [
  "West Sussex",
  "Adur",
  "Arun",
  "Chichester",
  "Crawley",
  "Horsham",
  "Mid Sussex",
  "Worthing",
];

var areas_1a = [
  "West Sussex",
  "Adur",
  "Arun",
  "Chichester",
  "Crawley",
  "Horsham",
  "Mid Sussex",
  "Worthing",
  "South East region",
  "England",
];

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/case_change_dates.json", false);
request.send(null);
var data_dates = JSON.parse(request.responseText).map(function (d) {
  return d.Date_label;
});

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/wsx_case_summary.json", false);
request.send(null);
var case_summary = JSON.parse(request.responseText); // parse the fetched json data into a variable

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/wsx_daily_cases.json", false);
request.send(null);
var daily_cases = JSON.parse(request.responseText); // parse the fetched json data into a variable

var dates = d3
  .map(daily_cases, function (d) {
    return d.Date_label;
  })
  .keys();

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/range_dates.json", false);
request.send(null);

var first_date = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "First";
})[0]["Date_label"];

var complete_date = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "Complete";
})[0]["Date_label"];

var seven_days_date = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "Seven_days_ago";
})[0]["Date_label"];

var complete_date_actual = JSON.parse(request.responseText).filter(function (
  d
) {
  return d.Order == "Complete";
})[0]["Date"];

var first_incomplete_date = JSON.parse(request.responseText).filter(function (
  d
) {
  return d.Order == "First_incomplete";
})[0]["Date_label"];

var first_incomplete_period = JSON.parse(request.responseText).filter(function (
  d
) {
  return d.Order == "First_incomplete";
})[0]["Period"];

var first_incomplete_date_actual = JSON.parse(request.responseText).filter(
  function (d) {
    return d.Order == "First_incomplete";
  }
)[0]["Date"];

var latest_date = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "Last";
})[0]["Date_label"];

var most_recent = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "Last";
})[0]["Date"];

var most_recent_period = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "Last";
})[0]["Period"];

var rolling_seven_days_ago = JSON.parse(request.responseText).filter(function (
  d
) {
  return d.Order == "Seven_days_ago";
})[0]["Date_label"];

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/first_incomplete_daily_case.json", false);
request.send(null);
var incomplete_sm_date = JSON.parse(request.responseText)[0];

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/latest_daily_case.json", false);
request.send(null);
var latest_sm_date = JSON.parse(request.responseText)[0];

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/daily_case_update_date.json", false);
request.send(null);
var data_refreshed_date = JSON.parse(request.responseText)[0];

// Update text based on selected area
d3.select("#update_date").html(function (d) {
  return (
    "The case data has been refreshed on " +
    data_refreshed_date +
    " with cases confirmed up to " +
    latest_date
  );
});

// testing policy timelines
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/uk_testing_key_dates.json", false);
request.send(null);
var uk_testing_key_dates = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/uk_restrictions_key_dates.json", false);
request.send(null);
var uk_restrictions_key_dates = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/wsx_daily_case_limits.json", false);
request.send(null);
var daily_case_limits = JSON.parse(request.responseText);

// Daily cases bar chart
var svg_daily_new_case_bars = d3
  .select("#daily_new_case_bars")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .append("g")
  .attr("transform", "translate(" + 60 + "," + 10 + ")");

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_bars_daily_cases_1_area_button")
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

// Retrieve the selected area name
var selected_figure_1a_area_option = d3
  .select("#select_bars_daily_cases_1_area_button")
  .property("value");

// Update text based on selected area
d3.select("#selected_daily_cases_bars_1_compare_title").html(function (d) {
  return (
    "Pillar 1 and 2 combined daily confirmed COVID-19 cases; " +
    selected_figure_1a_area_option +
    "; as at " +
    data_refreshed_date
  );
});

var bars_daily_cases_1_chosen = daily_cases.filter(function (d) {
  return d.Name === selected_figure_1a_area_option;
});

var total_cases_daily_chosen = case_summary.filter(function (d) {
  return d.Name === selected_figure_1a_area_option;
})[0]["Total confirmed cases so far"];

var x_daily_cases = d3
  .scaleBand()
  .domain(
    bars_daily_cases_1_chosen.map(function (d) {
      return d.Date_label_2;
    })
  )
  .range([0, width_hm - 60]);

var xAxis_daily_cases = svg_daily_new_case_bars
  .append("g")
  .attr("transform", "translate(0," + (height_line - 80) + ")")
  .call(d3.axisBottom(x_daily_cases).tickValues(data_dates));

xAxis_daily_cases
  .selectAll("text")
  .attr(
    "transform",
    "translate(-" + (x_daily_cases.bandwidth() + 15) + ",10)rotate(-90)"
  )
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

x_summary = case_summary.filter(function (d, i) {
  return d.Name === selected_figure_1a_area_option;
});

d3.select("#x_latest_figures")
  .data(x_summary)
  .html(function (d) {
    return (
      "The total number of confirmed Covid-19 cases so far in " +
      selected_figure_1a_area_option +
      " is " +
      d3.format(",.0f")(d["Total confirmed cases so far"]) +
      ". This is " +
      d3.format(",.0f")(d["Total cases per 100,000 population"]) +
      " cases per 100,000 population. The current daily case count (using data from " +
      complete_date +
      ") is " +
      d3.format(",.0f")(
        d["Confirmed cases swabbed on most recent complete day"]
      ) +
      " new confirmed cases swabbed (" +
      d3.format(",.1f")(
        d[
          "Confirmed cases swabbed per 100,000 population on most recent complete day"
        ]
      ) +
      " per 100,000)."
    );
  });

max_limit_x = daily_case_limits.filter(function (d) {
  return d.Name === selected_figure_1a_area_option;
})[0]["Max_limit"];

var y_daily_cases = d3
  .scaleLinear()
  .domain([0, max_limit_x])
  .range([height_line - 80, 0])
  .nice();

var yAxis_daily_cases = svg_daily_new_case_bars
  .append("g")
  .call(d3.axisLeft(y_daily_cases));

yAxis_daily_cases.selectAll("text").style("font-size", ".8rem");

var tooltip_testing_key_dates = d3
  .select("#daily_new_case_bars")
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

var showTooltip_testing_key_dates = function (d) {
  tooltip_testing_key_dates
    .html(
      "<h5>" +
        d.Date_label_2 +
        "</h5><p class = 'tt_class'>On this date it was announced that " +
        d.Change +
        " would now be included in the UK Covid-19 swab testing eligible population.</p>"
    )
    .style("opacity", 1)
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("opacity", 1)
    .style("visibility", "visible");
};

var mouseleave_testing_key_dates = function (d) {
  tooltip_testing_key_dates.style("opacity", 0).style("visibility", "hidden");
};

// We can treat this as a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_daily_new_case_bars
  .selectAll("testing_timeline")
  .data(uk_testing_key_dates)
  .enter()
  .append("rect")
  .attr("class", "test_notes")
  .attr("x", function (d) {
    return x_daily_cases(d.Date_label_2) + x_daily_cases.bandwidth() / 2;
  })
  .attr("y", 0)
  .attr("width", 2)
  .attr("height", function (d) {
    return height_line - 80;
  })
  .style("fill", incomplete_colour);

// We can treat this like a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_daily_new_case_bars
  .selectAll("testing_timeline")
  .data(uk_testing_key_dates)
  .enter()
  .append("circle")
  .attr("class", "test_notes")
  .attr("cx", function (d) {
    return x_daily_cases(d.Date_label_2) + x_daily_cases.bandwidth() / 2;
  })
  .attr("y", 0)
  .attr("r", 6)
  .style("fill", incomplete_colour)
  .on("mousemove", showTooltip_testing_key_dates)
  .on("mouseout", mouseleave_testing_key_dates);

svg_daily_new_case_bars
  .append("text")
  .attr("id", "test_milestones")
  .attr("class", "test_notes")
  .attr("x", function (d) {
    return x_daily_cases("26 Mar 20") + x_daily_cases.bandwidth() / 2;
  })
  .attr("y", 2)
  .style("font-size", ".8rem")
  .text("Testing eligibility changes -")
  .attr("text-anchor", "end");

// Restriction changes

var tooltip_restrictions_key_dates = d3
  .select("#daily_new_case_bars")
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

var showTooltip_restrictions_key_dates = function (d) {
  tooltip_restrictions_key_dates
    .html(
      "<h5>" +
        d.Date_label_2 +
        "</h5><p class = 'tt_class'>" +
        d.Change +
        "</p>"
    )
    .style("opacity", 1)
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("opacity", 1)
    .style("visibility", "visible");
};

var mouseleave_restrictions_key_dates = function (d) {
  tooltip_restrictions_key_dates
    .style("opacity", 0)
    .style("visibility", "hidden");
};

// We can treat this as a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_daily_new_case_bars
  .selectAll("testing_timeline")
  .data(uk_restrictions_key_dates)
  .enter()
  .append("line")
  .attr("class", "restriction_notes")
  .attr("x1", function (d) {
    return x_daily_cases(d.Date_label_2) + x_daily_cases.bandwidth() / 2;
  })
  .attr("y1", 40)
  .attr("x2", function (d) {
    return x_daily_cases(d.Date_label_2) + x_daily_cases.bandwidth() / 2;
  })
  .attr("y2", height_line - 80)
  .attr("stroke", "#ec0909")
  .attr("stroke-dasharray", "3, 3");

// We can treat this like a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_daily_new_case_bars
  .selectAll("testing_timeline")
  .data(uk_restrictions_key_dates)
  .enter()
  .append("circle")
  .attr("class", "restriction_notes")
  .attr("cx", function (d) {
    return x_daily_cases(d.Date_label_2) + x_daily_cases.bandwidth() / 2;
  })
  .attr("cy", 40)
  .attr("r", 6)
  .style("fill", "#ec0909")
  .on("mousemove", showTooltip_restrictions_key_dates)
  .on("mouseout", mouseleave_restrictions_key_dates);

svg_daily_new_case_bars
  .append("text")
  .attr("id", "test_milestones")
  .attr("class", "restriction_notes")
  .attr("x", function (d) {
    return x_daily_cases("22 Mar 20") + x_daily_cases.bandwidth() / 2;
  })
  .attr("y", 42)
  .text("Lockdown begins -")
  .style("font-size", ".8rem")
  .attr("text-anchor", "end");

// Bars
var tooltip_daily_case_1 = d3
  .select("#daily_new_case_bars")
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

var showTooltip_daily_case_1 = function (d) {
  tooltip_daily_case_1
    .html(
      "<h5>" +
        d.Name +
        "</h5><p class = 'tt_text'><b>" +
        d.Date_label +
        '</b></p><p class = "tt_text">In ' +
        d.Name +
        " there were <b>" +
        d3.format(",.0f")(d.New_cases) +
        ' </b>specimens taken on this date which resulted in a positive result for Covid-19.</p><p class = "side">The new cases swabbed on this day represent ' +
        d3.format("0.1%")(d.New_cases / total_cases_daily_chosen) +
        " of the total number of cases confirmed so far (" +
        d3.format(",.0f")(total_cases_daily_chosen) +
        ') </p><p class = "tt_text">In the last seven days there have been a total of ' +
        d3.format(",.0f")(d.Rolling_7_day_new_cases) +
        " reported cases.</p>"
    )
    .style("opacity", 1)
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("opacity", 1)
    .style("visibility", "visible");
};

var mouseleave_daily_case_1 = function (d) {
  tooltip_daily_case_1.style("opacity", 0).style("visibility", "hidden");
};

svg_daily_new_case_bars
  .append("line")
  .attr("x1", x_daily_cases(incomplete_sm_date))
  .attr("y1", 0)
  .attr("x2", x_daily_cases(incomplete_sm_date))
  .attr("y2", height_line - 80)
  .attr("stroke", incomplete_colour)
  .attr("stroke-dasharray", "3, 3");

svg_daily_new_case_bars
  .append("rect")
  .attr("x", x_daily_cases(incomplete_sm_date))
  .attr("y1", 0)
  .attr(
    "width",
    x_daily_cases(latest_sm_date) +
      x_daily_cases.bandwidth() -
      x_daily_cases(incomplete_sm_date)
  )
  .attr("height", height_line - 80)
  .style("fill", incomplete_colour)
  .style("stroke", "none")
  .style("opacity", 0.2);

// daily case bars
var daily_new_case_bars = svg_daily_new_case_bars
  .selectAll("mybar")
  .data(bars_daily_cases_1_chosen)
  .enter()
  .append("rect")
  .attr("x", function (d) {
    return x_daily_cases(d.Date_label_2);
  })
  .attr("y", function (d) {
    return y_daily_cases(d.New_cases);
  })
  .attr("width", x_daily_cases.bandwidth())
  .attr("height", function (d) {
    return height_line - 80 - y_daily_cases(d.New_cases);
  })
  .attr("fill", function (d) {
    return "#071b7c";
  })
  .style("opacity", 0.75)
  .on("mousemove", showTooltip_daily_case_1)
  .on("mouseout", mouseleave_daily_case_1);

var svg_daily_average_case_bars = svg_daily_new_case_bars
  .append("path")
  .datum(bars_daily_cases_1_chosen)
  .attr("fill", "none")
  .attr("stroke", "#000000")
  .attr("stroke-width", 2)
  .attr(
    "d",
    d3
      .line()
      .defined((d) => !isNaN(d.Seven_day_average_new_cases))
      .x(function (d) {
        return x_daily_cases(d.Date_label_2) + x_daily_cases.bandwidth() / 2;
      })
      .y(function (d) {
        return y_daily_cases(d.Seven_day_average_new_cases);
      })
  );

svg_daily_new_case_bars
  .append("text")
  .attr("id", "test_milestones")
  .attr("x", function (d) {
    return x_daily_cases("31 Jan 20") + x_daily_cases.bandwidth() / 2;
  })
  .attr("y", function (d) {
    return height_line - 165;
  })
  .style("font-size", ".8rem")
  .text("The black line represents")
  .attr("text-anchor", "start");

svg_daily_new_case_bars
  .append("text")
  .attr("id", "test_milestones")
  .attr("x", function (d) {
    return x_daily_cases("31 Jan 20") + x_daily_cases.bandwidth() / 2;
  })
  .attr("y", function (d) {
    return height_line - 150;
  })
  .style("font-size", ".8rem")
  .text("seven day average new cases")
  .attr("text-anchor", "start");

function update_daily_bars() {
  var selected_figure_1a_area_option = d3
    .select("#select_bars_daily_cases_1_area_button")
    .property("value");

  d3.select("#selected_daily_cases_bars_1_compare_title").html(function (d) {
    return (
      "Pillar 1 and 2 combined daily confirmed COVID-19 cases; " +
      selected_figure_1a_area_option +
      "; as at " +
      data_refreshed_date
    );
  });

  var bars_daily_cases_1_chosen = daily_cases.filter(function (d) {
    return d.Name === selected_figure_1a_area_option;
  });

  var total_cases_daily_chosen = case_summary.filter(function (d) {
    return d.Name === selected_figure_1a_area_option;
  })[0]["Total confirmed cases so far"];

  x_summary = case_summary.filter(function (d, i) {
    return d.Name === selected_figure_1a_area_option;
  });

  d3.select("#x_latest_figures")
    .data(x_summary)
    .html(function (d) {
      return (
        "The total number of confirmed Covid-19 cases so far in " +
        selected_figure_1a_area_option +
        " is " +
        d3.format(",.0f")(d["Total confirmed cases so far"]) +
        ". This is " +
        d3.format(",.0f")(d["Total cases per 100,000 population"]) +
        " cases per 100,000 population. The current daily case count (using data from " +
        complete_date +
        ") is " +
        d3.format(",.0f")(
          d["Confirmed cases swabbed on most recent complete day"]
        ) +
        " new confirmed cases swabbed (" +
        d3.format(",.1f")(
          d[
            "Confirmed cases swabbed per 100,000 population on most recent complete day"
          ]
        ) +
        " per 100,000)."
      );
    });

  var showTooltip_daily_case_1 = function (d) {
    tooltip_daily_case_1
      .html(
        "<h5>" +
          d.Name +
          "</h5><p class = 'tt_class'><b>" +
          d.Date_label +
          '</b></p><p class = "tt_class">In ' +
          d.Name +
          " there were <b>" +
          d3.format(",.0f")(d.New_cases) +
          ' </b>specimens taken on this date which resulted in a positive result for Covid-19.</p><p class = "side">The new cases swabbed on this day represent ' +
          d3.format("0.1%")(d.New_cases / total_cases_daily_chosen) +
          " of the total number of cases confirmed so far (" +
          d3.format(",.0f")(total_cases_daily_chosen) +
          ') </p><p class = "tt_class">In the last seven days there have been a total of ' +
          d3.format(",.0f")(d.Rolling_7_day_new_cases) +
          " reported cases.</p>"
      )
      .style("opacity", 1)
      .style("top", event.pageY - 10 + "px")
      .style("left", event.pageX + 10 + "px")
      .style("opacity", 1)
      .style("visibility", "visible");
  };

  max_limit_x = daily_case_limits.filter(function (d) {
    return d.Name === selected_figure_1a_area_option;
  })[0]["Max_limit"];

  y_daily_cases.domain([0, max_limit_x]).nice();

  // Redraw axis
  yAxis_daily_cases
    .transition()
    .duration(1000)
    .call(d3.axisLeft(y_daily_cases));

  yAxis_daily_cases.selectAll("text").style("font-size", ".8rem");

  svg_daily_average_case_bars
    .datum(bars_daily_cases_1_chosen)
    .transition()
    .duration(1000)
    .attr(
      "d",
      d3
        .line()
        .defined((d) => !isNaN(d.Seven_day_average_new_cases))
        .x(function (d) {
          return x_daily_cases(d.Date_label_2) + x_daily_cases.bandwidth() / 2;
        })
        .y(function (d) {
          return y_daily_cases(d.Seven_day_average_new_cases);
        })
    );

  daily_new_case_bars
    .data(bars_daily_cases_1_chosen)
    .transition()
    .duration(1000)
    .attr("x", function (d) {
      return x_daily_cases(d.Date_label_2);
    })
    .attr("y", function (d) {
      return y_daily_cases(d.New_cases);
    })
    // .attr("width", x_daily_cases.bandwidth())
    .attr("height", function (d) {
      return height_line - 80 - y_daily_cases(d.New_cases);
    })
    .attr("fill", function (d) {
      return "#071b7c";
    })
    .style("opacity", 0.75);

  daily_new_case_bars
    .on("mousemove", showTooltip_daily_case_1)
    .on("mouseout", mouseleave_daily_case_1);
}

d3.select("#select_bars_daily_cases_1_area_button").on("change", function (d) {
  var selected_figure_1a_area_option = d3
    .select("#select_bars_daily_cases_1_area_button")
    .property("value");
  update_daily_bars();
});

// This function is gonna change the opacity and size of selected and unselected circles
function update_annotations_f1() {
  // For each check box:
  d3.selectAll(".checkbox").each(function (d) {
    cb = d3.select(this);
    grp = cb.property("value");

    // If the box is check, show the notes
    if (cb.property("checked")) {
      svg_daily_new_case_bars
        .selectAll("." + grp)
        .transition()
        .duration(1000)
        .style("opacity", 1);
    } else {
      svg_daily_new_case_bars
        .selectAll("." + grp)
        .transition()
        .duration(1000)
        .style("opacity", 0);
    }
  });
}

// When a button change, I run the update function
d3.selectAll(".checkbox").on("change", update_annotations_f1);

update_annotations_f1();
