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
request.open("GET", "./Outputs/daily_cases_bands.json", false);
request.send(null);
var new_cases_bands = JSON.parse(request.responseText); // parse the fetched json data into a variable

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/wsx_case_new_summary.json", false);
request.send(null);
var case_summary = JSON.parse(request.responseText); // parse the fetched json data into a variable

var new_cases_colours = [
  "#ffffb2",
  "#fed976",
  "#feb24c",
  "#fd8d3c",
  "#fc4e2a",
  "#e31a1c",
  "#b10026",
];
var color_new_cases = d3
  .scaleOrdinal()
  .domain(new_cases_bands)
  .range(new_cases_colours);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/daily_cases_per_100000_bands.json", false);
request.send(null);
var new_cases_per_100000_bands = JSON.parse(request.responseText); // parse the fetched json data into a variable

var new_case_rate_colours = [
  "#ffffcc",
  "#ffeda0",
  "#fed976",
  "#feb24c",
  "#fd8d3c",
  "#fc4e2a",
  "#e31a1c",
  "#bd0026",
  "#800026",
];
var color_new_per_100000_cases = d3
  .scaleOrdinal()
  .domain(new_cases_per_100000_bands)
  .range(new_case_rate_colours);

d3.select("#summary_cases_title").html(function (d) {
  return (
    "An overview of confirmed COVID-19 cases; as at " + data_refreshed_date
  );
});

d3.select("#complete_seven").html(function (d) {
  return "* seven days leading to " + latest_date + ".";
});

d3.select("#arrow_explainer").html(function (d) {
  return (
    "*This denotes whether confirmed cases are increasing or decreasing in the seven days to " +
    complete_date +
    " compared to the previous week (the seven days to " +
    rolling_seven_days_ago +
    ")."
  );
});

// d3.select("#heatmap_title").html(function (d) {
//   return "Showing actual confirmed case numbers";
// });

// d3.select("#arrow_explainer").html(function (d) {
//   return (
//     "*The arrows denote whether cases are increasing (red arrows pointing up) or decreasing (green arrows point down) in the seven days to " +
//     complete_date +
//     " compared to the previous week (the seven days to " +
//     previous_week_period +
//     "). A blue equals symbol denotes cases have remained the same across the two weeks."
//   );
// });

start_tile_x_pos = 480;

var x = d3
  .scaleBand()
  .range([start_tile_x_pos, width_hm])
  .domain(dates)
  .padding(0.05);

order_areas = [
  "Adur",
  "Arun",
  "Chichester",
  "Crawley",
  "Horsham",
  "Mid Sussex",
  "Worthing",
  "West Sussex",
];

var svg_title = d3
  .select("#case_headings")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_hm_title)
  .append("g");

svg_title
  .append("text")
  .attr("x", 1)
  .attr("y", 10)
  .text("Area")
  .attr("text-anchor", "start")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_title
  .append("text")
  .attr("x", 225)
  .attr("y", 10)
  .text("Cases in the")
  .attr("text-anchor", "end")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_title
  .append("text")
  .attr("x", 225)
  .attr("y", 25)
  .text("seven days to")
  .attr("text-anchor", "end")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_title
  .append("text")
  .attr("x", 225)
  .attr("y", 40)
  .text(complete_date)
  // .text(complete_date.substring(4, complete_date.length))
  .attr("text-anchor", "end")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_title
  .append("text")
  .attr("x", 250)
  .attr("y", 10)
  .text("Are cases going up")
  .attr("text-anchor", "start")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_title
  .append("text")
  .attr("x", 250)
  .attr("y", 25)
  .text("or down compared")
  .attr("text-anchor", "start")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_title
  .append("text")
  .attr("x", 250)
  .attr("y", 40)
  .text("to last week?*")
  .attr("text-anchor", "start")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_title
  .append("text")
  .attr("x", 465)
  .attr("y", 10)
  .text("Total number")
  .attr("text-anchor", "end")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_title
  .append("text")
  .attr("x", 465)
  .attr("y", 25)
  .text("of confirmed")
  .attr("text-anchor", "end")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_title
  .append("text")
  .attr("x", 465)
  .attr("y", 40)
  .text("cases so far")
  .attr("text-anchor", "end")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_title
  .append("text")
  .attr("x", start_tile_x_pos)
  .attr("y", 10)
  .attr("id", "what_am_i_showing_tiles")
  .text("New cases by day")
  .attr("text-anchor", "start")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_title
  .append("text")
  .attr("x", start_tile_x_pos)
  .attr("y", 40)
  .text(first_date)
  .attr("text-anchor", "start")
  .style("font-size", ".7rem");

svg_title
  .append("text")
  .attr("x", width_hm)
  .attr("y", 40)
  .text(latest_date)
  .attr("text-anchor", "end")
  .style("font-size", ".7rem");

svg_title
  .append("line")
  .attr("x1", 0)
  .attr("y1", 0)
  .attr("x2", width_hm)
  .attr("y2", 0)
  .attr("stroke", "#000000");

svg_title
  .append("line")
  .attr("x1", 0)
  .attr("y1", height_hm_title)
  .attr("x2", width_hm)
  .attr("y2", height_hm_title)
  .attr("stroke", "#000000");

svg_title
  .append("text")
  .attr("x", x(first_incomplete_date) - 5)
  .attr("y", 10)
  .text("latest complete date (" + complete_date + ")")
  .attr("text-anchor", "end")
  .style("font-size", ".7rem");

svg_title
  .append("line")
  .attr("x1", x(first_incomplete_date))
  .attr("y1", 0)
  .attr("x2", x(first_incomplete_date))
  .attr("y2", height_hm_title)
  .attr("stroke", "red")
  .attr("stroke-dasharray", "3, 3");

svg_title
  .append("rect")
  .attr("x", x(first_incomplete_date))
  .attr("y1", 0)
  .attr("width", width_hm)
  .attr("height", height_hm_title)
  .style("fill", incomplete_colour)
  .style("stroke", "none")
  .style("opacity", 0.2);

function counts_new_cases_tile_plot() {
  // Create a function for tabulating the data
  function new_case_daily_plot(area_x_chosen, svg_x) {
    area_x = daily_cases.filter(function (d) {
      // gets a subset of the json data
      return d.Name === area_x_chosen;
    });

    area_x_case_summary = case_summary.filter(function (d) {
      // gets a subset of the json data
      return d.Name === area_x_chosen;
    });

    d3.select("#heatmap_title").html(function (d) {
      return "Showing actual confirmed case numbers";
    });

    var svg = d3
      .select("#new_cases_plotted")
      .append("svg")
      .attr("id", "catch_me_svg")
      .attr("width", width_hm)
      .attr("height", height_hm)
      .append("g");

    var tooltip_new_case_day = d3
      .select("#new_cases_plotted")
      .append("div")
      .style("opacity", 0)
      .attr("class", "tooltip_class")
      .style("position", "absolute")
      .style("z-index", "10")
      .style("background-color", "white")
      .style("border", "solid")
      .style("font-size", ".7rem")
      .style("border-width", "1px")
      .style("border-radius", "5px")
      .style("padding", ".8rem");

    var mouseover = function (d) {
      d3.select(this).style("stroke", "black").style("stroke-width", "1px");
    };

    var mousemove = function (d) {
      tooltip_new_case_day
        .html(
          "<h4>" +
            d.Name +
            " - " +
            d.Date_label +
            "</h4><p>" +
            d.Case_label +
            "</p><p>" +
            d.Rate_label +
            "</p>"
        )
        .style("top", event.pageY - 10 + "px")
        .style("left", event.pageX + 10 + "px")
        .style("opacity", 1)
        .style("visibility", "visible");
    };

    var mouseleave = function (d) {
      tooltip_new_case_day.style("opacity", 0).style("visibility", "hidden");

      d3.select(this).style("stroke", "none");
    };

    svg
      .append("text")
      .attr("x", 1)
      .attr("y", height_hm * 0.5)
      .text(area_x_chosen)
      .attr("text-anchor", "start")
      .attr("font-weight", function (d) {
        if (area_x_chosen === "West Sussex") {
          return "bold";
        } else {
          return "normal";
        }
      })
      .style("font-size", ".8rem");

    svg
      .append("text")
      .attr("x", 225)
      .attr("y", height_hm * 0.5)
      .text(function (d) {
        return d3.format(",.0f")(
          area_x_case_summary[0]["Rolling_7_day_new_cases"]
        );
      })
      .attr("text-anchor", "end")
      .attr("font-weight", function (d) {
        if (area_x_chosen === "West Sussex") {
          return "bold";
        } else {
          return "normal";
        }
      })
      .style("font-size", ".8rem");

    svg
      .append("text")
      .attr("x", 250)
      .attr("y", height_hm * 0.5)
      .text(function (d) {
        return area_x_case_summary[0]["Change_direction"];
      })
      .attr("text-anchor", "start")
      .attr("font-weight", function (d) {
        if (area_x_chosen === "West Sussex") {
          return "bold";
        } else {
          return "normal";
        }
      })
      .style("font-size", ".8rem");

    svg
      .append("text")
      .attr("x", 465)
      .attr("y", height_hm * 0.5)
      .text(function (d) {
        return d3.format(",.0f")(area_x_case_summary[0]["Cumulative_cases"]);
      })
      .attr("text-anchor", "end")
      .attr("font-weight", function (d) {
        if (area_x_chosen === "West Sussex") {
          return "bold";
        } else {
          return "normal";
        }
      })
      .style("font-size", ".8rem");

    svg
      .append("line")
      .attr("x1", x(first_incomplete_date))
      .attr("y1", 0)
      .attr("x2", x(first_incomplete_date))
      .attr("y2", height_hm)
      .attr("stroke", incomplete_colour)
      .attr("stroke-dasharray", "3, 3");

    svg
      .append("rect")
      .attr("x", x(first_incomplete_date))
      .attr("y1", 0)
      .attr("width", width_hm)
      .attr("height", height_hm)
      .style("fill", incomplete_colour)
      .style("stroke", "none")
      .style("opacity", 0.2);

    svg
      .selectAll()
      .data(area_x)
      .enter()
      .append("rect")
      .attr("x", function (d) {
        return x(d.Date_label);
      })
      .attr("rx", 4)
      .attr("ry", 4)
      .attr("width", x.bandwidth())
      .attr("height", height_hm - 4)
      .style("fill", function (d) {
        return color_new_cases(d.new_case_key);
      })
      .style("stroke-width", 4)
      .style("stroke", "none")
      // .style("opacity", 0.8)
      .on("mouseover", mouseover)
      .on("mousemove", mousemove)
      .on("mouseleave", mouseleave);

    svg
      .append("line")
      .attr("x1", 0)
      .attr("y1", height_hm)
      .attr("x2", width_hm)
      .attr("y2", height_hm)
      .attr("stroke", "#c9c9c9");
  }

  new_case_daily_plot(order_areas[0]);
  new_case_daily_plot(order_areas[1]);
  new_case_daily_plot(order_areas[2]);
  new_case_daily_plot(order_areas[3]);
  new_case_daily_plot(order_areas[4]);
  new_case_daily_plot(order_areas[5]);
  new_case_daily_plot(order_areas[6]);
  new_case_daily_plot(order_areas[7]);

  function key_1_new_cases() {
    new_cases_bands.forEach(function (item, index) {
      var list = document.createElement("li");
      list.innerHTML = item;
      list.className = "key_list";
      list.style.borderColor = color_new_cases(index);
      var tt = document.createElement("div");
      tt.className = "side_tt";
      tt.style.borderColor = color_new_cases(index);
      var tt_h3_1 = document.createElement("h3");
      tt_h3_1.innerHTML = item.Cause;

      tt.appendChild(tt_h3_1);
      var div = document.getElementById("new_case_key_figure");
      div.appendChild(list);
    });
  }

  key_1_new_cases();
}

function counts_new_cases_rates_tile_plot() {
  function new_case_daily_plot(area_x_chosen, svg_x) {
    area_x = daily_cases.filter(function (d) {
      // gets a subset of the json data
      return d.Name === area_x_chosen;
    });

    area_x_case_summary = case_summary.filter(function (d) {
      // gets a subset of the json data
      return d.Name === area_x_chosen;
    });

    d3.select("#heatmap_title").html(function (d) {
      return "Showing confirmed case numbers per 100,000 population";
    });

    var svg = d3
      .select("#new_cases_plotted")
      .append("svg")
      .attr("id", "catch_me_svg")
      .attr("width", width_hm)
      .attr("height", height_hm)
      .append("g");

    var tooltip_new_case_day = d3
      .select("#new_cases_plotted")
      .append("div")
      .style("opacity", 0)
      .attr("class", "tooltip_class")
      .style("position", "absolute")
      .style("z-index", "10")
      .style("background-color", "white")
      .style("border", "solid")
      .style("font-size", ".7rem")
      .style("border-width", "1px")
      .style("border-radius", "5px")
      .style("padding", "10px");

    var mouseover = function (d) {
      d3.select(this).style("stroke", "black").style("stroke-width", "1px");
    };

    var mousemove = function (d) {
      tooltip_new_case_day
        .html(
          "<h4>" +
            d.Name +
            " - " +
            d.Date_label +
            "</h4><p>" +
            d.Case_label +
            "</p><p>" +
            d.Rate_label +
            "</p>"
        )
        .style("top", event.pageY - 10 + "px")
        .style("left", event.pageX + 10 + "px")
        .style("opacity", 1)
        .style("visibility", "visible");
    };

    var mouseleave = function (d) {
      tooltip_new_case_day.style("opacity", 0).style("visibility", "hidden");

      d3.select(this).style("stroke", "none");
    };

    svg
      .append("text")
      .attr("x", 1)
      .attr("y", height_hm * 0.5)
      .text(area_x_chosen)
      .attr("text-anchor", "start")
      .attr("font-weight", function (d) {
        if (area_x_chosen === "West Sussex") {
          return "bold";
        } else {
          return "normal";
        }
      })
      .style("font-size", ".8rem");

    svg
      .append("text")
      .attr("x", 225)
      .attr("y", height_hm * 0.5)
      .text(function (d) {
        return d3.format(",.1f")(
          area_x_case_summary[0]["Rolling_7_day_new_cases_per_100000"]
        );
      })
      .attr("text-anchor", "end")
      .attr("font-weight", function (d) {
        if (area_x_chosen === "West Sussex") {
          return "bold";
        } else {
          return "normal";
        }
      })
      .style("font-size", ".8rem");

    svg
      .append("text")
      .attr("x", 250)
      .attr("y", height_hm * 0.5)
      .text(function (d) {
        return area_x_case_summary[0]["Change_direction"];
      })
      .attr("text-anchor", "start")
      .attr("font-weight", function (d) {
        if (area_x_chosen === "West Sussex") {
          return "bold";
        } else {
          return "normal";
        }
      })
      .style("font-size", ".8rem");

    svg
      .append("text")
      .attr("x", 465)
      .attr("y", height_hm * 0.5)
      .text(function (d) {
        return d3.format(",.1f")(
          area_x_case_summary[0]["Cumulative_per_100000"]
        );
      })
      .attr("text-anchor", "end")
      .attr("font-weight", function (d) {
        if (area_x_chosen === "West Sussex") {
          return "bold";
        } else {
          return "normal";
        }
      })
      .style("font-size", ".8rem");

    svg
      .append("line")
      .attr("x1", x(first_incomplete_date))
      .attr("y1", 0)
      .attr("x2", x(first_incomplete_date))
      .attr("y2", height_hm)
      .attr("stroke", incomplete_colour)
      .attr("stroke-dasharray", "3, 3");

    svg
      .append("rect")
      .attr("x", x(first_incomplete_date))
      .attr("y1", 0)
      .attr("width", width_hm)
      .attr("height", height_hm_title)
      .style("fill", incomplete_colour)
      .style("stroke", "none")
      .style("opacity", 0.2);

    svg
      .selectAll()
      .data(area_x)
      .enter()
      .append("rect")
      .attr("x", function (d) {
        return x(d.Date_label);
      })
      .attr("rx", 4)
      .attr("ry", 4)
      .attr("width", x.bandwidth())
      .attr("height", height_hm - 4)
      .style("fill", function (d) {
        return color_new_per_100000_cases(d.new_case_per_100000_key);
      })
      .style("stroke-width", 4)
      .style("stroke", "none")
      // .style("opacity", 0.8)
      .on("mouseover", mouseover)
      .on("mousemove", mousemove)
      .on("mouseleave", mouseleave);

    svg
      .append("line")
      .attr("x1", 0)
      .attr("y1", height_hm)
      .attr("x2", width_hm)
      .attr("y2", height_hm)
      .attr("stroke", "#c9c9c9");
  }

  new_case_daily_plot(order_areas[0]);
  new_case_daily_plot(order_areas[1]);
  new_case_daily_plot(order_areas[2]);
  new_case_daily_plot(order_areas[3]);
  new_case_daily_plot(order_areas[4]);
  new_case_daily_plot(order_areas[5]);
  new_case_daily_plot(order_areas[6]);
  new_case_daily_plot(order_areas[7]);

  function key_1_new_cases() {
    new_cases_per_100000_bands.forEach(function (item, index) {
      var list = document.createElement("li");
      list.innerHTML = item;
      list.className = "key_list";
      list.style.borderColor = color_new_per_100000_cases(index);
      var tt = document.createElement("div");
      tt.className = "side_tt";
      tt.style.borderColor = color_new_cases(index);
      var tt_h3_1 = document.createElement("h3");
      tt_h3_1.innerHTML = item.Cause;

      tt.appendChild(tt_h3_1);
      var div = document.getElementById("new_case_key_figure");
      div.appendChild(list);
    });
  }
  key_1_new_cases();
}

counts_new_cases_tile_plot();

function toggle_count_rate_func() {
  var type = document.getElementsByName("toggle_count_rate");
  if (type[0].checked) {
    // console.log("We'll put the count version on for you");

    $(".key_list").remove();

    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();

    counts_new_cases_tile_plot();

    svg_title.selectAll("#what_am_i_showing_tiles").remove();

    svg_title
      .append("text")
      .attr("x", start_tile_x_pos)
      .attr("y", 10)
      .attr("id", "what_am_i_showing_tiles")
      .text("New cases by day")
      .attr("text-anchor", "start")
      .style("font-weight", "bold")
      .style("font-size", ".8rem");
  } else if (type[1].checked) {
    // console.log("We'll put the rate version on for you");

    $(".key_list").remove();

    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();
    $("#catch_me_svg").remove();

    counts_new_cases_rates_tile_plot();

    svg_title.selectAll("#what_am_i_showing_tiles").remove();

    svg_title
      .append("text")
      .attr("x", start_tile_x_pos)
      .attr("y", 10)
      .attr("id", "what_am_i_showing_tiles")
      .text("New cases per 100,000 population by day")
      .attr("text-anchor", "start")
      .style("font-weight", "bold")
      .style("font-size", ".8rem");
  }
}
