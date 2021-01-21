var request = new XMLHttpRequest();
request.open("GET", "./Outputs/utla_growth_since_november.json", false);
request.send(null);
var utla_growth_ts_data = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/growth_limits_dates.json", false);
request.send(null);
var utla_growth_ts_dates = JSON.parse(request.responseText);

var height_scatter = 400;
var width_growth_utla = width_hm * 0.8;

// append the svg object to the body of the page
var growth_svg_2 = d3
  .select("#utla_ts_growth_rate")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_scatter)
  .append("g")
  .attr("transform", "translate(" + 60 + "," + 20 + ")");

// When the slider moves the figure should be redrawn.
function new_date_growth_utla() {
  d3.select("#utla_growth_ts_rate_title").html(function (d) {
    return (
      "New confirmed COVID-19 cases per 100,000 population (all ages) in the seven days to " +
      d3.timeFormat("%A %d %B")(sliderTime.value()) +
      " by week on week change in number of cases; Upper Tier Local Authorities;"
    );
  });

  chosen_utla_ts = d3.timeFormat("%Y-%m-%d")(sliderTime.value());

  chosen_time_utla_df = utla_growth_ts_data.filter(function (d, i) {
    return d.Date === chosen_utla_ts;
  });

  console.log(chosen_utla_ts);

  //
  y_growth_utla_ts
    .domain([
      -1,
      d3.max(chosen_time_utla_df, function (d) {
        return +d.Change_actual_by_week;
      }),
    ])
    .nice();
  //
  // Redraw axis
  y_growth_utla_ts_axis
    .transition()
    .duration(1000)
    .call(d3.axisLeft(y_growth_utla_ts).tickFormat(d3.format(".0%")));

  y_growth_utla_ts_axis.selectAll("text").style("font-size", ".8rem");

  x_growth_utla_ts
    .domain([
      0,
      d3.max(chosen_time_utla_df, function (d) {
        return +d.Rolling_7_day_rate;
      }),
    ])
    .nice();

  x_growth_utla_ts_axis
    .transition()
    .duration(1000)
    .call(d3.axisBottom(x_growth_utla_ts));

  x_growth_utla_ts_axis.selectAll("text").style("font-size", ".8rem");

  utla_growth_ts_dots
    .data(chosen_time_utla_df)
    .transition()
    .duration(1000)
    .attr("cx", function (d) {
      return x_growth_utla_ts(d.Rolling_7_day_rate);
    })
    .attr("cy", function (d) {
      return y_growth_utla_ts(d.Change_actual_by_week);
    })
    .style("fill", function (d) {
      if (d.Name == "Brighton and Hove") {
        return "#0000cc";
      } else if (d.Name == "East Sussex") {
        return "#930157";
      } else if (d.Name == "West Sussex") {
        return "orange";
      } else if (d.Name == "England") {
        return "black";
      } else {
        return "#69b3a2";
      }
    });

  utla_growth_ts_line.selectAll("#zero_line_utla_growth").remove();

  growth_svg_2.selectAll("#zero_line_utla_growth_t1").remove();

  growth_svg_2.selectAll("#zero_line_utla_growth_t2").remove();

  growth_svg_2.selectAll("#zero_line_utla_growth_t3").remove();

  utla_growth_ts_line
    .attr("id", "zero_line_utla_growth")
    .attr("x1", 0)
    .attr("y1", function (d) {
      return y_growth_utla_ts(0);
    })
    .attr("x2", width_growth_utla - 80)
    .attr("y2", function (d) {
      return y_growth_utla_ts(0);
    })
    .attr("stroke", "#000000")
    .attr("stroke-dasharray", "3, 3");

  growth_svg_2
    .append("text")
    .attr("x", width_growth_utla - 70)
    .attr("y", function (d) {
      return y_growth_utla_ts(0) - 12;
    })
    .attr("id", "zero_line_utla_growth_t1")
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("Areas above this line are increasing");

  growth_svg_2
    .append("text")
    .attr("x", width_growth_utla - 70)
    .attr("y", function (d) {
      return y_growth_utla_ts(0);
    })
    .attr("id", "zero_line_utla_growth_t2")
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("and areas below are decreasing");

  growth_svg_2
    .append("text")
    .attr("x", width_growth_utla - 70)
    .attr("y", function (d) {
      return y_growth_utla_ts(0) + 12;
    })
    .attr("id", "zero_line_utla_growth_t3")
    .attr("text-anchor", "start")
    .style("font-size", ".8rem")
    .text("compared to cases 7 days before.");
}

wsx_latest_growth = utla_growth_ts_data.filter(function (d) {
  return (d.Name == "West Sussex") & (d.Date === complete_date_actual);
});

england_latest_growth = utla_growth_ts_data.filter(function (d) {
  return (d.Name == "England") & (d.Date === complete_date_actual);
});

d3.select("#wsx_growth_rate_latest").html(function (d) {
  return (
    wsx_latest_growth[0]["Label_2"].replace(
      "In ",
      "Overall in West Sussex, in "
    ) +
    " " +
    wsx_latest_growth[0]["Label_1"] +
    "."
  );
});

////////// slider //////////
// This is not great, but it takes the number of unique dates and recreates an array of dates from september 01
var dataTime = d3.range(0, utla_growth_ts_dates.length).map(function (d) {
  return new Date(2020, 10, 01 + d);
});

var sliderTime = d3
  .sliderBottom()
  .min(d3.min(dataTime))
  .max(d3.max(dataTime))
  .width(300)
  .tickFormat(d3.timeFormat(""))
  .tickValues(dataTime)
  .default(new Date(2020, 10, 1 + utla_growth_ts_dates.length))
  .on("onchange", new_date_growth_utla);

var utla_growth_slider_svg = d3
  .select("#utla_growth_slider")
  .append("svg")
  .attr("width", 400)
  .attr("height", 40)
  .append("g")
  .attr("transform", "translate(30,20)");

utla_growth_slider_svg.call(sliderTime);

chosen_utla_ts = d3.timeFormat("%Y-%m-%d")(sliderTime.value());
chosen_time_utla_df = utla_growth_ts_data.filter(function (d, i) {
  return d.Date === chosen_utla_ts;
});

// Add X axis
var x_growth_utla_ts = d3
  .scaleLinear()
  .domain([
    0,
    d3.max(chosen_time_utla_df, function (d) {
      return +d.Rolling_7_day_rate;
    }),
  ])
  .range([0, width_growth_utla - 80]);

var x_growth_utla_ts_axis = growth_svg_2
  .append("g")
  .attr("transform", "translate(0," + (height_scatter - 80) + ")")
  .call(d3.axisBottom(x_growth_utla_ts));

x_growth_utla_ts_axis.selectAll("text").style("font-size", ".8rem");

// Add Y axis
var y_growth_utla_ts = d3
  .scaleLinear()
  .domain([
    -1,
    d3.max(chosen_time_utla_df, function (d) {
      return +d.Change_actual_by_week;
    }),
  ])
  .range([height_scatter - 80, 0]);

var y_growth_utla_ts_axis = growth_svg_2
  .append("g")
  .call(d3.axisLeft(y_growth_utla_ts).tickFormat(d3.format(".0%")));

y_growth_utla_ts_axis.selectAll("text").style("font-size", ".8rem");

var tooltip_growth_utla_ts = d3
  .select("#utla_ts_growth_rate")
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

var showTooltip_growth_ts = function (d) {
  tooltip_growth_utla_ts
    .html(
      "<h5>" +
        d.Name +
        "</h5><p>" +
        d.Label_2 +
        "</p><p>" +
        d.Label_1 +
        "</p><p>" +
        d.Label_3 +
        "</p>"
    )
    .style("opacity", 1)
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("opacity", 1)
    .style("visibility", "visible");
};

var mouseleave_growth_utla_ts = function (d) {
  tooltip_growth_utla_ts.style("opacity", 0).style("visibility", "hidden");
};

utla_growth_ts_dots = growth_svg_2
  .append("g")
  .selectAll("dot")
  .data(chosen_time_utla_df)
  .enter()
  .append("circle")
  .attr("cx", function (d) {
    return x_growth_utla_ts(d.Rolling_7_day_rate);
  })
  .attr("cy", function (d) {
    return y_growth_utla_ts(d.Change_actual_by_week);
  })
  .attr("r", 7)
  .style("fill", function (d) {
    if (d.Name == "Brighton and Hove") {
      return "#0000cc";
    } else if (d.Name == "East Sussex") {
      return "#930157";
    } else if (d.Name == "West Sussex") {
      return "orange";
    } else if (d.Name == "England") {
      return "black";
    } else {
      return "#69b3a2";
    }
  })
  .style("opacity", 0.8)
  .style("stroke", "white")
  .on("mousemove", showTooltip_growth_ts)
  .on("mouseout", mouseleave_growth_utla_ts);

utla_growth_ts_line = growth_svg_2
  .append("line")
  .attr("id", "zero_line_utla_growth")
  .attr("x1", 0)
  .attr("y1", function (d) {
    return y_growth_utla_ts(0);
  })
  .attr("x2", width_growth_utla - 80)
  .attr("y2", function (d) {
    return y_growth_utla_ts(0);
  })
  .attr("stroke", "#000000")
  .attr("stroke-dasharray", "3, 3");

growth_svg_2
  .append("text")
  .attr("x", width_growth_utla - 70)
  .attr("y", function (d) {
    return y_growth_utla_ts(0) - 10;
  })
  .attr("id", "zero_line_utla_growth_t1")
  .attr("text-anchor", "start")
  .style("font-size", "10px")
  .text("Areas above this line are increasing");

growth_svg_2
  .append("text")
  .attr("x", width_growth_utla - 70)
  .attr("y", function (d) {
    return y_growth_utla_ts(0);
  })
  .attr("id", "zero_line_utla_growth_t2")
  .attr("text-anchor", "start")
  .style("font-size", "10px")
  .text("and areas below are decreasing");

growth_svg_2
  .append("text")
  .attr("x", width_growth_utla - 70)
  .attr("y", function (d) {
    return y_growth_utla_ts(0) + 10;
  })
  .attr("id", "zero_line_utla_growth_t3")
  .attr("text-anchor", "start")
  .style("font-size", "10px")
  .text("compared to cases 7 days before.");

// Run the function
new_date_growth_utla();

var area_growth_comp = [
  "Brighton and Hove",
  "East Sussex",
  "West Sussex",
  "Other UTLAs",
  "England",
];

var utla_growth_colour_func = d3
  .scaleOrdinal()
  .domain(area_growth_comp)
  .range(["#0000cc", "#930157", "orange", "#69b3a2", "black"]);

area_growth_comp.forEach(function (item, index) {
  var list = document.createElement("li");
  list.innerHTML = item;
  list.className = "key_growth_utla";
  list.style.borderColor = utla_growth_colour_func(index);
  var tt = document.createElement("div");
  tt.className = "side_tt";
  tt.style.borderColor = utla_growth_colour_func(index);
  var tt_h3_1 = document.createElement("h3");
  tt_h3_1.innerHTML = item;
  tt.appendChild(tt_h3_1);
  var div = document.getElementById("growth_key_utla");
  div.appendChild(list);
});
