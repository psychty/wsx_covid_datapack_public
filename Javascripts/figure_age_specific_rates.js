
var svg_cumulative_actual_linear = d3.select("#cumulative_ts_actual_linear")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .append("g")
  .attr("transform", "translate(" + 50 + "," + 20 + ")");

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_line_1_area_button")
  .selectAll('myOptions')
  .data(areas_line)
  .enter()
  .append('option')
  .text(function(d) {
    return d;
  })
  .attr("value", function(d) {
    return d;
  })

// Retrieve the selected area name
var selected_line_1_area_option = d3.select('#select_line_1_area_button').property("value")

// Update text based on selected area
d3.select("#selected_line_1_compare_title")
  .html(function(d) {
    return 'Covid-19 pillar 1 and 2 confirmed cumulative cases over time; ' + selected_line_1_area_option + first_date + ' - ' + latest_date
  });

var line_1_chosen = daily_cases.filter(function(d) {
  return d.Name === selected_line_1_area_option
});

var average_line_1_chosen = daily_cases.filter(function(d) {
    return d.Name === selected_line_1_area_option
  })
  .filter(function(d) {
    if (isNaN(d.Seven_day_average_cumulative_cases)) {
      return false;
    }
    return true;
  });

var new_n = line_1_chosen.filter(function(d) {
  return d.Date === most_recent;
})[0]['Cumulative_cases']

var x_c1 = d3.scaleLinear()
  .domain(d3.extent(line_1_chosen, function(d) {
    return d3.timeParse("%Y-%m-%d")(d.Date);
  }))
  .range([0, width_hm - 60]); // margin left (we pushed the start of the axis over by 50) + another 10 so things do not get cut off

var xAxis_line = svg_cumulative_actual_linear
  .append("g")
  .attr("transform", 'translate(0,' + (height_line - 120 ) + ")")
  .call(d3.axisBottom(x_c1).tickFormat(d3.timeFormat("%d-%B")).tickValues(line_1_chosen.map(function(d) {
    return d3.timeParse("%Y-%m-%d")(d.Date);
  })));

xAxis_line
  .selectAll("text")
  .attr("transform", 'translate(-10,10)rotate(-90)')
  .style("text-anchor", "end")
  .each(function(d,i) { // find the text in that tick and removing it: Thanks Gerardo Furtado on stackoverflow
    if (i%2 == 0) d3.select(this).remove();
    });

var y_c1_ts = d3.scaleLinear()
  .domain([0, d3.max(line_1_chosen, function(d) {
    return +d.Cumulative_cases;
  })])
  .range([height_line - 120 , 0]);

var y_c1_ts_axis = svg_cumulative_actual_linear
  .append("g")
  .attr("transform", 'translate(0,0)')
  .call(d3.axisLeft(y_c1_ts));

var tooltip_c1 = d3.select("#cumulative_ts_actual_linear")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip_class")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("padding", "10px")

var showTooltip_c1 = function(d) {

  tooltip_c1
    .html("<h5>" + d.Name + '</h5><p><b>' + d.Date_label + '</b></p><p class = "side">' + d.double_time_label_1 + '.</p><p class = "side">' + d.Seven_day_ave_cumulative_label + '. </p><p><i>Note: the rolling average has been rounded to the nearest whole number</i>.</p><p class = "side">' + d.double_time_label_2 + '</p>')
    .style("opacity", 1)
    .style("top", (event.pageY - 10) + "px")
    .style("left", (event.pageX + 10) + "px")
    .style('opacity', 1)
    .style("visibility", "visible")
}

var mouseleave_c1 = function(d) {
  tooltip_c1
    .style('opacity', 0)
    .style("visibility", "hidden")
}

svg_cumulative_actual_linear
  .append('line')
  .attr('x1', x_c1(d3.timeParse('%Y-%m-%d')(first_incomplete_date_actual)))
  .attr('y1', 0)
  .attr('x2', x_c1(d3.timeParse('%Y-%m-%d')(first_incomplete_date_actual)))
  .attr('y2', height_line - 120 )
  .attr('stroke', incomplete_colour)
  .attr("stroke-dasharray", ("3, 3"))

svg_cumulative_actual_linear
  .append('rect')
  .attr('x', x_c1(d3.timeParse('%Y-%m-%d')(first_incomplete_date_actual)))
  .attr('y1', 0)
  .attr('width', x_c1(d3.timeParse('%Y-%m-%d')(most_recent)) - x_c1(d3.timeParse('%Y-%m-%d')(first_incomplete_date_actual)))
  .attr('height', height_line - 120 )
  .style('fill', incomplete_colour)
  .style('stroke', 'none')
  .style('opacity', 0.2)

var lines_c1 = svg_cumulative_actual_linear
  .append("path")
  .datum(line_1_chosen)
  .attr("d", d3.line()
    .x(function(d) {
      return x_c1(d3.timeParse("%Y-%m-%d")(d.Date))
    })
    .y(function(d) {
      return y_c1_ts(d.Cumulative_cases)
    })
  )
  .style("fill", "none")
  .style("stroke-width", 1.5)
  .attr("stroke", '#646464')

var dots_c1 = svg_cumulative_actual_linear
  .selectAll('myCircles')
  .data(line_1_chosen)
  .enter()
  .append("circle")
  .attr("cx", function(d) {
    return x_c1(d3.timeParse("%Y-%m-%d")(d.Date))
  })
  .attr("cy", function(d) {
    return y_c1_ts(d.Cumulative_cases)
  })
  .attr("r", 3)
  .style("fill", function(d) {
    return Area_colours(d.Name)
  })
  .attr("stroke", function(d) {
    return Area_colours(d.Name)
  })
  .on("mousemove", showTooltip_c1)
  .on('mouseout', mouseleave_c1);

var lines_c1_seven_day_smooth = svg_cumulative_actual_linear
  .append("path")
  .datum(average_line_1_chosen)
  .attr("stroke", '#000000')
  .attr("stroke-width", 1)
  .attr("stroke-dasharray", ("3, 3"))
  .style("fill", "none")
  .attr("d", d3.line()
    .x(function(d) {
      return x_c1(d3.timeParse("%Y-%m-%d")(d.Date))
    })
    .y(function(d) {
      return y_c1_ts(d.Seven_day_average_cumulative_cases)
    })
  );

var dots_c1_seven_day_smooth = svg_cumulative_actual_linear
  .selectAll('myCircles')
  .data(average_line_1_chosen)
  .enter()
  .append("circle")
  .attr("cx", function(d) {
    return x_c1(d3.timeParse("%Y-%m-%d")(d.Date))
  })
  .attr("cy", function(d) {
    return y_c1_ts(d.Seven_day_average_cumulative_cases)
  })
  .attr("r", 3)
  .style("fill", function(d) {
    return '#c9c9c9'
  })
  .attr("stroke", "#000000")
  .on("mousemove", showTooltip_c1)
  .on('mouseout', mouseleave_c1);

var number_cumulative_cases_total = svg_cumulative_actual_linear
  .append("text")
  .attr("x", width_hm * .03)
  .attr("y", 40)
  .attr('id', 'number_within_2542')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "22px")

var i_change = d3.interpolate(0, new_n);

number_cumulative_cases_total
  .transition()
  .duration(1500)
  .tween("interventions", function() {
    return function(t) {
      current_n = i_change(t);
      number_cumulative_cases_total.text(d3.format(",.0f")(current_n));
    };
  });

svg_cumulative_actual_linear
  .append("text")
  .attr("x", width_hm * .03)
  .attr("y", 50)
  .text('cases confirmed so far')
  .attr("text-anchor", "start")

svg_cumulative_actual_linear
  .append("text")
  .attr('id', 'area_c1')
  .attr("x", width_hm * .03)
  .attr("y", 60)
  .text('in ' + selected_line_1_area_option)
  .attr("text-anchor", "start")

svg_cumulative_actual_linear
  .append("text")
  .attr('id', 'area_c1')
  .attr("x", x_c1(d3.timeParse('%Y-%m-%d')(complete_date_actual)))
  .attr("y", height_line - 135)
  .text('Incomplete data')
  .attr("text-anchor", "end")

svg_cumulative_actual_linear
  .append("text")
  .attr('id', 'area_c1')
  .attr("x", x_c1(d3.timeParse('%Y-%m-%d')(complete_date_actual)))
  .attr("y", height_line - 120)
  .text('' + first_incomplete_date.substring(4, first_incomplete_date.length) + '-' + latest_date.substring(4, latest_date.length))
  .attr("text-anchor", "end")

function update_cumulative_actual_linear() {

  var tooltip_c1 = d3.select("#cumulative_ts_actual_linear")
    .append("div")
    .style("opacity", 0)
    .attr("class", "tooltip_class")
    .style("position", "absolute")
    .style("z-index", "10")
    .style("background-color", "white")
    .style("border", "solid")
    .style("border-width", "1px")
    .style("border-radius", "5px")
    .style("padding", "10px")

  var showTooltip_c1 = function(d) {
    tooltip_c1
      .html("<h5>" + d.Name + '</h5><p><b>' + d.Date_label + '</b></p><p class = "side">' + d.double_time_label_1 + '.</p><p class = "side">The cumulative total averaged over the last seven days (rounded) is ' + d3.format(',.0f')(d.Seven_day_average_cumulative_cases) + ' cases.<p class = "side">' + d.double_time_label_2 + '</p>')
      .style("opacity", 1)
      .style("top", (event.pageY - 10) + "px")
      .style("left", (event.pageX + 10) + "px")
      .style('opacity', 1)
      .style("visibility", "visible")
  }

  var mouseleave_c1 = function(d) {
    tooltip_c1
      .style('opacity', 0)
      .style("visibility", "hidden")
  }

  var selected_line_1_area_option = d3.select('#select_line_1_area_button').property("value")

  d3.select("#selected_line_1_compare_title")
    .html(function(d) {
      return 'Covid-19 pillar 1 and 2 confirmed cumulative cases over time; ' + selected_line_1_area_option + first_date + ' - ' + latest_date
    });

  var old_n = line_1_chosen.filter(function(d) {
    return d.Date === most_recent;
  })[0]['Cumulative_cases']

  line_1_chosen = daily_cases.filter(function(d) {
    return d.Name === selected_line_1_area_option
  });

  var average_line_1_chosen = daily_cases.filter(function(d) {
      return d.Name === selected_line_1_area_option
    })
    .filter(function(d) {
      if (isNaN(d.Seven_day_average_cumulative_cases)) {
        return false;
      }
      return true;
    });

  var new_n = line_1_chosen.filter(function(d) {
    return d.Date === most_recent;
  })[0]['Cumulative_cases']

  y_c1_ts
    .domain([0, d3.max(line_1_chosen, function(d) {
      return +d.Cumulative_cases;
    })])

  // Redraw axis
  y_c1_ts_axis
    .transition()
    .duration(1000)
    .call(d3.axisLeft(y_c1_ts));

  lines_c1
    .datum(line_1_chosen)
    .transition()
    .delay(1000)
    .duration(1000)
    .attr("d", d3.line()
      .x(function(d) {
        return x_c1(d3.timeParse("%Y-%m-%d")(d.Date))
      })
      .y(function(d) {
        return y_c1_ts(d.Cumulative_cases)
      })
    )

  dots_c1
    .data(line_1_chosen)
    .transition()
    .delay(1000)
    .duration(1000)
    .attr("cx", function(d) {
      return x_c1(d3.timeParse("%Y-%m-%d")(d.Date))
    })
    .attr("cy", function(d) {
      return y_c1_ts(d.Cumulative_cases)
    })
    .style("fill", function(d) {
      return Area_colours(d.Name)
    })
    .style("stroke", function(d) {
      return Area_colours(d.Name)
    })

  lines_c1_seven_day_smooth
    .datum(average_line_1_chosen)
    .transition()
    .delay(1000)
    .duration(1000)
    .attr("d", d3.line()
      .x(function(d) {
        return x_c1(d3.timeParse("%Y-%m-%d")(d.Date))
      })
      .y(function(d) {
        return y_c1_ts(d.Seven_day_average_cumulative_cases)
      }));

  dots_c1_seven_day_smooth
    .data(average_line_1_chosen)
    .transition()
    .delay(1000)
    .duration(1000)
    .attr("cx", function(d) {
      return x_c1(d3.timeParse("%Y-%m-%d")(d.Date))
    })
    .attr("cy", function(d) {
      return y_c1_ts(d.Seven_day_average_cumulative_cases)
    })

  var i_change = d3.interpolate(current_n, new_n);

  number_cumulative_cases_total
    .transition()
    .duration(2500)
    .tween("interventions", function() {
      return function(t) {
        current_n = i_change(t);
        number_cumulative_cases_total.text(d3.format(",.0f")(current_n));
      };
    });

  svg_cumulative_actual_linear
    .selectAll("#area_c1")
    .remove();

  svg_cumulative_actual_linear
    .append("text")
    .attr('id', 'area_c1')
    .attr("x", width_hm * .03)
    .attr("y", 60)
    .text('in ' + selected_line_1_area_option)
    .attr("text-anchor", "start")

svg_cumulative_actual_linear
  .append("text")
  .attr('id', 'area_c1')
  .attr("x", x_c1(d3.timeParse('%Y-%m-%d')(complete_date_actual)))
  .attr("y", height_line - 135)
  .text('Incomplete data')
  .attr("text-anchor", "end")

svg_cumulative_actual_linear
  .append("text")
  .attr('id', 'area_c1')
  .attr("x", x_c1(d3.timeParse('%Y-%m-%d')(complete_date_actual)))
  .attr("y", height_line - 120)
  .text('' + first_incomplete_date.substring(4, first_incomplete_date.length) + '-' + latest_date.substring(4, latest_date.length))
  .attr("text-anchor", "end")
}

d3.select("#select_line_1_area_button").on("change", function(d) {
  var selected_line_1_area_option = d3.select('#select_line_1_area_button').property("value")
  update_cumulative_actual_linear()
})
