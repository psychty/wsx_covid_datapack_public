
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/daily_cases_bands.json", false);
request.send(null);
var new_cases_bands = JSON.parse(request.responseText); // parse the fetched json data into a variable

var new_cases_colours = ['#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026']
var color_new_cases = d3.scaleOrdinal()
  .domain(new_cases_bands)
  .range(new_cases_colours)

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/daily_cases_per_100000_bands.json", false);
request.send(null);
var new_cases_per_100000_bands = JSON.parse(request.responseText); // parse the fetched json data into a variable

var new_case_rate_colours = ['#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026']
var color_new_per_100000_cases = d3.scaleOrdinal()
  .domain(new_cases_per_100000_bands)
  .range(new_case_rate_colours)


d3.select("#summary_cases_title")
  .html(function(d) {
    return 'Heatmap of Covid-19 pillar 1 and 2 confirmed cases; ' + first_date + ' - ' + latest_date
  });

d3.select("#complete_seven")
  .html(function(d) {
    return '* seven days leading to ' + latest_date + '.'
  });

start_tile_x_pos = 450

var x = d3.scaleBand()
  .range([start_tile_x_pos, width_hm])
  .domain(dates)
  .padding(0.05);

order_areas = ['West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing']

var svg_title = d3.select('#case_headings')
  .append('svg')
  .attr('width', width_hm)
  .attr('height', height_hm_title)
  .append('g')

svg_title
  .append("text")
  .attr("x", 1)
  .attr("y", 10)
  .text('Area')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 125)
  .attr("y", 10)
  .text('Total')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 125)
  .attr("y", 25)
  .text('cases')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 125)
  .attr("y", 40)
  .text('so far')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 175)
  .attr("y", 10)
  .text('Total cases')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 175)
  .attr("y", 25)
  .text('per 100,000')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 175)
  .attr("y", 40)
  .text('population')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 250)
  .attr("y", 10)
  .text('Confirmed cases')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 250)
  .attr("y", 25)
  .text('swabbed on')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 250)
  .attr("y", 40)
  .text(complete_date.substring(4, complete_date.length))
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 335)
  .attr("y", 10)
  .text('Total number of')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 335)
  .attr("y", 25)
  .text('cases in most')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", 335)
  .attr("y", 40)
  .text('recent 7 days*')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", start_tile_x_pos)
  .attr("y", 10)
  .attr('id', 'what_am_i_showing_tiles')
  .text('New cases by day')
  .attr("text-anchor", "start")
  .style('font-weight', 'bold')
  .style("font-size", "10px")

svg_title
  .append("text")
  .attr("x", start_tile_x_pos)
  .attr("y", 40)
  .text(first_date)
  .attr("text-anchor", "start")
  .style("font-size", "8px")

svg_title
  .append("text")
  .attr("x", width_hm)
  .attr("y", 40)
  .text(latest_date)
  .attr("text-anchor", "end")
  .style("font-size", "8px")

svg_title
  .append('line')
  .attr('x1', 0)
  .attr('y1', 0)
  .attr('x2', width_hm)
  .attr('y2', 0)
  .attr('stroke', '#000000')

svg_title
  .append('line')
  .attr('x1', 0)
  .attr('y1', height_hm_title)
  .attr('x2', width_hm)
  .attr('y2', height_hm_title)
  .attr('stroke', '#000000')

svg_title
  .append("text")
  .attr("x", x(first_incomplete_date) - 5)
  .attr("y", 10)
  .text('latest complete date (' + complete_date + ')')
  .attr("text-anchor", "end")
  .style("font-size", "8px")

svg_title
  .append('line')
  .attr('x1', x(first_incomplete_date))
  .attr('y1', 0)
  .attr('x2', x(first_incomplete_date))
  .attr('y2', height_hm_title)
  .attr('stroke', 'red')
  .attr("stroke-dasharray", ("3, 3"))

svg_title
  .append('rect')
  .attr('x', x(first_incomplete_date))
  .attr('y1', 0)
  .attr('width', width_hm)
  .attr('height', height_hm_title)
  .style('fill', incomplete_colour)
  .style('stroke', 'none')
  .style('opacity', 0.2)

function counts_new_cases_tile_plot() {

  // Create a function for tabulating the data
  function new_case_daily_plot(area_x_chosen, svg_x) {

    area_x = daily_cases.filter(function(d) { // gets a subset of the json data
      return d.Name === area_x_chosen
    })

    area_x_case_summary = case_summary.filter(function(d) { // gets a subset of the json data
      return d.Name === area_x_chosen
    })

    var svg = d3.select('#new_cases_plotted')
      .append('svg')
      .attr('id', 'catch_me_svg')
      .attr('width', width_hm)
      .attr('height', height_hm)
      .append('g')

    var tooltip_new_case_day = d3.select("#new_cases_plotted")
      .append("div")
      .style("opacity", 0)
      .attr("class", "tooltip_class")
      .style("position", "absolute")
      .style("z-index", "10")
      .style("background-color", "white")
      .style("border", "solid")
      .style('font-size', '12px')
      .style("border-width", "1px")
      .style("border-radius", "5px")
      .style("padding", "10px")

    var mouseover = function(d) {
      d3.select(this)
        .style("stroke", "black")
        .style('stroke-width', '1px')
    }

    var mousemove = function(d) {
      tooltip_new_case_day
        .html('<h4>' + d.Name + ' - ' + d.Period + '</h4><p>' + d.Case_label + '</p><p>' + d.Rate_label + '</p>')
        .style("top", (event.pageY - 10) + "px")
        .style("left", (event.pageX + 10) + "px")
        .style('opacity', 1)
        .style('visibility', 'visible')
    }

    var mouseleave = function(d) {
      tooltip_new_case_day
        .style("opacity", 0)
        .style('visibility', 'hidden')

      d3.select(this)
        .style("stroke", "none")
    }

    svg
      .append("text")
      .attr("x", 1)
      .attr("y", height_hm * .5)
      .text(area_x_chosen)
      .attr("text-anchor", "start")
      .attr('font-weight', function(d) {
        if (area_x_chosen === 'West Sussex') {
          return ('bold')
        } else {
          return ('normal')
        }
      })
      .style("font-size", "10px")

    svg
      .append("text")
      .attr("x", 125)
      .attr("y", height_hm * .5)
      .text(function(d) {
        return d3.format(",.0f")(area_x_case_summary[0]['Total confirmed cases so far'])
      })
      .attr("text-anchor", "start")
      .attr('font-weight', function(d) {
        if (area_x_chosen === 'West Sussex') {
          return ('bold')
        } else {
          return ('normal')
        }
      })
      .style("font-size", "10px")

    svg
      .append("text")
      .attr("x", 175)
      .attr("y", height_hm * .5)
      .text(function(d) {
        return d3.format(",.1f")(area_x_case_summary[0]['Total cases per 100,000 population'])
      })
      .attr("text-anchor", "start")
      .attr('font-weight', function(d) {
        if (area_x_chosen === 'West Sussex') {
          return ('bold')
        } else {
          return ('normal')
        }
      })
      .style("font-size", "10px")

    svg
      .append("text")
      .attr("x", 250)
      .attr("y", height_hm * .5)
      .text(function(d) {
        return d3.format(",.0f")(area_x_case_summary[0]['Confirmed cases swabbed on most recent complete day'])
      })
      .attr("text-anchor", "start")
      .attr('font-weight', function(d) {
        if (area_x_chosen === 'West Sussex') {
          return ('bold')
        } else {
          return ('normal')
        }
      })
      .style("font-size", "10px")

    svg
      .append("text")
      .attr("x", 335)
      .attr("y", height_hm * .5)
      .text(function(d) {
      if (isNaN(area_x_case_summary[0]['Total cases confirmed in most recent seven days'])) {
      return 'No cases*';
      }
      return d3.format(",.0f")(area_x_case_summary[0]['Total cases confirmed in most recent seven days'])})
      .attr("text-anchor", "start")
      .attr('font-weight', function(d) {
        if (area_x_chosen === 'West Sussex') {
          return ('bold')
        } else {
          return ('normal')
        }
      })
      .style("font-size", "10px")

// var increasing_symbol = d3.symbol()
//   .type('triangle-up')
//   .size(30);
//
// var triangles = svg
//   .selectAll('path')
//   .attr('id', 'whatdis')
//   // .data(data)
//   // .enter()
//   .attr('x', 125)
//   .attr('y', 20)
//   .append('path')
//   .attr('d', increasing_symbol)
//   .attr('stroke', 'black')
//   .attr('fill', 'green')

svg
  .append('line')
  .attr('x1', x(first_incomplete_date))
  .attr('y1', 0)
  .attr('x2', x(first_incomplete_date))
  .attr('y2', height_hm)
  .attr('stroke', incomplete_colour)
  .attr("stroke-dasharray", ("3, 3"))

svg
  .append('rect')
  .attr('x', x(first_incomplete_date))
  .attr('y1', 0)
  .attr('width', width_hm)
  .attr('height', height_hm)
  .style('fill', incomplete_colour)
  .style('stroke', 'none')
  .style('opacity', 0.2)

svg
  .selectAll()
  .data(area_x)
  .enter()
  .append("rect")
  .attr("x", function(d) {
      return x(d.Date_label)
      })
  .attr("rx", 4)
  .attr("ry", 4)
  .attr("width", x.bandwidth())
  .attr("height", height_hm - 4)
  .style("fill", function(d) {
      return color_new_cases(d.new_case_key)
      })
  .style("stroke-width", 4)
  .style("stroke", "none")
  // .style("opacity", 0.8)
  .on("mouseover", mouseover)
  .on("mousemove", mousemove)
  .on("mouseleave", mouseleave)

svg
  .append('line')
  .attr('x1', 0)
  .attr('y1', height_hm)
  .attr('x2', width_hm)
  .attr('y2', height_hm)
  .attr('stroke', '#c9c9c9')

}

new_case_daily_plot(order_areas[0])
new_case_daily_plot(order_areas[1])
new_case_daily_plot(order_areas[2])
new_case_daily_plot(order_areas[3])
new_case_daily_plot(order_areas[4])
new_case_daily_plot(order_areas[5])
new_case_daily_plot(order_areas[6])
new_case_daily_plot(order_areas[7])

function key_1_new_cases() {
  new_cases_bands.forEach(function(item, index) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = 'key_list';
    list.style.borderColor = color_new_cases(index);
      var tt = document.createElement('div');
    tt.className = 'side_tt';
    tt.style.borderColor = color_new_cases(index);
    var tt_h3_1 = document.createElement('h3');
    tt_h3_1.innerHTML = item.Cause;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("new_case_key_figure");
    div.appendChild(list);
    })
}

key_1_new_cases();

}

function counts_new_cases_rates_tile_plot() {

function new_case_daily_plot(area_x_chosen, svg_x) {

area_x = daily_cases.filter(function(d) { // gets a subset of the json data
  return d.Name === area_x_chosen
  })

area_x_case_summary = case_summary.filter(function(d) { // gets a subset of the json data
  return d.Name === area_x_chosen
  })

var svg = d3.select('#new_cases_plotted')
  .append('svg')
  .attr('id', 'catch_me_svg')
  .attr('width', width_hm)
  .attr('height', height_hm)
  .append('g')

var tooltip_new_case_day = d3.select("#new_cases_plotted")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip_class")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style('font-size', '12px')
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("padding", "10px")

var mouseover = function(d) {
  d3.select(this)
    .style("stroke", "black")
    .style('stroke-width', '1px')
  }

var mousemove = function(d) {
  tooltip_new_case_day
    .html('<h4>' + d.Name + ' - ' + d.Period + '</h4><p>' + d.Case_label + '</p><p>' + d.Rate_label + '</p>')
    .style("top", (event.pageY - 10) + "px")
    .style("left", (event.pageX + 10) + "px")
    .style('opacity', 1)
    .style('visibility', 'visible')
  }

var mouseleave = function(d) {
  tooltip_new_case_day
  .style("opacity", 0)
  .style('visibility', 'hidden')

  d3.select(this)
    .style("stroke", "none")
  }

svg
  .append("text")
  .attr("x", 1)
  .attr("y", height_hm * .5)
  .text(area_x_chosen)
  .attr("text-anchor", "start")
  .attr('font-weight', function(d) {
    if (area_x_chosen === 'West Sussex') {
      return ('bold')
    } else {
      return ('normal')
    }
  })
  .style("font-size", "10px")

svg
  .append("text")
  .attr("x", 125)
  .attr("y", height_hm * .5)
  .text(function(d) {
    return d3.format(",.0f")(area_x_case_summary[0]['Total confirmed cases so far'])
  })
  .attr("text-anchor", "start")
  .attr('font-weight', function(d) {
    if (area_x_chosen === 'West Sussex') {
      return ('bold')
    } else {
      return ('normal')
    }
  })
  .style("font-size", "10px")

svg
  .append("text")
  .attr("x", 175)
  .attr("y", height_hm * .5)
  .text(function(d) {
      return d3.format(",.1f")(area_x_case_summary[0]['Total cases per 100,000 population'])
    })
  .attr("text-anchor", "start")
  .attr('font-weight', function(d) {
    if (area_x_chosen === 'West Sussex') {
      return ('bold')
    } else {
      return ('normal')
    }
  })
  .style("font-size", "10px")

svg
  .append("text")
  .attr("x", 250)
  .attr("y", height_hm * .5)
  .text(function(d) {
    return d3.format(",.0f")(area_x_case_summary[0]['Confirmed cases swabbed on most recent complete day'])
  })
  .attr("text-anchor", "start")
  .attr('font-weight', function(d) {
    if (area_x_chosen === 'West Sussex') {
    return ('bold')
  } else {
    return ('normal')
  }
  })
  .style("font-size", "10px")

svg
  .append("text")
  .attr("x", 335)
  .attr("y", height_hm * .5)
  .text(function(d) {
  if (isNaN(area_x_case_summary[0]['Total cases confirmed in most recent seven days'])) {
  return 'No cases*';
  }
  return d3.format(",.0f")(area_x_case_summary[0]['Total cases confirmed in most recent seven days']) })
  .attr("text-anchor", "start")
  .attr('font-weight', function(d) {
  if (area_x_chosen === 'West Sussex') {
  return ('bold')
  } else {
  return ('normal')
  }
})
  .style("font-size", "10px")

svg
  .append('line')
  .attr('x1', x(first_incomplete_date))
  .attr('y1', 0)
  .attr('x2', x(first_incomplete_date))
  .attr('y2', height_hm)
  .attr('stroke', incomplete_colour)
  .attr("stroke-dasharray", ("3, 3"))

svg
  .append('rect')
  .attr('x', x(first_incomplete_date))
  .attr('y1', 0)
  .attr('width', width_hm)
  .attr('height', height_hm_title)
  .style('fill', incomplete_colour)
  .style('stroke', 'none')
  .style('opacity', 0.2)

    svg
      .selectAll()
      .data(area_x)
      .enter()
      .append("rect")
      .attr("x", function(d) {
        return x(d.Date_label)
      })
      .attr("rx", 4)
      .attr("ry", 4)
      .attr("width", x.bandwidth())
      .attr("height", height_hm - 4)
      .style("fill", function(d) {
        return color_new_per_100000_cases(d.new_case_per_100000_key)
      })
      .style("stroke-width", 4)
      .style("stroke", "none")
      // .style("opacity", 0.8)
      .on("mouseover", mouseover)
      .on("mousemove", mousemove)
      .on("mouseleave", mouseleave)

    svg
      .append('line')
      .attr('x1', 0)
      .attr('y1', height_hm)
      .attr('x2', width_hm)
      .attr('y2', height_hm)
      .attr('stroke', '#c9c9c9')
  }

  new_case_daily_plot(order_areas[0])
  new_case_daily_plot(order_areas[1])
  new_case_daily_plot(order_areas[2])
  new_case_daily_plot(order_areas[3])
  new_case_daily_plot(order_areas[4])
  new_case_daily_plot(order_areas[5])
  new_case_daily_plot(order_areas[6])
  new_case_daily_plot(order_areas[7])

  function key_1_new_cases() {
    new_cases_per_100000_bands.forEach(function(item, index) {
      var list = document.createElement("li");
      list.innerHTML = item;
      list.className = 'key_list';
      list.style.borderColor = color_new_per_100000_cases(index);
      var tt = document.createElement('div');
      tt.className = 'side_tt';
      tt.style.borderColor = color_new_cases(index);
      var tt_h3_1 = document.createElement('h3');
      tt_h3_1.innerHTML = item.Cause;

      tt.appendChild(tt_h3_1);
      var div = document.getElementById("new_case_key_figure");
      div.appendChild(list);
    })
  }
  key_1_new_cases();
}

counts_new_cases_tile_plot()

function toggle_count_rate_func() {
  var type = document.getElementsByName('toggle_count_rate');
  if (type[0].checked) {
    console.log("We'll put the count version on for you")

    $('.key_list').remove();

    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();

    counts_new_cases_tile_plot()

    svg_title
      .selectAll("#what_am_i_showing_tiles")
      .remove();

    svg_title
      .append("text")
      .attr("x", start_tile_x_pos)
      .attr("y", 10)
      .attr('id', 'what_am_i_showing_tiles')
      .text('New cases by day')
      .attr("text-anchor", "start")
      .style('font-weight', 'bold')
      .style("font-size", "10px")

  } else if (type[1].checked) {
    console.log("We'll put the rate version on for you")

    $('.key_list').remove();

    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();
    $('#catch_me_svg').remove();

    counts_new_cases_rates_tile_plot()

    svg_title
      .selectAll("#what_am_i_showing_tiles")
      .remove();

    svg_title
      .append("text")
      .attr("x", start_tile_x_pos)
      .attr("y", 10)
      .attr('id', 'what_am_i_showing_tiles')
      .text('New cases per 100,000 population by day')
      .attr("text-anchor", "start")
      .style('font-weight', 'bold')
      .style("font-size", "10px")
  }
};
