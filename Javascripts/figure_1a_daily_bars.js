
// Daily cases bar chart
var svg_daily_new_case_bars = d3.select("#daily_new_case_bars")
.append("svg")
.attr("width", width_hm)
.attr("height", height_line)
.append("g")
.attr("transform", "translate(" + 60 + "," + 20 + ")");

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_bars_daily_cases_1_area_button")
  .selectAll('myOptions')
  .data(areas)
  .enter()
  .append('option')
  .text(function(d) {
    return d;
  })
  .attr("value", function(d) {
    return d;
  })

// Retrieve the selected area name
var selected_figure_1a_area_option = d3.select('#select_bars_daily_cases_1_area_button').property("value")

// Update text based on selected area
d3.select("#selected_daily_cases_bars_1_compare_title")
  .html(function(d) {
    return 'Covid-19 pillar 1 and 2 confirmed new daily confirmed cases over time; ' + selected_figure_1a_area_option
  });

var bars_daily_cases_1_chosen = daily_cases.filter(function(d) {
  return d.Name === selected_figure_1a_area_option
});

var total_cases_daily_chosen = case_summary.filter(function(d){
  return d.Name === selected_figure_1a_area_option})[0]['Cumulative_cases']

var x_daily_cases = d3.scaleBand()
    .domain(bars_daily_cases_1_chosen.map(function(d) {
      return d.Date_label_2}))
    .range([0, width_hm - 60])

var xAxis_daily_cases = svg_daily_new_case_bars
  .append("g")
  .attr("transform", 'translate(0,' + (height_line - 120 ) + ")")
  .call(d3.axisBottom(x_daily_cases));

xAxis_daily_cases
  .selectAll("text")
  .attr("transform", 'translate(0,10)rotate(-90)')
  .style("text-anchor", "end")
  .each(function(d,i) { // find the text in that tick and remove it: Thanks Gerardo Furtado on stackoverflow
    if (i%2 == 0) d3.select(this).remove();
    });

var y_daily_cases = d3.scaleLinear()
  .domain([0, d3.max(bars_daily_cases_1_chosen, function(d) {
    return +d.New_cases;
  })])
  .range([height_line - 120 , 0])
  .nice();

var yAxis_daily_cases = svg_daily_new_case_bars
.append("g")
.call(d3.axisLeft(y_daily_cases));

// testing policy timelines
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/uk_testing_key_dates.json", false);
request.send(null);
var uk_testing_key_dates = JSON.parse(request.responseText);

var tooltip_testing_key_dates = d3.select("#daily_new_case_bars")
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

var showTooltip_testing_key_dates = function(d) {
  tooltip_testing_key_dates
    .html("<h5>" + d.Date_label_2 + '</h5><p>On this date it was announced that ' + d.Change + ' would now be included in the UK Covid-19 swab testing eligible population.</p>')
    .style("opacity", 1)
    .style("top", (event.pageY - 10) + "px")
    .style("left", (event.pageX + 10) + "px")
    .style('opacity', 1)
    .style("visibility", "visible")
}

var mouseleave_testing_key_dates = function(d) {
  tooltip_testing_key_dates
    .style('opacity', 0)
    .style("visibility", "hidden")
}

// We can treat this as a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_daily_new_case_bars
.selectAll("testing_timeline")
.data(uk_testing_key_dates)
.enter()
.append("rect")
.attr("x", function(d) { return x_daily_cases(d.Date_label_2) + (x_daily_cases.bandwidth()/2)})
.attr("y", 0)
.attr("width", 2)
.attr("height", function(d) { return (height_line - 120 )})
.style("fill", incomplete_colour)
.on("mousemove", showTooltip_testing_key_dates)
.on('mouseout', mouseleave_testing_key_dates);

// We can treat this like a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_daily_new_case_bars
.selectAll("testing_timeline")
.data(uk_testing_key_dates)
.enter()
.append("circle")
.attr("cx", function(d) { return x_daily_cases(d.Date_label_2) + (x_daily_cases.bandwidth()/2)})
.attr("y", 0)
.attr("r", 6)
.style("fill", incomplete_colour)
.on("mousemove", showTooltip_testing_key_dates)
.on('mouseout', mouseleave_testing_key_dates);

svg_daily_new_case_bars
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_daily_cases('26 Mar') + (x_daily_cases.bandwidth()/2)})
  .attr("y", 2)
  .text('Testing eligibility changes -')
  .attr("text-anchor", "end")

var tooltip_daily_case_1 = d3.select("#daily_new_case_bars")
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

var showTooltip_daily_case_1 = function(d) {
  tooltip_daily_case_1
    .html("<h5>" + d.Name + '</h5><p><b>' + d.Date_label + '</b></p><p class = "side">In ' + d.Name + ' there were <b>' + d3.format(',.0f')(d.New_cases) + ' </b>specimens taken on this date which resulted in a positive result for Covid-19.</p><p class = "side">The new cases swabbed on this day represent ' + d3.format('0.1%')(d.New_cases / total_cases_daily_chosen) + ' of the total number of cases confirmed so far (' + d3.format(',.0f')(d.Cumulative_cases) + ') </p><p class = "side">' + d.Seven_day_ave_new_label + '</p>')
    .style("opacity", 1)
    .style("top", (event.pageY - 10) + "px")
    .style("left", (event.pageX + 10) + "px")
    .style('opacity', 1)
    .style("visibility", "visible")
}

var mouseleave_daily_case_1 = function(d) {
  tooltip_daily_case_1
    .style('opacity', 0)
    .style("visibility", "hidden")
}

svg_daily_new_case_bars
  .append('line')
  .attr('x1', x_daily_cases('23 Mar') + (x_daily_cases.bandwidth() / 2))
  .attr('y1', 30)
  .attr('x2', x_daily_cases('23 Mar') + (x_daily_cases.bandwidth() / 2))
  .attr('y2', height_line - 120 )
  .attr('stroke', 'red')
  .attr("stroke-dasharray", ("3, 3"))

svg_daily_new_case_bars
  .append("text")
  .attr("x", x_daily_cases('23 Mar'))
  .attr("y", 31)
  .text('lockdown starts -')
  .attr("text-anchor", "end")

svg_daily_new_case_bars
  .append('line')
  .attr('x1', x_daily_cases('13 May') + (x_daily_cases.bandwidth() / 2))
  .attr('y1', 30)
  .attr('x2', x_daily_cases('13 May') + (x_daily_cases.bandwidth() / 2))
  .attr('y2', height_line - 120 )
  .attr('stroke', 'red')
  .attr("stroke-dasharray", ("3, 3"))

// svg_daily_new_case_bars
//   .append("text")
//   .attr("x", x_daily_cases('13 May'))
//   .attr("y", 22)
//   .text('more people')
//   .attr("text-anchor", "end")
//
// svg_daily_new_case_bars
//   .append("text")
//   .attr("x", x_daily_cases('13 May'))
//   .attr("y", 31)
//   .text('return to work -')
//   .attr("text-anchor", "end")



svg_daily_new_case_bars
  .append('line')
  .attr('x1', x_daily_cases(incomplete_sm_date))
  .attr('y1', 0)
  .attr('x2', x_daily_cases(incomplete_sm_date))
  .attr('y2', height_line - 120 )
  .attr('stroke', incomplete_colour)
  .attr("stroke-dasharray", ("3, 3"))

svg_daily_new_case_bars
  .append('rect')
  .attr('x', x_daily_cases(incomplete_sm_date))
  .attr('y1', 0)
  .attr('width', (x_daily_cases(latest_sm_date) + x_daily_cases.bandwidth()) - x_daily_cases(incomplete_sm_date))
  .attr('height', height_line - 120 )
  .style('fill', incomplete_colour)
  .style('stroke', 'none')
  .style('opacity', 0.2)

// daily case bars
var daily_new_case_bars = svg_daily_new_case_bars
.selectAll("mybar")
.data(bars_daily_cases_1_chosen)
.enter()
.append("rect")
.attr("x", function(d) { return x_daily_cases(d.Date_label_2)})
.attr("y", function(d) { return y_daily_cases(d.New_cases); })
.attr("width", x_daily_cases.bandwidth())
.attr("height", function(d) { return (height_line - 120 ) - y_daily_cases(d.New_cases); })
.style("fill", function(d) { return '#071b7c'})
.on("mousemove", showTooltip_daily_case_1)
.on('mouseout', mouseleave_daily_case_1);

var svg_daily_average_case_bars = svg_daily_new_case_bars
.append("path")
.datum(bars_daily_cases_1_chosen)
.attr("fill", "none")
.attr("stroke", "#000000")
.attr("stroke-width", 2)
.attr("d", d3.line()
.defined(d => !isNaN(d.Seven_day_average_new_cases))
.x(function(d) { return x_daily_cases(d.Date_label_2) + (x_daily_cases.bandwidth() /2)})
.y(function(d) { return y_daily_cases(d.Seven_day_average_new_cases) }))

svg_daily_new_case_bars
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_daily_cases('31 Jan') + (x_daily_cases.bandwidth()/2)})
  .attr("y", function(d) { return height_line - 150 })
  .text('The black line represents seven day average new cases')
  .attr("text-anchor", "start")

svg_daily_new_case_bars
  .append("text")
  .attr("x", x_daily_cases('13 May'))
  .attr("y", 22)
  .text('more people')
  .attr("text-anchor", "end")

svg_daily_new_case_bars
  .append("text")
  .attr("x", x_daily_cases('13 May'))
  .attr("y", 31)
  .text('return to work -')
  .attr("text-anchor", "end")

function update_daily_bars() {

var selected_figure_1a_area_option = d3.select('#select_bars_daily_cases_1_area_button').property("value")

d3.select("#selected_daily_cases_bars_1_compare_title")
  .html(function(d) {
    return 'Covid-19 pillar 1 and 2 confirmed new daily confirmed cases over time; ' + selected_figure_1a_area_option
  });

var bars_daily_cases_1_chosen = daily_cases.filter(function(d) {
  return d.Name === selected_figure_1a_area_option
});

var total_cases_daily_chosen = case_summary.filter(function(d){
  return d.Name === selected_figure_1a_area_option})[0]['Cumulative_cases']

var showTooltip_daily_case_1 = function(d) {
  tooltip_daily_case_1
    .html("<h5>" + d.Name + '</h5><p><b>' + d.Date_label + '</b></p><p class = "side">In ' + d.Name + ' there were <b>' + d3.format(',.0f')(d.New_cases) + ' </b>specimens taken on this date which resulted in a positive result for Covid-19.</p><p class = "side">The new cases swabbed on this day represent ' + d3.format('0.1%')(d.New_cases / total_cases_daily_chosen) + ' of the total number of cases confirmed so far (' + d3.format(',.0f')(total_cases_daily_chosen) + ') </p><p class = "side">' + d.Seven_day_ave_new_label + '</p>')
    .style("opacity", 1)
    .style("top", (event.pageY - 10) + "px")
    .style("left", (event.pageX + 10) + "px")
    .style('opacity', 1)
    .style("visibility", "visible")
}

y_daily_cases
  .domain([0, d3.max(bars_daily_cases_1_chosen, function(d) {
    return +d.New_cases;
  })])
  .nice();

// Redraw axis
yAxis_daily_cases
  .transition()
  .duration(1000)
  .call(d3.axisLeft(y_daily_cases));

svg_daily_average_case_bars
  .datum(bars_daily_cases_1_chosen)
  .transition()
  .duration(1000)
  .attr("d", d3.line()
.defined(d => !isNaN(d.Seven_day_average_new_cases))
.x(function(d) { return x_daily_cases(d.Date_label_2) + (x_daily_cases.bandwidth() /2)})
.y(function(d) { return y_daily_cases(d.Seven_day_average_new_cases) }))

daily_new_case_bars
.data(bars_daily_cases_1_chosen)
.transition()
.duration(1000)
.attr("x", function(d) { return x_daily_cases(d.Date_label_2)})
.attr("y", function(d) { return y_daily_cases(d.New_cases); })
// .attr("width", x_daily_cases.bandwidth())
.attr("height", function(d) { return (height_line - 120 ) - y_daily_cases(d.New_cases); })
.style("fill", function(d) { return '#071b7c' })

daily_new_case_bars
  .on("mousemove", showTooltip_daily_case_1)
  .on('mouseout', mouseleave_daily_case_1);

}

d3.select("#select_bars_daily_cases_1_area_button").on("change", function(d) {
  var selected_figure_1a_area_option = d3.select('#select_bars_daily_cases_1_area_button').property("value")
  update_daily_bars()
})
