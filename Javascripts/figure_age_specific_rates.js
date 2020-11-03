
var areas_age_spec = ['West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'South East', 'England']

var svg_age_spec_1 = d3.select("#age_spec_fig_1")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .append("g")
  .attr("transform", "translate(" + 50 + "," + 20 + ")");

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_lines_age_1_area_button")
  .selectAll('myOptions')
  .data(areas_age_spec)
  .enter()
  .append('option')
  .text(function(d) {
    return d;
  })
  .attr("value", function(d) {
    return d;
  })

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/age_specific_rates_10_years_by_date.json", false);
request.send(null);
var daily_asr_cases = JSON.parse(request.responseText); // parse the fetched json data into a variable

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/age_specific_rate_dates.json", false);
request.send(null);
var dates_asr_ticks = JSON.parse(request.responseText).map(function(d){return d.Date_label});

// Retrieve the selected area name
var selected_age_line_1_area_option = d3.select('#select_lines_age_1_area_button').property("value")

// Update text based on selected area
d3.select("#selected_age_line_1_compare_title")
  .html(function(d) {
    return 'Covid-19 pillar 1 and 2 confirmed age-specific rolling 7 day cases over time; ' + selected_age_line_1_area_option + '; as at ' + data_refreshed_date
  });

var age_spec_1_chosen = daily_asr_cases.filter(function(d) {
  return d.Name === selected_age_line_1_area_option
});

var dates_asr = d3.map(age_spec_1_chosen, function(d) {
    return (d.Date_label)
  })
  .keys()

var chosen_dates_asr_ticks = dates_asr_ticks.filter(function(el) {
  return dates_asr.includes(el);
});

// Group the data: I want to draw one line per group
var age_spec_1_chosen_ts_group = d3.nest() // nest function allows to group the calculation per level of a factor
.key(function(d) { return d.Age;})
.entries(age_spec_1_chosen);

// This will be the x axis scale for days since case x - note this is NOT the date as in previous timeseries. We have used this to compare each area over the same time period.
var x_asr_1 = d3.scaleBand()
  .domain(dates_asr)
  .range([0, width_hm - 60]);

var xAxis_asr_1 = svg_age_spec_1
  .append("g")
  .attr("transform", 'translate(0,' + (height_line - 80 ) + ")")
  .call(d3.axisBottom(x_asr_1).tickValues(chosen_dates_asr_ticks));

xAxis_asr_1
  .selectAll("text")
  .attr("transform", 'translate(-' + (x_asr_1.bandwidth() + 10) + ',10)rotate(-90)')
  .style("text-anchor", "end")
  .style("font-size", "10px")

y_asr_1_ts = d3.scaleLinear()
  .domain([0, d3.max(age_spec_1_chosen, function(d) {
    return +d.ASR;
  })])
  .range([height_line - 80, 0])
  .nice();

var y_asr_1_ts_axis = svg_age_spec_1
  .append("g")
  .attr("transform", 'translate(0,0)')
  .call(d3.axisLeft(y_asr_1_ts).tickFormat(d3.format(',.0f')));

var tooltip_asr_1_dot = d3.select("#age_spec_fig_1")
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

var Area_age_colours = d3.scaleOrdinal()
  .domain(['0-9 years', '10-19 years', '20-29 years', '30-39 years', '40-49 years', '50-59 years', '60-69 years', '70-79 years', '80 and over'])
  .range(['#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6']);

var showTooltip_asr_1 = function(d){

tooltip_asr_1_dot
  .html("<h4>" + d.Age + ' - '+ d.Name + '</h4><p class = "side">The number of cases among those aged ' + d.Age + ' in the seven days to ' + d.Date_label + ' is <b>' + d3.format(',.0f')(d.Rolling_7_day_new_cases) + '</b>. This is '  + d3.format(',.0f')(d.ASR) + ' per 100,000 population.</p><p class = "side">So far, there have been a total of ' + d3.format(',.0f')(d.Cumulative_cases) + ' cases among this age group in ' + d.Name + '.</p>')
  .style("opacity", 1)
  .style("top", (event.pageY - 10) + "px")
  .style("left", (event.pageX + 10) + "px")
  .style("visibility", "visible");

}

var mouseleave_asr_1 = function(d) {

tooltip_asr_1_dot
  .style("visibility", "hidden")

  }

var lines_asr_1 = svg_age_spec_1
.selectAll(".line")
.data(age_spec_1_chosen_ts_group)
.enter()
.append("path")
.attr('id', 'c3_lines')
.attr('class', 'c3_all_lines')
.attr("d", function(d){
    return d3.line()
  .x(function(d) { return x_asr_1(d.Date_label); })
  .y(function(d) { return y_asr_1_ts(+d.ASR); })
  (d.values)})
.style("fill", "none")
.style("stroke-width", 1.5)
.attr('stroke', function(d){ return Area_age_colours(d.Key)});

var dots_asr_1 = svg_age_spec_1
  .selectAll('myCircles')
  .data(age_spec_1_chosen)
  .enter()
  .append("circle")
  .attr("cx", function(d) { return x_asr_1(d.Date_label)})
  .attr("cy", function(d) { return y_asr_1_ts(+d.ASR)})
  .attr("r", 1.5)
  .style("fill", function(d) { return Area_age_colours(d.Age)})
  .attr("stroke", function(d) { return Area_age_colours(d.Age)})
  .on("mousemove", showTooltip_asr_1)
  .on('mouseout', mouseleave_asr_1);


function update_asr_1() {

// Retrieve the selected area name
var selected_age_line_1_area_option = d3.select('#select_lines_age_1_area_button').property("value")

// Update text based on selected area
d3.select("#selected_age_line_1_compare_title")
  .html(function(d) {
    return 'Covid-19 pillar 1 and 2 confirmed age-specific rolling 7 day cases over time; ' + selected_age_line_1_area_option + '; as at ' + data_refreshed_date
  });

var age_spec_1_chosen = daily_asr_cases.filter(function(d) {
  return d.Name === selected_age_line_1_area_option
});

var dates_asr = d3.map(age_spec_1_chosen, function(d) {
    return (d.Date_label)
  })
  .keys()

var chosen_dates_asr_ticks = dates_asr_ticks.filter(function(el) {
  return dates_asr.includes(el);
});

// Group the data: I want to draw one line per group
var age_spec_1_chosen_ts_group = d3.nest() // nest function allows to group the calculation per level of a factor
.key(function(d) { return d.Age;})
.entries(age_spec_1_chosen);

x_asr_1
  .domain(dates_asr);

xAxis_asr_1
.transition()
.duration(500)
  .call(d3.axisBottom(x_asr_1).tickValues(chosen_dates_asr_ticks));

xAxis_asr_1
  .selectAll("text")
  .attr("transform", 'translate(-' + (x_asr_1.bandwidth() + 10) + ',10)rotate(-90)')
  .style("text-anchor", "end")
  .style("font-size", "10px")

y_asr_1_ts
  .domain([0, d3.max(age_spec_1_chosen, function(d) {
    return +d.ASR;
  })])
  .nice();

y_asr_1_ts_axis.transition()
.duration(1000)
.call(d3.axisLeft(y_asr_1_ts));

dots_asr_1
  .data(age_spec_1_chosen)
  .transition()
  .duration(1000)
  .attr("cx", function(d) { return x_asr_1(d.Date_label)})
  .attr("cy", function(d) { return y_asr_1_ts(+d.ASR)});

lines_asr_1
.data(age_spec_1_chosen_ts_group)
  .transition()
  .duration(1000)
.attr("d", function(d){
    return d3.line()
  .x(function(d) { return x_asr_1(d.Date_label); })
  .y(function(d) { return y_asr_1_ts(+d.ASR); })
  (d.values)});

}

d3.select("#select_lines_age_1_area_button").on("change", function(d) {
  var selected_age_line_1_area_option = d3.select('#select_lines_age_1_area_button').property("value")
  update_asr_1()
})
