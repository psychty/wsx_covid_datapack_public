
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/first_incomplete_daily_case.json", false);
request.send(null);
var incomplete_sm_date = JSON.parse(request.responseText)[0]

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/latest_daily_case.json", false);
request.send(null);
var latest_sm_date = JSON.parse(request.responseText)[0]

d3.select("#data_recency")
  .html(function(d) {
    return 'The latest available data in this analysis are for <b>' + latest_date + '</b>. However, whilst the capacity for testing and returning results has increased with new results reported in as soon as 24 hours, there can be some delay and as such data for very recent days are likely to change, and so <b>only data up to ' + complete_date + ' should be treated as complete.</b> In some parts of his data view, we do compare the most recent seven day period (including incomplete days) to the number of cases in the previous week to show how cases are changing. This may help to identify areas that are potentially increasing, and we would want to investigate this as soon as possible rather than wait for five days.' });

d3.select("#case_recency_text")
  .html(function(d) {
    return 'As noted above, the most recent four days are to be considered incomplete and may underestimate the true number of new cases. As such, the total number of confirmed COVID-19 cases in the most recent complete seven day period (seven days leading to ' + complete_date + ') as well as the number of cases in the seven days before that (up to ' + seven_days_date + ') are given on the figure to show changes over time.' });

var tooltip_area_sm = d3.select("#my_sm_dataviz")
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

  var showTooltip_sm1 = function(d) {
    tooltip_area_sm
    .html("<h4>" + d.Name + "</h4><p><b>" + d.Date_label + "</b></p><p>Current change status: <b>" + change_case_label(d.Colour_key) + "</b></p><p>" + d3.format(',.0f')(d.New_cases) + ' new cases (' + d3.format(',.1f')(d.New_cases_per_100000) + ' per 100,000 population)</p><p>' + d.Case_label + '</p>')
    .style("opacity", 1)
    .style("top", (event.pageY - 10) + "px")
    .style("left", (event.pageX + 10) + "px")
    .style("visibility", "visible")
  }

  var mouseleave_sm = function(d) {
  tooltip_area_sm
      .style("visibility", "hidden")
}

d3.select("#selected_ltla_sm_title")
  .html(function(d) {
    return 'Covid-19 pillar 1 and 2 confirmed new daily confirmed cases over time; areas within ' +  'West Sussex' + '; ' + first_date + ' - ' + latest_date});

// d3.select("#case_key_title")
//   .html(function(d) {
//     return 'The bars on each figure are also coloured to show the most recent changes in case growth, by comparing the average number of cases in the last complete seven day period (' + data_date_range[0]['range'] + ') with the average number of cases in the seven day period prior to that (' + data_date_range[1]['range'] + '). Areas with bars coloured in red indicate that there is an increase in cases, whilst blue areas show a decline in confirmed cases and green areas indicate no confirmed cases in the most recent 7 day period (excluding incomplete days - see right).'});

chosen_utla_df = daily_cases.filter(function(d) { // gets a subset of the json data
  return d.Name !== 'West Sussex'
  })

var areas_for_sm_1 = d3.nest()
  .key(function(d) { return d.Name;})
  .entries(chosen_utla_df);

var small_areas = areas_for_sm_1.map(function(d){return d.key})
var date_points_n = d3.map(chosen_utla_df, function(d){return(d.Date_label_2)}).keys()

var x_sm_1 = d3.scaleBand()
   .domain(date_points_n)
   .range([0, width_sm - 70]);

//Add Y axis
var y_sm_1 = d3.scaleLinear()
    .domain([0, d3.max(chosen_utla_df, function(d) { return +d.New_cases; })])
    .range([height_sm, 0 ])
    .nice();

// Add an svg element for each group. The will be one beside each other and will go on the next row when no more room available
var sm_svg_1 = d3.select("#my_sm_dataviz")
   .selectAll("small_multiples")
   .data(areas_for_sm_1)
   .enter()
   .append("svg")
   .attr("width", width_sm)
   .attr("height", height_sm + 70)
   .append("g")
   .attr("transform", "translate(" + 50 + "," + 20 + ")");

// Add axes
 sm_svg_1_y_axis = sm_svg_1
   .append("g")
   .call(d3.axisLeft(y_sm_1).ticks(6));

 sm_svg_1_x_axis = sm_svg_1
   .append("g")
   .attr("transform", "translate(0," + height_sm + ")")
   .call(d3.axisBottom(x_sm_1).tickValues(data_dates));

sm_svg_1_x_axis
  .selectAll("text")
  .attr("transform", 'translate(-10,10)rotate(-90)')
  .style("text-anchor", "end")
  // .each(function(d,i) {
    // if (i%2 != 0) d3.select(this).remove();
    // });

// Accessing nested data: https://groups.google.com/forum/#!topic/d3-js/kummm9mS4EA
// data(function(d) {return d.values;}) will dereference the values for nested data for each group
sm_svg_1_bars = sm_svg_1.selectAll(".bar")
      .data(function(d) {return d.values;})
      .enter()
      .append("rect")
      .attr("class", "bar")
      .attr("x", function(d) { return x_sm_1(d.Date_label_2); })
      .attr("width", x_sm_1.bandwidth())
      .attr("y", function(d) { return y_sm_1(d.New_cases); })
      .attr("height", function(d) { return height_sm - y_sm_1(d.New_cases); })
      // .attr("fill", function(d) {return case_change_colour(d.Colour_key)})
.attr("fill", function(d) { return '#071b7c'})
.style('opacity', .75)
  .on("mousemove", showTooltip_sm1)
  .on('mouseout', mouseleave_sm);

 // Draw the line
sm_svg_1_lines = sm_svg_1
  .append("path")
  .attr("fill", "none")
  .attr("stroke", '#000000')
  .attr("stroke-width", 1.9)
  .attr("d", function(d){
    return d3.line()
      .defined(d => !isNaN(d.Seven_day_average_new_cases))
      .x(function(d) { return x_sm_1(d.Date_label_2); })
      .y(function(d) { return y_sm_1(+d.Seven_day_average_new_cases); })
      (d.values)
      })
  .style('opacity', 0)
  .transition()
  .duration(500)
  .style('opacity', 1)

// Add plot headings
sm_svg_1_titles = sm_svg_1
  .append("text")
  .attr("text-anchor", "start")
  .attr("y", 10)
  .attr("x", 10)
  .text(function(d){ return(d.key)})
  .style('fill', '#000000')

sm_svg_1
  .append("text")
  .attr("text-anchor", "start")
  .attr("y", 25)
  .attr("x", 10)
  .text(function(d){ return(d3.format(',.0f')(d.values[date_points_n.length - 6]['Rolling_7_day_new_cases']) + ' cases in last 7 complete days')})
  .style("font-size", ".7rem")
  .style('fill', '#383838')

sm_svg_1
  .append("text")
  .attr("text-anchor", "start")
  .attr("y", 40)
  .attr("x", 10)
  .text(function(d){ return(d3.format(',.0f')(d.values[date_points_n.length - 13]['Rolling_7_day_new_cases']) + ' cases in previous 7 days')})
  .style("font-size", ".7rem")
  .style('fill', '#383838')

sm_svg_1
  .append('line')
  .attr('x1', x_sm_1(incomplete_sm_date))
  .attr('y1', 0)
  .attr('x2', x_sm_1(incomplete_sm_date))
  .attr('y2', height_sm)
  .attr('stroke', incomplete_colour)
  .attr("stroke-dasharray", ("3, 3"))

sm_svg_1
  .append('rect')
  .attr('x', x_sm_1(incomplete_sm_date))
  .attr('y1', 0)
  .attr('width', (x_sm_1(latest_sm_date) + x_sm_1.bandwidth()) - x_sm_1(incomplete_sm_date))
  .attr('height', height_sm)
  .style('fill', incomplete_colour)
  .style('stroke', 'none')
  .style('opacity', 0.2)


function update_ltla_sm(chosen_utla_area){
chosen_utla_df = daily_cases.filter(function(d) { // gets a subset of the json data
  return d.Name !== 'West Sussex'
  })

var areas_for_sm_1 = d3.nest()
  .key(function(d) { return d.Name;})
  .entries(chosen_utla_df);

// Which areas are present
small_areas = areas_for_sm_1.map(function(d){return d.key})

d3.selectAll('.sm-container svg').remove();

sm_svg_1 = d3.select("#my_sm_dataviz")
   .selectAll("small_multiples")
   .data(areas_for_sm_1)
   .enter()
   .append("svg")
   .attr("width", width_sm)
   .attr("height", height_sm + 70)
   .append("g")
   .attr("transform", "translate(" + 50 + "," + 20 + ")");

var type_sm_scale = document.getElementsByName('toggle_sm_rate');

var areas_for_sm_1 = d3.nest()
  .key(function(d) { return d.Name;})
  .entries(chosen_utla_df);

// Which areas are present
small_areas = areas_for_sm_1.map(function(d){return d.key})

  if (type_sm_scale[0].checked) {
console.log('User selected actual numbers')

//Add Y axis
var y_sm_1 = d3.scaleLinear()
    .domain([0, d3.max(chosen_utla_df, function(d) { return +d.New_cases; })])
    .range([height_sm, 0 ])
    .nice();

 sm_svg_1_y_axis = sm_svg_1
   .append("g")
   .call(d3.axisLeft(y_sm_1).ticks(6));

 sm_svg_1_x_axis = sm_svg_1
   .append("g")
   .attr("transform", "translate(0," + height_sm + ")")
   .call(d3.axisBottom(x_sm_1).tickValues(data_dates));

sm_svg_1_x_axis
  .selectAll("text")
  .attr("transform", 'translate(-10,10)rotate(-90)')
  .style("text-anchor", "end")

sm_svg_1_bars = sm_svg_1.selectAll(".bar")
      .data(function(d) {return d.values;})
      .enter()
      .append("rect")
      .attr("class", "bar")
      .attr("x", function(d) { return x_sm_1(d.Date_label_2); })
      .attr("width", x_sm_1.bandwidth())
      .attr("y", function(d) { return y_sm_1(d.New_cases); })
      .attr("height", function(d) { return height_sm - y_sm_1(d.New_cases); })
      // .attr("fill", function(d) {return case_change_colour(d.Colour_key)})
.attr("fill", function(d) { return '#071b7c'})
.style('opacity', .75)
.on("mousemove", showTooltip_sm1)
.on('mouseout', mouseleave_sm);

sm_svg_1_lines = sm_svg_1
  .append("path")
  .attr("fill", "none")
  .attr("stroke", '#000000')
  .attr("stroke-width", 1.9)
  .attr("d", function(d){
    return d3.line()
      .defined(d => !isNaN(d.Seven_day_average_new_cases))
      .x(function(d) { return x_sm_1(d.Date_label_2); })
      .y(function(d) { return y_sm_1(+d.Seven_day_average_new_cases); })
      (d.values)
      })
  .style('opacity', 0)
  .transition()
  .duration(500)
  .style('opacity', 1)

sm_svg_1_titles = sm_svg_1
  .append("text")
  .attr("text-anchor", "start")
  .attr("y", 10)
  .attr("x", 10)
  .text(function(d){ return(d.key)})
  .style('fill', '#000000')

sm_svg_1
  .append("text")
  .attr("text-anchor", "start")
  .attr("y", 25)
  .attr("x", 10)
  .text(function(d){ return(d3.format(',.0f')(d.values[date_points_n.length - 6]['Rolling_7_day_new_cases']) + ' cases in last 7 complete days')})
  .style("font-size", ".7rem")
  .style('fill', '#383838')

sm_svg_1
  .append("text")
  .attr("text-anchor", "start")
  .attr("y", 40)
  .attr("x", 10)
  .text(function(d){ return(d3.format(',.0f')(d.values[date_points_n.length - 13]['Rolling_7_day_new_cases']) + ' cases in previous 7 days')})
  .style("font-size", ".7rem")
  .style('fill', '#383838')

sm_svg_1
  .append("text")
  .attr("y", 10)
  .attr("x", width_sm - 80)
  .attr("text-anchor", "end")
  .text('Showing actual cases')
  .style("font-size", ".7rem")
  .style('fill', 'red')

sm_svg_1
  .append('line')
  .attr('x1', x_sm_1(incomplete_sm_date))
  .attr('y1', 0)
  .attr('x2', x_sm_1(incomplete_sm_date))
  .attr('y2', height_sm)
  .attr('stroke', incomplete_colour)
  .attr("stroke-dasharray", ("3, 3"))

sm_svg_1
  .append('rect')
  .attr('x', x_sm_1(incomplete_sm_date))
  .attr('y1', 0)
  .attr('width', (x_sm_1(latest_sm_date) + x_sm_1.bandwidth()) - x_sm_1(incomplete_sm_date))
  .attr('height', height_sm)
  .style('fill', incomplete_colour)
  .style('stroke', 'none')
  .style('opacity', 0.2)

} else if (type_sm_scale[1].checked) {

console.log('User selected rate per 100,000')

//Add Y axis
var y_sm_1 = d3.scaleLinear()
    .domain([0, d3.max(chosen_utla_df, function(d) { return +d.New_cases_per_100000; })])
    .range([height_sm, 0 ])
    .nice();

 sm_svg_1_y_axis = sm_svg_1
   .append("g")
   .call(d3.axisLeft(y_sm_1).ticks(6));

 sm_svg_1_x_axis = sm_svg_1
   .append("g")
   .attr("transform", "translate(0," + height_sm + ")")
   .call(d3.axisBottom(x_sm_1).tickValues(data_dates));

sm_svg_1_x_axis
  .selectAll("text")
  .attr("transform", 'translate(-10,10)rotate(-90)')
  .style("text-anchor", "end")

sm_svg_1_bars = sm_svg_1.selectAll(".bar")
.data(function(d) {return d.values;})
.enter()
.append("rect")
.attr("class", "bar")
.attr("x", function(d) { return x_sm_1(d.Date_label_2); })
.attr("width", x_sm_1.bandwidth())
.attr("y", function(d) { return y_sm_1(d.New_cases_per_100000); })
.attr("height", function(d) { return height_sm - y_sm_1(d.New_cases_per_100000); })
      // .attr("fill", function(d) {return case_change_colour(d.Colour_key)})
.attr("fill", function(d) { return '#071b7c'})
.style('opacity', .75)
.on("mousemove", showTooltip_sm1)
.on('mouseout', mouseleave_sm);

sm_svg_1_titles = sm_svg_1
  .append("text")
  .attr("text-anchor", "start")
  .attr("y", 10)
  .attr("x", 10)
  .text(function(d){ return(d.key)})
  .style('fill', '#000000')

sm_svg_1
  .append("text")
  .attr("text-anchor", "start")
  .attr("y", 25)
  .attr("x", 10)
  .text(function(d){ return(d3.format(',.1f')(d.values[date_points_n.length - 6]['Rolling_7_day_new_cases_per_100000']) + ' cases per 100,000 in last 7 complete days')})
  .style("font-size", ".7rem")
  .style('fill', '#383838')

sm_svg_1
  .append("text")
  .attr("text-anchor", "start")
  .attr("y", 40)
  .attr("x", 10)
  .text(function(d){ return(d3.format(',.1f')(d.values[date_points_n.length - 13]['Rolling_7_day_new_cases_per_100000']) + ' cases per 100,000 in previous 7 days')})
  .style("font-size", ".7rem")
  .style('fill', '#383838')

sm_svg_1
  .append("text")
  .attr("y", 10)
  .attr("x", width_sm - 80)
  .attr("text-anchor", "end")
  .text('Showing rate per 100,000')
  .style("font-size", ".7rem")
  .style('fill', 'red')

// sm_svg_1
//   .append("text")
//   .attr("y", 25)
//   .attr("x", width_sm - 80)
//   .attr("text-anchor", "end")
//   .text('per 100,000')
//   .style("font-size", ".7rem")
//   .style('fill', 'red')

sm_svg_1
  .append('line')
  .attr('x1', x_sm_1(incomplete_sm_date))
  .attr('y1', 0)
  .attr('x2', x_sm_1(incomplete_sm_date))
  .attr('y2', height_sm)
  .attr('stroke', incomplete_colour)
  .attr("stroke-dasharray", ("3, 3"))

sm_svg_1
  .append('rect')
  .attr('x', x_sm_1(incomplete_sm_date))
  .attr('y1', 0)
  .attr('width', (x_sm_1(latest_sm_date) + x_sm_1.bandwidth()) - x_sm_1(incomplete_sm_date))
  .attr('height', height_sm)
  .style('fill', incomplete_colour)
  .style('stroke', 'none')
  .style('opacity', 0.2)

}};

  // function key_2_sm_cases() {
  //   case_change_values.forEach(function(item, index) {
  //     var list = document.createElement("li");
  //     list.innerHTML = item;
  //     list.className = 'key_list';
  //     list.style.borderColor = case_change_colour(index);
  //     var tt = document.createElement('div');
  //     tt.className = 'side_tt';
  //     tt.style.borderColor = case_change_colour(index);
  //     var tt_h3_1 = document.createElement('h3');
  //     tt_h3_1.innerHTML = item.Cause;
  //
  //     tt.appendChild(tt_h3_1);
  //     var div = document.getElementById("ltla_sm_key_figure");
  //     div.appendChild(list);
  //   })
  // }
  //
  // key_2_sm_cases();

update_ltla_sm()

function toggle_sm_rate_func() {
update_ltla_sm()
}
