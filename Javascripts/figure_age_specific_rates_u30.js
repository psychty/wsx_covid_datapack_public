
var areas_age_spec = ['West Sussex', 'South East', 'England', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing']

var svg_age_spec_u30 = d3.select("#age_spec_fig_u30")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .append("g")
  .attr("transform", "translate(" + 50 + "," + 20 + ")");

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_lines_age_2_area_button")
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
request.open("GET", "./Outputs/age_specific_rates_u30_by_date.json", false);
request.send(null);
var daily_u30_asr_cases = JSON.parse(request.responseText); // parse the fetched json data into a variable

// var request = new XMLHttpRequest();
// request.open("GET", "./Outputs/age_specific_rate_dates.json", false);
// request.send(null);
// var dates_u30_asr_ticks = JSON.parse(request.responseText).map(function(d){return d.Date_label});

// Retrieve the selected area name
var selected_age_line_2_area_option = d3.select('#select_lines_age_2_area_button').property("value")

// Update text based on selected area
d3.select("#selected_age_line_2_compare_title")
  .html(function(d) {
    return 'Covid-19 pillar 1 and 2 confirmed age-specific rolling 7 day case rates (per 100,000 population) over time among under 30s up to ' + complete_date + '; ' + selected_age_line_2_area_option + '; as at ' + data_refreshed_date
  });

var age_spec_u30_chosen = daily_u30_asr_cases.filter(function(d) {
  return d.Name === selected_age_line_2_area_option
});

var dates_u30_asr = d3.map(age_spec_u30_chosen, function(d) {
    return (d.Date_label)
  })
  .keys()

var chosen_dates_u30_asr_ticks = dates_asr_ticks.filter(function(el) {
  return dates_u30_asr.includes(el);
});

// Group the data
var age_spec_u30_chosen_ts_group = d3.nest() // nest function allows to group the calculation per level of a factor
.key(function(d) { return d.Age;})
.entries(age_spec_u30_chosen);

var x_asr_u30 = d3.scaleBand()
  .domain(dates_u30_asr)
  .range([0, width_hm - 60]);

var xAxis_asr_u30 = svg_age_spec_u30
  .append("g")
  .attr("transform", 'translate(0,' + (height_line - 80 ) + ")")
  .call(d3.axisBottom(x_asr_u30).tickValues(chosen_dates_u30_asr_ticks));

xAxis_asr_u30
  .selectAll("text")
  .attr("transform", 'translate(-' + (x_asr_u30.bandwidth() + 10) + ',10)rotate(-90)')
  .style("text-anchor", "end")
  .style("font-size", "10px")

y_asr_u30_ts = d3.scaleLinear()
  .domain([0, d3.max(age_spec_u30_chosen, function(d) {
    return +d.ASR;
  })])
  .range([height_line - 80, 0])
  .nice();

var y_asr_u30_ts_axis = svg_age_spec_u30
  .append("g")
  .attr("transform", 'translate(0,0)')
  .call(d3.axisLeft(y_asr_u30_ts).tickFormat(d3.format(',.0f')));

var tooltip_asr_u30_dot = d3.select("#age_spec_fig_u30")
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

var area_ages_u30 = ['0-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years'];

var area_age_u30_colours = d3.scaleOrdinal()
  .domain(area_ages_u30)
  .range(['#269f37', '#1644ab', '#fe4567', '#ff8f20','#CB3298', '#750475']);

// We need to create two different functions for when a user hovers over the dots and when they hover over the lines
var dotshover_asr_u30 = function(d){

tooltip_asr_u30_dot
  .html("<h4>" + d.Age + ' - '+ d.Name + '</h4><p class = "side">The number of cases among those aged ' + d.Age + ' in the seven days to ' + d.Date_label + ' is <b>' + d3.format(',.0f')(d.Rolling_7_day_new_cases) + '</b>. This is '  + d3.format(',.0f')(d.ASR) + ' per 100,000 population.</p><p class = "side">So far, there have been a total of ' + d3.format(',.0f')(d.Cumulative_cases) + ' cases among this age group in ' + d.Name + '.</p>')
  .style("opacity", 1)
  .style("top", (event.pageY - 10) + "px")
  .style("left", (event.pageX + 10) + "px")
  .style("visibility", "visible");

var highlighted_key_u30 = d.Age

lines_asr_u30
.transition()
.duration(300)
.style("stroke",  function(d){
  if (d.key === highlighted_key_u30) {
     return  area_age_u30_colours(highlighted_key_u30)}
     else {
     return '#d2d2d2'}
      })
.attr("stroke-width",  function(d){
  if (d.key === highlighted_key_u30) {
     return  2}
     else {
     return .25}
      })
// .transition()
// .delay(1000)
// .duration(500)
// .style("visibility",function(d){
//   if (d.key === highlighted_key_u30) {
//      return  'visible'}
//      else {
//      return 'hidden'}
//       })

dots_asr_u30
.transition()
.duration(300)
.style("fill",  function(d){
  if (d.Age === highlighted_key_u30) {
     return  area_age_u30_colours(highlighted_key_u30)}
     else {
     return '#d2d2d2'}
      })
.style("stroke",  function(d){
  if (d.Age === highlighted_key_u30) {
     return  area_age_u30_colours(highlighted_key_u30)}
     else {
     return '#d2d2d2'}
      })
.attr("r",  function(d){
  if (d.Age === highlighted_key_u30) {
     return  2}
     else {
     return 0}
      })
// .transition()
// .delay(1000)
// .duration(500)
// .style("visibility",function(d){
//   if (d.Age === highlighted_key_u30) {
//      return  'visible'}
//      else {
//      return 'hidden'}
//       })

}

var linehover_asr_u30 = function(d){

tooltip_asr_u30_dot
  .html("<h4>" + d.key + '</h4><p class = "side">Hover over a dot to find out more.</p>')
  .style("opacity", 1)
  .style("top", (event.pageY - 10) + "px")
  .style("left", (event.pageX + 10) + "px")
  .style("visibility", "visible");

var highlighted_key_u30 = d.key

lines_asr_u30
.transition()
.duration(300)
.style("stroke",  function(d){
  if (d.key === highlighted_key_u30) {
     return  area_age_u30_colours(highlighted_key_u30)}
     else {
     return '#d2d2d2'}
      })
.attr("stroke-width",  function(d){
  if (d.key === highlighted_key_u30) {
     return  2}
     else {
     return .25}
      })
// .transition()
// .delay(1000)
// .duration(500)
// .style("visibility",function(d){
//   if (d.key === highlighted_key_u30) {
//      return  'visible'}
//      else {
//      return 'hidden'}
//       })

dots_asr_u30
.transition()
.duration(300)
.style("fill",  function(d){
  if (d.Age === highlighted_key_u30) {
     return  area_age_u30_colours(highlighted_key_u30)}
     else {
     return '#d2d2d2'}
      })
.attr("stroke",  function(d){
  if (d.Age === highlighted_key_u30) {
     return  area_age_u30_colours(highlighted_key_u30)}
     else {
     return '#d2d2d2'}
      })
.attr("r",  function(d){
  if (d.Age === highlighted_key_u30) {
     return  2}
     else {
     return 0}
      })
// .delay(1000)
// .duration(500)
// .style("visibility",function(d){
//   if (d.Age === highlighted_key_u30) {
//      return  'visible'}
//      else {
//      return 'hidden'}
//       })
}

// No matter which function was called, on mouseleave restore everything back to the way it was.
var mouseleave_asr_u30 = function(d) {
tooltip_asr_u30_dot
.style("visibility", "hidden")

lines_asr_u30
.transition()
.delay(1000)
.duration(500)
.style("stroke",  function(d){ return  area_age_u30_colours(d.key)})
.style('visibility', 'visible')
.attr('stroke-width', 2)

dots_asr_u30
.transition()
.delay(1000)
.duration(500)
.style("fill", function(d) { return area_age_u30_colours(d.Age)})
.style("stroke", function(d) { return area_age_u30_colours(d.Age)})
.attr('r', 2)
.style('visibility', 'visible')
}

var lines_asr_u30 = svg_age_spec_u30
.selectAll(".line")
.data(age_spec_u30_chosen_ts_group)
.enter()
.append("path")
.attr('id', 'c3_lines')
.attr('class', 'c3_all_lines')
.attr("stroke", function(d){ return area_age_u30_colours(d.key)})
.attr("d", function(d){
    return d3.line()
  .x(function(d) { return x_asr_u30(d.Date_label); })
  .y(function(d) { return y_asr_u30_ts(+d.ASR); })
  (d.values)})
.style("stroke-width", 2)
.on('mouseover', linehover_asr_u30)
.on('mouseout', mouseleave_asr_u30);

var dots_asr_u30 = svg_age_spec_u30
  .selectAll('myCircles')
  .data(age_spec_u30_chosen)
  .enter()
  .append("circle")
  .attr("cx", function(d) { return x_asr_u30(d.Date_label)})
  .attr("cy", function(d) { return y_asr_u30_ts(+d.ASR)})
  .attr("r", 2)
  .style("fill", function(d) { return area_age_u30_colours(d.Age)})
  .attr("stroke", function(d) { return area_age_u30_colours(d.Age)})
  .on("mousemove", dotshover_asr_u30)
  .on('mouseout', mouseleave_asr_u30);


function update_asr_u30() {

// Retrieve the selected area name
var selected_age_line_2_area_option = d3.select('#select_lines_age_2_area_button').property("value")

// Update text based on selected area
d3.select("#selected_age_line_2_compare_title")
  .html(function(d) {
    return 'Covid-19 pillar 1 and 2 confirmed age-specific rolling 7 day case rates (per 100,000 population) over time among under 30s up to ' + complete_date + '; ' + selected_age_line_2_area_option + '; as at ' + data_refreshed_date
  });

var age_spec_u30_chosen = daily_u30_asr_cases.filter(function(d) {
  return d.Name === selected_age_line_2_area_option
});

var dates_u30_asr = d3.map(age_spec_u30_chosen, function(d) {
    return (d.Date_label)
  })
  .keys()

var chosen_dates_u30_asr_ticks = dates_asr_ticks.filter(function(el) {
  return dates_u30_asr.includes(el);
});

// Group the data: I want to draw one line per group
var age_spec_u30_chosen_ts_group = d3.nest() // nest function allows to group the calculation per level of a factor
.key(function(d) { return d.Age;})
.entries(age_spec_u30_chosen);

x_asr_u30
  .domain(dates_u30_asr);

xAxis_asr_u30
.transition()
.duration(500)
  .call(d3.axisBottom(x_asr_u30).tickValues(chosen_dates_u30_asr_ticks));

xAxis_asr_u30
  .selectAll("text")
  .attr("transform", 'translate(-' + (x_asr_u30.bandwidth() + 10) + ',10)rotate(-90)')
  .style("text-anchor", "end")
  .style("font-size", "10px")

y_asr_u30_ts
  .domain([0, d3.max(age_spec_u30_chosen, function(d) {
    return +d.ASR;
  })])
  .nice();

y_asr_u30_ts_axis.transition()
.duration(1000)
.call(d3.axisLeft(y_asr_u30_ts));

dots_asr_u30
  .data(age_spec_u30_chosen)
  .transition()
  .duration(1000)
  .attr("cx", function(d) { return x_asr_u30(d.Date_label)})
  .attr("cy", function(d) { return y_asr_u30_ts(+d.ASR)});

lines_asr_u30
.data(age_spec_u30_chosen_ts_group)
.transition()
.duration(1000)
.attr("d", function(d){
  return d3.line()
.x(function(d) { return x_asr_u30(d.Date_label); })
.y(function(d) { return y_asr_u30_ts(+d.ASR); })
  (d.values)});
}

d3.select("#select_lines_age_2_area_button").on("change", function(d) {
  var selected_age_line_2_area_option = d3.select('#select_lines_age_2_area_button').property("value")
  update_asr_u30()
})

area_ages_u30.forEach(function(d, i) {
    var list = document.createElement("li");
    list.innerHTML = d;
    list.className = 'key_age_areas';
    list.style.borderColor = area_age_u30_colours(i);
      var tt = document.createElement('div');
    tt.className = 'side_tt';
    tt.style.borderColor = area_age_u30_colours(i);
    var tt_h3_asr = document.createElement('h3');
    tt_h3_asr.innerHTML = d;
    tt.appendChild(tt_h3_asr);
    var div = document.getElementById("age_band_u30_key_figure");
    div.appendChild(list);
    })
