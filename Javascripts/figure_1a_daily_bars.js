
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
    return 'Pillar 1 and 2 combined daily confirmed COVID-19 cases; ' + selected_figure_1a_area_option + '; ' + first_date + ' - ' + latest_date
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
  .attr("transform", 'translate(0,' + (height_line - 80 ) + ")")
   .call(d3.axisBottom(x_daily_cases).tickValues(data_dates));

xAxis_daily_cases
  .selectAll("text")
  .attr("transform", 'translate(-' + (x_daily_cases.bandwidth() + 10) + ',10)rotate(-90)')
  .style("text-anchor", "end")

x_summary = case_summary.filter(function(d, i) {
  return d.Name === selected_figure_1a_area_option
})

d3.select("#x_latest_figures")
  .data(x_summary)
  .html(function(d) {
    return 'The total number of confirmed Covid-19 cases so far in ' + selected_figure_1a_area_option + ' is ' + d3.format(',.0f')(d['Total confirmed cases so far'])  + '. This is ' + d3.format(',.0f')(d['Total cases per 100,000 population']) + ' cases per 100,000 population. The current daily case count (using data from ' + complete_date + ') is ' + d3.format(',.0f')(d['Confirmed cases swabbed on most recent complete day']) + ' new confirmed cases swabbed (' + d3.format(',.1f')(d['Confirmed cases swabbed per 100,000 population on most recent complete day']) + ' per 100,000).' });

max_limit_x = daily_case_limits.filter(function(d){
  return d.Name === selected_figure_1a_area_option})[0]['Max_limit']

var y_daily_cases = d3.scaleLinear()
  .domain([0, max_limit_x])
  .range([height_line - 80 , 0])
  .nice();

var yAxis_daily_cases = svg_daily_new_case_bars
.append("g")
.call(d3.axisLeft(y_daily_cases));

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
.attr('class', 'test_notes')
.attr("x", function(d) { return x_daily_cases(d.Date_label_2) + (x_daily_cases.bandwidth()/2)})
.attr("y", 0)
.attr("width", 2)
.attr("height", function(d) { return (height_line - 80 )})
.style("fill", incomplete_colour);

// We can treat this like a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_daily_new_case_bars
.selectAll("testing_timeline")
.data(uk_testing_key_dates)
.enter()
.append("circle")
.attr('class', 'test_notes')
.attr("cx", function(d) { return x_daily_cases(d.Date_label_2) + (x_daily_cases.bandwidth()/2)})
.attr("y", 0)
.attr("r", 6)
.style("fill", incomplete_colour)
.on("mousemove", showTooltip_testing_key_dates)
.on('mouseout', mouseleave_testing_key_dates);

svg_daily_new_case_bars
.append("text")
.attr('id', 'test_milestones')
.attr('class', 'test_notes')
.attr("x", function(d) { return x_daily_cases('26 Mar') + (x_daily_cases.bandwidth()/2)})
.attr("y", 2)
.text('Testing eligibility changes -')
.attr("text-anchor", "end")

// Restriction changes

var tooltip_restrictions_key_dates = d3.select("#daily_new_case_bars")
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

var showTooltip_restrictions_key_dates = function(d) {
  tooltip_restrictions_key_dates
    .html("<h5>" + d.Date_label_2 + '</h5><p>' + d.Change + '</p>')
    .style("opacity", 1)
    .style("top", (event.pageY - 10) + "px")
    .style("left", (event.pageX + 10) + "px")
    .style('opacity', 1)
    .style("visibility", "visible")
}

var mouseleave_restrictions_key_dates = function(d) {
  tooltip_restrictions_key_dates
    .style('opacity', 0)
    .style("visibility", "hidden")
}

// We can treat this as a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_daily_new_case_bars
.selectAll("testing_timeline")
.data(uk_restrictions_key_dates)
.enter()
.append('line')
.attr('class', 'restriction_notes')
.attr('x1', function(d) { return x_daily_cases(d.Date_label_2) + (x_daily_cases.bandwidth()/2)})
.attr('y1', 40)
.attr('x2',  function(d) { return x_daily_cases(d.Date_label_2) + (x_daily_cases.bandwidth()/2)})
.attr('y2', height_line - 80 )
.attr('stroke', '#ec0909')
.attr("stroke-dasharray", ("3, 3"))

// We can treat this like a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_daily_new_case_bars
.selectAll("testing_timeline")
.data(uk_restrictions_key_dates)
.enter()
.append("circle")
.attr('class', 'restriction_notes')
.attr("cx", function(d) { return x_daily_cases(d.Date_label_2) + (x_daily_cases.bandwidth()/2)})
.attr("cy", 40)
.attr("r", 6)
.style("fill", '#ec0909')
.on("mousemove", showTooltip_restrictions_key_dates)
.on('mouseout', mouseleave_restrictions_key_dates);

svg_daily_new_case_bars
.append("text")
.attr('id', 'test_milestones')
.attr('class', 'restriction_notes')
.attr("x", function(d) { return x_daily_cases('22 Mar') + (x_daily_cases.bandwidth()/2)})
.attr("y", 42)
.text('Lockdown begins -')
.attr("text-anchor", "end")

// Bars
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
  .attr('x1', x_daily_cases(incomplete_sm_date))
  .attr('y1', 0)
  .attr('x2', x_daily_cases(incomplete_sm_date))
  .attr('y2', height_line - 80 )
  .attr('stroke', incomplete_colour)
  .attr("stroke-dasharray", ("3, 3"))

svg_daily_new_case_bars
  .append('rect')
  .attr('x', x_daily_cases(incomplete_sm_date))
  .attr('y1', 0)
  .attr('width', (x_daily_cases(latest_sm_date) + x_daily_cases.bandwidth()) - x_daily_cases(incomplete_sm_date))
  .attr('height', height_line - 80 )
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
.attr("height", function(d) { return (height_line - 80 ) - y_daily_cases(d.New_cases); })
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
  .attr("y", function(d) { return height_line - 165 })
  .text('The black line represents')
  .attr("text-anchor", "start")

svg_daily_new_case_bars
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_daily_cases('31 Jan') + (x_daily_cases.bandwidth()/2)})
  .attr("y", function(d) { return height_line - 150 })
  .text('seven day average new cases')
  .attr("text-anchor", "start")

svg_daily_new_case_bars
  .append("text")
  .attr("x", x_daily_cases('12 May'))
  .attr("y", 30)
  .attr('class', 'restriction_notes')
  .text('more people')
  .attr("text-anchor", "end")

svg_daily_new_case_bars
  .append("text")
  .attr("x", x_daily_cases('13 May'))
  .attr("y", 40)
  .attr('class', 'restriction_notes')
  .text('return to work -')
  .attr("text-anchor", "end")

function update_daily_bars() {

var selected_figure_1a_area_option = d3.select('#select_bars_daily_cases_1_area_button').property("value")

d3.select("#selected_daily_cases_bars_1_compare_title")
  .html(function(d) {
    return 'Pillar 1 and 2 combined daily confirmed COVID-19 cases; ' + selected_figure_1a_area_option + '; ' + first_date + ' - ' + latest_date
  });

var bars_daily_cases_1_chosen = daily_cases.filter(function(d) {
  return d.Name === selected_figure_1a_area_option
});

var total_cases_daily_chosen = case_summary.filter(function(d){
  return d.Name === selected_figure_1a_area_option})[0]['Cumulative_cases']

x_summary = case_summary.filter(function(d, i) {
  return d.Name === selected_figure_1a_area_option
})

d3.select("#x_latest_figures")
  .data(x_summary)
  .html(function(d) {
    return 'The total number of confirmed Covid-19 cases so far in ' + selected_figure_1a_area_option + ' is ' + d3.format(',.0f')(d['Total confirmed cases so far'])  + '. This is ' + d3.format(',.0f')(d['Total cases per 100,000 population']) + ' cases per 100,000 population. The current daily case count (using data from ' + complete_date + ') is ' + d3.format(',.0f')(d['Confirmed cases swabbed on most recent complete day']) + ' new confirmed cases swabbed (' + d3.format(',.1f')(d['Confirmed cases swabbed per 100,000 population on most recent complete day']) + ' per 100,000).' });


var showTooltip_daily_case_1 = function(d) {
  tooltip_daily_case_1
    .html("<h5>" + d.Name + '</h5><p><b>' + d.Date_label + '</b></p><p class = "side">In ' + d.Name + ' there were <b>' + d3.format(',.0f')(d.New_cases) + ' </b>specimens taken on this date which resulted in a positive result for Covid-19.</p><p class = "side">The new cases swabbed on this day represent ' + d3.format('0.1%')(d.New_cases / total_cases_daily_chosen) + ' of the total number of cases confirmed so far (' + d3.format(',.0f')(total_cases_daily_chosen) + ') </p><p class = "side">' + d.Seven_day_ave_new_label + '</p>')
    .style("opacity", 1)
    .style("top", (event.pageY - 10) + "px")
    .style("left", (event.pageX + 10) + "px")
    .style('opacity', 1)
    .style("visibility", "visible")
}

max_limit_x = daily_case_limits.filter(function(d){
  return d.Name === selected_figure_1a_area_option})[0]['Max_limit']

y_daily_cases
  .domain([0, max_limit_x])
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
.attr("height", function(d) { return (height_line - 80 ) - y_daily_cases(d.New_cases); })
.style("fill", function(d) { return '#071b7c' })

daily_new_case_bars
  .on("mousemove", showTooltip_daily_case_1)
  .on('mouseout', mouseleave_daily_case_1);

}

d3.select("#select_bars_daily_cases_1_area_button").on("change", function(d) {
  var selected_figure_1a_area_option = d3.select('#select_bars_daily_cases_1_area_button').property("value")
  update_daily_bars()
})

// This function is gonna change the opacity and size of selected and unselected circles
function update_annotations_f1(){

// For each check box:
d3.selectAll(".checkbox").each(function(d){
  cb = d3.select(this);
  grp = cb.property("value")

// If the box is check, show the notes
  if(cb.property("checked")){
svg_daily_new_case_bars.selectAll("."+grp)
.transition()
.duration(1000)
.style("opacity", 1)
  }
else{
svg_daily_new_case_bars.selectAll("."+grp)
.transition()
.duration(1000)
.style("opacity", 0)}
  })
  };

// When a button change, I run the update function
d3.selectAll(".checkbox").on("change",update_annotations_f1);

update_annotations_f1()
