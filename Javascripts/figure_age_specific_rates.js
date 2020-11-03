
var areas_age_spec = ['West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'South East region', 'England']

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

// Retrieve the selected area name
var selected_age_line_1_area_option = d3.select('#select_lines_age_1_area_button').property("value")

// Update text based on selected area
d3.select("#selected_age_line_1_compare_title")
  .html(function(d) {
    return 'Covid-19 pillar 1 and 2 confirmed age-specific rolling 7 day cases over time; ' + selected_age_line_1_area_option + '; as at ' + data_refreshed_date
  });

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/age_specific_rates_10_years_by_date.json", false);
request.send(null);
var daily_asr_cases = JSON.parse(request.responseText); // parse the fetched json data into a variable

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/age_specific_rate_dates.json", false);
request.send(null);
var dates_asr_ticks = JSON.parse(request.responseText).map(function(d){return d.Date_label});

var dates_asr = d3.map(daily_asr_cases, function(d) {
    return (d.Date_label)
  })
  .keys()

var age_spec_1_chosen = daily_asr_cases.filter(function(d) {
  return d.Name === selected_age_line_1_area_option
});

// Group the data: I want to draw one line per group
var age_spec_1_chosen_ts_group = d3.nest() // nest function allows to group the calculation per level of a factor
.key(function(d) { return d.Age;})
.entries(age_spec_1_chosen);

// This will be the x axis scale for days since case x - note this is NOT the date as in previous timeseries. We have used this to compare each area over the same time period.
var x_as_1 = d3.scaleBand()
  .domain(dates_asr)
  .range([0, width_hm - 60])

var xAxis_asr_1 = svg_age_spec_1
  .append("g")
  .attr("transform", 'translate(0,' + (height_line - 80 ) + ")")
  .call(d3.axisBottom(x_as_1).tickValues(dates_asr_ticks));

xAxis_asr_1
  .selectAll("text")
  .attr("transform", 'translate(-' + (x_as_1.bandwidth() + 10) + ',10)rotate(-90)')
  .style("text-anchor", "end")
  .style("font-size", "10px")
