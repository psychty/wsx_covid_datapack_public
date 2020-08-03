var width_hm = document.getElementById("content_size").offsetWidth,
    height_hm = 25,
    height_hm_title = 45,
    height_hm_explainer = 15,
    height_sm = 210,
    incomplete_colour = '#999999',
    height_line = 400;

var width_sm = (width_hm / 3) - 10

// We dont want the small plots to be less than 325 pixels wide so this says if the
if (width_sm < 300){
 width_sm = 300
}

var areas = ['West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing']

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/wsx_case_summary.json", false);
request.send(null);
var case_summary = JSON.parse(request.responseText); // parse the fetched json data into a variable

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/wsx_daily_cases.json", false);
request.send(null);
var daily_cases = JSON.parse(request.responseText); // parse the fetched json data into a variable

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

var dates = d3.map(daily_cases, function(d) {
    return (d.Date_label)
  })
  .keys()

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/range_dates.json", false);
request.send(null);

var first_date = JSON.parse(request.responseText).filter(function(d) {
  return d.Order == 'First'
})[0]['Date_label']

var complete_date = JSON.parse(request.responseText).filter(function(d) {
  return d.Order == 'Complete'
})[0]['Date_label']

var seven_days_date = JSON.parse(request.responseText).filter(function(d) {
  return d.Order == 'Seven_days_ago'
})[0]['Date_label']

var complete_date_actual = JSON.parse(request.responseText).filter(function(d) {
  return d.Order == 'Complete'
})[0]['Date']

var first_incomplete_date = JSON.parse(request.responseText).filter(function(d) {
  return d.Order == 'First_incomplete'
})[0]['Date_label']

var first_incomplete_period = JSON.parse(request.responseText).filter(function(d) {
  return d.Order == 'First_incomplete'
})[0]['Period']

var first_incomplete_date_actual = JSON.parse(request.responseText).filter(function(d) {
  return d.Order == 'First_incomplete'
})[0]['Date']

var latest_date = JSON.parse(request.responseText).filter(function(d) {
  return d.Order == 'Last'
})[0]['Date_label']

var most_recent = JSON.parse(request.responseText).filter(function(d) {
  return d.Order == 'Last'
})[0]['Date']

var most_recent_period = JSON.parse(request.responseText).filter(function(d) {
  return d.Order == 'Last'
})[0]['Period']

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/case_change_dates.json", false);
request.send(null);
var data_dates = JSON.parse(request.responseText).map(function(d){return d.Date_label});

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/case_change_date_range.json", false);
request.send(null);
var data_date_range = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/first_incomplete_daily_case.json", false);
request.send(null);
var incomplete_sm_date = JSON.parse(request.responseText)[0]

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/latest_daily_case.json", false);
request.send(null);
var latest_sm_date = JSON.parse(request.responseText)[0]

case_change_values = ['No change in average cases','Increasing average number of cases over past 7 days', 'Decreasing average number of cases over past 7 days', 'Less than half the previous 7-day average', 'No confirmed cases in past 7 days']

// color palette
var case_change_colour = d3.scaleOrdinal()
  .domain(case_change_values)
  .range(['#aaaaaa','#721606', '#005bd6', '#1fbfbb', '#c2f792'])

var change_case_label = d3.scaleOrdinal()
  .domain(case_change_values)
  .range(['the average number of cases has not changed','7 day average cases appear to be increasing', '7 day average cases appear to be decreasing', 'average number of cases half what it was in previous 7 days', 'no confirmed cases in most recent complete 7 day period'])

// Maps
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/ltla_rate_bins.json", false);
request.send(null);

var ltla_rate_bins = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/utla_cumulative_rate_bins.json", false);
request.send(null);

var utla_rate_bins = JSON.parse(request.responseText);
var decile_colours = ['#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695']

var utla_decile_colour_func = d3.scaleOrdinal()
  .domain(utla_rate_bins)
  .range(decile_colours)

var ltla_decile_colour_func = d3.scaleOrdinal()
  .domain(ltla_rate_bins)
  .range(decile_colours)

var width_map = document.getElementById("content_size").offsetWidth;

// Add AJAX request for data
var utla = $.ajax({
  url:"./Outputs/utla_covid_rate_latest.geojson",
  dataType: "json",
  success: console.log("UTLA boundary data successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText)
  }
})

// Add AJAX request for data
var ltla = $.ajax({
  url:"./Outputs/ltla_covid_cumulative_rate_latest.geojson",
  dataType: "json",
  success: console.log("LTLA boundary data successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText)
  }
})

// testing policy timelines
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/uk_testing_key_dates.json", false);
request.send(null);
var uk_testing_key_dates = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/uk_restrictions_key_dates.json", false);
request.send(null);
var uk_restrictions_key_dates = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/wsx_daily_case_limits.json", false);
request.send(null);
var daily_case_limits = JSON.parse(request.responseText);

// Pathways
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/NHS_pathways_df.json", false);
request.send(null);
var pathways_df = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/NHS_pathways_dates.json", false);
request.send(null);
var pathway_dates = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/pathways_changes.json", false);
request.send(null);
var pathway_changes = JSON.parse(request.responseText);
