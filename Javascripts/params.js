var width_hm = document.getElementById("content_size").offsetWidth * 0.75 - 50,
  height_hm = 25,
  height_hm_title = 45,
  height_hm_explainer = 15,
  height_sm = 220,
  incomplete_colour = "#999999",
  height_line = 410;

var width_sm = width_hm / 2 - 10;

// We dont want the small plots to be less than 325 pixels wide so this says if the
if (width_sm < 300) {
  width_sm = 300;
}

var areas = [
  "West Sussex",
  "Adur",
  "Arun",
  "Chichester",
  "Crawley",
  "Horsham",
  "Mid Sussex",
  "Worthing",
];

var areas_1a = [
  "West Sussex",
  "Adur",
  "Arun",
  "Chichester",
  "Crawley",
  "Horsham",
  "Mid Sussex",
  "Worthing",
  "South East region",
  "England",
];

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/case_change_dates.json", false);
request.send(null);
var data_dates = JSON.parse(request.responseText).map(function (d) {
  return d.Date_label;
});

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/wsx_case_summary.json", false);
request.send(null);
var case_summary = JSON.parse(request.responseText); // parse the fetched json data into a variable

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/wsx_daily_cases.json", false);
request.send(null);
var daily_cases = JSON.parse(request.responseText); // parse the fetched json data into a variable

var dates = d3
  .map(daily_cases, function (d) {
    return d.Date_label;
  })
  .keys();

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/range_dates.json", false);
request.send(null);

var first_date = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "First";
})[0]["Date_label"];

var complete_date = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "Complete";
})[0]["Date_label"];

var seven_days_date = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "Seven_days_ago";
})[0]["Date_label"];

var complete_date_actual = JSON.parse(request.responseText).filter(function (
  d
) {
  return d.Order == "Complete";
})[0]["Date"];

var first_incomplete_date = JSON.parse(request.responseText).filter(function (
  d
) {
  return d.Order == "First_incomplete";
})[0]["Date_label"];

var first_incomplete_period = JSON.parse(request.responseText).filter(function (
  d
) {
  return d.Order == "First_incomplete";
})[0]["Period"];

var first_incomplete_date_actual = JSON.parse(request.responseText).filter(
  function (d) {
    return d.Order == "First_incomplete";
  }
)[0]["Date"];

var latest_date = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "Last";
})[0]["Date_label"];

var most_recent = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "Last";
})[0]["Date"];

var most_recent_period = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "Last";
})[0]["Period"];

var rolling_seven_days_ago = JSON.parse(request.responseText).filter(function (
  d
) {
  return d.Order == "Seven_days_ago";
})[0]["Date_label"];

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/first_incomplete_daily_case.json", false);
request.send(null);
var incomplete_sm_date = JSON.parse(request.responseText)[0];

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/latest_daily_case.json", false);
request.send(null);
var latest_sm_date = JSON.parse(request.responseText)[0];

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/daily_case_update_date.json", false);
request.send(null);
var data_refreshed_date = JSON.parse(request.responseText)[0];

// ! Labels for small multiples

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/case_date_labels.json", false);
request.send(null);
var case_dates_df = JSON.parse(request.responseText);

var first_case_period = case_dates_df.filter(function (d) {
  return d.Item === "first_case_period";
})[0]["Label"];

var last_case_period = case_dates_df.filter(function (d) {
  return d.Item === "last_case_period";
})[0]["Label"];

// console.log(first_case_period, last_case_period);

// Update text based on selected area
d3.select("#update_date").html(function (d) {
  return (
    "The case data has been refreshed on " +
    data_refreshed_date +
    " with cases confirmed up to " +
    latest_date
  );
});
