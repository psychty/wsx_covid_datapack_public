var width_hm = document.getElementById("content_size").offsetWidth * 0.75 - 50,
  height_hm = 25,
  height_hm_title = 45,
  height_hm_explainer = 15,
  height_sm = 220,
  incomplete_colour = "#999999",
  jcvi_colour = "#857e7e",
  height_line = window.innerHeight * 0.25,
  half_width = document.getElementById("content_size").offsetWidth * 0.5;

// console.log(height_line, width_hm);

if (width_hm >= 800) {
  height_line = window.innerHeight * 0.3;
}

if (window.innerHeight < 800) {
  height_line = window.innerHeight * 0.4;
}

if (width_hm < 400) {
  width_hm = 400;
}

var width_sm = width_hm / 3 - 10;

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
request.open("GET", "./Outputs/range_dates.json", false);
request.send(null);

var first_date = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "First";
})[0]["Date_label"];

var complete_date = JSON.parse(request.responseText).filter(function (d) {
  return d.Order == "Complete";
})[0]["Date_label"];

var complete_date_minus_1 = JSON.parse(request.responseText).filter(function (
  d
) {
  return d.Order == "Complete minus 1";
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

var last_pcr_test_period = case_dates_df.filter(function (d) {
  return d.Item === "last_pcr_test_period";
})[0]["Label"];

var last_lfd_test_period = case_dates_df.filter(function (d) {
  return d.Item === "last_lfd_test_period";
})[0]["Label"];

// console.log(first_case_period, last_case_period);

// Update text based on selected area
d3.select("#update_date").html(function (d) {
  return (
    "The data has been refreshed on <b>" +
    data_refreshed_date +
    "</b> with cases confirmed up to " +
    latest_date
  );
});

d3.select("#update_date_nav_label").html(function (d) {
  return "Updated: " + data_refreshed_date;
});

d3.select("#data_recency").html(function (d) {
  return (
    "Case results are published each day in the afternoon (around 4pm) and represent cases reported up to 9am of that day. Cases are assigned to the day on which the test (specimen) was taken and whilst results can be reported quickly, it can take several days for all results to be reported for specimens taken on a particular date. As such, data for very recent days are likely to change, and only data <b>up to " +
    complete_date +
    " should be treated as complete.</b>"
  );
});
