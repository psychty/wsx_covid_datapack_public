var request = new XMLHttpRequest();
request.open("GET", "./Outputs/wsx_cumulative_rolling_label.json", false);
request.send(null);
var wsx_cumulative_text_obj = JSON.parse(request.responseText);

d3.select("#wsx_growth_rate_latest").html(function (d) {
  return wsx_cumulative_text_obj;
});

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/england_cumulative.json", false);
request.send(null);
var england_cumulative_text_obj = JSON.parse(request.responseText);

d3.select("#england_rate_text").html(function (d) {
  return england_cumulative_text_obj;
});

// append the svg object to the body of the page
var rate_map_1_ph = d3
  .select("#rate_map_1")
  .append("svg")
  .attr("width", "600")
  .attr("height", "800")
  .attr("class", "place_holder")
  .append("g")
  .attr("transform", "translate(" + 60 + "," + 20 + ")");
