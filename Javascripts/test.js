var request = new XMLHttpRequest();
request.open("GET", "./Outputs/utla_growth_since_september.json", false);
request.send(null);
var utla_growth_ts_data = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/utla_growth_limits.json", false);
request.send(null);
var utla_growth_ts_limits = JSON.parse(request.responseText);

var height_scatter = 500;

// append the svg object to the body of the page
var growth_svg_2 = d3.select("#utla_ts_growth_rate")
.append("svg")
.attr("width", width_hm)
.attr("height", height_scatter)
.append("g")
.attr("transform", "translate(" + 60 + "," + 20 + ")");

// Add X axis
var x_growth_utla_ts = d3.scaleLinear()
  .domain([0, utla_growth_ts_limits[0].Max_rolling_rate])
  .range([0, width_hm  - 80]);

d3.select("#utla_growth_rate_title_2")
  .html(function(d) {
    return 'New confirmed COVID-19 case rate cases per 100,000 population (all ages) in the seven days to ' + complete_date +' by week on week change in number of cases; Upper Tier Local Authorities;'
  });

growth_svg_2.append("g")
  .attr("transform", 'translate(0,' + (height_scatter - 80 ) + ")")
  .call(d3.axisBottom(x_growth_utla_ts));

// Add Y axis
var y_growth_utla_ts = d3.scaleLinear()
   .domain([-1, utla_growth_ts_limits[0].Max_change_week])
   .range([ height_scatter - 80, 0])

growth_svg_2.append("g")
   .call(d3.axisLeft(y_growth_utla_ts).tickFormat(d3.format('.0%')));

var tooltip_growth_utla_ts = d3.select("#utla_ts_growth_rate")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip_class_scatter")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("padding", "10px")

var showTooltip_growth_ts = function(d) {

  tooltip_growth_utla_ts
   .html("<h5>" + d.Name + '</h5><p>' + d.Label_2 + '</p><p>' + d.Label_1 + '</p><p>' + d.Label_3 + '</p>')
    .style("opacity", 1)
    .style("top", (event.pageY - 10) + "px")
    .style("left", (event.pageX + 10) + "px")
    .style('opacity', 1)
    .style("visibility", "visible")
}

var mouseleave_growth_utla_ts = function(d) {
  tooltip_growth_utla_ts
    .style('opacity', 0)
    .style("visibility", "hidden")
}

growth_svg_2.append('g')
  .selectAll("dot")
  .data(utla_growth_ts_data) // the .filter part is just to keep a few dots on the chart, not all of them
  .enter()
  .append("circle")
  .attr("cx", function (d) { return x_growth_utla_ts(d.Rolling_7_day_rate); } )
  .attr("cy", function (d) { return y_growth_utla_ts(d.Change_actual_by_week); } )
  .attr("r", 7)
  .style("fill", function(d){if(d.Name == 'West Sussex'){return "orange"} else if (d.Name == 'England') {return 'black'} else {return "#69b3a2"}})
  .style("opacity", .8)
  .style("stroke", "white")
  .on('mousemove', showTooltip_growth_ts)
  .on('mouseout', mouseleave_growth_utla_ts)

growth_svg_2
  .append('line')
  .attr('x1', function (d) { return x_growth_utla_ts(0); } )
  .attr('y1', function (d) { return y_growth_utla_ts(0); } )
  .attr('x2', function (d) { return  x_growth_utla_latest(utla_growth_ts_limits[0].Max_rolling_rate); } )
  .attr('y2', function (d) { return y_growth_utla_ts(0); } )
  .attr('stroke', '#000000')
  // .attr('stroke', 'red')
  .attr("stroke-dasharray", ("3, 3"))
