var request = new XMLHttpRequest();
request.open("GET", "./Outputs/utla_growth_latest.json", false);
request.send(null);
var utla_growth_latest_data = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/utla_growth_limits_complete_date.json", false);
request.send(null);
var utla_growth_latest_limits = JSON.parse(request.responseText);

var height_scatter = 500;

// append the svg object to the body of the page
var growth_svg_1 = d3.select("#utla_latest_growth_rate")
.append("svg")
.attr("width", width_hm)
.attr("height", height_scatter)
.append("g")
.attr("transform", "translate(" + 60 + "," + 20 + ")");

// Add X axis
var x_growth_utla_latest = d3.scaleLinear()
  .domain([0, utla_growth_latest_limits[0].Max_rolling_rate])
  .range([0, width_hm  - 80]);

d3.select("#utla_growth_rate_title")
  .html(function(d) {
    return 'New confirmed COVID-19 case rate cases per 100,000 population (all ages) in the seven days to ' + complete_date +' by week on week change in number of cases; Upper Tier Local Authorities;'
  });

wsx_latest_growth = utla_growth_latest_data.filter(function(d) {
  return d.Name == 'West Sussex'
})

england_latest_growth = utla_growth_latest_data.filter(function(d) {
  return d.Name == 'England'
})

d3.select("#wsx_growth_rate_latest")
  .html(function(d) {
    return wsx_latest_growth[0]['Label_2'].replace('In ', 'In West Sussex, in ') + ' ' + wsx_latest_growth[0]['Label_1'] + '.'
  });

growth_svg_1.append("g")
  .attr("transform", 'translate(0,' + (height_scatter - 80 ) + ")")
  .call(d3.axisBottom(x_growth_utla_latest));

// Add Y axis
var y_growth_utla_latest = d3.scaleLinear()
   .domain([-1, utla_growth_latest_limits[0].Max_change_week])
   .range([ height_scatter - 80, 0])

growth_svg_1.append("g")
   .call(d3.axisLeft(y_growth_utla_latest).tickFormat(d3.format('.0%')));

var tooltip_growth_utla_latest = d3.select("#utla_latest_growth_rate")
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

var showTooltip_growth_latest = function(d) {

  tooltip_growth_utla_latest
    .html("<h5>" + d.Name + '</h5><p>' + d.Label_2 + '</p><p>' + d.Label_1 + '</p><p>' + d.Label_3 + '</p>')
    .style("opacity", 1)
    .style("top", (event.pageY - 10) + "px")
    .style("left", (event.pageX + 10) + "px")
    .style('opacity', 1)
    .style("visibility", "visible")
}

var mouseleave_growth_utla_latest = function(d) {
  tooltip_growth_utla_latest
    .style('opacity', 0)
    .style("visibility", "hidden")
}

growth_svg_1.append('g')
  .selectAll("dot")
  .data(utla_growth_latest_data) // the .filter part is just to keep a few dots on the chart, not all of them
  .enter()
  .append("circle")
  .attr("cx", function (d) { return x_growth_utla_latest(d.Rolling_7_day_rate); } )
  .attr("cy", function (d) { return y_growth_utla_latest(d.Change_actual_by_week); } )
  .attr("r", 7)
  .style("fill", function(d){if(d.Name == 'West Sussex'){return "orange"} else if (d.Name == 'England') {return 'black'} else {return "#69b3a2"}})
  .style("opacity", .8)
  .style("stroke", "white")
  .on('mousemove', showTooltip_growth_latest)
  .on('mouseout', mouseleave_growth_utla_latest)

growth_svg_1.append('g')
  .selectAll("dot")
  .data(england_latest_growth) // the .filter part is just to keep a few dots on the chart, not all of them
  .enter()
  .append("circle")
  .attr("cx", function (d) { return x_growth_utla_latest(d.Rolling_7_day_rate); } )
  .attr("cy", function (d) { return y_growth_utla_latest(d.Change_actual_by_week); } )
  .attr("r", 7)
  .style("fill", 'black')
  .style("opacity", .8)
  .style("stroke", "white")
  .on('mousemove', showTooltip_growth_latest)
  .on('mouseout', mouseleave_growth_utla_latest)

growth_svg_1.append('g')
  .selectAll("dot")
  .data(wsx_latest_growth) // the .filter part is just to keep a few dots on the chart, not all of them
  .enter()
  .append("circle")
  .attr("cx", function (d) { return x_growth_utla_latest(d.Rolling_7_day_rate); } )
  .attr("cy", function (d) { return y_growth_utla_latest(d.Change_actual_by_week); } )
  .attr("r", 7)
  .style("fill", 'orange')
  .style("opacity", 1)
  .style("stroke", "white")
  .on('mousemove', showTooltip_growth_latest)
  .on('mouseout', mouseleave_growth_utla_latest)

growth_svg_1
  .append('line')
  .attr('x1', function (d) { return x_growth_utla_latest(0); } )
  .attr('y1', function (d) { return y_growth_utla_latest(0); } )
  .attr('x2', function (d) { return  x_growth_utla_latest(utla_growth_latest_limits[0].Max_rolling_rate); } )
  .attr('y2', function (d) { return y_growth_utla_latest(0); } )
  .attr('stroke', '#000000')
  // .attr('stroke', 'red')
  .attr("stroke-dasharray", ("3, 3"))


var area_growth_comp = ['West Sussex', 'Other UTLAs', 'England'];

var utla_growth_colour_func = d3.scaleOrdinal()
  .domain(area_growth_comp)
  .range(['orange','#69b3a2', 'black'])

area_growth_comp.forEach(function(item, index) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = 'key_list_rate_ltla';
    list.style.borderColor = utla_growth_colour_func(index);
      var tt = document.createElement('div');
    tt.className = 'side_tt';
    tt.style.borderColor = utla_growth_colour_func(index);
    var tt_h3_1 = document.createElement('h3');
    tt_h3_1.innerHTML = item;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("growth_key_utla");
    div.appendChild(list);
    })
