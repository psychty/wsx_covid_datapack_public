latest_triage_date = '27 July'

d3.select("#pathways_title")
  .html(function(d) {
    return 'Total number of complete triages to NHS Pathways for COVID-19; NHS West Sussex CCG' + '; 19 March - ' + latest_triage_date});

var svg_pathways_fig = d3.select("#pathways_fig")
.append("svg")
.attr("width", width_hm)
.attr("height", height_line)
.append("g")
.attr("transform", "translate(" + 60 + "," + 20 + ")");

var x_pathways = d3.scaleBand()
    .domain(pathways_df.map(function(d) { return d.Date}))
    .range([0, width_hm - 60])

latest_p_summary = pathways_df.filter(function(d) {
return d.Date === pathway_dates[pathway_dates.length-1]})[0];

var xAxis_pathways = svg_pathways_fig
  .append("g")
  .attr("transform", 'translate(0,' + (height_line - 80 ) + ")")
   .call(d3.axisBottom(x_pathways).tickValues(pathway_dates));

xAxis_pathways
  .selectAll("text")
  .attr("transform", 'translate(-' + (x_pathways.bandwidth() + 5) + ',10)rotate(-90)')
  .style("text-anchor", "end")

//Add Y axis
var y_pathways = d3.scaleLinear()
    .domain([0, d3.max(pathways_df, function(d) { return +d.Triage_count; })])
    .range([height_line - 80, 0 ])
    .nice();

var yAxis_pathways = svg_pathways_fig
.append("g")
.call(d3.axisLeft(y_pathways));

var tooltip_pathways = d3.select("#pathways_fig")
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

var showTooltip_pathways = function(d) {
  tooltip_pathways
    .html("<h5>" + d.Date + '</h5><p>' + d.label_1 + '</p><p>' + d.label_2 + '</p>')
    .style("opacity", 1)
    .style("top", (event.pageY - 10) + "px")
    .style("left", (event.pageX + 10) + "px")
    .style('opacity', 1)
    .style("visibility", "visible")
}

var mouseleave_pathways = function(d) {
  tooltip_pathways
    .style('opacity', 0)
    .style("visibility", "hidden")
}

var lines_triages = svg_pathways_fig
    .append('g')
    .append("path")
    .datum(pathways_df)
    .attr("d", d3.line()
        .x(function (d) {
            return x_pathways(d.Date) + (x_pathways.bandwidth() /2)
        })
        .y(function (d) {
            return y_pathways(+d.Triage_count)
        }))
    .attr("stroke", '#000')
    .style("stroke-width", 2)
    .style("fill", "none");



// We can treat this as a set of thin bars. With tooltips (maybe adding a dot or symbol at the top of each line as a easier thing to mouseover)
svg_pathways_fig
.selectAll("changes_timeline")
.data(pathway_changes)
.enter()
.append('line')
.attr('x1', function(d) { return x_pathways(d.Date) + (x_pathways.bandwidth()/2)})
.attr('y1',  function(d) { return y_pathways(+d.Triage_count) } )
.attr('x2',  function(d) { return x_pathways(d.Date) + (x_pathways.bandwidth()/2)})
.attr('y2', height_line - 80 )
.attr('stroke', function(d) { return d.direction})
.attr("stroke-dasharray", ("3, 3"))

svg_pathways_fig
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_pathways('08 Apr') + (x_pathways.bandwidth()/2)})
  .attr("y", function(d) { return y_pathways(+565) + 5})
  .text('9th April')
  .style('font-weight', 'bold')
  .attr("text-anchor", "end")

svg_pathways_fig
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_pathways('08 Apr') + (x_pathways.bandwidth()/2)})
  .attr("y", function(d) { return y_pathways(+565) + 15})
  .text('111 online removed')
  .attr("text-anchor", "end")

svg_pathways_fig
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_pathways('08 Apr') + (x_pathways.bandwidth()/2)})
  .attr("y", function(d) { return y_pathways(+565) + 25})
  .text('for 0-18 year olds')
  .attr("text-anchor", "end")

svg_pathways_fig
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_pathways('23 Apr') + (x_pathways.bandwidth()/2)})
  .attr("y", function(d) { return y_pathways(+292) - 45})
  .text('23rd April')
  .style('font-weight', 'bold')
  .attr("text-anchor", "start")

svg_pathways_fig
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_pathways('23 Apr') + (x_pathways.bandwidth()/2)})
  .attr("y", function(d) { return y_pathways(+292) - 35})
  .text('111 online reinstated')
  .attr("text-anchor", "start")

svg_pathways_fig
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_pathways('23 Apr') + (x_pathways.bandwidth()/2)})
  .attr("y", function(d) { return y_pathways(+292) - 25})
  .text('for 5-18 year olds')
  .attr("text-anchor", "start")

svg_pathways_fig
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_pathways('18 May') + (x_pathways.bandwidth()/2)})
  .attr("y", function(d) { return y_pathways(+334) - 35})
  .text('18 May')
  .style('font-weight', 'bold')
  .attr("text-anchor", "start")

svg_pathways_fig
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_pathways('18 May') + (x_pathways.bandwidth()/2)})
  .attr("y", function(d) { return y_pathways(+334) - 25})
  .text('Pathway case')
  .attr("text-anchor", "start")

svg_pathways_fig
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_pathways('18 May') + (x_pathways.bandwidth()/2)})
  .attr("y", function(d) { return y_pathways(+334) - 15})
  .text('definition change')
  .attr("text-anchor", "start")

svg_pathways_fig
  .append("text")
  .attr('id', 'test_milestones')
  .attr("x", function(d) { return x_pathways(pathway_dates[pathway_dates.length-5]) + (x_pathways.bandwidth()/2)})
  .attr("y", 25)
  .text(latest_p_summary.Triage_count + ' triages in last 24 hours')
  .style('font-weight', 'bold')
  .attr("fill", "#273f8a")
  .style('font-size', '1.8em')
  .attr("text-anchor", "end")

var dots_triages = svg_pathways_fig
  .selectAll('myoutbreakCircles')
  .data(pathways_df)
  .enter()
  .append("circle")
  .attr("cx", function(d) { return x_pathways(d.Date) + (x_pathways.bandwidth() /2) } )
  .attr("cy", function(d) { return y_pathways(+d.Triage_count) } )
  .attr("r", 3)
  .style("fill", '#000')
  // .attr("stroke", "white")
  .on('mousemove', showTooltip_pathways)
  .on('mouseout', mouseleave_pathways)

d3.select("#pathways_label_1")
  .html(function(d) {
    return latest_p_summary.label_1});

d3.select("#pathways_label_2")
  .html(function(d) {
    return latest_p_summary.label_2});
