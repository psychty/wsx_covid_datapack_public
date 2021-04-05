
column_names = ['MSOA name', 'Number of cats', 'Proportion']

var svg_container_msoa_table = d3
  .select("#msoa_table_vaccines")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .attr('id', 'bumshackle')
  .append("g")

d3.select("#bumshackle").append("div")
  .attr("id", "FilterableTable");

d3.select("#FilterableTable").append("h1")
  .attr("id", "title")
  .text("My Youtube Channels")

d3.select("#FilterableTable").append("div")
  .attr("class", "SearchBar")
  .append("p")
    .attr("class", "SearchBar")
    .text("Search by MSOA name:");

d3.select(".SearchBar")
  .append("input")
    .attr("class", "SearchBar")
    .attr("id", "search")
    .attr("type", "text")
    .attr("placeholder", "Search...");

    var table = d3.select("#FilterableTable").append("table");
table.append("thead").append("tr"); 

var headers = table.select("tr").selectAll("th")
    .data(column_names)
  .enter()
    .append("th")
    .text(function(d) { return d; });

var rows, row_entries, row_entries_no_anchor, row_entries_with_anchor;
  