var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_msoa_data.json", false);
request.send(null);
var vaccine_msoa_data = JSON.parse(request.responseText);

console.log(vaccine_msoa_data);

column_names = [
  "District",
  "MSOA name",
  "msoa11hclnm",
  "Number of individuals receiving at least one dose",
  "Proportion",
];

var svg_container_msoa_table = d3
  .select("#msoa_table_vaccines")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .attr("id", "msoa_vaccine_table_1")
  .append("g");

d3.select("#msoa_vaccine_table_1").append("div").attr("id", "FilterableTable");

d3.select("#FilterableTable")
  .append("h1")
  .attr("id", "title")
  .text("Local vaccination uptake");

d3.select("#FilterableTable")
  .append("div")
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

var mosa_table = d3.select("#FilterableTable").append("table");

mosa_table.append("thead").append("tr");

var headers = mosa_table
  .select("tr")
  .selectAll("th")
  .data(column_names)
  .enter()
  .append("th")
  .text(function (d) {
    return d;
  });

var rows, row_entries, row_entries_no_anchor, row_entries_with_anchor;

// draw table body with rows
mosa_table.append("tbody");

rows = mosa_table
  .select("tbody")
  .selectAll("tr")
  .data(vaccine_msoa_data, function (d) {
    return d.id;
  });

// enter the rows
rows.enter().append("tr");

// enter td's in each row
row_entries = rows
  .selectAll("td")
  .data(function (d) {
    var arr = [];
    for (var k in d) {
      if (d.hasOwnProperty(k)) {
        arr.push(d[k]);
      }
    }
    return [arr[3], arr[1], arr[2], arr[0]];
  })
  .enter()
  .append("td");

// draw row entries with no anchor
row_entries_no_anchor = row_entries.filter(function (d) {
  return /https?:\/\//.test(d) == false;
});
row_entries_no_anchor.text(function (d) {
  return d;
});

// draw row entries with anchor
row_entries_with_anchor = row_entries.filter(function (d) {
  return /https?:\/\//.test(d) == true;
});
row_entries_with_anchor
  .append("a")
  .attr("href", function (d) {
    return d;
  })
  .attr("target", "_blank")
  .text(function (d) {
    return d;
  });
