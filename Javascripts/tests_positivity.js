// ! Table

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/positivity_at_a_glance.json", false);
request.send(null);
var p_at_a_glance_all_ages = JSON.parse(request.responseText);

window.onload = () => {
  loadTable_positivity(p_at_a_glance_all_ages);
};

d3.select("#positivity_text_1").html(function (d) {
  return (
    "The table below shows the number of individuals tested (using PCR tests) and the PCR positivity for West Sussex districts and the regional and national comparision for the seven days to " +
    complete_date +
    ". It also shows the number of LFD tests conducted in the same time period. However, it should be noted that the LFD tests are not included in the measure of individuals tested or seven day positivity."
  );
});

d3.select("#positivity_date_heading_1").html(function (d) {
  return (
    "Number of people receiving a PCR (Polymerase chain reaction) test in the seven days to " +
    complete_date
  );
});

d3.select("#positivity_date_heading_2").html(function (d) {
  return (
    "Number of LFD (Lateral flow device) tests in the seven days to " +
    complete_date
  );
});

function loadTable_positivity(p_at_a_glance_all_ages) {
  const tableBody = document.getElementById("positivity_table_1");
  var dataHTML = "";

  for (let item of p_at_a_glance_all_ages) {
    dataHTML += `<tr><td>${item.Name}</td><td>${item.uniquePeopleTestedBySpecimenDateRollingSum}</td><td>${item.uniqueCasePositivityBySpecimenDateRollingSum}</td><td>${item.LFD_7_day_tests}</td></tr>`;
  }

  tableBody.innerHTML = dataHTML;
}

// ! Figure number of people tested

// This will be to highlight a particular line on the figure (and show some key figures)
d3.select("#select_line_tested_button")
  .selectAll("myOptions")
  .data(areas_1a)
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

// Create svgs
// Daily cases bar chart
var svg_pcr_tested_figure = d3
  .select("#pcr_tested_figure")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .append("g")
  .attr("transform", "translate(" + 80 + "," + 10 + ")");

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/positivity_df.json", false);
request.send(null);
var test_df = JSON.parse(request.responseText); // parse the fetched json data into a variable

var pcr_tested_dates = d3
  .map(test_df, function (d) {
    return d.Date_label;
  })
  .keys();

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/test_dates.json", false);
request.send(null);
var test_data_dates = JSON.parse(request.responseText).map(function (d) {
  return d.Date_label;
});

// Retrieve the selected area name
var selected_pcr_tested_area = d3
  .select("#select_line_tested_button")
  .property("value");

// Update text based on selected area
d3.select("#selected_pcr_tested_bars_1_compare_title").html(function (d) {
  return (
    "Number of people receiving a PCR (Polymerase chain reaction) test in the previous 7 days; " +
    selected_pcr_tested_area +
    "; up to " +
    complete_date +
    " as at " +
    data_refreshed_date
  );
});

var bars_7_day_pcr_chosen = test_df.filter(function (d) {
  return d.Name === selected_pcr_tested_area;
});

var x_pcr_tested = d3
  .scaleBand()
  .domain(pcr_tested_dates)
  .range([0, width_hm - 65]);

var xAxis_pcr_tested = svg_pcr_tested_figure
  .append("g")
  .attr("transform", "translate(0," + (height_line - 80) + ")")
  .call(d3.axisBottom(x_pcr_tested).tickValues(test_data_dates));

xAxis_pcr_tested
  .selectAll("text")
  .attr(
    "transform",
    "translate(-" + (x_pcr_tested.bandwidth() + 15) + ",10)rotate(-90)"
  )
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

var y_pcr_tested = d3
  .scaleLinear()
  .domain([
    0,
    d3.max(bars_7_day_pcr_chosen, function (d) {
      return +d.Seven_day_PCR_tested_individuals;
    }),
  ])
  .range([height_line - 80, 0])
  .nice();

var yAxis_pcr_tested = svg_pcr_tested_figure
  .append("g")
  .call(d3.axisLeft(y_pcr_tested));

yAxis_pcr_tested.selectAll("text").style("font-size", ".8rem");

var weekly_pcr_tested_bars = svg_pcr_tested_figure
  .selectAll("mybar")
  .data(bars_7_day_pcr_chosen)
  .enter()
  .append("rect")
  .attr("x", function (d) {
    return x_pcr_tested(d.Date_label);
  })
  .attr("y", function (d) {
    return y_pcr_tested(d.Seven_day_PCR_tested_individuals);
  })
  .attr("width", x_pcr_tested.bandwidth())
  .attr("height", function (d) {
    return height_line - 80 - y_pcr_tested(d.Seven_day_PCR_tested_individuals);
  })
  .attr("fill", function (d) {
    return "#a35112";
  })
  .style("opacity", 1);

// ! on change
function update_pcr_tested() {
  var selected_pcr_tested_area = d3
    .select("#select_line_tested_button")
    .property("value");

  // Update text based on selected area
  d3.select("#selected_pcr_tested_bars_1_compare_title").html(function (d) {
    return (
      "Number of people receiving a PCR (Polymerase chain reaction) test in the previous 7 days; " +
      selected_pcr_tested_area +
      "; up to " +
      complete_date +
      " as at " +
      data_refreshed_date
    );
  });

  var bars_7_day_pcr_chosen = test_df.filter(function (d) {
    return d.Name === selected_pcr_tested_area;
  });

  y_pcr_tested
    .domain([
      0,
      d3.max(bars_7_day_pcr_chosen, function (d) {
        return +d.Seven_day_PCR_tested_individuals;
      }),
    ])
    .nice();

  yAxis_pcr_tested.transition().duration(1000).call(d3.axisLeft(y_pcr_tested));

  yAxis_pcr_tested.selectAll("text").style("font-size", ".8rem");

  weekly_pcr_tested_bars
    .data(bars_7_day_pcr_chosen)
    .transition()
    .duration(1000)
    .attr("x", function (d) {
      return x_pcr_tested(d.Date_label);
    })
    .attr("y", function (d) {
      return y_pcr_tested(d.Seven_day_PCR_tested_individuals);
    })
    .attr("width", x_pcr_tested.bandwidth())
    .attr("height", function (d) {
      return (
        height_line - 80 - y_pcr_tested(d.Seven_day_PCR_tested_individuals)
      );
    })
    .attr("fill", function (d) {
      return "#a35112";
    })
    .style("opacity", 1);
}

d3.select("#select_line_tested_button").on("change", function (d) {
  var selected_pcr_tested_area = d3
    .select("#select_line_tested_button")
    .property("value");
  update_pcr_tested();
});
