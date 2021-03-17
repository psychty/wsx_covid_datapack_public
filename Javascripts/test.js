// var request = new XMLHttpRequest();
// request.open("GET", "./Outputs/vaccine_ltla_age.json", false);
// request.send(null);
// var vaccine_ltla_age = JSON.parse(request.responseText);

// This will be to highlight a particular line on the figure (and show some key figures)
d3.select("#select_guage_area_button")
  .selectAll("myOptions")
  .data([
    "West Sussex",
    "Adur",
    "Arun",
    "Chichester",
    "Crawley",
    "Horsham",
    "Mid Sussex",
    "Worthing",
  ])
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

// ! Guage percentage donut

var width_guage = 230;
var height_guage = width_guage;

var twoPi = 2 * Math.PI;

var attributed = 0;
var total = 1;

// Load the svgs before reading data

var svg_overall_vaccinated = d3
  .select("#overall_guage_1")
  .append("svg")
  .attr("width", width_guage)
  .attr("height", height_guage)
  .append("g")
  .attr(
    "transform",
    "translate(" + width_guage / 2 + "," + height_guage / 2 + ")"
  )
  .attr("class", "percentage_guage");

// Retrieve the selected area name
var selected_vaccine_area = d3
  .select("#select_guage_area_button")
  .property("value");

overall_cumulative = vaccine_at_a_glance.filter(function (d) {
  return d.Name === selected_vaccine_area;
});

number_vaccinated = overall_cumulative[0].Total_where_age_known;
proportion_vaccinated = overall_cumulative[0].Proportion_age_known;
estimated_population = overall_cumulative[0].Population_16_and_over;

var arc_vaccine_overall = d3
  .arc()
  .startAngle(0)
  .innerRadius(85)
  .outerRadius(115);

svg_overall_vaccinated
  .append("path")
  .attr("class", "background")
  .attr("d", arc_vaccine_overall.endAngle(twoPi));

var foreground_vaccinated = svg_overall_vaccinated
  .append("path")
  .attr("class", "foreground");

var Percent_vaccinated_1 = svg_overall_vaccinated
  .append("text")
  .attr("id", "vaccine_overall_perc")
  .attr("text-anchor", "middle")
  .attr("class", "percent-vaccine")
  .attr("dy", "-0.25em");
// .text(d3.format(".0%")(proportion_vaccinated));

svg_overall_vaccinated
  .append("text")
  .attr("text-anchor", "middle")
  .attr("id", "vaccinated_label_1")
  .attr("class", "description")
  .attr("dy", "0.5em")
  .text(
    d3.format(",.0f")(number_vaccinated) +
      " / " +
      d3.format(",.0f")(estimated_population)
  );

svg_overall_vaccinated
  .append("text")
  .attr("text-anchor", "middle")
  .attr("id", "deaths_label_2")
  .attr("class", "description")
  .attr("dy", "1.5em")
  .text("aged 16+ received");

svg_overall_vaccinated
  .append("text")
  .attr("text-anchor", "middle")
  .attr("id", "deaths_label_3")
  .attr("class", "description")
  .attr("dy", "2.5em")
  .text("at least one dose");

var i_vaccinated_prop = d3.interpolate(0, proportion_vaccinated);

svg_overall_vaccinated
  .transition()
  .duration(5000)
  .tween("vaccinated", function () {
    return function (t) {
      vaccinated = i_vaccinated_prop(t);
      foreground_vaccinated
        .attr("d", arc_vaccine_overall.endAngle(twoPi * vaccinated))
        .attr("fill", "#ff4f03");
      Percent_vaccinated_1.text(d3.format(".1%")(vaccinated));
    };
  });

// // Initialize the plot with the first dataset

// function update_attributable_risk(selectedCondition_attribOption) {
//   attributed_deaths = deaths_attributed[0].Proportion;
//   var old_attributed_deaths = attributed_deaths;

//   if (old_attributed_deaths === undefined) {
//     old_attributed_deaths = 0.001;
//   }

//   total_deaths = deaths_attributed[0].Total_burden;
//   var old_total_deaths = total_deaths;

//   attributed_yll = yll_attributed[0].Proportion;
//   var old_attributed_yll = attributed_yll;

//   if (old_attributed_yll === undefined) {
//     old_attributed_yll = 0.001;
//   }

//   var selectedCondition_attribOption = d3
//     .select("#selectCondition_attribButton")
//     .property("value");

//   // Select the div id total_death_string (this is where you want the result of this to be displayed in the html page)
//   d3.select("#selected_condition_attrib_title").text(function (d) {
//     return (
//       "Burden attributed to risk factors associated with " +
//       d3
//         .select("#selectCondition_attribButton")
//         .property("value")
//         .replace("All causes", "all causes of ill health") +
//       "; both males and females; all ages; West Sussex; 2017"
//     );
//   });

//   deaths_attributed = explained_burden.filter(function (d) {
//     return (
//       (d.Measure === "Deaths") & (d.Cause === selectedCondition_attribOption)
//     );
//   });

//   var number_deaths = deaths_attributed[0].Number;
//   var attributed_deaths = deaths_attributed[0].Proportion;
//   var total_deaths = deaths_attributed[0].Total_burden;
//   var i_deaths = d3.interpolate(
//     old_attributed_deaths,
//     attributed_deaths / total
//   );

//   meter_deaths.selectAll("#deaths_label_1").remove();

//   meter_deaths.selectAll("#deaths_label_2").remove();

//   meter_deaths.selectAll("#deaths_label_3").remove();

//   meter_deaths.selectAll("#deaths_label_4");

//   if (total_deaths === 0) {
//     meter_deaths.selectAll("#attributed_deaths_perc").style("opacity", 0);
//   }

//   if (total_deaths !== 0) {
//     meter_deaths
//       .selectAll("#attributed_deaths_perc")
//       .transition()
//       .duration(750)
//       .style("opacity", 1);
//   }

//   foreground_deaths
//     .transition()
//     .duration(1500)
//     .attr("fill", function (d) {
//       return color_cause_for_risk(selectedCondition_attribOption);
//     });

//   meter_deaths
//     .transition()
//     .duration(3000)
//     .tween("attributed", function () {
//       return function (t) {
//         attributed = i_deaths(t);
//         foreground_deaths.attr("d", arc_vaccine_overall.endAngle(twoPi * attributed));
//         percentAttributed_deaths.text(d3.format(".1%")(attributed));
//       };
//     });

//   if (total_deaths !== 0) {
//     meter_deaths
//       .append("text")
//       .attr("text-anchor", "middle")
//       .attr("id", "deaths_label_1")
//       .attr("class", "description")
//       .attr("dy", "0.5em")
//       .text(
//         d3.format(",.0f")(number_deaths) +
//           " / " +
//           d3.format(",.0f")(total_deaths)
//       );

//     meter_deaths
//       .append("text")
//       .attr("text-anchor", "middle")
//       .attr("id", "deaths_label_2")
//       .attr("class", "description")
//       .attr("dy", "1.5em")
//       .text("deaths attributed");

//     meter_deaths
//       .append("text")
//       .attr("text-anchor", "middle")
//       .attr("id", "deaths_label_3")
//       .attr("class", "description")
//       .attr("dy", "2.5em")
//       .text("to risk factors");
//   }

//   if (total_deaths === 0) {
//     meter_deaths
//       .append("text")
//       .attr("text-anchor", "middle")
//       .attr("id", "deaths_label_1")
//       .attr("class", "description_none")
//       .attr("dy", "-1em")
//       .text("There were no");

//     meter_deaths
//       .append("text")
//       .attr("text-anchor", "middle")
//       .attr("id", "deaths_label_2")
//       .attr("class", "description_none")
//       .attr("dy", "0em")
//       .text("deaths for this");

//     meter_deaths
//       .append("text")
//       .attr("text-anchor", "middle")
//       .attr("id", "deaths_label_3")
//       .attr("class", "description_none")
//       .attr("dy", "1em")
//       .text("condition group");
//   }
// }

// d3.select("#selectCondition_attribButton").on("change", function (d) {
//   var selectedCondition_attribOption = d3
//     .select("#selectCondition_attribButton")
//     .property("value");
//   update_attributable_risk(selectedCondition_attribOption);
// });
