// This will be to highlight a particular line on the figure (and show some key figures)
d3.select("#select_line_positivity_button")
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

// Retrieve the selected area name
// var chosen_positivity_area = d3
//   .select("#select_line_positivity_button")
//   .property("value");

// ! Figure positivity

// ! Table

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/positivity_at_a_glance.json", false);
request.send(null);
var p_at_a_glance_all_ages = JSON.parse(request.responseText);

console.log(p_at_a_glance_all_ages);

window.onload = () => {
  loadTable_positivity(p_at_a_glance_all_ages);
};

d3.select("#positivity_text_1").html(function (d) {
  return (
    "The table below shows the number of PCR tests and the positivity for West Sussex districts and the regional and national comparision for the seven days to " +
    complete_date +
    "."
  );
});

d3.select("#positivity_date_heading_1").html(function (d) {
  return (
    "Number of people receiving a PCR (Polymerase chain reaction) test in the seven days to " +
    complete_date
  );
});

function loadTable_positivity(p_at_a_glance_all_ages) {
  const tableBody = document.getElementById("positivity_table_1");
  var dataHTML = "";

  for (let item of p_at_a_glance_all_ages) {
    dataHTML += `<tr><td>${item.Name}</td><td>${item.uniquePeopleTestedBySpecimenDateRollingSum}</td><td>${item.uniqueCasePositivityBySpecimenDateRollingSum}</td></tr>`;
  }

  // for (let item of at_a_glance_all_ages) {
  //   dataHTML += `<tr><td>${item.Name}</td><td>${d3.format(",.0f")(
  //     item.Rolling_7_day_new_cases
  //   )}</td><td>${d3.format(",.1f")(
  //     item.Rolling_7_day_new_cases_per_100000
  //   )}</td></tr>`;
  // }

  tableBody.innerHTML = dataHTML;
}
