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
var chosen_positivity_area = d3
  .select("#select_line_positivity_button")
  .property("value");

// ! Figure positivity

// ! Table
