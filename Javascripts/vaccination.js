var vaccine_update_date = "25th February 2021";
var vaccine_administered_date = "21st February 2021";

d3.select("#latest_vaccine_publication_date").html(function (d) {
  return (
    "The vaccination data for local areas was last updated on " +
    vaccine_update_date +
    " and includes vaccines administered up to " +
    vaccine_administered_date +
    "."
  );
});
