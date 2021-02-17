var request = new XMLHttpRequest();
request.open("GET", "./Outputs/hospital_meta.json", false);
request.send(null);
var healthcare_dates = JSON.parse(request.responseText);

admission_date_label = healthcare_dates.filter(function (d) {
  return d.item == "Admissions";
})[0]["Date_label"];

occupied_date_label = healthcare_dates.filter(function (d) {
  return d.item == "Patients in hospital";
})[0]["Date_label"];

healthcare_publish_date_label = healthcare_dates.filter(function (d) {
  return d.item == "Publish date";
})[0]["Date_label"];

var trusts = [
  "England",
  "South East",
  "Brighton and Sussex University Hospitals NHS Trust",
  "Surrey and Sussex Healthcare NHS Trust",
  "Sussex Community NHS Foundation Trust",
  "Sussex Partnership NHS Foundation Trust",
  "Western Sussex Hospitals NHS Foundation Trust",
];

// This will be to highlight a particular line on the figure (and show some key figures)
d3.select("#select_trust_1_button")
  .selectAll("myOptions")
  .data(trusts)
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

// This will be to highlight a particular line on the figure (and show some key figures)
d3.select("#select_trust_2_button")
  .selectAll("myOptions")
  .data(trusts)
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

d3.select("#hospital_data_dates").html(function (d) {
  return (
    "The data available at NHS Trust level includes admissions or new diagnoses that occurred up to " +
    admission_date_label +
    ". It also includes an count of the number of COVID-19 positive patients in hospital as at 8am on " +
    occupied_date_label +
    ". Whilst data at region and national level are available more frequently, the Trust level data we present here is updated once per week on a Thursday for hospital activity occuring in the previous week."
  );
});

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/latest_hospital_summary.json", false);
request.send(null);
var healthcare_summary = JSON.parse(request.responseText);

// ! Table
loadTable_healthcare(healthcare_summary);

d3.select("#hospital_date_heading_1").html(function (d) {
  return (
    "Number of new admissions or diagnoses of COVID-19 in the seven days to " +
    admission_date_label
  );
});

d3.select("#hospital_date_heading_2").html(function (d) {
  return (
    "Total number of admissions or inpatient diagnoses of COVID-19 as at " +
    admission_date_label
  );
});

d3.select("#hospital_date_heading_3").html(function (d) {
  return (
    "Number of COVID-19+ patients occupying beds as at 8:00 am on " +
    occupied_date_label
  );
});

d3.select("#hospital_date_heading_4").html(function (d) {
  return (
    "Number of COVID-19+ patients occupying mechanical ventilation beds as at 8:00 am on " +
    occupied_date_label
  );
});

function loadTable_healthcare(healthcare_summary) {
  const tableBody_hc = document.getElementById("healthcare_table_1");
  var dataHTML_hc = "";

  for (let item of healthcare_summary) {
    dataHTML_hc += `<tr><td>${item.Name}</td><td>${d3.format(",.0f")(
      item.Rolling_7_day_admissions
    )}</td><td>${d3.format(",.0f")(item.Total_admissions)}</td><td>${d3.format(
      ",.0f"
    )(item.Patients_occupying_beds)}</td><td>${d3.format(",.0f")(
      item.Patients_occupying_mv_beds
    )}</td></tr>`;
  }

  tableBody_hc.innerHTML = dataHTML_hc;
}

// ! Rolling number of admissions

// Daily admissions bar chart
var svg_daily_admissions_bars = d3
  .select("#daily_admissions_bars")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .append("g")
  .attr("transform", "translate(" + 60 + "," + 10 + ")");

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/admissions_export_df.json", false);
request.send(null);
var daily_admissions = JSON.parse(request.responseText); // parse the fetched json data into a variable

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/admission_date_labels.json", false);
request.send(null);
var admission_data_dates = JSON.parse(request.responseText).map(function (d) {
  return d.Date_label;
});

var admissions_dates = d3
  .map(daily_admissions, function (d) {
    return d.Date_label;
  })
  .keys();

var selected_trust_1_area_option = d3
  .select("#select_trust_1_button")
  .property("value");

d3.select("#admissions_title_1").html(function (d) {
  return (
    "Daily number of new admissions or new positive results among inpatients;  " +
    selected_trust_1_area_option +
    "; data correct as at " +
    healthcare_publish_date_label
  );
});

var admissions_1_chosen = daily_admissions.filter(function (d) {
  return d.Name === selected_trust_1_area_option;
});

var x_daily_admissions = d3
  .scaleBand()
  .domain(admissions_dates)
  .range([0, width_hm - 60]);

var xAxis_admissions = svg_daily_admissions_bars
  .append("g")
  .attr("transform", "translate(0," + (height_line - 80) + ")")
  .call(d3.axisBottom(x_daily_admissions).tickValues(admission_data_dates));

xAxis_admissions
  .selectAll("text")
  .attr(
    "transform",
    "translate(-" + (x_daily_admissions.bandwidth() + 15) + ",10)rotate(-90)"
  )
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

var y_daily_admissions = d3
  .scaleLinear()
  .domain([
    0,
    d3.max(admissions_1_chosen, function (d) {
      return +d.Admissions_or_new_cases_in_last_24hrs;
    }),
  ])
  .range([height_line - 80, 0])
  .nice();

var yAxis_daily_admissions = svg_daily_admissions_bars
  .append("g")
  .call(d3.axisLeft(y_daily_admissions));

yAxis_daily_admissions.selectAll("text").style("font-size", ".8rem");

// daily admission bars

var change_ave_admissions = d3
  .scaleOrdinal()
  .domain(["Up", "Down", "Same"])
  .range([
    "admissions were going up",
    "admissions were coming down",
    "admissions were the same",
  ]);

// Bars
var tooltip_daily_admissions_1 = d3
  .select("#daily_admissions_bars")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip_class")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("padding", ".7rem");

var showTooltip_daily_admissions_1 = function (d) {
  tooltip_daily_admissions_1
    .html(
      "<h5>" +
        d.Name +
        "</h5><p class = 'tt_text'><b>" +
        d.Date_label +
        "</b></p><p class = 'tt_text'><b>" +
        d3.format(",.0f")(d.Admissions_or_new_cases_in_last_24hrs) +
        ' new admissions</b></p><p class = "tt_text">In the seven days to ' +
        d.Date_label +
        " there were " +
        d3.format(",.0f")(d.Rolling_7_day_admissions) +
        " admissions or new diagnoses among inpatients in NHS hospitals, which means on average, <b>" +
        d3.format(",.0f")(d.Rolling_average_7_day_admissions) +
        "</b> new patients were admitted each day.</p><p>At this point in the pandemic, the 7 day rolling number of new " +
        change_ave_admissions(d.Perc_change_rolling_7_day_admissions) +
        " compared to the previous week."
    )
    .style("opacity", 1)
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("opacity", 1)
    .style("visibility", "visible");
};

var mouseleave_daily_admissions_1 = function (d) {
  tooltip_daily_admissions_1.style("opacity", 0).style("visibility", "hidden");
};

var daily_admissions_bars = svg_daily_admissions_bars
  .selectAll("mybar")
  .data(admissions_1_chosen)
  .enter()
  .append("rect")
  .attr("x", function (d) {
    return x_daily_admissions(d.Date_label);
  })
  .attr("y", function (d) {
    return y_daily_admissions(d.Admissions_or_new_cases_in_last_24hrs);
  })
  .attr("width", x_daily_admissions.bandwidth())
  .attr("height", function (d) {
    return (
      height_line -
      80 -
      y_daily_admissions(d.Admissions_or_new_cases_in_last_24hrs)
    );
  })
  .attr("fill", function (d) {
    return "#901020";
  })
  .style("opacity", 0.75)
  .on("mousemove", showTooltip_daily_admissions_1)
  .on("mouseout", mouseleave_daily_admissions_1);

var daily_average_admission_line = svg_daily_admissions_bars
  .append("path")
  .datum(admissions_1_chosen)
  .attr("fill", "none")
  .attr("stroke", "#000000")
  .attr("stroke-width", 2)
  .attr(
    "d",
    d3
      .line()
      .defined((d) => !isNaN(d.Rolling_average_7_day_admissions))
      .x(function (d) {
        return (
          x_daily_admissions(d.Date_label) + x_daily_admissions.bandwidth() / 2
        );
      })
      .y(function (d) {
        return y_daily_admissions(d.Rolling_average_7_day_admissions);
      })
  );

// ! on change
function update_daily_admissions() {
  var selected_trust_1_area_option = d3
    .select("#select_trust_1_button")
    .property("value");

  var admissions_1_chosen = daily_admissions.filter(function (d) {
    return d.Name === selected_trust_1_area_option;
  });

  // Update text based on selected area
  d3.select("#admissions_title_1").html(function (d) {
    return (
      "Daily number of new admissions or new positive results among inpatients;  " +
      selected_trust_1_area_option +
      "; data correct as at " +
      healthcare_publish_date_label
    );
  });

  var showTooltip_daily_admissions_1 = function (d) {
    tooltip_daily_admissions_1
      .html(
        "<h5>" +
          d.Name +
          "</h5><p class = 'tt_text'><b>" +
          d.Date_label +
          "</b></p><p class = 'tt_text'><b>" +
          d3.format(",.0f")(d.Admissions_or_new_cases_in_last_24hrs) +
          ' new admissions</b></p><p class = "tt_text">In the seven days to ' +
          d.Date_label +
          " there were " +
          d3.format(",.0f")(d.Rolling_7_day_admissions) +
          " admissions or new diagnoses among inpatients in NHS hospitals, which means on average, <b>" +
          d3.format(",.0f")(d.Rolling_average_7_day_admissions) +
          "</b> new patients were admitted each day.</p><p>At this point in the pandemic, the 7 day rolling number of new " +
          change_ave_admissions(d.Perc_change_rolling_7_day_admissions) +
          " compared to the previous week."
      )
      .style("opacity", 1)
      .style("top", event.pageY - 10 + "px")
      .style("left", event.pageX + 10 + "px")
      .style("opacity", 1)
      .style("visibility", "visible");
  };

  y_daily_admissions
    .domain([
      0,
      d3.max(admissions_1_chosen, function (d) {
        return +d.Admissions_or_new_cases_in_last_24hrs;
      }),
    ])
    .nice();

  // Redraw axis
  yAxis_daily_admissions
    .transition()
    .duration(1000)
    .call(d3.axisLeft(y_daily_admissions));

  yAxis_daily_admissions.selectAll("text").style("font-size", ".8rem");

  daily_average_admission_line
    .datum(admissions_1_chosen)
    .transition()
    .duration(1000)
    .attr(
      "d",
      d3
        .line()
        .defined((d) => !isNaN(d.Rolling_average_7_day_admissions))
        .x(function (d) {
          return (
            x_daily_admissions(d.Date_label) +
            x_daily_admissions.bandwidth() / 2
          );
        })
        .y(function (d) {
          return y_daily_admissions(d.Rolling_average_7_day_admissions);
        })
    );

  daily_admissions_bars
    .data(admissions_1_chosen)
    .transition()
    .duration(1000)
    .attr("x", function (d) {
      return x_daily_admissions(d.Date_label);
    })
    .attr("y", function (d) {
      return y_daily_admissions(d.Admissions_or_new_cases_in_last_24hrs);
    })
    // .attr("width", x_daily_cases.bandwidth())
    .attr("height", function (d) {
      return (
        height_line -
        80 -
        y_daily_admissions(d.Admissions_or_new_cases_in_last_24hrs)
      );
    })
    .attr("fill", function (d) {
      return "#901020";
    })
    .style("opacity", 0.75);

  daily_admissions_bars
    .on("mousemove", showTooltip_daily_admissions_1)
    .on("mouseout", mouseleave_daily_admissions_1);
}

d3.select("#select_trust_1_button").on("change", function (d) {
  var selected_trust_1_area_option = d3
    .select("#select_trust_1_button")
    .property("value");
  update_daily_admissions();
});

// ! Patients in hospital

// Daily beds occupied bar chart
var svg_daily_patients_bars = d3
  .select("#daily_occupied_beds_bars")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_line)
  .append("g")
  .attr("transform", "translate(" + 60 + "," + 10 + ")");

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/beds_occupied_export_df.json", false);
request.send(null);
var daily_beds_occupied = JSON.parse(request.responseText); // parse the fetched json data into a variable

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/occupied_date_labels.json", false);
request.send(null);
var occupied_data_dates = JSON.parse(request.responseText).map(function (d) {
  return d.Date_label;
});

var beds_ouccupied_dates = d3
  .map(daily_beds_occupied, function (d) {
    return d.Date_label;
  })
  .keys();

var x_daily_beds_occupied = d3
  .scaleBand()
  .domain(beds_ouccupied_dates)
  .range([0, width_hm - 60]);

var xAxis_beds_occupied = svg_daily_patients_bars
  .append("g")
  .attr("transform", "translate(0," + (height_line - 80) + ")")
  .call(d3.axisBottom(x_daily_beds_occupied).tickValues(occupied_data_dates));

xAxis_beds_occupied
  .selectAll("text")
  .attr(
    "transform",
    "translate(-" + (x_daily_beds_occupied.bandwidth() + 15) + ",10)rotate(-90)"
  )
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

var bed_type = ["Patients_occupying_non_mv_beds", "Patients_occupying_mv_beds"];

var bed_type_label = d3
  .scaleOrdinal()
  .domain(bed_type)
  .range([
    "Patients occupying other beds",
    "Patients occupying beds capable of mechanical ventilation",
  ]);

var colour_covid_bed_type = d3
  .scaleOrdinal()
  .domain(bed_type_label)
  .range(["#006900", "#669900"]);

bed_type.forEach(function (item, index) {
  var list = document.createElement("li");
  list.innerHTML = bed_type_label(item);
  list.className = "key_list";
  list.style.borderColor = colour_covid_bed_type(index);
  var tt = document.createElement("div");
  tt.className = "side_tt";
  tt.style.borderColor = colour_covid_bed_type(index);
  var tt_h3_1 = document.createElement("h3");
  tt_h3_1.innerHTML = item;
  tt.appendChild(tt_h3_1);
  var div = document.getElementById("covid_bed_type_key");
  div.appendChild(list);
});

var selected_trust_2_area_option = d3
  .select("#select_trust_2_button")
  .property("value");

d3.select("#admissions_title_2").html(function (d) {
  return (
    "Number of COVID-19 positive inpatients in hospitals;  " +
    selected_trust_2_area_option +
    "; data correct as at " +
    healthcare_publish_date_label
  );
});

var beds_occupied_1_chosen = daily_beds_occupied.filter(function (d) {
  return d.Name === selected_trust_2_area_option;
});

var stackedData_beds_1 = d3.stack().keys(bed_type)(beds_occupied_1_chosen);

var y_daily_beds_occupied = d3
  .scaleLinear()
  .domain([
    0,
    d3.max(beds_occupied_1_chosen, function (d) {
      return +d.Patients_occupying_beds;
    }),
  ])
  .range([height_line - 80, 0])
  .nice();

var yAxis_daily_beds_occupied = svg_daily_patients_bars
  .append("g")
  .call(d3.axisLeft(y_daily_beds_occupied));

yAxis_daily_beds_occupied.selectAll("text").style("font-size", ".8rem");

var change_beds_occupied = d3
  .scaleOrdinal()
  .domain(["Up", "Down", "Same"])
  .range(["were going up", "were coming down", "were the same"]);

// Create a tooltip for the lines and functions for displaying the tooltips as well as highlighting certain lines.
var tooltip_beds_stacked_1 = d3
  .select("#daily_occupied_beds_bars")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip_class")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("padding", "10px");

var showTooltip_beds_occupied_1 = function (d, i) {
  var bed_type_Name = d3.select(this.parentNode).datum().key;
  var bed_type_Value = d.data[bed_type_Name];

  tooltip_beds_stacked_1
    .html(
      "<h5>" +
        d.data.Name +
        '</h5><b class = "side">' +
        d3.format(",.0f")(bed_type_Value) +
        " " +
        bed_type_label(bed_type_Name).replace("Patients", "patients") +
        " as at " +
        d.data.Date_label +
        '.</b ><p class = "side">There were a total of <b>' +
        d3.format(",.0f")(d.data.Patients_occupying_beds) +
        " patients</b> occupying beds as at 8:00am on " +
        d.data.Date_label +
        '.</p><p class="side">Compared to the previous week, the number of patients in beds ' +
        change_beds_occupied(d.data.Change_direction) +
        "</p>"
    )
    .style("opacity", 1)
    .attr("visibility", "visible")
    .style("top", event.pageY - 10 + "px")
    .style("left", event.pageX + 10 + "px")
    .style("visibility", "visible");
};

var mouseleave_beds_occupied_1 = function (d) {
  tooltip_beds_stacked_1.style("visibility", "hidden");
};

// daily patients bars

var bars_beds_occupied_1 = svg_daily_patients_bars
  .append("g")
  .selectAll("g")
  .data(stackedData_beds_1)
  .enter()
  .append("g")
  .attr("fill", function (d) {
    return colour_covid_bed_type(d.key);
  })
  .selectAll("rect")
  .data(function (d) {
    return d;
  })
  .enter()
  .append("rect")
  .attr("id", "bars1")
  .attr("x", function (d) {
    return x_daily_beds_occupied(d.data.Date_label);
  })
  .attr("y", function (d) {
    return y_daily_beds_occupied(d[1]);
  })
  .attr("height", function (d) {
    return y_daily_beds_occupied(d[0]) - y_daily_beds_occupied(d[1]);
  })
  .attr("width", x_daily_beds_occupied.bandwidth())
  .on("mousemove", showTooltip_beds_occupied_1)
  .on("mouseout", mouseleave_beds_occupied_1);

// ! Update beds
function update_beds_occupied_1() {
  var selected_trust_2_area_option = d3
    .select("#select_trust_2_button")
    .property("value");

  d3.select("#admissions_title_2").html(function (d) {
    return (
      "Number of COVID-19 positive inpatients in hospitals;  " +
      selected_trust_2_area_option +
      "; data correct as at " +
      healthcare_publish_date_label
    );
  });

  var beds_occupied_1_chosen = daily_beds_occupied.filter(function (d) {
    return d.Name === selected_trust_2_area_option;
  });

  var stackedData_beds_1 = d3.stack().keys(bed_type)(beds_occupied_1_chosen);

  y_daily_beds_occupied
    .domain([
      0,
      d3.max(beds_occupied_1_chosen, function (d) {
        return +d.Patients_occupying_beds;
      }),
    ])
    .range([height_line - 80, 0])
    .nice();

  yAxis_daily_beds_occupied
    .transition()
    .duration(1000)
    .call(d3.axisLeft(y_daily_beds_occupied));

  yAxis_daily_beds_occupied.selectAll("text").style("font-size", ".8rem");

  svg_daily_patients_bars.selectAll("#bars1").remove();

  var bars_beds_occupied_1 = svg_daily_patients_bars
    .append("g")
    .selectAll("g")
    .data(stackedData_beds_1)
    .enter()
    .append("g")
    .attr("fill", function (d) {
      return colour_covid_bed_type(d.key);
    })
    .selectAll("rect")
    .data(function (d) {
      return d;
    })
    .enter()
    .append("rect")
    .attr("id", "bars1")
    .attr("x", function (d) {
      return x_daily_beds_occupied(d.data.Date_label);
    })
    .attr("y", function (d) {
      return y_daily_beds_occupied(d[1]);
    })
    .attr("height", function (d) {
      return y_daily_beds_occupied(d[0]) - y_daily_beds_occupied(d[1]);
    })
    .attr("width", x_daily_beds_occupied.bandwidth());

  bars_beds_occupied_1
    .on("mousemove", showTooltip_beds_occupied_1)
    .on("mouseout", mouseleave_beds_occupied_1);
}

update_beds_occupied_1();

d3.select("#select_trust_2_button").on("change", function (d) {
  var selected_trust_2_area_option = d3
    .select("#select_trust_2_button")
    .property("value");
  update_beds_occupied_1();
});

// ! bed share

// Daily beds occupied bar chart
// var svg_daily_patients_share_ga_bars = d3
//   .select("#daily_occupied_beds_as_share_ga_bars")
//   .append("svg")
//   .attr("width", width_hm)
//   .attr("height", height_line)
//   .append("g")
//   .attr("transform", "translate(" + 60 + "," + 10 + ")");

// var request = new XMLHttpRequest();
// request.open("GET", "./Outputs/patient_share_occupied_beds.json", false);
// request.send(null);
// var daily_beds_ga_occupied = JSON.parse(request.responseText); // parse the fetched json data into a variable

// var request = new XMLHttpRequest();
// request.open("GET", "./Outputs/bed_share_ga_date_labels.json", false);
// request.send(null);
// var occupied_bed_ga_dates = JSON.parse(request.responseText).map(function (d) {
//   return d.Date_label;
// });

// var beds_ouccupied_dates = d3
//   .map(daily_beds_occupied, function (d) {
//     return d.Date_label;
//   })
//   .keys();

// var x_daily_beds_occupied = d3
//   .scaleBand()
//   .domain(beds_ouccupied_dates)
//   .range([0, width_hm - 60]);

// var xAxis_beds_occupied = svg_daily_patients_bars
//   .append("g")
//   .attr("transform", "translate(0," + (height_line - 80) + ")")
//   .call(d3.axisBottom(x_daily_beds_occupied).tickValues(occupied_data_dates));

// xAxis_beds_occupied
//   .selectAll("text")
//   .attr(
//     "transform",
//     "translate(-" + (x_daily_beds_occupied.bandwidth() + 15) + ",10)rotate(-90)"
//   )
//   .style("text-anchor", "end")
//   .style("font-size", ".8rem");

// var bed_type = ["Patients_occupying_non_mv_beds", "Patients_occupying_mv_beds"];

// var bed_type_label = d3
//   .scaleOrdinal()
//   .domain(bed_type)
//   .range([
//     "Patients occupying other beds",
//     "Patients occupying beds capable of mechanical ventilation",
//   ]);

// var colour_covid_bed_type = d3
//   .scaleOrdinal()
//   .domain(bed_type_label)
//   .range(["#006900", "#669900"]);

// bed_type.forEach(function (item, index) {
//   var list = document.createElement("li");
//   list.innerHTML = bed_type_label(item);
//   list.className = "key_list";
//   list.style.borderColor = colour_covid_bed_type(index);
//   var tt = document.createElement("div");
//   tt.className = "side_tt";
//   tt.style.borderColor = colour_covid_bed_type(index);
//   var tt_h3_1 = document.createElement("h3");
//   tt_h3_1.innerHTML = item;
//   tt.appendChild(tt_h3_1);
//   var div = document.getElementById("covid_bed_type_key");
//   div.appendChild(list);
// });

// var selected_trust_2_area_option = d3
//   .select("#select_trust_2_button")
//   .property("value");

// d3.select("#admissions_title_2").html(function (d) {
//   return (
//     "Number of COVID-19 positive inpatients in hospitals;  " +
//     selected_trust_2_area_option +
//     "; data correct as at " +
//     healthcare_publish_date_label
//   );
// });

// var beds_occupied_1_chosen = daily_beds_occupied.filter(function (d) {
//   return d.Name === selected_trust_2_area_option;
// });

// var stackedData_beds_1 = d3.stack().keys(bed_type)(beds_occupied_1_chosen);

// var y_daily_beds_occupied = d3
//   .scaleLinear()
//   .domain([
//     0,
//     d3.max(beds_occupied_1_chosen, function (d) {
//       return +d.Patients_occupying_beds;
//     }),
//   ])
//   .range([height_line - 80, 0])
//   .nice();

// var yAxis_daily_beds_occupied = svg_daily_patients_bars
//   .append("g")
//   .call(d3.axisLeft(y_daily_beds_occupied));

// yAxis_daily_beds_occupied.selectAll("text").style("font-size", ".8rem");

// var change_beds_occupied = d3
//   .scaleOrdinal()
//   .domain(["Up", "Down", "Same"])
//   .range(["were going up", "were coming down", "were the same"]);

// // Create a tooltip for the lines and functions for displaying the tooltips as well as highlighting certain lines.
// var tooltip_beds_stacked_1 = d3
//   .select("#daily_occupied_beds_bars")
//   .append("div")
//   .style("opacity", 0)
//   .attr("class", "tooltip_class")
//   .style("position", "absolute")
//   .style("z-index", "10")
//   .style("background-color", "white")
//   .style("border", "solid")
//   .style("border-width", "1px")
//   .style("border-radius", "5px")
//   .style("padding", "10px");

// var showTooltip_beds_occupied_1 = function (d, i) {
//   var bed_type_Name = d3.select(this.parentNode).datum().key;
//   var bed_type_Value = d.data[bed_type_Name];

//   tooltip_beds_stacked_1
//     .html(
//       "<h5>" +
//         d.data.Name +
//         '</h5><b class = "side">' +
//         d3.format(",.0f")(bed_type_Value) +
//         " " +
//         bed_type_label(bed_type_Name).replace("Patients", "patients") +
//         " as at " +
//         d.data.Date_label +
//         '.</b ><p class = "side">There were a total of <b>' +
//         d3.format(",.0f")(d.data.Patients_occupying_beds) +
//         " patients</b> occupying beds as at 8:00am on " +
//         d.data.Date_label +
//         '.</p><p class="side">Compared to the previous week, the number of patients in beds ' +
//         change_beds_occupied(d.data.Change_direction) +
//         "</p>"
//     )
//     .style("opacity", 1)
//     .attr("visibility", "visible")
//     .style("top", event.pageY - 10 + "px")
//     .style("left", event.pageX + 10 + "px")
//     .style("visibility", "visible");
// };

// var mouseleave_beds_occupied_1 = function (d) {
//   tooltip_beds_stacked_1.style("visibility", "hidden");
// };

// // daily patients bars

// var bars_beds_occupied_1 = svg_daily_patients_bars
//   .append("g")
//   .selectAll("g")
//   .data(stackedData_beds_1)
//   .enter()
//   .append("g")
//   .attr("fill", function (d) {
//     return colour_covid_bed_type(d.key);
//   })
//   .selectAll("rect")
//   .data(function (d) {
//     return d;
//   })
//   .enter()
//   .append("rect")
//   .attr("id", "bars1")
//   .attr("x", function (d) {
//     return x_daily_beds_occupied(d.data.Date_label);
//   })
//   .attr("y", function (d) {
//     return y_daily_beds_occupied(d[1]);
//   })
//   .attr("height", function (d) {
//     return y_daily_beds_occupied(d[0]) - y_daily_beds_occupied(d[1]);
//   })
//   .attr("width", x_daily_beds_occupied.bandwidth())
//   .on("mousemove", showTooltip_beds_occupied_1)
//   .on("mouseout", mouseleave_beds_occupied_1);

// // ! Update beds
// function update_beds_occupied_1() {
//   var selected_trust_2_area_option = d3
//     .select("#select_trust_2_button")
//     .property("value");

//   d3.select("#admissions_title_2").html(function (d) {
//     return (
//       "Number of COVID-19 positive inpatients in hospitals;  " +
//       selected_trust_2_area_option +
//       "; data correct as at " +
//       healthcare_publish_date_label
//     );
//   });

//   var beds_occupied_1_chosen = daily_beds_occupied.filter(function (d) {
//     return d.Name === selected_trust_2_area_option;
//   });

//   var stackedData_beds_1 = d3.stack().keys(bed_type)(beds_occupied_1_chosen);

//   y_daily_beds_occupied
//     .domain([
//       0,
//       d3.max(beds_occupied_1_chosen, function (d) {
//         return +d.Patients_occupying_beds;
//       }),
//     ])
//     .range([height_line - 80, 0])
//     .nice();

//   yAxis_daily_beds_occupied
//     .transition()
//     .duration(1000)
//     .call(d3.axisLeft(y_daily_beds_occupied));

//   yAxis_daily_beds_occupied.selectAll("text").style("font-size", ".8rem");

//   svg_daily_patients_bars.selectAll("#bars1").remove();

//   var bars_beds_occupied_1 = svg_daily_patients_bars
//     .append("g")
//     .selectAll("g")
//     .data(stackedData_beds_1)
//     .enter()
//     .append("g")
//     .attr("fill", function (d) {
//       return colour_covid_bed_type(d.key);
//     })
//     .selectAll("rect")
//     .data(function (d) {
//       return d;
//     })
//     .enter()
//     .append("rect")
//     .attr("id", "bars1")
//     .attr("x", function (d) {
//       return x_daily_beds_occupied(d.data.Date_label);
//     })
//     .attr("y", function (d) {
//       return y_daily_beds_occupied(d[1]);
//     })
//     .attr("height", function (d) {
//       return y_daily_beds_occupied(d[0]) - y_daily_beds_occupied(d[1]);
//     })
//     .attr("width", x_daily_beds_occupied.bandwidth());

//   bars_beds_occupied_1
//     .on("mousemove", showTooltip_beds_occupied_1)
//     .on("mouseout", mouseleave_beds_occupied_1);
// }

// update_beds_occupied_1();

// d3.select("#select_trust_2_button").on("change", function (d) {
//   var selected_trust_2_area_option = d3
//     .select("#select_trust_2_button")
//     .property("value");
//   update_beds_occupied_1();
// });
