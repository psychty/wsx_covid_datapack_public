var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_update_date.json", false);
request.send(null);
var vaccine_update_date = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_administered_date.json", false);
request.send(null);
var vaccine_administered_date = JSON.parse(request.responseText);

column_names = [
  "Local Authority",
  "MSOA name",
  "Number of individuals receiving at least one dose",
  "Number aged 45+ receiving at least one dose",
  "Rank of uptake (%) aged 45+ within Local Authority area",
  "Proportion aged 45+ receiving at least one dose",
  "Estimated number of people aged 45+ still to receive their first dose",
  "Number aged 65+ receiving at least one dose",
  "Rank of uptake (%) aged 65+ within Local Authority area",
  "Proportion aged 65+ receiving at least one dose",
  "Estimated number of people aged 65+ still to receive their first dose",
];

var clicks = {
  Total_where_age_known: 0,
  Age_45_and_over: 0,
  Proportion_rank_45_plus_within_LTLA: 0,
  Proportion_45_plus: 0,
  Estimated_left_to_vaccinate_45_plus: 0,
  Age_65_and_over: 0,
  Proportion_rank_65_plus_within_LTLA: 0,
  Proportion_65_plus: 0,
  Estimated_left_to_vaccinate_65_plus: 0,
};

// draw the table

d3.select("#container_search")
  .append("div")
  .attr("class", "SearchBar")
  .append("p")
  .attr("class", "SearchBar")
  .text("Search By Local Authority:");

d3.select(".SearchBar")
  .append("input")
  .attr("class", "SearchBar")
  .attr("id", "search_ltla")
  .attr("type", "text")
  .attr("placeholder", "Search...");

d3.select("#container_search_msoa")
  .append("div")
  .attr("class", "SearchBar1")
  .append("p")
  .attr("class", "SearchBar")
  .text("Search By MSOA:");

d3.select(".SearchBar1")
  .append("input")
  .attr("class", "SearchBar")
  .attr("id", "search_msoa")
  .attr("type", "text")
  .attr("placeholder", "Search...");

var msoa_table = d3.select("#msoa_table_vaccines").append("table");

msoa_table.append("thead").append("tr");

var headers = msoa_table
  .select("tr")
  .selectAll("th")
  .data(column_names)
  .enter()
  .append("th")
  .text(function (d) {
    return d;
  });

var rows, row_entries, row_entries_no_anchor, row_entries_with_anchor;

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/vaccine_msoa_explore_data.json", false);
request.send(null);
var vaccine_msoa_data = JSON.parse(request.responseText);

// draw table body with rows
msoa_table.append("tbody").attr("class", "msoa_table");

// data bind
rows = msoa_table
  .select("tbody")
  .selectAll("tr")
  .attr("class", "msoa_table")
  .data(vaccine_msoa_data, function (d) {
    return d.MSOA_name;
  });

// enter the rows
rows.enter().append("tr").attr("class", "msoa_table");

// enter td's in each row
row_entries = rows
  .selectAll("td")
  .attr("class", "msoa_table")
  .data(function (d) {
    var arr = [];
    for (var k in d) {
      if (d.hasOwnProperty(k)) {
        arr.push(d[k]);
      }
    }
    return [
      arr[0],
      arr[1],
      d3.format(",.0f")(arr[2]),
      d3.format(",.0f")(arr[3]),
      arr[4],
      d3.format(".1%")(arr[5]),
      d3.format(",.0f")(arr[6]),
      d3.format(",.0f")(arr[7]),
      arr[8],
      d3.format(".1%")(arr[9]),
      d3.format(",.0f")(arr[10]),
    ];
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

function initial_table_load() {
  var searched_data = vaccine_msoa_data,
    text = "adur";

  var searchResults = searched_data.map(function (r) {
    var regex = new RegExp("^" + text + ".*", "i");
    if (regex.test(r.LTLA_name)) {
      // if there are any results
      return regex.exec(r.LTLA_name)[0]; // return them to searchResults
    }
  });

  // filter blank entries from searchResults
  searchResults = searchResults.filter(function (r) {
    return r != undefined;
  });

  // filter dataset with searchResults
  searched_data = searchResults.map(function (r) {
    return vaccine_msoa_data.filter(function (p) {
      return p.LTLA_name.indexOf(r) != -1;
    });
  });

  // flatten array
  searched_data = [].concat.apply([], searched_data);

  // data bind with new data
  rows = msoa_table
    .select("tbody")
    .selectAll("tr")
    .data(searched_data, function (d) {
      return d.MSOA_name;
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
      return [
        arr[0],
        arr[1],
        d3.format(",.0f")(arr[2]),
        d3.format(",.0f")(arr[3]),
        arr[4],
        d3.format(".1%")(arr[5]),
        d3.format(",.0f")(arr[6]),
        d3.format(",.0f")(arr[7]),
        arr[8],
        d3.format(".1%")(arr[9]),
        d3.format(",.0f")(arr[10]),
      ];
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

  // exit
  rows.exit().remove();
}

initial_table_load();

// ! search functionality

d3.select("#search_ltla").on("keyup", function () {
  // filter according to key pressed
  var searched_data = vaccine_msoa_data,
    text = this.value.trim();

  var searchResults = searched_data.map(function (r) {
    var regex = new RegExp("^" + text + ".*", "i");
    if (regex.test(r.LTLA_name)) {
      // if there are any results
      return regex.exec(r.LTLA_name)[0]; // return them to searchResults
    }
  });

  // filter blank entries from searchResults
  searchResults = searchResults.filter(function (r) {
    return r != undefined;
  });

  // filter dataset with searchResults
  searched_data = searchResults.map(function (r) {
    return vaccine_msoa_data.filter(function (p) {
      return p.LTLA_name.indexOf(r) != -1;
    });
  });

  // flatten array
  searched_data = [].concat.apply([], searched_data);

  // data bind with new data
  rows = msoa_table
    .select("tbody")
    .selectAll("tr")
    .data(searched_data, function (d) {
      return d.MSOA_name;
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
      return [
        arr[0],
        arr[1],
        d3.format(",.0f")(arr[2]),
        d3.format(",.0f")(arr[3]),
        arr[4],
        d3.format(".1%")(arr[5]),
        d3.format(",.0f")(arr[6]),
        d3.format(",.0f")(arr[7]),
        arr[8],
        d3.format(".1%")(arr[9]),
        d3.format(",.0f")(arr[10]),
      ];
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

  // exit
  rows.exit().remove();
});

d3.select("#search_msoa").on("keyup", function () {
  // filter according to key pressed
  var searched_data = vaccine_msoa_data,
    text = this.value.trim();

  var searchResults = searched_data.map(function (r) {
    var regex = new RegExp("^" + text + ".*", "i");
    if (regex.test(r.MSOA_name)) {
      // if there are any results
      return regex.exec(r.MSOA_name)[0]; // return them to searchResults
    }
  });

  // filter blank entries from searchResults
  searchResults = searchResults.filter(function (r) {
    return r != undefined;
  });

  // filter dataset with searchResults
  searched_data = searchResults.map(function (r) {
    return vaccine_msoa_data.filter(function (p) {
      return p.MSOA_name.indexOf(r) != -1;
    });
  });

  // flatten array
  searched_data = [].concat.apply([], searched_data);

  // data bind with new data
  rows = msoa_table
    .select("tbody")
    .selectAll("tr")
    .data(searched_data, function (d) {
      return d.MSOA_name;
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
      return [
        arr[0],
        arr[1],
        d3.format(",.0f")(arr[2]),
        d3.format(",.0f")(arr[3]),
        arr[4],
        d3.format(".1%")(arr[5]),
        d3.format(",.0f")(arr[6]),
        d3.format(",.0f")(arr[7]),
        arr[8],
        d3.format(".1%")(arr[9]),
        d3.format(",.0f")(arr[10]),
      ];
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

  // exit
  rows.exit().remove();
});

// ! Create sort function for every column you want to be sortable - the code will depend on the type of data

headers.on("click", function (d) {
  if (d == "Number of individuals receiving at least one dose") {
    clicks.Total_where_age_known++;
    // even number of clicks
    if (clicks.Total_where_age_known % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.Total_where_age_known < +b.Total_where_age_known) {
          return -1;
        } else if (+a.Total_where_age_known > +b.Total_where_age_known) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Total_where_age_known % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.Total_where_age_known < +b.Total_where_age_known) {
          return 1;
        } else if (+a.Total_where_age_known > +b.Total_where_age_known) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == "Number aged 45+ receiving at least one dose") {
    clicks.Age_45_and_over++;
    // even number of clicks
    if (clicks.Age_45_and_over % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.Age_45_and_over < +b.Age_45_and_over) {
          return -1;
        } else if (+a.Age_45_and_over > +b.Age_45_and_over) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Age_45_and_over % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.Age_45_and_over < +b.Age_45_and_over) {
          return 1;
        } else if (+a.Age_45_and_over > +b.Age_45_and_over) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == "Rank of uptake (%) aged 45+ within Local Authority area") {
    clicks.Proportion_rank_45_plus_within_LTLA++;
    // even number of clicks
    if (clicks.Proportion_rank_45_plus_within_LTLA % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (
          +a.Proportion_rank_45_plus_within_LTLA <
          +b.Proportion_rank_45_plus_within_LTLA
        ) {
          return -1;
        } else if (
          +a.Proportion_rank_45_plus_within_LTLA >
          +b.Proportion_rank_45_plus_within_LTLA
        ) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Proportion_rank_45_plus_within_LTLA % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (
          +a.Proportion_rank_45_plus_within_LTLA <
          +b.Proportion_rank_45_plus_within_LTLA
        ) {
          return 1;
        } else if (
          +a.Proportion_rank_45_plus_within_LTLA >
          +b.Proportion_rank_45_plus_within_LTLA
        ) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == "Proportion aged 45+ receiving at least one dose") {
    clicks.Proportion_45_plus++;
    // even number of clicks
    if (clicks.Proportion_45_plus % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.Proportion_45_plus < +b.Proportion_45_plus) {
          return -1;
        } else if (+a.Proportion_45_plus > +b.Proportion_45_plus) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Proportion_45_plus % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.Proportion_45_plus < +b.Proportion_45_plus) {
          return 1;
        } else if (+a.Proportion_45_plus > +b.Proportion_45_plus) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (
    d == "Estimated number of people aged 45+ still to receive their first dose"
  ) {
    clicks.Estimated_left_to_vaccinate_45_plus++;
    // even number of clicks
    if (clicks.Estimated_left_to_vaccinate_45_plus % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (
          +a.Estimated_left_to_vaccinate_45_plus <
          +b.Estimated_left_to_vaccinate_45_plus
        ) {
          return -1;
        } else if (
          +a.Estimated_left_to_vaccinate_45_plus >
          +b.Estimated_left_to_vaccinate_45_plus
        ) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Estimated_left_to_vaccinate_45_plus % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (
          +a.Estimated_left_to_vaccinate_45_plus <
          +b.Estimated_left_to_vaccinate_45_plus
        ) {
          return 1;
        } else if (
          +a.Estimated_left_to_vaccinate_45_plus >
          +b.Estimated_left_to_vaccinate_45_plus
        ) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == "Number aged 65+ receiving at least one dose") {
    clicks.Age_65_and_over++;
    // even number of clicks
    if (clicks.Age_65_and_over % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.Age_65_and_over < +b.Age_65_and_over) {
          return -1;
        } else if (+a.Age_65_and_over > +b.Age_65_and_over) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Age_65_and_over % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.Age_65_and_over < +b.Age_65_and_over) {
          return 1;
        } else if (+a.Age_65_and_over > +b.Age_65_and_over) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == "Rank of uptake (%) aged 65+ within Local Authority area") {
    clicks.Proportion_rank_65_plus_within_LTLA++;
    // even number of clicks
    if (clicks.Proportion_rank_65_plus_within_LTLA % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (
          +a.Proportion_rank_65_plus_within_LTLA <
          +b.Proportion_rank_65_plus_within_LTLA
        ) {
          return -1;
        } else if (
          +a.Proportion_rank_65_plus_within_LTLA >
          +b.Proportion_rank_65_plus_within_LTLA
        ) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Proportion_rank_65_plus_within_LTLA % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (
          +a.Proportion_rank_65_plus_within_LTLA <
          +b.Proportion_rank_65_plus_within_LTLA
        ) {
          return 1;
        } else if (
          +a.Proportion_rank_65_plus_within_LTLA >
          +b.Proportion_rank_65_plus_within_LTLA
        ) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == "Proportion aged 65+ receiving at least one dose") {
    clicks.Proportion_65_plus++;
    // even number of clicks
    if (clicks.Proportion_65_plus % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.Proportion_65_plus < +b.Proportion_65_plus) {
          return -1;
        } else if (+a.Proportion_65_plus > +b.Proportion_65_plus) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Proportion_65_plus % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.Proportion_65_plus < +b.Proportion_65_plus) {
          return 1;
        } else if (+a.Proportion_65_plus > +b.Proportion_65_plus) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (
    d == "Estimated number of people aged 65+ still to receive their first dose"
  ) {
    clicks.Estimated_left_to_vaccinate_65_plus++;
    // even number of clicks
    if (clicks.Estimated_left_to_vaccinate_65_plus % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (
          +a.Estimated_left_to_vaccinate_65_plus <
          +b.Estimated_left_to_vaccinate_65_plus
        ) {
          return -1;
        } else if (
          +a.Estimated_left_to_vaccinate_65_plus >
          +b.Estimated_left_to_vaccinate_65_plus
        ) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Estimated_left_to_vaccinate_65_plus % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (
          +a.Estimated_left_to_vaccinate_65_plus <
          +b.Estimated_left_to_vaccinate_65_plus
        ) {
          return 1;
        } else if (
          +a.Estimated_left_to_vaccinate_65_plus >
          +b.Estimated_left_to_vaccinate_65_plus
        ) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  // ! Date
  // if (d == "Created On") {
  //   clicks.created_on++;
  //   if (clicks.created_on % 2 == 0) {
  //     // sort ascending: by date
  //     rows.sort(function (a, b) {
  //       // grep date and time, split them apart, make Date objects for comparing
  //       var date = /[\d]{4}-[\d]{2}-[\d]{2}/.exec(a.created_on);
  //       date = date[0].split("-");
  //       var time = /[\d]{2}:[\d]{2}:[\d]{2}/.exec(a.created_on);
  //       time = time[0].split(":");
  //       var a_date_obj = new Date(
  //         +date[0],
  //         +date[1] - 1,
  //         +date[2],
  //         +time[0],
  //         +time[1],
  //         +time[2]
  //       );

  //       date = /[\d]{4}-[\d]{2}-[\d]{2}/.exec(b.created_on);
  //       date = date[0].split("-");
  //       time = /[\d]{2}:[\d]{2}:[\d]{2}/.exec(b.created_on);
  //       time = time[0].split(":");
  //       var b_date_obj = new Date(
  //         +date[0],
  //         +date[1] - 1,
  //         +date[2],
  //         +time[0],
  //         +time[1],
  //         +time[2]
  //       );

  //       if (a_date_obj < b_date_obj) {
  //         return -1;
  //       } else if (a_date_obj > b_date_obj) {
  //         return 1;
  //       } else {
  //         return 0;
  //       }
  //     });
  //     // odd number of clicks
  //   } else if (clicks.created_on % 2 != 0) {
  //     // sort descending: by date
  //     rows.sort(function (a, b) {
  //       // grep date and time, split them apart, make Date objects for comparing
  //       var date = /[\d]{4}-[\d]{2}-[\d]{2}/.exec(a.created_on);
  //       date = date[0].split("-");
  //       var time = /[\d]{2}:[\d]{2}:[\d]{2}/.exec(a.created_on);
  //       time = time[0].split(":");
  //       var a_date_obj = new Date(
  //         +date[0],
  //         +date[1] - 1,
  //         +date[2],
  //         +time[0],
  //         +time[1],
  //         +time[2]
  //       );

  //       date = /[\d]{4}-[\d]{2}-[\d]{2}/.exec(b.created_on);
  //       date = date[0].split("-");
  //       time = /[\d]{2}:[\d]{2}:[\d]{2}/.exec(b.created_on);
  //       time = time[0].split(":");
  //       var b_date_obj = new Date(
  //         +date[0],
  //         +date[1] - 1,
  //         +date[2],
  //         +time[0],
  //         +time[1],
  //         +time[2]
  //       );

  //       if (a_date_obj < b_date_obj) {
  //         return 1;
  //       } else if (a_date_obj > b_date_obj) {
  //         return -1;
  //       } else {
  //         return 0;
  //       }
  //     });
  //   }
  // }

  //  ! URL
  // if (d == "URL") {
  //   clicks.url++;
  //   // even number of clicks
  //   if (clicks.url % 2 == 0) {
  //     // sort ascending: alphabetically
  //     rows.sort(function (a, b) {
  //       if (
  //         a.thumb_url_default.toUpperCase() < b.thumb_url_default.toUpperCase()
  //       ) {
  //         return -1;
  //       } else if (
  //         a.thumb_url_default.toUpperCase() > b.thumb_url_default.toUpperCase()
  //       ) {
  //         return 1;
  //       } else {
  //         return 0;
  //       }
  //     });
  //     // odd number of clicks
  //   } else if (clicks.url % 2 != 0) {
  //     // sort descending: alphabetically
  //     rows.sort(function (a, b) {
  //       if (
  //         a.thumb_url_default.toUpperCase() < b.thumb_url_default.toUpperCase()
  //       ) {
  //         return 1;
  //       } else if (
  //         a.thumb_url_default.toUpperCase() > b.thumb_url_default.toUpperCase()
  //       ) {
  //         return -1;
  //       } else {
  //         return 0;
  //       }
  //     });
  //   }
  // }
}); // end of click listeners

// ! Vaccine sites table

var request = new XMLHttpRequest();
request.open(
  "GET",
  "./Outputs/total_vaccination_sites_summary_table.json",
  false
);
request.send(null);
var vaccine_sites_at_a_glance = JSON.parse(request.responseText);

// console.log(vaccine_sites_at_a_glance);

window.onload = () => {
  loadTable_ltla_vaccine_sites(vaccine_sites_at_a_glance);
};

function loadTable_ltla_vaccine_sites(vaccine_sites_at_a_glance) {
  const tableBody = document.getElementById("vaccine_sites_table_1");
  var dataHTML = "";

  for (let item of vaccine_sites_at_a_glance) {
    dataHTML += `<tr><td>${item.Area}</td><td>${d3.format(",.0f")(
      item.Total
    )}</td><td>${d3.format(",.0f")(item.GP_led)}</td><td>${d3.format(",.0f")(
      item.Pharmacies
    )}</td><td>${d3.format(",.0f")(item.Hospital_hub)}</td><td>${d3.format(
      ",.0f"
    )(item.Vaccination_centre)}</td></tr>`;
  }

  tableBody.innerHTML = dataHTML;
}

// ! Map

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/Sussex_vaccination_sites.json", false);
request.send(null);
var sussex_vaccination_sites = JSON.parse(request.responseText);

// Parameters

var deprivation_deciles = [
  "10% most deprived",
  "Decile 2",
  "Decile 3",
  "Decile 4",
  "Decile 5",
  "Decile 6",
  "Decile 7",
  "Decile 8",
  "Decile 9",
  "10% least deprived",
];

var deprivation_colours = [
  "#0000FF",
  "#2080FF",
  "#40E0FF",
  "#70FFD0",
  "#90FFB0",
  "#C0E1B0",
  "#E0FFA0",
  "#E0FF70",
  "#F0FF30",
  "#FFFF00",
];

var msoa_covid_imd_colour_func = d3
  .scaleOrdinal()
  .domain(deprivation_deciles)
  .range(deprivation_colours);

var msoa_covid_vaccines_ages_currently_eligible_proportion_raw = [
  "Less than 70%",
  "70-74%",
  "75-79%",
  "80-84%",
  "85-89%",
  "90-94%",
  "95-99%",
  "100% of estimated population",
];

var msoa_covid_vaccines_ages_currently_eligible_proportion_colours = [
  "#F0F921",
  "#FEBC2A",
  "#F48849",
  "#DB5C68",
  "#B93289",
  "#8B0AA5",
  "#5402A3",
  "#0D0887",
];

var msoa_covid_vaccines_ages_currently_eligible_colour_proportions_func = d3
  .scaleOrdinal()
  .domain(msoa_covid_vaccines_ages_currently_eligible_proportion_raw)
  .range(msoa_covid_vaccines_ages_currently_eligible_proportion_colours);

// Add AJAX request for data
var msoa_imd_map_data = $.ajax({
  url: "./Outputs/msoa_covid_vaccine_latest.geojson",
  dataType: "json",
  success: console.log("MSOA boundary data successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText);
  },
});

var tileUrl = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png";
var attribution =
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br> Contains Ordnance Survey data © Crown copyright and database right 2020.<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons. Click on an area to find out more';

// IMD style

function vaccine_msoa_covid_imd_style(d) {
  return d === deprivation_deciles[0]
    ? deprivation_colours[0]
    : d === deprivation_deciles[1]
    ? deprivation_colours[1]
    : d === deprivation_deciles[2]
    ? deprivation_colours[2]
    : d === deprivation_deciles[3]
    ? deprivation_colours[3]
    : d === deprivation_deciles[4]
    ? deprivation_colours[4]
    : d === deprivation_deciles[5]
    ? deprivation_colours[5]
    : d === deprivation_deciles[6]
    ? deprivation_colours[6]
    : d === deprivation_deciles[7]
    ? deprivation_colours[7]
    : d === deprivation_deciles[8]
    ? deprivation_colours[8]
    : d === deprivation_deciles[9]
    ? deprivation_colours[9]
    : "#feebe2";
}

function style_msoa_imd_decile_national(feature) {
  return {
    fillColor: vaccine_msoa_covid_imd_style(
      feature.properties.National_pop_weighted_decile
    ),
    weight: 0.5,
    opacity: 1,
    color: "#000000",
    // dashArray: "3",
    fillOpacity: 0.95,
  };
}

function style_msoa_imd_decile_sussex(feature) {
  return {
    fillColor: vaccine_msoa_covid_imd_style(
      feature.properties.Decile_in_Sussex
    ),
    weight: 0.5,
    opacity: 1,
    color: "#000000",
    // dashArray: "3",
    fillOpacity: 0.95,
  };
}

// Ages currently eligible proportion
function vaccine_msoa_colour_ages_currently_eligible_proportion(d) {
  return d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[0]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[0]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[1]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[1]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[2]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[2]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[3]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[3]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[4]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[4]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[5]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[5]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[6]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[6]
    : d === msoa_covid_vaccines_ages_currently_eligible_proportion_raw[7]
    ? msoa_covid_vaccines_ages_currently_eligible_proportion_colours[7]
    : "#feebe2";
}

function style_msoa_vaccine_ages_currently_eligible_proportion(feature) {
  return {
    fillColor: vaccine_msoa_colour_ages_currently_eligible_proportion(
      feature.properties.Proportion_45_plus_banded
    ),
    weight: 0.5,
    opacity: 1,
    color: "#000000",
    // dashArray: "3",
    fillOpacity: 0.75,
  };
}

$.when(msoa_imd_map_data).done(function () {
  var msoa_map_vaccine_imd_leaf = L.map("msoa_map_vaccine_deprivation");

  var msoa_imd_national_map_layer = L.geoJSON(msoa_imd_map_data.responseJSON, {
    style: style_msoa_imd_decile_national,
  })
    .addTo(msoa_map_vaccine_imd_leaf)
    .bindPopup(function (layer) {
      return (
        "<p><b>" +
        layer.feature.properties.msoa11hclnm +
        " (" +
        layer.feature.properties.msoa11cd +
        ")</b></p>This MSOA is ranked in <b>" +
        layer.feature.properties.National_pop_weighted_decile +
        "</b> with 1 being the most deprived 10% of neighbourhoods nationally and 10 being the least deprived 10% of neighbourhoods.</p><p> A total of <b> " +
        d3.format(",.0f")(layer.feature.properties.Total_where_age_known) +
        "</b> people aged 16+ have received at least one dose of a COVID-19 vaccine. This is <b>" +
        d3.format(".1%")(layer.feature.properties.Proportion_age_known) +
        " </b>of the estimated population in this area.</p><p>A total of <b>" +
        d3.format(",.0f")(layer.feature.properties.Age_45_and_over) +
        " </b>people aged 45+ have received at least one dose (<b>" +
        d3.format(".1%")(layer.feature.properties.Proportion_45_plus) +
        "</b>).</p>"
      );
    });

  msoa_map_vaccine_imd_leaf.fitBounds(msoa_imd_national_map_layer.getBounds());

  var msoa_imd_sussex_map_layer = L.geoJSON(msoa_imd_map_data.responseJSON, {
    style: style_msoa_imd_decile_sussex,
  }).bindPopup(function (layer) {
    return (
      "<p><b>" +
      layer.feature.properties.msoa11hclnm +
      " (" +
      layer.feature.properties.msoa11cd +
      ")</b></p>This MSOA is ranked in <b>" +
      layer.feature.properties.Decile_in_Sussex +
      "</b> with 1 being the most deprived 10% of neighbourhoods in Sussex and 10 being the least deprived 10% of neighbourhoods.</p><p> A total of <b> " +
      d3.format(",.0f")(layer.feature.properties.Total_where_age_known) +
      "</b> people aged 16+ have received at least one dose of a COVID-19 vaccine. This is <b>" +
      d3.format(".1%")(layer.feature.properties.Proportion_age_known) +
      " </b>of the estimated population in this area.</p><p>A total of <b>" +
      d3.format(",.0f")(layer.feature.properties.Age_45_and_over) +
      " </b>people aged 45+ have received at least one dose (<b>" +
      d3.format(".1%")(layer.feature.properties.Proportion_45_plus) +
      "</b>).</p>"
    );
  });

  var msoa_vaccine_45_plus_proportion_map_layer = L.geoJSON(
    msoa_imd_map_data.responseJSON,
    {
      style: style_msoa_vaccine_ages_currently_eligible_proportion,
    }
  ).bindPopup(function (layer) {
    return (
      "<p><b>" +
      layer.feature.properties.msoa11hclnm +
      " (" +
      layer.feature.properties.msoa11cd +
      ")</b></p>This MSOA is ranked in <b>" +
      layer.feature.properties.Decile_in_Sussex +
      "</b> with 1 being the most deprived 10% of neighbourhoods in Sussex and <b>" +
      layer.feature.properties.National_pop_weighted_decile +
      "</b> nationallly, with 10 being the least deprived 10 % of neighbourhoods.</p ><p> A total of <b> " +
      d3.format(",.0f")(layer.feature.properties.Total_where_age_known) +
      "</b> people aged 16+ have received at least one dose of a COVID-19 vaccine. This is <b>" +
      d3.format(".1%")(layer.feature.properties.Proportion_age_known) +
      " </b>of the estimated population in this area.</p><p>A total of <b>" +
      d3.format(",.0f")(layer.feature.properties.Age_45_and_over) +
      " </b>people aged 45+ have received at least one dose (<b>" +
      d3.format(".1%")(layer.feature.properties.Proportion_45_plus) +
      "</b>).</p>"
    );
  });

  var baseMaps_imd = {
    "Deprivation deciles (national ranks)": msoa_imd_national_map_layer,
    "Deprivation deciles (ranks within Sussex)": msoa_imd_sussex_map_layer,
    "Proportion of those aged 45+ receiving first dose": msoa_vaccine_45_plus_proportion_map_layer,
  };

  var basemap_msoa_imd_vaccine = L.tileLayer(tileUrl, {
    attribution,
    minZoom: 6,
  }).addTo(msoa_map_vaccine_imd_leaf);

  L.control
    .layers(baseMaps_imd, null, { collapsed: false })
    .addTo(msoa_map_vaccine_imd_leaf);

  msoa_map_vaccine_imd_leaf.fitBounds(msoa_imd_national_map_layer.getBounds());

  msoa_map_vaccine_imd_leaf.on("baselayerchange", function (ev) {
    console.log("Base layer changes");
    var selected_base_layer = ev.name;
    if (selected_base_layer === "Deprivation deciles (national ranks)") {
      key_msoa_imd_national();
    }
    if (selected_base_layer === "Deprivation deciles (ranks within Sussex)") {
      key_msoa_imd_sussex();
    }
    if (
      selected_base_layer ===
      "Proportion of those aged 45+ receiving first dose"
    ) {
      key_msoa_vaccines_ages_currently_eligible_proportion();
    }
  });
});

function key_msoa_imd_national() {
  $(".key_list_vaccine_all").remove();

  d3.select("#msoa_map_vaccine_deprivation_title").html(function (d) {
    return "Indices of multiple deprivation (2019); Population weighted to MSOA level; National deprivation deciles; Sussex MSOAs;";
  });

  d3.select("#imd_msoa_map_key_title").html(function (d) {
    return "Neighbourhoods ranked nationally";
  });

  deprivation_deciles.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = "key_list_vaccine_all";
    list.style.borderColor = msoa_covid_imd_colour_func(index);
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = msoa_covid_imd_colour_func(index);
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("list_imd_all_key");
    div.appendChild(list);
  });
}

function key_msoa_imd_sussex() {
  $(".key_list_vaccine_all").remove();

  d3.select("#msoa_map_vaccine_deprivation_title").html(function (d) {
    return "Indices of multiple deprivation (2019); Population weighted to MSOA level; Deprivation ranked within Sussex; Sussex MSOAs;";
  });

  d3.select("#imd_msoa_map_key_title").html(function (d) {
    return "Neighbourhoods ranked within Sussex only";
  });

  deprivation_deciles.forEach(function (item, index) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = "key_list_vaccine_all";
    list.style.borderColor = msoa_covid_imd_colour_func(index);
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = msoa_covid_imd_colour_func(index);
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("list_imd_all_key");
    div.appendChild(list);
  });
}

function key_msoa_vaccines_ages_currently_eligible_proportion() {
  $(".key_list_vaccine_all").remove();

  d3.select("#msoa_map_vaccine_deprivation_title").html(function (d) {
    return "Proportion of individuals (aged 45+) receiving at least one Covid-19 vaccination dose; Sussex MSOAs;";
  });

  d3.select("#imd_msoa_map_key_title").html(function (d) {
    return "Proportion of people aged 45+ receiving at least one dose";
  });

  msoa_covid_vaccines_ages_currently_eligible_proportion_raw.forEach(function (
    item,
    index
  ) {
    var list = document.createElement("li");
    list.innerHTML = item;
    list.className = "key_list_vaccine_all";
    list.style.borderColor = msoa_covid_vaccines_ages_currently_eligible_colour_proportions_func(
      index
    );
    var tt = document.createElement("div");
    tt.className = "side_tt";
    tt.style.borderColor = msoa_covid_vaccines_ages_currently_eligible_colour_proportions_func(
      index
    );
    var tt_h3_1 = document.createElement("h3");
    tt_h3_1.innerHTML = item;

    tt.appendChild(tt_h3_1);
    var div = document.getElementById("list_imd_all_key");
    div.appendChild(list);
  });
}

key_msoa_imd_national();

// ! Scatter plot
var height_scatter = 500;

var ltla_area_scatter_comp = [
  "Adur",
  "Arun",
  "Chichester",
  "Crawley",
  "Horsham",
  "Mid_Sussex",
  "Worthing",
  "Brighton_and_Hove",
  "Eastbourne",
  "Hastings",
  "Lewes",
  "Rother",
  "Wealden",
];

var ltla_scatter_colour_func = d3
  .scaleOrdinal()
  .domain(ltla_area_scatter_comp)
  .range([
    "#0E2F6A",
    "#490B2F",
    "#880D2A",
    "#B02C1A",
    "#FD7726",
    "#FDD147",
    "#8CC216",
    "#0000cc",
    "black",
    "blue",
    "red",
    "green",
    "yellow",
  ]);

var scatter_options = [
  "Proportion over 45",
  "Proportion over 65",
  "Proportion aged 45-64",
  "Proportion aged 16+",
];

d3.select("#select_scatter_measure_button")
  .selectAll("myOptions")
  .data(scatter_options)
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

var selected_vaccine_scatter_choice = d3
  .select("#select_scatter_measure_button")
  .property("value");

var column_to_select_scatter = d3
  .scaleOrdinal()
  .domain(scatter_options)
  .range([
    "Proportion_45_plus",
    "Proportion_65_plus",
    "Proportion_45_64",
    "Proportion_age_known",
  ]);

var column_to_select_scatter_count = d3
  .scaleOrdinal()
  .domain(scatter_options)
  .range([
    "Age_45_and_over",
    "Age_65_and_over",
    "Age_45_64",
    "Total_where_age_known",
  ]);

var column_to_select_scatter_text_1 = d3
  .scaleOrdinal()
  .domain(scatter_options)
  .range([
    "people aged 45+ years",
    "people aged 65+ years",
    "people aged 45-64 years",
    "people aged 16+",
  ]);

var svg_scatter = d3
  .select("#msoa_vaccine_scatter_deprivation")
  .append("svg")
  .attr("width", width_hm)
  .attr("height", height_scatter)
  .append("g")
  .attr("transform", "translate(" + 60 + "," + 30 + ")");

var tooltip_scatter_dep_uptake = d3
  .select("#msoa_vaccine_scatter_dep_tt")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip_outside_box")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("font-size", ".6rem")
  .style("padding", "10px");

var showTooltip_scatter_dep_uptake = function (d) {
  tooltip_scatter_dep_uptake
    .html(
      "<p><b>" +
        d.MSOA_name +
        " in  " +
        d.LTLA_name +
        "</b></p><p> A total of <b> " +
        d3.format(",.0f")(
          d[column_to_select_scatter_count(selected_vaccine_scatter_choice)]
        ) +
        "</b> " +
        column_to_select_scatter_text_1(selected_vaccine_scatter_choice) +
        " have received at least one dose of a COVID - 19 vaccine.This is <b> " +
        d3.format(".1%")(
          d[column_to_select_scatter(selected_vaccine_scatter_choice)]
        ) +
        " </b>of the estimated population in this age group in this area.</p>" +
        "<p>This MSOA has a population weighted deprivation score of <b>" +
        d3.format(",.1f")(d.Pop_weighted_imd_score) +
        "</b> and a rank of <b>" +
        d3.format(",.0f")(d.National_pop_weighted_rank) +
        "</b> out of 6,790 neighbourhoods nationally, and <b>" +
        d.Rank_in_Sussex +
        "</b> out of 202 neighbourhood areas in Sussex, with 1 being most deprived.</p> "
    )
    .style("opacity", 1)
    // .style("top", event.pageY - 10 + "px")
    // .style("left", event.pageX + 10 + "px")
    .style("opacity", 1)
    .style("font-size", ".6rem")
    .style("visibility", "visible");

  selected_MSOA_scatter = d.LTLA_name_ns;

  d3.selectAll(".dot." + selected_MSOA_scatter)
    .transition()
    .duration(200)
    .style("fill", "maroon")
    .attr("r", 9);
};

var Mouseleave_scatter_dep_uptake = function (d) {
  tooltip_scatter_dep_uptake.style("opacity", 0).style("visibility", "hidden");

  d3.selectAll(".dot." + selected_MSOA_scatter)
    .transition()
    .duration(200)
    .style("fill", "#2e84d5")
    .attr("r", 6);
};

// Tell me which check boxes are checked
var choices = [];
d3.selectAll(".my_scatter_Checkbox_1").each(function (d) {
  cb = d3.select(this);
  if (cb.property("checked")) {
    choices.push(cb.property("value"));
  }
});

included_vaccine_msoa_data = vaccine_msoa_data.filter(function (d, i) {
  return choices.indexOf(d.UTLA) >= 0;
});

// Add X axis
var x_dep_vs_uptake = d3
  .scaleLinear()
  .domain([
    0,
    d3.max(included_vaccine_msoa_data, function (d) {
      return +d.Pop_weighted_imd_score;
    }),
  ])
  .range([0, width_hm - 120]) // We move 0 over 60 pixels when we created the svg so to get this symetrical we need to minues 60 * 2
  .nice();

xAxis_dep_vs_uptake = svg_scatter
  .append("g")
  .attr("transform", "translate(0," + (height_scatter - 60) + ")") // Again, we moved the 0 up 30
  .call(d3.axisBottom(x_dep_vs_uptake).tickFormat(d3.format(",.0f")));

xAxis_dep_vs_uptake.selectAll("text").style("font-size", ".8rem");

// Add Y axis
var y_dep_vs_uptake = d3
  .scaleLinear()
  .domain([
    d3.min(included_vaccine_msoa_data, function (d) {
      return +d.Proportion_45_plus;
    }),
    1,
  ])
  .range([height_scatter - 60, 0])
  .nice();

var yAxis_dep_vs_uptake = svg_scatter
  .append("g")
  .attr("transform", "translate(0,0)")
  .call(d3.axisLeft(y_dep_vs_uptake).tickFormat(d3.format(".0%")));

yAxis_dep_vs_uptake
  .selectAll("text")
  .attr("transform", "translate(0,0)")
  .style("text-anchor", "end")
  .style("font-size", ".8rem");

// ! Some annotations

svg_scatter
  .append("text")
  .attr("x", function (d) {
    return x_dep_vs_uptake(2);
  })
  .attr("y", height_scatter - 90)
  .attr("id", "less_deprived_label")
  .text("Less deprived")
  .attr("text-anchor", "start")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_scatter
  .append("text")
  .attr("x", function (d) {
    return x_dep_vs_uptake(
      d3.max(included_vaccine_msoa_data, function (d) {
        return +d.Pop_weighted_imd_score;
      })
    );
  })
  .attr("y", height_scatter - 90)
  .attr("id", "more_deprived_label")
  .text("More deprived")
  .attr("text-anchor", "end")
  .style("font-weight", "bold")
  .style("font-size", ".8rem");

svg_scatter
  .append("text")
  .attr("x", function (d) {
    return x_dep_vs_uptake(
      d3.max(included_vaccine_msoa_data, function (d) {
        return +d.Pop_weighted_imd_score;
      })
    );
  })
  .attr("y", 10)
  .attr("id", "hover_label_1")
  .text("Hover over a dot")
  .attr("text-anchor", "end")
  .style("font-size", ".8rem");

svg_scatter
  .append("text")
  .attr("x", function (d) {
    return x_dep_vs_uptake(
      d3.max(included_vaccine_msoa_data, function (d) {
        return +d.Pop_weighted_imd_score;
      })
    );
  })
  .attr("y", 25)
  .attr("id", "hover_label_2")
  .text("to highlight other")
  .attr("text-anchor", "end")
  .style("font-size", ".8rem");

svg_scatter
  .append("text")
  .attr("x", function (d) {
    return x_dep_vs_uptake(
      d3.max(included_vaccine_msoa_data, function (d) {
        return +d.Pop_weighted_imd_score;
      })
    );
  })
  .attr("y", 40)
  .attr("id", "hover_label_3")
  .text("neighbhourhoods in")
  .attr("text-anchor", "end")
  .style("font-size", ".8rem");

svg_scatter
  .append("text")
  .attr("x", function (d) {
    return x_dep_vs_uptake(
      d3.max(included_vaccine_msoa_data, function (d) {
        return +d.Pop_weighted_imd_score;
      })
    );
  })
  .attr("y", 55)
  .attr("id", "hover_label_4")
  .text("the same local authority")
  .attr("text-anchor", "end")
  .style("font-size", ".8rem");

function update_scatter_dep() {
  var selected_vaccine_scatter_choice = d3
    .select("#select_scatter_measure_button")
    .property("value");

  console.log(column_to_select_scatter(selected_vaccine_scatter_choice));

  var choices = [];
  d3.selectAll(".my_scatter_Checkbox_1").each(function (d) {
    cb = d3.select(this);
    if (cb.property("checked")) {
      choices.push(cb.property("value"));
    }
  });

  d3.select("#msoa_vaccine_scatter_deprivation_title").html(function (d) {
    return (
      "Proportion of individuals (" +
      column_to_select_scatter_text_1(selected_vaccine_scatter_choice) +
      ") receiving at least one Covid-19 vaccination dose by deprivation score; Sussex MSOAs; vaccinations administered from " +
      vaccine_administered_date
    );
  });

  var showTooltip_scatter_dep_uptake = function (d) {
    tooltip_scatter_dep_uptake
      .html(
        "<p><b>" +
          d.MSOA_name +
          " in  " +
          d.LTLA_name +
          "</b></p><p> A total of <b> " +
          d3.format(",.0f")(
            d[column_to_select_scatter_count(selected_vaccine_scatter_choice)]
          ) +
          "</b> " +
          column_to_select_scatter_text_1(selected_vaccine_scatter_choice) +
          " have received at least one dose of a COVID - 19 vaccine.This is <b> " +
          d3.format(".1%")(
            d[column_to_select_scatter(selected_vaccine_scatter_choice)]
          ) +
          " </b>of the estimated population in this age group in this area.</p>" +
          "<p>This MSOA has a population weighted deprivation score of <b>" +
          d3.format(",.1f")(d.Pop_weighted_imd_score) +
          "</b> and a rank of <b>" +
          d3.format(",.0f")(d.National_pop_weighted_rank) +
          "</b> out of 6,790 neighbourhoods nationally, and <b>" +
          d.Rank_in_Sussex +
          "</b> out of 202 neighbourhood areas in Sussex, with 1 being most deprived.</p> "
      )
      .style("opacity", 1)
      // .style("top", event.pageY - 10 + "px")
      // .style("left", event.pageX + 10 + "px")
      .style("opacity", 1)
      .style("visibility", "visible");

    selected_MSOA_scatter = d.LTLA_name_ns;

    d3.selectAll(".dot." + selected_MSOA_scatter)
      .transition()
      .duration(200)
      .style("fill", "maroon")
      .attr("r", 9);
  };

  var Mouseleave_scatter_dep_uptake = function (d) {
    tooltip_scatter_dep_uptake
      .style("opacity", 0)
      .style("visibility", "hidden");

    d3.selectAll(".dot." + selected_MSOA_scatter)
      .transition()
      .duration(200)
      .style("fill", "#2e84d5")
      .attr("r", 6);
  };

  included_vaccine_msoa_data = vaccine_msoa_data.filter(function (d, i) {
    return choices.indexOf(d.UTLA) >= 0;
  });

  x_dep_vs_uptake
    .domain([
      0,
      d3.max(included_vaccine_msoa_data, function (d) {
        return +d.Pop_weighted_imd_score;
      }),
    ])
    .nice();

  xAxis_dep_vs_uptake
    .transition()
    // .delay(1500)
    .duration(1250)
    .call(d3.axisBottom(x_dep_vs_uptake));

  xAxis_dep_vs_uptake.selectAll("text").style("font-size", ".8rem");

  y_dep_vs_uptake
    .domain([
      d3.min(included_vaccine_msoa_data, function (d) {
        return +d[column_to_select_scatter(selected_vaccine_scatter_choice)];
      }),
      1,
    ])
    .nice();

  yAxis_dep_vs_uptake
    .transition()
    // .delay(1500)
    .duration(1250)
    .call(d3.axisLeft(y_dep_vs_uptake).tickFormat(d3.format(".0%")));

  yAxis_dep_vs_uptake.selectAll("text").style("font-size", ".8rem");

  var dep_uptake_points = svg_scatter
    .selectAll("circle")
    .data(included_vaccine_msoa_data);

  dep_uptake_points
    .enter()
    .append("circle")
    .merge(dep_uptake_points)
    .attr("class", function (d) {
      return "dot " + d.LTLA_name_ns;
    })
    .attr("cx", function (d) {
      return x_dep_vs_uptake(d.Pop_weighted_imd_score);
    })
    .attr("cy", function (d) {
      return y_dep_vs_uptake(
        d[column_to_select_scatter(selected_vaccine_scatter_choice)]
      );
    })
    .attr("r", 6)
    .attr("fill", "#2e84d5")
    .style("stroke", "#ffffff")
    .on("mousemove", showTooltip_scatter_dep_uptake)
    .on("mouseout", Mouseleave_scatter_dep_uptake);

  dep_uptake_points.exit().remove();
}

d3.selectAll(".my_scatter_Checkbox_1").on("change", update_scatter_dep);
d3.selectAll("#select_scatter_measure_button").on("change", update_scatter_dep);

update_scatter_dep();
