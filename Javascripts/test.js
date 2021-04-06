column_names = [
  "Local Authority",
  "MSOA name",
  "Number of individuals receiving at least one dose",
  "Number aged 50+ receiving at least one dose",
  "Proportion aged 50+ receiving at least one dose ranked within Local Authority area",
  "Proportion aged 50+ receiving at least one dose",
  "Estimated number of people aged 50+ still to receive their first dose",
  "Number aged 65+ receiving at least one dose",
  "Proportion aged 65+ receiving at least one dose ranked within Local Authority area",
  "Proportion aged 65+ receiving at least one dose",
  "Estimated number of people aged 50+ still to receive their first dose",
];

var clicks = {
  Total_where_age_known: 0,
  Age_50_and_over: 0,
  Proportion_rank_50_plus_within_LTLA: 0,
  Proportion_50_plus: 0,
  Estimated_left_to_vaccinate_50_plus: 0,
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

console.log(vaccine_msoa_data);

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
        arr[2],
        arr[3],
        arr[4],
        arr[5],
        arr[6],
        arr[7],
        arr[8],
        arr[9],
        arr[10],
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
        arr[2],
        arr[3],
        arr[4],
        arr[5],
        arr[6],
        arr[7],
        arr[8],
        arr[9],
        arr[10],
      ];
    })
    .enter()
    .append("td");

  console.log(row_entries);

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
        arr[2],
        arr[3],
        arr[4],
        arr[5],
        arr[6],
        arr[7],
        arr[8],
        arr[9],
        arr[10],
      ];
    })
    .enter()
    .append("td");

  console.log(row_entries);

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
  // if (d == "Number of individuals receiving at least one dose") {
  //   clicks.Total_where_age_known++;
  //   // even number of clicks
  //   if (clicks.Total_where_age_known % 2 == 0) {
  //     // sort ascending: alphabetically
  //     rows.sort(function (a, b) {
  //       if (
  //         a.Total_where_age_known.toUpperCase() <
  //         b.Total_where_age_known.toUpperCase()
  //       ) {
  //         return -1;
  //       } else if (
  //         a.Total_where_age_known.toUpperCase() >
  //         b.Total_where_age_known.toUpperCase()
  //       ) {
  //         return 1;
  //       } else {
  //         return 0;
  //       }
  //     });
  //     // odd number of clicks
  //   } else if (clicks.Total_where_age_known % 2 != 0) {
  //     // sort descending: alphabetically
  //     rows.sort(function (a, b) {
  //       if (
  //         a.Total_where_age_known.toUpperCase() <
  //         b.Total_where_age_known.toUpperCase()
  //       ) {
  //         return 1;
  //       } else if (
  //         a.Total_where_age_known.toUpperCase() >
  //         b.Total_where_age_known.toUpperCase()
  //       ) {
  //         return -1;
  //       } else {
  //         return 0;
  //       }
  //     });
  //   }
  // }
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
  if (d == "Number aged 50+ receiving at least one dose") {
    clicks.Age_50_and_over++;
    // even number of clicks
    if (clicks.Age_50_and_over % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.Age_50_and_over < +b.Age_50_and_over) {
          return -1;
        } else if (+a.Age_50_and_over > +b.Age_50_and_over) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Age_50_and_over % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.Age_50_and_over < +b.Age_50_and_over) {
          return 1;
        } else if (+a.Age_50_and_over > +b.Age_50_and_over) {
          return -1;
        } else {
          return 0;
        }
      });
    }
  }
  if (d == "Proportion aged 50+ receiving at least one dose") {
    clicks.Proportion_50_plus++;
    // even number of clicks
    if (clicks.Proportion_50_plus % 2 == 0) {
      // sort ascending: numerically
      rows.sort(function (a, b) {
        if (+a.Proportion_50_plus < +b.Proportion_50_plus) {
          return -1;
        } else if (+a.Proportion_50_plus > +b.Proportion_50_plus) {
          return 1;
        } else {
          return 0;
        }
      });
      // odd number of clicks
    } else if (clicks.Proportion_50_plus % 2 != 0) {
      // sort descending: numerically
      rows.sort(function (a, b) {
        if (+a.Proportion_50_plus < +b.Proportion_50_plus) {
          return 1;
        } else if (+a.Proportion_50_plus > +b.Proportion_50_plus) {
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
