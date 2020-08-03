
// var request = new XMLHttpRequest();
// request.open("GET", "./Outputs/case_change_dates.json", false);
// request.send(null);
// var data_dates = JSON.parse(request.responseText).map(function(d){return d.Date_label});
//
// var request = new XMLHttpRequest();
// request.open("GET", "./Outputs/case_change_date_range.json", false);
// request.send(null);
// var data_date_range = JSON.parse(request.responseText);

// case_change_values = ['No change in average cases','Increasing average number of cases over past 7 days', 'Decreasing average number of cases over past 7 days', 'Less than half the previous 7-day average', 'No confirmed cases in past 7 days']

// color palette
// var case_change_colour = d3.scaleOrdinal()
  // .domain(case_change_values)
  // .range(['#aaaaaa','#721606', '#005bd6', '#1fbfbb', '#c2f792'])

// var change_case_label = d3.scaleOrdinal()
  // .domain(case_change_values)
  // .range(['the average number of cases has not changed','7 day average cases appear to be increasing', '7 day average cases appear to be decreasing', 'average number of cases half what it was in previous 7 days', 'no confirmed cases in most recent complete 7 day period'])
