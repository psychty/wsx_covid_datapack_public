
library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'fingertipsR', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis'))

capwords = function(s, strict = FALSE) {
  cap = function(s) paste(toupper(substring(s, 1, 1)),
                          {s = substring(s, 2); if(strict) tolower(s) else s},sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}

options(scipen = 999)

ph_theme = function(){
  theme( 
    plot.title = element_text(colour = "#000000", face = "bold", size = 10),    
    plot.subtitle = element_text(colour = "#000000", size = 10),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"), 
    panel.grid.major.y = element_line(colour = "#E7E7E7", size = .3),
    panel.grid.minor.y = element_blank(), 
    strip.text = element_text(colour = "#000000", size = 10, face = "bold"), 
    strip.background = element_blank(), 
    axis.ticks = element_line(colour = "#dbdbdb"), 
    legend.position = "bottom", 
    legend.title = element_text(colour = "#000000", size = 9, face = "bold"), 
    legend.background = element_rect(fill = "#ffffff"), 
    legend.key = element_rect(fill = "#ffffff", colour = "#ffffff"), 
    legend.text = element_text(colour = "#000000", size = 9), 
    axis.text.y = element_text(colour = "#000000", size = 8), 
    axis.text.x = element_text(colour = "#000000", angle = 0, hjust = 1, vjust = .5, size = 8), 
    axis.title =  element_text(colour = "#000000", size = 9, face = "bold"),   
    axis.line = element_line(colour = "#dbdbdb")
  ) 
}

github_repo_dir <- "~/Documents/Repositories/wsx_covid_datapack_public"
output_directory_x <- paste0(github_repo_dir, '/Outputs')
areas_to_loop <- c('West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')

# 2019 MYE

mye_total <- read_csv('http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1816133633...1816133848,1820327937...1820328318,2092957697...2092957703,2013265921...2013265932&date=latest&gender=0&c_age=200&measures=20100&select=date_name,geography_name,geography_type,geography_code,obs_value') %>% 
  rename(Population = OBS_VALUE,
         Code = GEOGRAPHY_CODE,
         Name = GEOGRAPHY_NAME,
         Type = GEOGRAPHY_TYPE) %>% 
  select(-DATE_NAME) %>% 
  unique() %>% 
  group_by(Name, Code) %>% 
  mutate(Count = n()) %>% 
  mutate(Type = ifelse(Count == 2, 'Unitary Authority', ifelse(Type == 'local authorities: county / unitary (as of April 2019)', 'Upper Tier Local Authority', ifelse(Type == 'local authorities: district / unitary (as of April 2019)', 'Lower Tier Local Authority', ifelse(Type == 'regions', 'Region', ifelse(Type == 'countries', 'Country', Type)))))) %>% 
  ungroup() %>% 
  select(-Count) %>% 
  unique()

mye_total %>% 
  write.csv(., '/Users/richtyler/Documents/Repositories/wsx_covid_datapack_public/Source files/mye2019_ltla.csv', row.names = FALSE)

if(exists('mye_total') == FALSE) {
  mye_total <- read_csv('/Users/richtyler/Documents/Repositories/wsx_covid_datapack_public/Source files/mye2019_ltla.csv') %>%
    rename(Population = `All ages`,
           Type = Geography1)
}

area_code_names <- mye_total %>% 
  select(Code, Name)

mye_total <- mye_total %>%
  select(-Name)

# Pillar 1 and 2 combined time series ####

daily_cases <- read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv') %>%   
  rename(Name = `Area name`) %>% 
  rename(Code = `Area code`) %>% 
  rename(Date = `Specimen date`) %>% 
  rename(New_cases = `Daily lab-confirmed cases`) %>% 
  rename(Cumulative_cases = `Cumulative lab-confirmed cases`) %>% 
  arrange(Name, Date) %>% 
  select(Name, Code, `Area type`, Date, New_cases, Cumulative_cases) %>% 
  group_by(Name, Code, Date) %>% 
  mutate(Count = n()) %>% 
  filter(!(`Area type` == 'ltla' & Count == 2)) %>% 
  select(-c(`Area type`, Count)) %>% 
  left_join(mye_total, by = 'Code') %>% 
  ungroup()

# If no specimens are taken on a day, there is no row for it, and it would be missing data. Indeed, the only zeros are on the latest day. We need to therefore backfill and say if no date exists where it should, then add it, with the cumulative total and zero for new cases.

# One way to do this is to create a new dataframe with a row for each area and date, and left join the daily_cases data to it.
first_date <- min(daily_cases$Date)
last_case_date <- max(daily_cases$Date)

# remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
# install.packages("ukcovid19")
library(ukcovid19)

query_filters <- c(
  # "areaType=utla"
  'areaName=West Sussex'
)

query_structure <- list(
  date = "date", 
  name = "areaName", 
  code = "areaCode", 
  daily = "newCasesBySpecimenDate",
  cumulative = "cumCasesBySpecimenDate"
)

last_date <- as.Date(last_update(filters = query_filters, structure = query_structure))
# daily_cases <- get_data(filters = query_filters, structure = query_structure)
# last_date <- as.Date('2020-08-26')

Areas = daily_cases %>% 
  select(Name, Code, Type) %>% 
  unique()

Dates = seq.Date(first_date, last_case_date, by = '1 day')

daily_cases_reworked <- data.frame(Name = rep(Areas$Name, length(Dates)), Code = rep(Areas$Code, length(Dates)), Type = rep(Areas$Type, length(Dates)), check.names = FALSE) %>% 
  arrange(Name) %>% 
  group_by(Name) %>% 
  mutate(Date = seq.Date(first_date, last_case_date, by = '1 day')) %>% 
  left_join(daily_cases, by = c('Name', 'Code', 'Type', 'Date')) %>% 
  mutate(New_cases = ifelse(is.na(New_cases), 0, New_cases)) %>% 
  mutate(New_cumulative = cumsum(New_cases)) %>% 
  filter(!is.na(Cumulative_cases)) %>% 
  mutate(Calculated_same_as_original = ifelse(Cumulative_cases == New_cumulative, 'Yaas', 'Negative'))

# PHE say the last four data points are incomplete (perhaps they should not publish them). Instead, we need to make sure we account for this so that it is not misinterpreted.
complete_date <- last_date - 5

# We create a doubling time using the most recent 5 day period (and doubling time is recalculated every day once new data is available)

double_time_period <- 7 # this could be 5 or 7
case_x_number = 1 # This could be changed

# I've made an ifelse function that identifies 7 day periods for 100 days or 140 days of data if 7 day doubling time is used and this may have to change in the future.

daily_cases_reworked <- daily_cases_reworked %>%
  filter(Date >= '2020-09-01') %>%
  group_by(Name, Date) %>%
  summarise(New_cases = sum(New_cases, na.rm = 0)) %>%
  group_by(Name) %>%
  arrange(Name, Date) %>%
  mutate(Cumulative_cases = cumsum(New_cases)) %>%
  mutate(Seven_day_rolling_average_cases = round(rollapplyr(New_cases, 7, mean, align = 'right', partial = TRUE),0)) %>%
  mutate(Seven_day_sum_cases = round(rollapplyr(New_cases, 7, sum, align = 'right', partial = TRUE),0)) %>%
  mutate(Data_completeness = ifelse(Date > complete_date, 'Considered incomplete', 'Complete')) %>%
  ungroup() %>%
  mutate(Log10Cumulative_cases = log10(Cumulative_cases))

first_case_date <- daily_cases_reworked %>%
  group_by(Name) %>%
  filter(Cumulative_cases >= 1) %>%
  slice(1) %>%
  ungroup() %>%
  select(Name, Date) %>%
  rename(First_case_date = Date)

first_x_date <- daily_cases_reworked %>%
  group_by(Name) %>%
  filter(Cumulative_cases >= 1) %>%
  slice(1) %>%
  ungroup() %>%
  select(Name, Date) %>%
  rename(First_date_x_cases = Date)

firsts <- first_case_date %>%
  left_join(first_x_date, by = 'Name')

rm(first_case_date, first_x_date)

daily_cases_reworked <- daily_cases_reworked %>%
  group_by(Name) %>%
  left_join(firsts, by = c('Name')) %>%
  mutate(Days_since_first_case = as.numeric(difftime(Date, First_case_date, units = c("days")))) %>% # add days since start of data
  mutate(Days_since_case_x = as.numeric(difftime(Date, First_date_x_cases, units = c("days")))) %>%
  select(-c(First_case_date, First_date_x_cases)) %>%
  ungroup()

# Importantly, given that the data has changed from reported date to specimen date, and that the last five days have been concluded as incomplete, it would not be useful to report a doubling time for this time period.

doubling_time_df <- daily_cases_reworked %>%
  filter(Days_since_case_x >= 0) %>%
  filter(Data_completeness == 'Complete') %>%
  group_by(Name) %>%
  arrange(Name,Date) %>%
  mutate(period_in_reverse = ifelse(Date > max(Date) - (double_time_period), 1, ifelse(Date > max(Date) - (double_time_period * 2), 2, ifelse(Date > max(Date) - (double_time_period * 3), 3, ifelse(Date > max(Date) - (double_time_period * 4), 4, ifelse(Date > max(Date) - (double_time_period * 5), 5, ifelse(Date > max(Date) - (double_time_period * 6), 6, ifelse(Date > max(Date) - (double_time_period * 7), 7, ifelse(Date > max(Date) - (double_time_period * 8), 8, ifelse(Date > max(Date) - (double_time_period * 9), 9, ifelse(Date > max(Date) - (double_time_period * 10), 10, ifelse(Date > max(Date) - (double_time_period * 11), 11, ifelse(Date > max(Date) - (double_time_period * 12), 12, ifelse(Date > max(Date) - (double_time_period * 13), 13, ifelse(Date > max(Date) - (double_time_period * 14), 14, ifelse(Date > max(Date) - (double_time_period * 15), 15, ifelse(Date > max(Date) - (double_time_period * 16), 16, ifelse(Date > max(Date) - (double_time_period * 17), 17, ifelse(Date > max(Date) - (double_time_period * 18), 18, ifelse(Date > max(Date) - (double_time_period * 19), 19, ifelse(Date > max(Date) - (double_time_period * 20), 20, NA))))))))))))))))))))) %>%
  group_by(Name, period_in_reverse) %>%  
  mutate(Slope = coef(lm(Log10Cumulative_cases ~ Days_since_first_case))[2]) %>%
  mutate(Double_time = log(2, base = 10)/coef(lm(Log10Cumulative_cases ~ Days_since_first_case))[2]) %>%
  mutate(N_days_in_doubling_period = n()) %>%
  mutate(Cases_in_doubling_period = sum(New_cases, na.rm = TRUE)) %>%
  mutate(Double_time = ifelse(N_days_in_doubling_period != double_time_period, NA, ifelse(Cases_in_doubling_period == 0, NA, Double_time))) %>%
  mutate(Slope = ifelse(N_days_in_doubling_period != double_time_period, NA, Slope)) %>%
  mutate(date_range_label = paste0(ifelse(period_in_reverse == '1', paste0('most recent complete ', double_time_period, ' days ('), ifelse(period_in_reverse == '2', paste0('previous ', double_time_period, ' days ('), paste0('period ', period_in_reverse, ' ('))), format(min(Date), '%d-%B'), '-', format(max(Date), '%d-%B'), ')')) %>%
  mutate(date_range_label = ifelse(N_days_in_doubling_period != double_time_period, NA, date_range_label)) %>%
  mutate(short_date_label = paste0(format(min(Date), '%d-%b'), '-', format(max(Date), '%d-%b'))) %>%
  mutate(long_date_label = paste0(format(min(Date), '%d-%B'), ' and ', format(max(Date), '%d-%B'))) %>%
  mutate(date_range_label = ifelse(N_days_in_doubling_period != double_time_period, NA, date_range_label)) %>%
  mutate(short_date_label = ifelse(N_days_in_doubling_period != double_time_period, NA, short_date_label)) %>%
  mutate(long_date_label = ifelse(N_days_in_doubling_period != double_time_period, NA, long_date_label)) %>%
  ungroup()

# This transposes the period_in_reverse to show change over time (1 is last week)
doubling_time_df_summary <- doubling_time_df %>%
  select(Name, period_in_reverse, Double_time) %>%
  unique() %>%
  spread(period_in_reverse, Double_time) %>%
  select(Name, `1`,`2`) %>%
  rename(Latest_doubling_time = `1`,
         Previous_doubling_time = `2`)

doubling_time_df %>%
  select(period_in_reverse, date_range_label, short_date_label, long_date_label) %>%
  unique() %>%
  filter(!is.na(date_range_label)) %>%
  arrange(period_in_reverse)

doubling_time_latest <- doubling_time_df %>%
  filter(Date == max(Date)) %>%
  filter(Name %in% areas_to_loop) %>% 
  select(Name, Date, Cumulative_cases) %>%
  left_join(doubling_time_df_summary, by = 'Name') %>%
  mutate(Name = factor(Name, levels = c('Adur', 'Arun', 'Chichester', 'Crawley' ,'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex'))) %>%
  arrange(Name) %>% 
  mutate(`Date at which cases double given current doubling time` = format(Date + Latest_doubling_time, '%a %d %B')) %>% 
  mutate(`Change in doubling time` = ifelse(Latest_doubling_time > Previous_doubling_time, 'Cases slowing', ifelse(Latest_doubling_time < Previous_doubling_time, 'Cases speeding up', NA)))

red_threshold <- daily_cases %>% 
  select(Name, Population) %>% 
  unique() %>% 
  mutate(Threshold = Population * .001)

daily_cases_reworked <- daily_cases_reworked %>% 
  left_join(doubling_time_df[c('Name','Date','period_in_reverse', 'Slope', 'Double_time', 'Cases_in_doubling_period', 'date_range_label', 'short_date_label')], by = c('Name', 'Date')) %>% 
  mutate(Data_type = 'Observed') %>% 
  select(Name, Date, New_cases, Cumulative_cases, Data_type, Data_completeness, Double_time)

# This is the next point in our data series
predicted_double_time <- daily_cases_reworked %>% 
  select(Name, Date, Cumulative_cases, Data_completeness, Double_time) %>% 
  group_by(Name) %>% 
  filter(Data_completeness == 'Complete') %>% 
  filter(Date == max(Date)) %>% 
  mutate(Date = Date + Double_time) %>% 
  mutate(Cumulative_cases = Cumulative_cases * 2) %>% 
  mutate(Data_type = 'Predicted') %>% 
  select(Name, Date, Cumulative_cases, Data_type) 

daily_cases_final <- daily_cases_reworked %>% 
  arrange(Name, desc(Date)) %>% 
  mutate(Date_label = format(Date, '%a %d %B'))

i = 1

area_x <- areas_to_loop[i]

area_x_daily_cases <- daily_cases_final %>% 
  filter(Name == area_x)

area_x_predicted_double <- predicted_double_time %>% 
  filter(Name == area_x)

area_dates <- seq.Date(complete_date, area_x_predicted_double$Date, by = '1 day')

area_x_double_df <- daily_cases_final %>% 
  filter(Name == area_x) %>% 
  filter(Data_completeness != 'Considered incomplete') %>% 
  bind_rows(area_x_predicted_double) %>% 
  mutate(Cumulative_cases = as.numeric(Cumulative_cases)) %>% 
  mutate(Date = as.character(Date))

area_x_double_df_incomplete <- daily_cases_final %>% 
  filter(Name == area_x) %>% 
  filter(Data_completeness == 'Considered incomplete')

area_x_interpolated <- data.frame(Name = rep(area_x, length(area_dates))) %>% 
  mutate(Date = seq.Date(complete_date, area_x_predicted_double_date$Date, by = '1 day')) %>%
  mutate(Data_type = 'Predicted') %>%
  mutate(Date = as.character(Date)) %>% 
  left_join(area_x_double_df[c('Date', 'Cumulative_cases')], by = 'Date') %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Data_completeness = 'Predicted') %>% 
  mutate(Cumulative_cases = na.approx(Cumulative_cases))

area_x_double_df_complete <- area_x_double_df %>% 
  filter(Data_completeness == 'Complete') %>% 
  mutate(Date = as.Date(Date))

area_x_double_df <- area_x_interpolated %>% 
  filter(Date != complete_date) %>% 
  bind_rows(area_x_double_df_complete) %>% 
  arrange(Date) %>% 
  mutate(New_cases = Cumulative_cases - lag(Cumulative_cases, 1)) %>% 
  mutate(Rolling_7_day_new_cases = rollapply(New_cases, 7, sum, align = 'right', fill = NA)) %>% 
  left_join(red_threshold, by = 'Name')
  
ggplot() +
  geom_point(data = area_x_double_df,
             aes(x = Date,
                 y = Cumulative_cases,
                 group = Name,
                 colour = Data_completeness)) +
  geom_point(data = area_x_double_df_incomplete,
             aes(x = Date,
                 y = Cumulative_cases,
                 group = Name,
                 colour = Data_completeness)) 
