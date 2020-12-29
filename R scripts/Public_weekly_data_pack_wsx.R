# Public facing data pack - West Sussex and LTLAs

library(easypackages)

libraries("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis', 'epitools')

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

github_repo_dir <- '~/GitHub/wsx_covid_datapack_public'
# github_repo_dir <- "~/Documents/GitHub/wsx_covid_datapack_public"
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
 write.csv(., paste0(github_repo_dir,'/Source files/mye2019_ltla.csv'), row.names = FALSE)

if(exists('mye_total') == FALSE) {
mye_total <- read_csv(paste0(github_repo_dir,'/Source files/mye2019_ltla.csv')) %>%
rename(Type = Geography1)
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
# Case results are generally published in the afternoon and represent cases reported up to 9am of the reporting day. However, cases are assigned to the date of which the specimen was taken rather than when it was reported. This means it will be very unlikely that a specimen would be taken and results returned by 9am of the day of publication. As such, we consider the last five days (four days plus the day of reporting) as incomplete.

p12_test_df_raw <- data.frame(Name = rep(Areas$Name, length(Dates)), Code = rep(Areas$Code, length(Dates)), Type = rep(Areas$Type, length(Dates)), check.names = FALSE) %>% 
  arrange(Name) %>% 
  group_by(Name) %>% 
  mutate(Date = seq.Date(first_date, last_case_date, by = '1 day')) %>% 
  mutate(Data_completeness = ifelse(Date > complete_date, 'Considered incomplete', 'Complete')) %>% 
  left_join(daily_cases, by = c('Name', 'Code', 'Type', 'Date')) %>% 
  mutate(New_cases = ifelse(is.na(New_cases), 0, New_cases)) %>% 
  rename(Original_cumulative = Cumulative_cases) %>% # We should keep the original cumulative cases for reference
  mutate(Cumulative_cases = cumsum(New_cases)) %>% # These are based on the new cases data being accurate
  group_by(Name) %>% 
  mutate(Seven_day_average_new_cases = rollapply(New_cases, 7, mean, align = 'right', fill = NA)) %>%
  mutate(Seven_day_average_cumulative_cases = rollapply(Cumulative_cases, 7, mean, align = 'right', fill = NA)) %>% 
  mutate(Period = format(Date, '%d %B')) %>%
  select(-Population) %>% 
  left_join(mye_total[c('Code', 'Population')], by = 'Code') %>% 
  mutate(Cumulative_per_100000 = (Cumulative_cases / Population) * 100000) %>% 
  mutate(New_cases_per_100000 = (New_cases / Population) * 100000) %>% 
  mutate(new_case_key = factor(ifelse(New_cases == 0, 'No new cases', ifelse(New_cases >= 1 & New_cases <= 10, '1-10 cases', ifelse(New_cases >= 11 & New_cases <= 25, '11-25 cases', ifelse(New_cases >= 26 & New_cases <= 50, '26-50 cases', ifelse(New_cases >= 51 & New_cases <= 75, '51-75 cases', ifelse(New_cases >= 76 & New_cases <= 100, '76-100 cases', ifelse(New_cases >100, 'More than 100 cases', NA))))))), levels =  c('No new cases', '1-10 cases', '11-25 cases', '26-50 cases', '51-75 cases', '76-100 cases', 'More than 100 cases'))) %>%
  mutate(new_case_per_100000_key = factor(ifelse(New_cases_per_100000 < 0, 'Data revised down', ifelse(New_cases_per_100000 == 0, 'No new cases', ifelse(New_cases_per_100000 < 1, 'Less than 1 case per 100,000', ifelse(New_cases_per_100000 >= 1 & New_cases_per_100000 <= 2, '1-2 new cases per 100,000', ifelse(New_cases_per_100000 <= 4, '3-4 new cases per 100,000', ifelse(New_cases_per_100000 <= 6, '5-6 new cases per 100,000', ifelse(New_cases_per_100000 <= 8, '7-8 new cases per 100,000', ifelse(New_cases_per_100000 <= 10, '9-10 new cases per 100,000', ifelse(New_cases_per_100000 > 10, 'More than 10 new cases per 100,000', NA))))))))), levels =  c('No new cases', 'Less than 1 case per 100,000', '1-2 new cases per 100,000', '3-4 new cases per 100,000', '5-6 new cases per 100,000', '7-8 new cases per 100,000', '9-10 new cases per 100,000', 'More than 10 new cases per 100,000'))) %>%
  mutate(Case_label = paste0('A total of ', format(New_cases, big.mark = ',', trim = TRUE), ' people who had sample specimens taken on this day (representing new cases) were confirmed to have the virus',  ifelse(Data_completeness == 'Considered incomplete', paste0('.<font color = "#bf260a"> However, these figures should be considered incomplete until at least ', format(Date + 4, '%d %B'),'.</font>'),'.'), 'The total (cumulative) number of cases reported for people with specimens taken by this date (', Period, ') was ', format(Cumulative_cases, big.mark = ',', trim = TRUE),'.')) %>% 
  mutate(Rate_label = paste0('The new cases (swabbed on this date) represent <b>',format(round(New_cases_per_100000,1), big.mark = ',', trim = TRUE), '</b> cases per 100,000 population</p><p>The total (cumulative) number of Covid-19 cases per 100,000 population reported to date (', Period, ') is <b>', format(round(Cumulative_per_100000,1), big.mark = ',', trim = TRUE), '</b> cases per 100,000 population.')) %>% 
  mutate(Seven_day_ave_new_label = ifelse(is.na(Seven_day_average_new_cases), paste0('It is not possible to calculate a seven day rolling average of new cases for this date (', Period, ') because one of the values in the last seven days is missing.'), ifelse(Data_completeness == 'Considered incomplete', paste0('It can take around four days for results to be fully reported and data for this date (', Period, ') should be considered incomplete.', paste0('As such, the rolling average number of new cases in the last seven days (<b>', format(round(Seven_day_average_new_cases, 0), big.mark = ',', trim = TRUE), ' cases</b>) should be treated with caution.')), paste0('The rolling average number of new cases in the last seven days is <b>', format(round(Seven_day_average_new_cases, 0), big.mark = ',', trim = TRUE), '  cases</b>.')))) %>% 
  ungroup() %>% 
  mutate(Name = ifelse(Name == 'South East', 'South East region', Name))  %>% 
  mutate(Test_pillar = 'Pillars 1 and 2') %>% 
  group_by(Name) %>% 
  # mutate(Ten_day_average_new_cases = rollapply(New_cases, 10, mean, align = 'right', fill = NA)) %>% 
  # mutate(Fourteen_day_average_new_cases = rollapply(New_cases, 14, mean, align = 'right', fill = NA)) %>% 
  mutate(Rolling_7_day_new_cases = rollapply(New_cases, 7, sum, align = 'right', fill = NA)) %>% 
  mutate(Rolling_7_day_new_cases_per_100000 = ifelse(is.na(Rolling_7_day_new_cases), NA, (Rolling_7_day_new_cases / Population) * 100000)) %>% 
  mutate(Perc_change_on_rolling_7_days_actual = round((Rolling_7_day_new_cases - lag(Rolling_7_day_new_cases, 7))/ lag(Rolling_7_day_new_cases, 7), 2)) %>% 
  mutate(Perc_change_on_rolling_7_days_tidy = factor(ifelse(Rolling_7_day_new_cases == 0 & lag(Rolling_7_day_new_cases, 7) == 0, 'No change (zero cases)', ifelse(Perc_change_on_rolling_7_days_actual == Inf, '0 cases in previous 7 days', ifelse(Perc_change_on_rolling_7_days_actual == -1, '100% fewer cases (now zero cases in recent period)', ifelse(Perc_change_on_rolling_7_days_actual <= -.5, '50%+ fewer cases', ifelse(Perc_change_on_rolling_7_days_actual <= -.5, '50% fewer cases', ifelse(Perc_change_on_rolling_7_days_actual < 0, 'Up to 50% fewer cases', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'No change', ifelse(Perc_change_on_rolling_7_days_actual > 0  & Perc_change_on_rolling_7_days_actual <= .5, 'Up to 50% more cases', ifelse(Perc_change_on_rolling_7_days_actual > .5 & Perc_change_on_rolling_7_days_actual <= 1, 'Up to 100% more cases (double the cases from the previous 7 days)', ifelse(Perc_change_on_rolling_7_days_actual > 1 & Perc_change_on_rolling_7_days_actual <= 2, 'Up to 200% more cases (3x the cases in previous 7 days)', ifelse(Perc_change_on_rolling_7_days_actual > 2 & Perc_change_on_rolling_7_days_actual <= 5, 'Up to 5x the cases in previous 7 days', 'More than 5x the cases in the previous 7 days'))))))))))), levels = c("No change (zero cases)", "100% fewer cases (now zero cases in recent period)",   "50%+ fewer cases" ,  "Up to 50% fewer cases" ,  "No change" ,   "Up to 50% more cases" ,      "Up to 100% more cases (double the cases from the previous 7 days)", "Up to 200% more cases (3x the cases in previous 7 days)","Up to 5x the cases in previous 7 days", "More than 5x the cases in the previous 7 days", "0 cases in previous 7 days"))) %>% 
  mutate(Rolling_period = paste0('seven days to ', format(Date, '%d %B')),
         Rolling_compare_period = paste0('seven days to ', format(Date, '%d %B'), ' compared to seven days to ', format(lag(Date,7), '%d %B'))) %>% 
   mutate(Label_3 = paste0('In the ', Rolling_period, ', there ', ifelse(Rolling_7_day_new_cases == 1, paste0('was 1 new confirmed case (', format(round(Rolling_7_day_new_cases_per_100000, 1), big.mark = ',', trim = TRUE), ' new cases per 100,000). '),  paste0('were ', format(Rolling_7_day_new_cases, big.mark = ',', trim = TRUE), ' new confirmed cases (', format(round(Rolling_7_day_new_cases_per_100000, 1), big.mark = ',', trim = TRUE), ' new cases per 100,000). ')), ifelse(lag(Rolling_7_day_new_cases, 7) == 0 & Rolling_7_day_new_cases == 0, paste0('This is the same as the ', lag(Rolling_period, 7), '.'), ifelse(lag(Rolling_7_day_new_cases, 7) == 0 & Rolling_7_day_new_cases > 0, paste0('There were no cases in the ', lag(Rolling_period, 7), '.'), ifelse(Perc_change_on_rolling_7_days_actual == 0, paste0('There is <b>no change in cases</b> compared to those confirmed in the previous week (', lag(Rolling_period, 7), ').'), ifelse(Perc_change_on_rolling_7_days_actual < 0, paste0('In the ', lag(Rolling_period, 7), ', there ', ifelse(lag(Rolling_7_day_new_cases,7) ==1, 'was 1 new cases', paste0('were ', format(lag(Rolling_7_day_new_cases,7), big.mark = ',', trim = TRUE), ' new cases')),', and so the new cases have<b> decreased by ', abs(round(Perc_change_on_rolling_7_days_actual*100, 1)), '% (', format(lag(Rolling_7_day_new_cases, 7) - Rolling_7_day_new_cases, big.mark = ',', trim = TRUE), ' fewer cases)</b>.'), ifelse(Perc_change_on_rolling_7_days_actual > 0, paste0('In the ', lag(Rolling_period, 7), ', there ', ifelse(lag(Rolling_7_day_new_cases,7) == 1, 'was 1 new case ', paste0(' were ', format(lag(Rolling_7_day_new_cases,7), big.mark = ',', trim = TRUE), ' new cases')),' and so the new cases have<b> risen by ', round(Perc_change_on_rolling_7_days_actual* 100, 1), '% (', ifelse(Rolling_7_day_new_cases - lag(Rolling_7_day_new_cases, 7) == 1, '1 extra case).</b>', paste0(format(Rolling_7_day_new_cases - lag(Rolling_7_day_new_cases, 7), big.mark = ',', trim = TRUE),' extra cases).</b>'))), NA))))))) %>% 
  mutate(Previous_7_days_sum = lag(Rolling_7_day_new_cases, 7)) %>% 
  ungroup()

rm(daily_cases, Areas, Dates, first_date, mye_total, area_code_names, daily_cases_reworked)

# Small multiples plot with daily cases bars and rolling average.

p12_test_df_2 <- p12_test_df_raw %>% 
  group_by(Name) %>% 
  filter(Date %in% c(last_date - 1, last_date - 8)) %>% 
  select(Name, Date, Seven_day_average_new_cases) %>% 
  arrange(desc(Date)) %>% 
  mutate(Date = c('Latest_7_day_average', 'Previous_7_day_average')) %>% 
  spread(Date, Seven_day_average_new_cases) %>% 
  mutate(Colour_key = factor(ifelse(Latest_7_day_average == Previous_7_day_average, 'No change in average cases', ifelse(Latest_7_day_average > Previous_7_day_average, 'Increasing average number of\ncases over past 7 days', ifelse(Latest_7_day_average < Previous_7_day_average, 'Decreasing average number of\ncases over past 7 days', ifelse(Latest_7_day_average < (Previous_7_day_average/2), 'Less than half the previous 7-day average', ifelse(Latest_7_day_average == 0, 'No confirmed cases in past 7 days', NA))))), levels = c('No change in average cases','Increasing average number of\ncases over past 7 days', 'Decreasing average number of\ncases over past 7 days', 'Less than half the previous 7-day average', 'No confirmed cases in past 7 days')))

p12_test_df <- p12_test_df_raw %>% 
  left_join(p12_test_df_2, by = 'Name')

rm(p12_test_df_2, p12_test_df_raw)

for(i in 1:length(areas_to_loop)){
  
area_x <- areas_to_loop[i]

area_x_df_1 <- p12_test_df %>%
  filter(Name == area_x)

peak_case_label <- area_x_df_1 %>%
  filter(New_cases == max(New_cases, na.rm = TRUE)) %>%
  filter(Date == min(Date, na.rm = TRUE))

max_daily_case_limit <- ifelse(max(area_x_df_1$New_cases, na.rm = TRUE) < 50, round_any(max(area_x_df_1$New_cases, na.rm = TRUE), 5, ceiling), ifelse(max(area_x_df_1$New_cases, na.rm = TRUE) < 100, round_any(max(area_x_df_1$New_cases, na.rm = TRUE), 10, ceiling), ifelse(max(area_x_df_1$New_cases, na.rm = TRUE) < 250, round_any(max(area_x_df_1$New_cases, na.rm = TRUE), 25, ceiling), ifelse(max(area_x_df_1$New_cases, na.rm = TRUE) < 500, round_any(max(area_x_df_1$New_cases, na.rm = TRUE), 50, ceiling), round_any(max(area_x_df_1$New_cases, na.rm = TRUE), 100, ceiling)))))

max_daily_case_break <- ifelse(max(area_x_df_1$New_cases, na.rm = TRUE) < 20, 2, ifelse(max(area_x_df_1$New_cases, na.rm = TRUE) < 50, 5, ifelse(max(area_x_df_1$New_cases, na.rm = TRUE) < 100, 10, ifelse(max(area_x_df_1$New_cases, na.rm = TRUE) < 250, 25, ifelse(max(area_x_df_1$New_cases, na.rm = TRUE) < 500, 50, 100)))))


total_cases_reported_plot <- ggplot(area_x_df_1,
                                      aes(x = Date,
                                          y = New_cases,
                                          group = Test_pillar)) +
  geom_bar(stat = 'identity',
           position = 'stack',
           colour = '#ffffff',
           aes(fill = Test_pillar)) +
  geom_line(data = area_x_df_1,
            aes(x = Date,
                y = Seven_day_average_new_cases),
            group = 1,
            colour = '#000000') +
  scale_fill_manual(values = c('#071b7c'),
                    name = 'Pillar') +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date((last_date - 1) -(52*7), last_date -1, by = 7),
               limits = c(min(area_x_df_1$Date), last_date),
               expand = c(0.01,0.01)) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     breaks = seq(0,max_daily_case_limit, max_daily_case_break),
                     limits = c(0,max_daily_case_limit)) +
  geom_segment(x = complete_date+1,
               y = 0,
               xend = complete_date+1,
               yend = Inf,
               color = "red",
               linetype = "dashed") +
  annotate('rect',
           xmin = complete_date + 1,
           xmax = max(area_x_df_1$Date),
           ymin = 0,
           ymax = Inf,
           fill = '#cccccc',
           alpha = .25) +
  annotate('text',
           x = complete_date,
           y = max_daily_case_limit * .8,
           label = 'Totals for\nthese days\nnot considered\ncomplete:',
           size = 2.5,
           hjust = 1) +
  labs(x = 'Date',
       y = 'Number of daily confirmed cases',
       title = paste0('Daily number of confirmed Covid-19 cases; Pillar 1 and 2 combined; ', area_x),
       subtitle = paste0('Confirmed cases by specimen date; as at ', format(last_date, '%d %B %Y')),
       caption = 'The black line represents the average number of new cases confirmed in the previous seven days.') +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  theme(legend.position = 'none')

png(paste0(output_directory_x, '/Figure_1_', gsub(' ', '_', area_x), '_confirmed_daily_cases.png'),
    width = 1580,
    height = 750,
    res = 200)
print(total_cases_reported_plot)
dev.off()

}

ltla_p12_test_df <- p12_test_df %>% 
  filter(Name %in% areas_to_loop) %>% 
  filter(Name != 'West Sussex')

max_daily_case_limit <- ifelse(max(ltla_p12_test_df$New_cases, na.rm = TRUE) < 50, round_any(max(ltla_p12_test_df$New_cases, na.rm = TRUE), 5, ceiling), ifelse(max(ltla_p12_test_df$New_cases, na.rm = TRUE) < 100, round_any(max(ltla_p12_test_df$New_cases, na.rm = TRUE), 10, ceiling), ifelse(max(ltla_p12_test_df$New_cases, na.rm = TRUE) < 250, round_any(max(ltla_p12_test_df$New_cases, na.rm = TRUE), 25, ceiling), ifelse(max(ltla_p12_test_df$New_cases, na.rm = TRUE) < 500, round_any(max(ltla_p12_test_df$New_cases, na.rm = TRUE), 50, ceiling), round_any(max(ltla_p12_test_df$New_cases, na.rm = TRUE), 100, ceiling)))))

max_daily_case_break <- ifelse(max(ltla_p12_test_df$New_cases, na.rm = TRUE) < 20, 2, ifelse(max(ltla_p12_test_df$New_cases, na.rm = TRUE) < 50, 5, ifelse(max(ltla_p12_test_df$New_cases, na.rm = TRUE) < 100, 10, ifelse(max(ltla_p12_test_df$New_cases, na.rm = TRUE) < 250, 25, ifelse(max(ltla_p12_test_df$New_cases, na.rm = TRUE) < 500, 50, 100)))))

total_cases_reported_plot_2 <- ggplot(ltla_p12_test_df,
       aes(x = Date,
           y = New_cases,
           group = Test_pillar)) +
  geom_bar(stat = 'identity',
           position = 'stack',
           colour = '#ffffff',
           fill = '#071b7c',
           aes(fill = Test_pillar)) +
  geom_line(data = ltla_p12_test_df,
            aes(x = Date,
                y = Seven_day_average_new_cases),
            group = 1,
            colour = '#000000') +
  scale_fill_manual(values = c('#071b7c'),
                    name = 'Pillar') +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date((last_date -1) -(52*7), last_date-1, by = 7),
               limits = c(min(ltla_p12_test_df$Date), last_date),
               expand = c(0.01,0.01)) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     breaks = seq(0,max_daily_case_limit, max_daily_case_break),
                     limits = c(0,max_daily_case_limit)) +
  geom_segment(x = complete_date+1,
               y = 0,
               xend = complete_date+1,
               yend = Inf,
               color = "red",
               linetype = "dashed") +
  annotate('rect',
           xmin = complete_date + 1,
           xmax = max(ltla_p12_test_df$Date),
           ymin = 0,
           ymax = Inf,
           fill = '#cccccc',
           alpha = .25) +
  annotate('text',
           x = complete_date,
           y = max_daily_case_limit * .8,
           label = 'Totals for\nthese days\nnot considered\ncomplete:',
           size = 2.5,
           hjust = 1) +
  labs(x = 'Date',
       y = 'Number of daily confirmed cases',
       title = paste0('Daily number of confirmed Covid-19 cases; Pillar 1 and 2 combined; West Sussex lower tier Local Authorities'),
       subtitle = paste0('Confirmed cases by specimen date; as at ', format(last_date, '%d %B %Y')),
       caption = 'The black line represents the average number of new cases confirmed in the previous seven days.') +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  facet_wrap(~Name, ncol = 3) +
  theme(legend.position = 'none')

png(paste0(output_directory_x, '/Small_multiples_covid_19_confirmed_cases.png'),
    width = 1580,
    height = 1050,
    res = 120)
print(total_cases_reported_plot_2)
dev.off()

# exporting for web ####

test_timeline <- data.frame(Date_label_2 = c('27 Mar', '15 Apr','17 Apr','23 Apr','28 Apr', '18 May', '27 May', '06 Jul'), Change = c('front-line NHS Staff', 'those in social care settings','additional front-line workers', 'all symptomatic essential worker and members of their households','anyone aged 65+ who must leave their home for work plus asymptomatic NHS and Social Care staff and care home residents', 'anyone aged 5+ who is showing signs of Covid-19', 'anyone with Covid-19 symptoms regardless of age', 'care homes receive more frequent routine testing and even without symptoms')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/uk_testing_key_dates.json'))

easing_timeline <- data.frame(Date_label_2 = c('23 Mar', '13 May', '01 Jun', '15 Jun', '04 Jul', '13 Jul', '24 Jul', '25 Jul', '30 Jul', '01 Aug', '03 Aug', '15 Aug', '26 Aug', '14 Sep', '21 Sep', '24 Sep', '14 Oct', '05 Nov'), Change = c('Lockdown starts, schools were closed to all but a few children and people are asked to stay at home.', 'Some people began returning to work if they were unable to work from home.', 'Schools reopened for more pupils in early years, reception, and years 1 and 6.<br>People were allowed to meet outdoors and those who were shielding advised that they could now go outdoors with people in their household.', 'Non-essential shops were allowed to reopen if safe, more year groups back to school, and face coverings became mandatory on public transport.', 'Change to social distancing advice from 2m to 1m+, some hospitality and leisure businesses allow to reopen, and two households allowed to meet inside whilst up to six people from different households allowed to meet outside.', 'Beauty salons, nail bars, tattoo studios allowed to reopen but cannot offer treatments or services which involve work directly in front of the face.', 'Face coverings became mandatory in many enclosed public spaces such as shops and banks.', 'Indoor gyms, swimming pools, and leisure centres reopen.', 'Self-isolation period for those with COVID-19 symptoms or a positive test increases from 7 to 10 days.', 'Shielding for clinically extremely vulnerable paused.<br>The government no longer recommends working from home if it is safe to go to work.', 'People encouraged to go to restaurants through the Eat Out to Help Out scheme offering discounted food from Monday-Wednesdays in August.', 'Casinos, indoor play and soft play centres, skating rinks, and bowling alleys reopen.<br>Beauty salons, spas, tattoo studios, and barbers now able to offer close contact services.', 'Schools and colleges have discretion to require face coverings in indoor communal areas where social distancing cannot be safely managed.', 'People can no longer meet with other households socially in groups of more than six people (including children). This includes private homes and gardens.', 'Childcare and unpaid care exempted from any interhousehold mixing restrictions in local areas.', 'Restaurants and bars must close at 10pm.<br>Those who can work from home now encouraged to do so by the government.', 'A three tiered set of restrictions for local areas in England began, mostly in northern England','A new national lockdown was announced, with education remaining open and no official shielding return')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/uk_restrictions_key_dates.json'))

wsx_summary_p1 <- p12_test_df %>% 
  filter(Date == max(Date)) %>% 
  select(Name, Cumulative_cases, Cumulative_per_100000, Seven_day_average_new_cases, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000) %>% 
  rename('Total confirmed cases so far' = Cumulative_cases,
         'Total cases per 100,000 population' = Cumulative_per_100000,
         'Average number of confirmed cases tested in most recent seven days' = Seven_day_average_new_cases,
         'Total cases confirmed in most recent seven days' = Rolling_7_day_new_cases,
         'Total cases per 100,000 population confirmed in most recent seven days' = Rolling_7_day_new_cases_per_100000)

wsx_summary_p2 <- p12_test_df %>% 
  filter(Data_completeness == 'Complete') %>% 
  filter(Date == max(Date)) %>% 
  select(Name, New_cases, New_cases_per_100000, Colour_key) %>% 
  rename('Confirmed cases swabbed on most recent complete day' = New_cases,
         'Confirmed cases swabbed per 100,000 population on most recent complete day' = New_cases_per_100000,
         'Change in average cases' = Colour_key)

wsx_summary <- wsx_summary_p1 %>% 
  left_join(wsx_summary_p2, by = 'Name')

wsx_summary %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/wsx_case_summary.json'))

wsx_daily_cases <- p12_test_df %>% 
mutate(Date_label = format(Date, '%a %d %B')) %>% 
select(Name, Date, New_cases, new_case_key, New_cases_per_100000, new_case_per_100000_key, Seven_day_average_new_cases, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Case_label, Rate_label, Seven_day_ave_new_label, Colour_key, Cumulative_cases)

wsx_daily_cases %>% 
  filter(Name %in% c('West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex','Worthing', 'South East region', 'England')) %>% 
  mutate(Period = format(Date, '%d %B')) %>% 
  mutate(Date_label = format(Date, '%a %d %B')) %>% 
  mutate(Date_label_2 = format(Date, '%d %b')) %>% 
  mutate(Colour_key = gsub('\n',' ', Colour_key)) %>% 
  # select(Name, Date, Date_label, Date_label_2, New_cases, new_case_key, New_cases_per_100000, new_case_per_100000_key, Seven_day_average_new_cases, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Case_label, Rate_label, Seven_day_ave_new_label) %>% 
  select(Name, Date_label, Date_label_2, New_cases, new_case_key, New_cases_per_100000, new_case_per_100000_key, Seven_day_average_new_cases, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Case_label, Rate_label) %>%
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/wsx_daily_cases.json'))

wsx_daily_cases %>% 
  ungroup() %>% 
  filter(Name %in% c('West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex','Worthing', 'South East region', 'England')) %>% 
  group_by(Name) %>% 
  summarise(Max_limit = round_any(max(New_cases), 10, ceiling)) %>% 
  ungroup() %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/wsx_daily_case_limits.json'))

levels(wsx_daily_cases$new_case_key) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/daily_cases_bands.json'))

levels(wsx_daily_cases$new_case_per_100000_key) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/daily_cases_per_100000_bands.json'))

wsx_daily_cases %>% 
  ungroup() %>% 
  filter(Date == min(Date) | Date == last_date -1) %>% 
  select(Date) %>% 
  unique() %>% 
  add_row(Date = complete_date - 7) %>% 
  add_row(Date = complete_date) %>% 
  add_row(Date = complete_date + 1) %>% 
  mutate(Period = format(Date, '%d %B')) %>% 
  mutate(Date_label = format(Date, '%a %d %B')) %>% 
  arrange(Date) %>% 
  add_column(Order = c('First', 'Seven_days_ago', 'Complete', 'First_incomplete', 'Last')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/range_dates.json'))

p12_test_df %>% 
  ungroup() %>% 
  filter(Date %in% seq.Date((last_date-1) -(52*7), last_date -1, by = 14)) %>% 
  mutate(Date_label = format(Date, '%d %b')) %>% 
  select(Date_label) %>% 
  unique() %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/case_change_dates.json'))

data.frame(time = c('Latest', 'Previous'), range = c(paste0(format(last_date - 7, '%d %b') , ' and ', format(last_date -1, '%d %b')),  paste0(format(last_date - 14, '%d %b') , '-', format(last_date - 8, '%d %b')))) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/case_change_date_range.json'))

format(complete_date+1, '%d %b') %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/first_incomplete_daily_case.json'))

format(last_date - 1, '%d %b') %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/latest_daily_case.json'))

format(last_date, '%d %B %Y') %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/daily_case_update_date.json'))

# Export df for KG - 7 day rolling trend ####

per_day_trend <- p12_test_df %>% 
  select(Code, Name, Type, Date, New_cases, Cumulative_cases, Rolling_7_day_new_cases, Seven_day_average_new_cases, Population) %>% 
  rename(MYE2019 = Population)

per_day_trend %>% 
  write.csv(., paste0(output_directory_x, '/rolling_7_day_cases_all_areas.csv'), row.names = FALSE)

# Heatmap of cases ####
hm_theme = function(){
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
    legend.position = "bottom",
    legend.text = element_text(colour = "#323232", size = 8),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(colour = "#000000", face = "bold", size = 9, vjust = 1), 
    legend.title = element_text(colour = "#323232", face = "bold", size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.key.size = unit(0.65, "lines"),
    legend.background = element_rect(fill = "#ffffff"),
    legend.key = element_rect(fill = "#ffffff", colour = "#E2E2E3"),
    strip.text = element_text(colour = "#000000", face = "bold"),
    strip.background = element_rect(fill = "#ffffff"),
    axis.ticks.x = element_blank())
}

hm_df <- p12_test_df %>%
  filter(Test_pillar == 'Pillars 1 and 2') %>%
  filter(Name %in% areas_to_loop) %>%
  select(Date, Name, Test_pillar, New_cases_per_100000) %>%
  mutate(new_case_per_100000_key = factor(ifelse(New_cases_per_100000 < 0, 'Data revised down', ifelse(New_cases_per_100000 == 0, 'No new cases', ifelse(New_cases_per_100000 < 1, 'Less than 1 case per 100,000', ifelse(New_cases_per_100000 >= 1 & New_cases_per_100000 <= 2, '1-2 new cases per 100,000', ifelse(New_cases_per_100000 <= 4, '3-4 new cases per 100,000', ifelse(New_cases_per_100000 <= 6, '5-6 new cases per 100,000', ifelse(New_cases_per_100000 <= 8, '7-8 new cases per 100,000', ifelse(New_cases_per_100000 <= 10, '9-10 new cases per 100,000', ifelse(New_cases_per_100000 > 10, 'More than 10 new cases per 100,000', NA))))))))), levels =  c('No new cases', 'Less than 1 case per 100,000', '1-2 new cases per 100,000', '3-4 new cases per 100,000', '5-6 new cases per 100,000', '7-8 new cases per 100,000', '9-10 new cases per 100,000', 'More than 10 new cases per 100,000'))) %>%
  mutate(Name = factor(Name, levels = rev(areas_to_loop))) %>%
  arrange(Name)

new_case_rate_plot <- ggplot(hm_df, aes(x = Date,
                                        y = Name,
                                        fill = new_case_per_100000_key)) +
  scale_fill_manual(values = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026'),
                    name = 'Tile\ncolour\nkey',
                    drop = FALSE) +
  geom_tile(colour = "#ffffff") +
  labs(title = paste0('Summary of new confirmed Covid-19 cases per 100,000 population (all ages); as at ', format(last_date, '%d %B')),
       x = NULL,
       y = NULL,
       caption = 'Cases for dates after the red dashed line are not considered complete due to a lag in test result reporting.') +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date((last_date -1) -(52*7), last_date -1, by = 7),
               limits = c(min(hm_df$Date), max(hm_df$Date)),
               expand = c(0,0.0)) +
  # scale_y_discrete(position = 'right') +
  hm_theme() +
  theme(axis.text.y = element_text(colour = "#323232", 
                                   #face = case_summary$highlight, 
                                   size = 8)) +
  theme(legend.position = 'top') +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  geom_segment(x = complete_date + 1,
               y = 0,
               xend = complete_date + 1,
               yend = Inf,
               color = "red",
               size = .15,
               linetype = "dashed") 

png(paste0(output_directory_x, '/Figure_2_confirmed_heatmap_rate.png'),
    width = 1480,
    height = 650,
    res = 200)
print(new_case_rate_plot)
dev.off()

# UTLA Cumulative rate map ####
utla_rate_1 <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == max(Date)) %>%
  # filter(Date == '2020-07-12') %>% 
  filter(Type %in% c('Upper Tier Local Authority', 'Unitary Authority')) %>% 
  select(Code, Name, Date, Cumulative_cases, Cumulative_per_100000) %>%
  mutate(Cumulative_rate_rank = rank(-Cumulative_per_100000)) %>% 
  mutate(Cumulative_Rate_decile_actual = abs(ntile(Cumulative_per_100000, 10) - 11)) %>% 
  mutate(Cumulative_Rate_decile = factor(ifelse(Cumulative_Rate_decile_actual == 1, '10% of authorities\nwith highest rate', ifelse(Cumulative_Rate_decile_actual == 10, '10% of authorities\nwith lowest rate', paste0('Decile ', Cumulative_Rate_decile_actual))), levels = c('10% of authorities\nwith highest rate','Decile 2','Decile 3','Decile 4','Decile 5','Decile 6','Decile 7','Decile 8','Decile 9','10% of authorities\nwith lowest rate'))) 

england_cumulative <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == max(Date)) %>% 
  filter(Name == 'England') %>% 
  select(Code, Name, Date, Cumulative_cases, Cumulative_per_100000)

england_rolling <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == complete_date) %>% 
  filter(Name == 'England') %>% 
  select(Name, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Rolling_period, Label_3)

paste0('The number of confirmed COVID-19 cases in England so far as at ', format(last_date, '%d %B'), ' is ', format(england_cumulative$Cumulative_cases, big.mark = ',', trim = TRUE), ' (<b>', format(round(england_cumulative$Cumulative_per_100000, 1), big.mark = ',', trim = TRUE), ' cases per 100,000 population</b>). ', england_rolling$Label_3) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/england_cumulative.json'))

wsx_cumulative <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == max(Date)) %>% 
  filter(Name == 'West Sussex') %>% 
  select(Code, Name, Date, Cumulative_cases, Cumulative_per_100000)

wsx_rolling <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == complete_date) %>% 
  filter(Name == 'West Sussex') %>% 
  select(Name, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Rolling_period, Label_3)

paste0('The number of confirmed COVID-19 cases in West Sussex so far as at ', format(last_date, '%d %B'), ' is ', format(wsx_cumulative$Cumulative_cases, big.mark = ',', trim = TRUE), ' (<b>', format(round(wsx_cumulative$Cumulative_per_100000, 1), big.mark = ',', trim = TRUE), ' cases per 100,000 population</b>). ', wsx_rolling$Label_3) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/wsx_cumulative_rolling_label.json'))

utla_rate_2 <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == complete_date) %>%
  filter(Type %in% c('Upper Tier Local Authority', 'Unitary Authority')) %>% 
  select(Code, Name, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Rolling_period, Perc_change_on_rolling_7_days_tidy, Label_3) %>%
  mutate(Rolling_rate_rank = rank(-Rolling_7_day_new_cases_per_100000)) %>% 
  mutate(Rolling_Rate_decile_actual = abs(ntile(Rolling_7_day_new_cases_per_100000, 10) - 11)) %>% 
  mutate(Rolling_Rate_decile = factor(ifelse(Rolling_Rate_decile_actual == 1, '10% of authorities\nwith highest rate', ifelse(Rolling_Rate_decile_actual == 10, '10% of authorities\nwith lowest rate', paste0('Decile ', Rolling_Rate_decile_actual))), levels = c('10% of authorities\nwith highest rate','Decile 2','Decile 3','Decile 4','Decile 5','Decile 6','Decile 7','Decile 8','Decile 9','10% of authorities\nwith lowest rate'))) %>% 
  arrange(Code)

utla_rate <- utla_rate_1 %>% 
  left_join(utla_rate_2, by = c('Code', 'Name'))

rm(utla_rate_1, utla_rate_2)

utla_cumulative_rate_bins <- utla_rate %>% 
  group_by(Cumulative_Rate_decile) %>% 
  summarise(cumulative_bins = paste0(Cumulative_Rate_decile, ' (', format(round(min(Cumulative_per_100000),1), big.mark = ',', trim = TRUE), '-', format(round(max(Cumulative_per_100000)),big.mark = ',', trim = TRUE), ')')) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(cumulative_bins = factor(cumulative_bins, levels = cumulative_bins))

utla_rolling_rate_bins <- utla_rate %>% 
  group_by(Rolling_Rate_decile) %>% 
  summarise(rolling_bins = paste0(Rolling_Rate_decile, ' (', format(round(min(Rolling_7_day_new_cases_per_100000),1), big.mark = ',', trim = TRUE), '-', format(round(max(Rolling_7_day_new_cases_per_100000)),big.mark = ',', trim = TRUE), ')')) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(rolling_bins = gsub('\n', ' ', rolling_bins)) %>% 
  mutate(rolling_bins = factor(rolling_bins, levels = rolling_bins))

utla_rate <- utla_rate %>% 
  left_join(utla_cumulative_rate_bins, by = 'Cumulative_Rate_decile') %>% 
  left_join(utla_rolling_rate_bins, by = 'Rolling_Rate_decile')

summary_table_rate_1 <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == max(Date)) %>% 
  select(Name, Cumulative_cases, Cumulative_per_100000) %>% 
  filter(Name %in% c('South East region', 'England')) %>% 
  mutate(Cumulative_rate_rank = '-',
         Cumulative_Rate_decile = '-')

summary_table_rate_2 <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == complete_date) %>% 
  select(Name, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Rolling_period) %>% 
  filter(Name %in% c('South East region', 'England')) %>% 
  mutate(Rolling_rate_rank = '-',
         Rolling_Rate_decile = '-')

summary_table_rate <- summary_table_rate_1 %>% 
  left_join(summary_table_rate_2, by = 'Name')

utla_rate_wsx <- utla_rate %>% 
  select(Name, Cumulative_cases, Cumulative_per_100000, Cumulative_rate_rank, Cumulative_Rate_decile, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Rolling_rate_rank, Rolling_Rate_decile, Rolling_period) %>% 
  mutate(Cumulative_rate_rank = ordinal(Cumulative_rate_rank)) %>% 
  mutate(Rolling_rate_rank = ordinal(Rolling_rate_rank)) %>% 
  filter(Name == 'West Sussex') %>% 
  bind_rows(summary_table_rate) %>% 
  mutate(Cumulative_cases = format(Cumulative_cases, big.mark = ',', trim = TRUE),
         Cumulative_per_100000 = format(round(Cumulative_per_100000, 1), big.mark = ',', trim = TRUE),
         Rolling_7_day_new_cases = format(Rolling_7_day_new_cases, big.mark = ',', trim = TRUE),
         Rolling_7_day_new_cases_per_100000 = format(round(Rolling_7_day_new_cases_per_100000, 1), big.mark = ',', trim = TRUE)) %>% 
  rename(`Cumulative cases` = Cumulative_cases,
        `Cumulative rate per 100,000 residents` =  Cumulative_per_100000,
        `Local Authority Rank (out of 149) where 1 = Highest Rate per 100,000` = Cumulative_rate_rank,
        `Decile of cumulative rate per 100,000` =  Cumulative_Rate_decile,
        `Rolling 7-day new cases` = Rolling_7_day_new_cases,
        `Rolling 7-day case rate per 100,000` = Rolling_7_day_new_cases_per_100000,
        `Local Authority Rank (out of 149) where 1 = Highest Rolling 7-day rate per 100,000` = Rolling_rate_rank,
        `Decile of rolling rate per 100,000` =  Rolling_Rate_decile)

utla_rate_wsx %>% 
  write.csv(., paste0(output_directory_x, '/utla_rate_wsx.csv'), row.names = FALSE)

bord_style <- fp_border(color = "black", style = "solid", width = .5)

cum_utla_rate_wsx <- utla_rate_wsx %>% 
  select(Name, `Cumulative cases`, `Cumulative rate per 100,000 residents`, `Local Authority Rank (out of 149) where 1 = Highest Rate per 100,000`, `Decile of cumulative rate per 100,000`)

ft_utla_rate_wsx <- flextable(cum_utla_rate_wsx) %>% 
  width(width = .9) %>%
  align(j = 1, align = 'left') %>% 
  width(j = 1, width = 1.3) %>%
  width(j = 4, width = 1.3) %>%
  width(j = 5, width = 1.3) %>%
  fontsize(part = "header", size = 10) %>% 
  fontsize(part = "body", size = 11) %>% 
  font(fontname = "Calibri") %>% 
  height_all(height = .2) %>% 
  valign(valign = "top", part = "all") %>% 
  bold(part = "header")%>% 
  align(j = 1, align = 'left') %>% 
  hline(i = 1, border = bord_style, part = 'header') %>% 
  hline_bottom(border = bord_style ) %>% 
  hline_top(border = bord_style, part = "all" )

rolling_utla_rate_wsx <- utla_rate_wsx %>% 
  select(Name, `Rolling 7-day new cases`, `Rolling 7-day case rate per 100,000`, `Local Authority Rank (out of 149) where 1 = Highest Rolling 7-day rate per 100,000`, `Decile of rolling rate per 100,000`)

rolling_period_x <- unique(utla_rate_wsx$Rolling_period)

ft_utla_rolling_rate_wsx <- flextable(rolling_utla_rate_wsx) %>% 
  width(width = .9) %>%
  align(j = 1, align = 'left') %>% 
  width(j = 1, width = 1.3) %>%
  width(j = 4, width = 1.3) %>%
  width(j = 5, width = 1.3) %>%
  fontsize(part = "header", size = 10) %>% 
  fontsize(part = "body", size = 11) %>% 
  font(fontname = "Calibri") %>% 
  height_all(height = .2) %>% 
  valign(valign = "top", part = "all") %>% 
  bold(part = "header")%>% 
  align(j = 1, align = 'left') %>% 
  hline(i = 1, border = bord_style, part = 'header') %>% 
  hline_bottom(border = bord_style ) %>% 
  hline_top(border = bord_style, part = "all" )

utla_ua_boundaries_json <- geojson_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson",  what = "sp") %>% 
  filter(substr(ctyua19cd, 1,1 ) == 'E') %>% 
  mutate(ctyua19nm = ifelse(ctyua19nm %in% c('Cornwall', 'Isles of Scilly'), 'Cornwall and Isles of Scilly', ifelse(ctyua19nm %in% c('City of London', 'Hackney'), 'Hackney and City of London', ctyua19nm))) %>% 
    mutate(ctyua19cd = ifelse(ctyua19cd %in% c('E06000053', 'E06000052'), 'E06000052', ifelse(ctyua19cd %in% c('E09000001', 'E09000012'), 'E09000012', ctyua19cd))) %>% 
  group_by(ctyua19cd, ctyua19nm) %>% 
  summarise() %>% 
  arrange(ctyua19cd) %>% 
  left_join(utla_rate, by = c('ctyua19cd' = 'Code')) 

utla_ua_boundaries <- geojson_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson",  what = "sp") %>% 
  fortify(region = "ctyua19cd") %>% 
  rename(ctyua19cd = id) %>% 
  filter(substr(ctyua19cd, 1,1 ) == 'E') %>% 
  left_join(utla_rate, by = c('ctyua19cd' = 'Code')) %>% 
  filter(!is.na(Cumulative_per_100000))

map_theme = function(){
  theme( 
    legend.position = "left", 
    legend.key.size = unit(.75,"line"),
    legend.title = element_text(size = 8, face = 'bold'),
    plot.background = element_blank(), 
    plot.title.position = "plot",
    panel.background = element_blank(),  
    panel.border = element_blank(),
    axis.text = element_blank(), 
    plot.title = element_text(colour = "#000000", face = "bold", size = 11), 
    plot.subtitle = element_text(colour = "#000000", size = 10), 
    axis.title = element_blank(),     
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    strip.text = element_text(colour = "white"), 
    strip.background = element_rect(fill = "#327d9c"), 
    axis.ticks = element_blank()
  ) 
} 

map_1 <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = utla_ua_boundaries,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = cumulative_bins),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_manual(values = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'),
                    name = 'Decile of cumulative\nrate per 100k') +
  labs(title = paste0('Cumulative rate of confirmed Covid-19 cases per 100,000 population (all ages);\nPillar 1 and 2 combined; Upper Tier Local and Unitary Authorities'),
       subtitle = paste0('Confirmed cases by specimen date; as at ', format(last_date, '%d %B %Y')))  +
  theme(legend.position = c(.1,.55))

inset_1 <- ggplot() +
coord_fixed(1.5) +
               map_theme() +
               geom_polygon(data = subset(utla_ua_boundaries, Name %in% c(c("Barking and Dagenham", "Barnet", "Bexley","Brent", "Bromley", "Camden", "City of London", "Croydon","Ealing", "Enfield", "Greenwich", "Hackney","Hammersmith and Fulham", "Haringey", "Harrow","Havering", "Hillingdon", "Hounslow", "Islington","Kensington and Chelsea", "Kingston upon Thames", "Lambeth","Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames","Southwark", "Sutton", "Tower Hamlets", "Waltham Forest","Wandsworth", "Westminster"))),
                            aes(x=long,
                                y=lat,
                                group = group,
                                fill = cumulative_bins),
                            color="#ffffff",
                            size = .1,
                            alpha = 1,
                            show.legend = FALSE) +
               scale_fill_manual(values = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'),
                                 name = 'Decile of cumulative\nrate per 100k') +
  labs(title = 'London') +
  theme(plot.background  = element_rect(colour = "black", fill=NA, size=.1),
        plot.title = element_text(size = 8))

png(paste0(output_directory_x, '/Figure_3_cumulative_rate_utla_latest.png'),
    width = 1480,
    height = 1480,
    res = 180)
print(map_1)
print(inset_1, vp = viewport(0.2, 0.8, width = 0.22, height = 0.22))
dev.off()

map_1b <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = utla_ua_boundaries,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = rolling_bins),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_manual(values = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'),
                    name = 'Decile of rolling\n7 day rate per 100k') +
  labs(title = paste0('Rolling 7 day rate of confirmed Covid-19 cases per 100,000 population (all ages);\nPillar 1 and 2 combined; Upper Tier Local and Unitary Authorities'),
       subtitle = paste0('Confirmed cases in the ', rolling_period_x, '; as at ', format(last_date, '%d %B %Y')))  +
  theme(legend.position = c(.1,.55))

inset_1b <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = subset(utla_ua_boundaries, Name %in% c(c("Barking and Dagenham", "Barnet", "Bexley","Brent", "Bromley", "Camden", "City of London", "Croydon","Ealing", "Enfield", "Greenwich", "Hackney","Hammersmith and Fulham", "Haringey", "Harrow","Havering", "Hillingdon", "Hounslow", "Islington","Kensington and Chelsea", "Kingston upon Thames", "Lambeth","Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames","Southwark", "Sutton", "Tower Hamlets", "Waltham Forest","Wandsworth", "Westminster"))),
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = rolling_bins),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = FALSE) +
  scale_fill_manual(values = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'),
                    name = 'Decile of rolling\nrate per 100k') +
  labs(title = 'London') +
  theme(plot.background  = element_rect(colour = "black", fill=NA, size=.1),
        plot.title = element_text(size = 8))

png(paste0(output_directory_x, '/Figure_3_rolling_rate_utla_latest.png'),
    width = 1480,
    height = 1480,
    res = 180)
print(map_1b)
print(inset_1b, vp = viewport(0.2, 0.8, width = 0.22, height = 0.22))
dev.off()

utla_cumulative_rate_bins <- utla_cumulative_rate_bins %>% 
  mutate(cumulative_bins = gsub('\n',' ', cumulative_bins)) %>% 
  mutate(cumulative_bins = factor(cumulative_bins, levels = cumulative_bins))

utla_ua_boundaries_rate_geo <- utla_ua_boundaries_json %>% 
  mutate(Label_1 = paste0('<b>', Name, '</b><br>', 'Number of cases so far as at ', format(last_date, '%d %B'), ': <b>', format(Cumulative_cases, big.mark = ',', trim = TRUE), ' (', format(round(Cumulative_per_100000,1), big.mark = ',', trim = TRUE), ' per 100,000 population)</b><br><br>', Name, ' has the ', ordinal(Cumulative_rate_rank), ' highest confirmed COVID-19 rate per 100,000 out of Upper Tier Local Authorities in England.')) %>% 
  mutate(Label_2 = paste0('Number of cases in the ', Rolling_period, ': <b>', format(Rolling_7_day_new_cases, big.mark = ',', trim = TRUE), ' (', format(round(Rolling_7_day_new_cases_per_100000,1), big.mark = ',', trim = TRUE), ' per 100,000 population)</b><br><br>', Name, ' has the ', ordinal(Rolling_rate_rank), ' highest confirmed COVID-19 rate of new cases in the most recent complete seven days per 100,000 out of Upper Tier Local Authorities in England.')) %>% 
  select(Name, Label_1, Label_2, Label_3, cumulative_bins, rolling_bins, Perc_change_on_rolling_7_days_tidy) %>% 
  mutate(cumulative_bins = gsub('\n',' ', cumulative_bins)) %>% 
  mutate(cumulative_bins = factor(cumulative_bins, levels = levels(utla_cumulative_rate_bins$cumulative_bins))) %>% 
  mutate(rolling_bins = gsub('\n',' ', rolling_bins)) %>% 
  mutate(rolling_bins = factor(rolling_bins, levels = levels(utla_rolling_rate_bins$rolling_bins)))

levels(utla_ua_boundaries_rate_geo@data$Perc_change_on_rolling_7_days_tidy)  %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/percentage_change_bins.json'))

# geojson_write(ms_simplify(geojson_json(utla_ua_boundaries_rate_geo), keep = 0.2), file = paste0(output_directory_x, '/utla_covid_rate_latest.geojson'))

geojson_write(geojson_json(utla_ua_boundaries_rate_geo), file = paste0(output_directory_x, '/utla_covid_rate_latest.geojson'))

levels(utla_cumulative_rate_bins$cumulative_bins) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/utla_cumulative_rate_bins.json'))

levels(utla_rolling_rate_bins$rolling_bins) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/utla_rolling_rate_bins.json'))

# LTLA rate ####

ltla_rate_1 <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == max(Date)) %>%
  # filter(Date == '2020-07-12') %>% 
  filter(Type %in% c('Lower Tier Local Authority', 'Unitary Authority')) %>% 
  select(Code, Name, Date, Cumulative_cases, Cumulative_per_100000) %>%
  mutate(Cumulative_rate_rank = rank(-Cumulative_per_100000)) %>% 
  mutate(Cumulative_Rate_decile_actual = abs(ntile(Cumulative_per_100000, 10) - 11)) %>% 
  mutate(Cumulative_Rate_decile = factor(ifelse(Cumulative_Rate_decile_actual == 1, '10% of authorities\nwith highest rate', ifelse(Cumulative_Rate_decile_actual == 10, '10% of authorities\nwith lowest rate', paste0('Decile ', Cumulative_Rate_decile_actual))), levels = c('10% of authorities\nwith highest rate','Decile 2','Decile 3','Decile 4','Decile 5','Decile 6','Decile 7','Decile 8','Decile 9','10% of authorities\nwith lowest rate'))) %>% 
  arrange(Code)

ltla_rate_2 <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == complete_date) %>%
  filter(Type %in% c('Lower Tier Local Authority', 'Unitary Authority')) %>% 
  select(Code, Name, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Rolling_period, Perc_change_on_rolling_7_days_tidy, Label_3) %>%
  mutate(Rolling_rate_rank = rank(-Rolling_7_day_new_cases_per_100000)) %>% 
  mutate(Rolling_Rate_decile_actual = abs(ntile(Rolling_7_day_new_cases_per_100000, 10) - 11)) %>% 
  mutate(Rolling_Rate_decile = factor(ifelse(Rolling_Rate_decile_actual == 1, '10% of authorities\nwith highest rate', ifelse(Rolling_Rate_decile_actual == 10, '10% of authorities\nwith lowest rate', paste0('Decile ', Rolling_Rate_decile_actual))), levels = c('10% of authorities\nwith highest rate','Decile 2','Decile 3','Decile 4','Decile 5','Decile 6','Decile 7','Decile 8','Decile 9','10% of authorities\nwith lowest rate'))) %>% 
  arrange(Code)

ltla_rate <- ltla_rate_1 %>% 
  left_join(ltla_rate_2, by = c('Code', 'Name'))

rm(ltla_rate_1, ltla_rate_2)

ltla_cumulative_rate_bins <- ltla_rate %>% 
  group_by(Cumulative_Rate_decile) %>% 
  summarise(cumulative_bins = paste0(Cumulative_Rate_decile, ' (', format(round(min(Cumulative_per_100000),1), big.mark = ',', trim = TRUE), '-', format(round(max(Cumulative_per_100000)),big.mark = ',', trim = TRUE), ')')) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(cumulative_bins = factor(cumulative_bins, levels = cumulative_bins))

ltla_rolling_rate_bins <- ltla_rate %>% 
  group_by(Rolling_Rate_decile) %>% 
  summarise(rolling_bins = paste0(Rolling_Rate_decile, ' (', format(round(min(Rolling_7_day_new_cases_per_100000),1), big.mark = ',', trim = TRUE), '-', format(round(max(Rolling_7_day_new_cases_per_100000)),big.mark = ',', trim = TRUE), ')')) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(rolling_bins = gsub('\n', ' ', rolling_bins)) %>% 
  mutate(rolling_bins = factor(rolling_bins, levels = rolling_bins))

ltla_rate <- ltla_rate %>% 
  left_join(ltla_cumulative_rate_bins, by = 'Cumulative_Rate_decile') %>% 
  left_join(ltla_rolling_rate_bins, by = 'Rolling_Rate_decile')

summary_table_rate <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == max(Date)) %>% 
  select(Name, Cumulative_cases, Cumulative_per_100000) %>% 
  filter(Name %in% c('West Sussex','South East region', 'England')) %>% 
  mutate(Cumulative_rate_rank = '-',
         Cumulative_Rate_decile = '-') %>% 
  mutate(Cumulative_cases = format(Cumulative_cases, big.mark = ',', trim = TRUE),
         Cumulative_per_100000 = format(round(Cumulative_per_100000, 1), big.mark = ',', trim = TRUE)) %>% 
  mutate(Name = factor(Name, levels = c('West Sussex', 'South East region', 'England'))) %>% 
  arrange(Name)

ltla_rate_wsx <- ltla_rate %>% 
  select(Name, Cumulative_cases, Cumulative_per_100000, Cumulative_rate_rank, Cumulative_Rate_decile, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Rolling_rate_rank, Rolling_Rate_decile, Rolling_period) %>% 
  mutate(Cumulative_rate_rank = ordinal(Cumulative_rate_rank)) %>% 
  mutate(Rolling_rate_rank = ordinal(Rolling_rate_rank)) %>% 
  filter(Name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>%
  mutate(Cumulative_cases = format(Cumulative_cases, big.mark = ',', trim = TRUE),
         Cumulative_per_100000 = format(round(Cumulative_per_100000, 1), big.mark = ',', trim = TRUE),
         Rolling_7_day_new_cases = format(Rolling_7_day_new_cases, big.mark = ',', trim = TRUE),
         Rolling_7_day_new_cases_per_100000 = format(round(Rolling_7_day_new_cases_per_100000, 1), big.mark = ',', trim = TRUE)) %>% 
  bind_rows(summary_table_rate) %>% 
  rename(`Cumulative cases` = Cumulative_cases,
         `Cumulative rate per 100,000 residents` =  Cumulative_per_100000,
         `Local Authority Rank (out of 315) where 1 = Highest Rate per 100,000` = Cumulative_rate_rank,
         `Decile of cumulative rate per 100,000` =  Cumulative_Rate_decile,
         `Rolling 7-day new cases` = Rolling_7_day_new_cases,
         `Rolling 7-day case rate per 100,000` = Rolling_7_day_new_cases_per_100000,
         `Local Authority Rank (out of 315) where 1 = Highest Rolling 7-day rate per 100,000` = Rolling_rate_rank,
         `Decile of rolling rate per 100,000` =  Rolling_Rate_decile)

ltla_rate_wsx %>% 
  write.csv(., paste0(output_directory_x, '/ltla_rate_wsx.csv'), row.names = FALSE)

cum_ltla_rate_wsx <- ltla_rate_wsx %>% 
  select(Name, `Cumulative cases`, `Cumulative rate per 100,000 residents`, `Local Authority Rank (out of 315) where 1 = Highest Rate per 100,000`, `Decile of cumulative rate per 100,000`)

ft_ltla_rate_wsx <- flextable(cum_ltla_rate_wsx) %>% 
  width(width = .9) %>%
  align(j = 1, align = 'left') %>% 
  width(j = 1, width = 1.3) %>%
  width(j = 4, width = 1.3) %>%
  width(j = 5, width = 1.3) %>%
  fontsize(part = "header", size = 10) %>% 
  fontsize(part = "body", size = 11) %>% 
  font(fontname = "Calibri") %>% 
  height_all(height = .2) %>% 
  valign(valign = "top", part = "all") %>% 
  bold(part = "header")%>% 
  align(j = 1, align = 'left') %>% 
  hline(i = 1, border = bord_style, part = 'header') %>% 
  hline_bottom(border = bord_style ) %>% 
  hline_top(border = bord_style, part = "all" )

rolling_ltla_wsx_updte <- rolling_utla_rate_wsx %>% 
  rename(`Local Authority Rank (out of 315) where 1 = Highest Rolling 7-day rate per 100,000` = `Local Authority Rank (out of 149) where 1 = Highest Rolling 7-day rate per 100,000`) %>% 
  mutate(`Local Authority Rank (out of 315) where 1 = Highest Rolling 7-day rate per 100,000` = '-') %>% 
  mutate(`Decile of rolling rate per 100,000` = '-')

rolling_ltla_rate_wsx <- ltla_rate_wsx %>% 
  select(Name, `Rolling 7-day new cases`, `Rolling 7-day case rate per 100,000`, `Local Authority Rank (out of 315) where 1 = Highest Rolling 7-day rate per 100,000`, `Decile of rolling rate per 100,000`) %>% 
  filter(!Name %in% c('West Sussex', 'South East region', 'England')) %>% 
  bind_rows(rolling_ltla_wsx_updte)

ft_ltla_rolling_rate_wsx <- flextable(rolling_ltla_rate_wsx) %>% 
  width(width = .9) %>%
  align(j = 1, align = 'left') %>% 
  width(j = 1, width = 1.3) %>%
  width(j = 4, width = 1.3) %>%
  width(j = 5, width = 1.3) %>%
  fontsize(part = "header", size = 10) %>% 
  fontsize(part = "body", size = 11) %>% 
  font(fontname = "Calibri") %>% 
  height_all(height = .2) %>% 
  valign(valign = "top", part = "all") %>% 
  bold(part = "header")%>% 
  align(j = 1, align = 'left') %>% 
  hline(i = 1, border = bord_style, part = 'header') %>% 
  hline_bottom(border = bord_style ) %>% 
  hline_top(border = bord_style, part = "all" )

ltla_boundaries <- geojson_read('https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson',  what = "sp") 

#download.file('https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson', paste0(output_directory_x, '/failsafe_ltla_boundary.geojson'), mode = 'wb')

if(exists('ltla_boundaries') == FALSE) {
  ltla_boundaries <- geojson_read(paste0(output_directory_x, '/failsafe_ltla_boundary.geojson'),  what = "sp") 
}

geo_rate_cumulative_ltla_bins <- ltla_cumulative_rate_bins %>% 
  mutate(cumulative_bins = gsub('\n',' ', cumulative_bins)) %>% 
  mutate(cumulative_bins = factor(cumulative_bins, levels = cumulative_bins))

levels(geo_rate_cumulative_ltla_bins$cumulative_bins) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/ltla_cumulative_rate_bins.json'))

geo_rate_rolling_ltla_bins <- ltla_rolling_rate_bins %>% 
  mutate(rolling_bins = gsub('\n',' ', rolling_bins)) %>% 
  mutate(rolling_bins = factor(rolling_bins, levels = rolling_bins))

levels(geo_rate_rolling_ltla_bins$rolling_bins) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/ltla_rolling_rate_bins.json'))

ltla_boundaries_geo <- ltla_boundaries %>% 
  filter(lad19cd %in% ltla_rate$Code) %>% 
  arrange(lad19cd)

ltla_boundaries_geo_df <- as.data.frame(ltla_rate %>% 
    arrange(Code) %>% 
    mutate(Label_1 = paste0('<b>', Name, '</b><br>', 'Number of cases so far as at ', format(last_date, '%d %B'), ': <b>', format(Cumulative_cases, big.mark = ',', trim = TRUE), ' (', format(round(Cumulative_per_100000,1), big.mark = ',', trim = TRUE), ' per 100,000 population)</b><br><br>', Name, ' has the ', ordinal(Cumulative_rate_rank), ' highest confirmed COVID-19 rate per 100,000 out of Lower Tier Local Authorities in England.')) %>% 
    mutate(Label_2 = paste0('Number of cases in the ', Rolling_period, ': <b>', format(Rolling_7_day_new_cases, big.mark = ',', trim = TRUE), ' (', format(round(Rolling_7_day_new_cases_per_100000,1), big.mark = ',', trim = TRUE), ' per 100,000 population)</b><br><br>', Name, ' has the ', ordinal(Rolling_rate_rank), ' highest confirmed COVID-19 rate of new cases in the most recent complete seven days per 100,000 out of Lower Tier Local Authorities in England.')) %>% 
    select(Name, Label_1, Label_2, Label_3, cumulative_bins, rolling_bins, Perc_change_on_rolling_7_days_tidy) %>% 
    mutate(cumulative_bins = gsub('\n',' ', cumulative_bins)) %>% 
    mutate(cumulative_bins = factor(cumulative_bins, levels = levels(geo_rate_cumulative_ltla_bins$cumulative_bins))) %>% 
    mutate(rolling_bins = gsub('\n',' ', rolling_bins)) %>% 
    mutate(rolling_bins = factor(rolling_bins, levels = levels(geo_rate_rolling_ltla_bins$rolling_bins))))

df <- data.frame(ID = character())

# Get the IDs of spatial polygon
  for (i in ltla_boundaries_geo@polygons ) { df <- rbind(df, data.frame(ID = i@ID, stringsAsFactors = FALSE))  }

# and set rowname = ID
row.names(ltla_boundaries_geo_df) <- df$ID

# Then use df as the second argument to the spatial dataframe conversion function:
ltla_boundaries_json <- SpatialPolygonsDataFrame(ltla_boundaries_geo, ltla_boundaries_geo_df)  

# geojson_write(ms_simplify(geojson_json(ltla_boundaries_json), keep = 0.2), file = paste0(output_directory_x, '/ltla_covid_cumulative_rate_latest.geojson'))

geojson_write(geojson_json(ltla_boundaries_json), file = paste0(output_directory_x, '/ltla_covid_cumulative_rate_latest.geojson'))

# For ggplot
ltla_ua_boundaries <- ltla_boundaries %>% 
    filter(lad19cd %in% ltla_rate$Code) %>% 
    fortify(region = "lad19cd") %>% 
    rename(lad19cd = id) %>% 
    left_join(ltla_rate, by = c('lad19cd' = 'Code'))

map_1_ltla <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = ltla_ua_boundaries,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = cumulative_bins),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_manual(values = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'),
                    name = 'Decile of cumulative\nrate per 100k',
                    drop = FALSE) +
  labs(title = paste0('Cumulative rate of confirmed Covid-19 cases per 100,000 population (all ages);\nPillar 1 and 2 combined; Lower Tier Local and Unitary Authorities'),
       subtitle = paste0('Confirmed cases by specimen date; as at ', format(last_date, '%d %B %Y')))  +
  theme(legend.position = c(.1,.55))

inset_1_ltla <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = subset(ltla_ua_boundaries, Name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')),
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = cumulative_bins),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = FALSE) +
  scale_fill_manual(values = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'),
                    name = 'Decile of cumulative\nrate per 100k',
                    drop = FALSE) +
  labs(title = 'West Sussex') +
  theme(plot.background  = element_rect(colour = "black", fill=NA, size=.1),
        plot.title = element_text(size = 8))

png(paste0(output_directory_x, '/Figure_4_cumulative_rate_ltla_latest.png'),
    width = 1480,
    height = 1480,
    res = 180)
print(map_1_ltla)
print(inset_1_ltla, vp = viewport(0.2, 0.8, width = 0.22, height = 0.22))
dev.off()

map_1b_ltla <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = ltla_ua_boundaries,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = rolling_bins),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_manual(values = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'),
                    name = 'Decile of rolling\n7 day rate per 100k',
                    drop = FALSE) +
  labs(title = paste0('Rolling 7 day rate of confirmed Covid-19 cases per 100,000 population (all ages);\nPillar 1 and 2 combined; Lower Tier Local and Unitary Authorities'),
       subtitle = paste0('Confirmed cases in the ', rolling_period_x, '; as at ', format(last_date, '%d %B %Y')))  +
  theme(legend.position = c(.1,.55))

inset_1b_ltla <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = subset(ltla_ua_boundaries, Name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')),
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = rolling_bins),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = FALSE) +
  scale_fill_manual(values = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'),
                    name = 'Decile of rolling\n7 day rate per 100k',
                    drop = FALSE) +
  labs(title = 'West Sussex') +
  theme(plot.background  = element_rect(colour = "black", fill=NA, size=.1),
        plot.title = element_text(size = 8))

png(paste0(output_directory_x, '/Figure_4_rolling_rate_ltla_latest.png'),
    width = 1480,
    height = 1480,
    res = 180)
print(map_1b_ltla)
print(inset_1b_ltla, vp = viewport(0.2, 0.8, width = 0.22, height = 0.22))
dev.off()

# NHS Pathways ####

ccg_region_2019 <- read_csv('https://opendata.arcgis.com/datasets/40f816a75fb14dfaaa6db375e6c3d5e6_0.csv') %>% 
  select(CCG19CD, CCG19NM) %>% 
  rename(Old_CCG_Name = CCG19NM,
         Old_CCG_Code = CCG19CD) %>% 
  mutate(New_CCG_Name = ifelse(Old_CCG_Name %in% c('NHS Bath and North East Somerset CCG', 'NHS Swindon CCG', 'NHS Wiltshire CCG'), 'NHS Bath and North East Somerset, Swindon and Wiltshire CCG', ifelse(Old_CCG_Name %in% c('NHS Airedale, Wharfedale and Craven CCG', 'NHS Bradford City CCG', 'NHS Bradford Districts CCG'), 'NHS Bradford District and Craven CCG', ifelse(Old_CCG_Name %in% c('NHS Eastern Cheshire CCG', 'NHS South Cheshire CCG', 'NHS Vale Royal CCG','NHS West Cheshire CCG'), 'NHS Cheshire CCG', ifelse(Old_CCG_Name %in% c('NHS Durham Dales, Easington and Sedgefield CCG', 'NHS North Durham CCG'), 'NHS County Durham CCG',  ifelse(Old_CCG_Name %in% c('NHS Eastbourne, Hailsham and Seaford CCG', 'NHS Hastings and Rother CCG', 'NHS High Weald Lewes Havens CCG'), 'NHS East Sussex CCG', ifelse(Old_CCG_Name %in% c('NHS Herefordshire CCG', 'NHS Redditch and Bromsgrove CCG', 'NHS South Worcestershire CCG','NHS Wyre Forest CCG'), 'NHS Herefordshire and Worcestershire CCG', ifelse(Old_CCG_Name %in% c('NHS Ashford CCG', 'NHS Canterbury and Coastal CCG', 'NHS Dartford, Gravesham and Swanley CCG', 'NHS Medway CCG', 'NHS South Kent Coast CCG', 'NHS Swale CCG', 'NHS Thanet CCG','NHS West Kent CCG'), 'NHS Kent and Medway CCG', ifelse(Old_CCG_Name %in% c('NHS Lincolnshire East CCG', 'NHS Lincolnshire West CCG', 'NHS South Lincolnshire CCG', 'NHS South West Lincolnshire CCG'), 'NHS Lincolnshire CCG', ifelse(Old_CCG_Name %in% c('NHS Great Yarmouth and Waveney CCG', 'NHS North Norfolk CCG', 'NHS Norwich CCG', 'NHS South Norfolk CCG', 'NHS West Norfolk CCG'), 'NHS Norfolk and Waveney CCG', ifelse(Old_CCG_Name %in% c('NHS Barnet CCG', 'NHS Camden CCG', 'NHS Enfield CCG', 'NHS Haringey CCG', 'NHS Islington CCG'), 'NHS North Central London CCG', ifelse(Old_CCG_Name %in% c('NHS Hambleton, Richmondshire and Whitby CCG', 'NHS Scarborough and Ryedale CCG', 'NHS Harrogate and Rural District CCG'),'NHS North Yorkshire CCG', ifelse(Old_CCG_Name %in% c('NHS Corby CCG', 'NHS Nene CCG'), 'NHS Northamptonshire CCG', ifelse(Old_CCG_Name %in% c('NHS Mansfield and Ashfield CCG', 'NHS Newark and Sherwood CCG', 'NHS Nottingham City CCG', 'NHS Nottingham North and East CCG', 'NHS Nottingham West CCG', 'NHS Rushcliffe CCG'), 'NHS Nottingham and Nottinghamshire CCG', ifelse(Old_CCG_Name %in% c('NHS Bexley CCG', 'NHS Bromley CCG', 'NHS Greenwich CCG', 'NHS Lambeth CCG', 'NHS Lewisham CCG','NHS Southwark CCG'), 'NHS South East London CCG',ifelse(Old_CCG_Name %in% c('NHS Croydon CCG', 'NHS Kingston CCG', 'NHS Merton CCG', 'NHS Richmond CCG', 'NHS Sutton CCG', 'NHS Wandsworth CCG'), 'NHS South West London CCG',ifelse(Old_CCG_Name %in% c('NHS East Surrey CCG', 'NHS Guildford and Waverley CCG', 'NHS North West Surrey CCG', 'NHS Surrey Downs CCG'), 'NHS Surrey Heartlands CCG', ifelse(Old_CCG_Name %in% c('NHS Darlington CCG', 'NHS Hartlepool and Stockton-on-Tees CCG', 'NHS South Tees CCG'), 'NHS Tees Valley CCG', ifelse(Old_CCG_Name %in% c('NHS Coastal West Sussex CCG', 'NHS Crawley CCG', 'NHS Horsham and Mid Sussex CCG'), 'NHS West Sussex CCG', Old_CCG_Name)))))))))))))))))))

ccg_region_2020 <- read_csv('https://opendata.arcgis.com/datasets/888dc5cc66ba4ad9b4d935871dcce251_0.csv') %>% 
  select(CCG20CD, CCG20NM, NHSER20NM) %>% 
  rename(New_CCG_Code = CCG20CD,
         CCG_Name = CCG20NM,
         NHS_region = NHSER20NM)

ccg_region_2019 <- ccg_region_2019 %>% 
  left_join(ccg_region_2020[c('New_CCG_Code', 'CCG_Name')], by = c('New_CCG_Name'='CCG_Name'))

# NHS Pathway Data
calls_webpage <- read_html('https://digital.nhs.uk/data-and-information/publications/statistical/mi-potential-covid-19-symptoms-reported-through-nhs-pathways-and-111-online/latest') %>%
  html_nodes("a") %>%
  html_attr("href")

nhs_111_pathways_raw <- read_csv(grep('NHS%20Pathways%20Covid-19%20data%202020', calls_webpage, value = T))  %>% 
  rename(CCG_Name = CCGName) %>% 
  mutate(Date = as.Date(`Call Date`, format = '%d/%m/%Y')) %>% 
  select(-`Call Date`) %>% 
  mutate(AgeBand = ifelse(is.na(AgeBand), 'Unknown', AgeBand))

nhs_111_pathways_pre_april <- nhs_111_pathways_raw %>% 
  filter(Date < '2020-04-01') %>% 
  left_join(ccg_region_2019[c('Old_CCG_Code','New_CCG_Code', 'New_CCG_Name')], by = c('CCGCode' = 'Old_CCG_Code')) %>% 
  group_by(New_CCG_Code, New_CCG_Name, Date, AgeBand, Sex, SiteType) %>% 
  summarise(TriageCount = sum(TriageCount, na.rm = TRUE)) %>% 
  left_join(ccg_region_2020[c('New_CCG_Code', 'NHS_region')], by = 'New_CCG_Code') %>% 
  ungroup() %>% 
  filter(!is.na(NHS_region))

nhs_111_pathways_post_april <- nhs_111_pathways_raw %>% 
  filter(Date >= '2020-04-01') %>% 
  left_join(ccg_region_2020[c('New_CCG_Code', 'NHS_region')], by = c('CCGCode' ='New_CCG_Code')) %>% 
  filter(!is.na(NHS_region)) %>% 
  rename(New_CCG_Code = CCGCode,
         New_CCG_Name = CCG_Name)

nhs_111_pathways <- nhs_111_pathways_pre_april %>% 
  bind_rows(nhs_111_pathways_post_april) %>% 
  mutate(Pathway = paste0(SiteType, ' triage')) %>% 
  select(-SiteType) %>% 
  rename(Triage_count = TriageCount)

rm(nhs_111_pathways_pre_april, nhs_111_pathways_post_april)

nhs_111_online_raw <- read_csv(grep('111%20Online%20Covid-19%20data_2020', calls_webpage, value = T)) %>% 
  rename(CCG_Name = ccgname,
         CCG_Code = ccgcode,
         Sex = sex,
         AgeBand = ageband) %>% 
  mutate(Date = as.Date(journeydate, format = '%d/%m/%Y')) %>% 
  select(-journeydate) %>% 
  mutate(AgeBand = ifelse(is.na(AgeBand), 'Unknown', AgeBand))

nhs_111_online_post_april <- nhs_111_online_raw %>% 
  filter(Date >= '2020-04-01') %>% 
  left_join(ccg_region_2020[c('New_CCG_Code', 'NHS_region')], by = c('CCG_Code' ='New_CCG_Code')) %>% 
  filter(!is.na(NHS_region)) %>% 
  rename(New_CCG_Code = CCG_Code,
         New_CCG_Name = CCG_Name)

nhs_111_online_pre_april <- nhs_111_online_raw %>% 
  filter(Date < '2020-04-01') %>% 
  left_join(ccg_region_2019[c('Old_CCG_Code','New_CCG_Code', 'New_CCG_Name')], by = c('CCG_Code' = 'Old_CCG_Code')) %>% 
  group_by(New_CCG_Code, New_CCG_Name, Date, AgeBand, Sex) %>% 
  summarise(Total = sum(Total, na.rm = TRUE)) %>% 
  left_join(ccg_region_2020[c('New_CCG_Code', 'NHS_region')], by = 'New_CCG_Code') %>% 
  ungroup() %>% 
  filter(!is.na(NHS_region))

nhs_111_online <- nhs_111_online_pre_april %>% 
  bind_rows(nhs_111_online_post_april) %>% 
  mutate(Pathway = '111 online Journey') %>% 
  rename(Triage_count = Total)

nhs_pathways_p1 <- nhs_111_pathways %>% 
  bind_rows(nhs_111_online) %>% 
  rename(Area_Code = New_CCG_Code,
         Area_Name = New_CCG_Name)

sussex_pathways <- nhs_pathways_p1 %>% 
  filter(Area_Name %in% c('NHS West Sussex CCG', 'NHS East Sussex CCG', 'NHS Brighton and Hove CCG')) %>% 
  group_by(Date, Pathway, Sex, AgeBand) %>% 
  summarise(Triage_count = sum(Triage_count)) %>% 
  mutate(Area_Name = 'Sussex areas combined',
         Area_Code = '-',
         NHS_region = '-')

nhs_pathways <- nhs_pathways_p1 %>% 
  bind_rows(sussex_pathways)

nhs_pathways_all_ages_persons <- nhs_pathways %>% 
  group_by(Area_Code, Area_Name, Date, Pathway) %>% 
  summarise(Triage_count = sum(Triage_count)) %>% 
  group_by(Area_Code, Area_Name, Pathway) %>% 
  arrange(Area_Code, Pathway, Date) %>% 
  mutate(Number_change = Triage_count - lag(Triage_count),
         Percentage_change = (Triage_count - lag(Triage_count))/ lag(Triage_count))

rm(ccg_region_2019, ccg_region_2020, nhs_111_online, nhs_111_online_pre_april, nhs_111_online_post_april, nhs_111_online_raw, nhs_111_pathways, nhs_111_pathways_raw, calls_webpage, nhs_pathways_p1, sussex_pathways)

nhs_pathways_all_ages_persons_all_pathways <- nhs_pathways %>% 
  group_by(Area_Code, Area_Name, Date) %>% 
  summarise(Triage_count = sum(Triage_count)) %>% 
  group_by(Area_Code, Area_Name) %>% 
  arrange(Area_Code, Date) %>% 
  mutate(Number_change = Triage_count - lag(Triage_count),
         Percentage_change = (Triage_count - lag(Triage_count))/ lag(Triage_count))

latest_triage_date = nhs_pathways %>% 
  filter(Date == max(Date)) %>% 
  select(Date) %>% 
  unique() %>% 
  mutate(Date = format(Date, '%d %B'))

pathways_x <- nhs_pathways_all_ages_persons_all_pathways %>% 
  filter(Area_Name == 'NHS West Sussex CCG')

utla_pathways <- read_csv('https://files.digital.nhs.uk/46/536793/NHS%20Pathways%20Covid-19%20data_UTLA_2020-07-27.csv') %>% 
  mutate(Date = as.character.Date(CallDate))

whole_timeseries_plot <- ggplot(pathways_x,
       aes(x = Date,
           y = Triage_count,
           group = 1)) +
  # geom_segment(x = as.Date('2020-04-09'), y = 0, xend = as.Date('2020-04-09'), yend = as.numeric(subset(pathways_x, Date == as.Date('2020-04-09'), select = Triage_count)), color = "red", linetype = "dashed") +
  # geom_segment(x = as.Date('2020-04-23'), y = 0, xend = as.Date('2020-04-23'), yend = as.numeric(subset(pathways_x, Date == as.Date('2020-04-23'), select = Triage_count)), color = "blue", linetype = "dashed") +
  geom_segment(x = as.Date('2020-05-18'), y = 0, xend = as.Date('2020-05-18'), yend = as.numeric(subset(pathways_x, Date == as.Date('2020-05-18'), select = Triage_count)), color = "red", linetype = "dashed") +
  geom_line() +
  geom_point(size = .8) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date(max(nhs_pathways$Date) -(52*7), max(nhs_pathways$Date), by = 7),
               limits = c(min(nhs_pathways$Date), max(nhs_pathways$Date)),
               expand = c(0.01,0.01)) +
  scale_y_continuous(labels = comma,
                     breaks = seq(0,round_any(max(pathways_x$Triage_count, na.rm = TRUE), 500, ceiling),250)) +
  labs(x = 'Date',
       y = 'Number of complete triages',
       title = paste0('Total number of complete triages to NHS Pathways for Covid-19; ', unique(pathways_x$Area_Name)),
       subtitle = paste0('Triages via 111 online, 111 phone calls and 999 calls; 18 March - ', latest_triage_date$Date),
       caption = 'Note: red dashed line = some patients excluded,\nblue dashed line = additional patients added') +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  # annotate(geom = 'text',
  #          x = as.Date('2020-04-08'), 
  #          y = as.numeric(subset(pathways_x, Date == as.Date('2020-04-09'), select = Triage_count)),
  #          label = '9th April',
  #          fontface = 'bold',
  #          size = 2.5,
  #          hjust = 1,
  #          vjust = 1) +
  # annotate(geom = 'text',
  #          x = as.Date('2020-04-08'), 
  #          y = as.numeric(subset(pathways_x, Date == as.Date('2020-04-09'), select = Triage_count)),
  #          label = '111 online removed\nfor 0-18 year olds',
  #          size = 2.5,
  #          hjust = 1,
  #          vjust = 1.75) +
  # annotate(geom = 'text',
  #          x = as.Date('2020-04-23'), 
  #          y = as.numeric(subset(pathways_x, Date == as.Date('2020-04-23'), select = Triage_count)),
  #          label = '23rd April',
  #          fontface = 'bold',
  #          size = 2.5,
  #          hjust = 0,
  #          vjust = -8) +
  # annotate(geom = 'text',
  #          x = as.Date('2020-04-23'),
  #          y = as.numeric(subset(pathways_x, Date == as.Date('2020-04-23'), select = Triage_count)),
  #          label = '111 online reinstated\nfor 5-18 year olds',
  #          size = 2.5,
  #          hjust = 0,
  #          vjust = -1.25) +
  annotate(geom = 'text',
           x = as.Date('2020-05-18'), 
           y = as.numeric(subset(pathways_x, Date == as.Date('2020-05-18'), select = Triage_count)),
           label = '18th May',
           size = 2.5,
           fontface = 'bold',
           hjust = 0,
           vjust = -6) +
  annotate(geom = 'text',
           x = as.Date('2020-05-18'), 
           y = as.numeric(subset(pathways_x, Date == as.Date('2020-05-18'), select = Triage_count)),
           label = 'Covid-19 pathway case\ndefinition change',
           size = 2.5,
           hjust = 0,
           vjust = -.5)

png(paste0(output_directory_x, '/Figure_5_complete_triages_nhs_pathways.png'),
    width = 1580, 
    height = 1300, 
    res = 200)
print(whole_timeseries_plot)
dev.off()

Report_df_x <- nhs_pathways_all_ages_persons_all_pathways %>% 
  filter(Area_Name == 'NHS West Sussex CCG') %>% 
  mutate(seven_day_total_triages = rollapplyr(Triage_count, 7, sum, align = 'right', partial = TRUE)) %>% 
  mutate(seven_day_average_triages = round(rollapplyr(Triage_count, 7, mean, align = 'right', partial = TRUE),0))

latest_report_date <- Report_df_x %>% 
  filter(Date == max(Date))

latest_report_date_minus1 <- Report_df_x %>% 
  filter(Date == max(Date) -1)

nhs_pways_text_1 <- paste0('In the last 24 hours there were ', format(latest_report_date$Triage_count, big.mark = ',', trim = TRUE), ' triages made. This is an ', ifelse(latest_report_date$Number_change >= 0, 'increase', ifelse(latest_report_date$Number_change <0, 'decrease', NA)), ' of ', format(abs(latest_report_date$Number_change), big.mark = ',', trim = TRUE), ' triages compared to the previous day (', format(latest_report_date_minus1$Triage_count, big.mark = ',', trim = TRUE) ,' triages).') 

nhs_pways_text_2 <- paste0('In the seven days leading to ', format(latest_report_date$Date, '%d %B'), ' there were ', format(latest_report_date$seven_day_total_triages, big.mark = ',', trim = TRUE), ' triages to NHS Pathways for COVID-19, this is an average of ', format(round(latest_report_date$seven_day_average_triages, 0), big.mark = ',', trim = TRUE), ' each day.')

# Exporting for web

Report_df_x %>% 
  ungroup() %>% 
  mutate(label_1 = paste0('In the seven days leading to ', format(Date, '%d %B'), ' there were ', format(seven_day_total_triages, big.mark = ',', trim = TRUE), ' triages to NHS Pathways for COVID-19, this is an average of ', format(round(seven_day_average_triages, 0), big.mark = ',', trim = TRUE), ' each day.')) %>% 
  mutate(label_2 = paste0('In the last 24 hours there were ', format(Triage_count, big.mark = ',', trim = TRUE), ' triages made. This is an ', ifelse(Number_change >= 0, 'increase', ifelse(latest_report_date$Number_change <0, 'decrease', NA)), ' of ', format(abs(Number_change), big.mark = ',', trim = TRUE), ' triages compared to the previous day (', format(lag(Triage_count,1), big.mark = ',', trim = TRUE) ,' triages).')) %>% 
  mutate(Date = format(Date, '%d %b')) %>% 
  select(Date, Triage_count, label_1, label_2) %>%
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/NHS_pathways_df.json'))

pathways_dates <- Report_df_x %>% 
  ungroup() %>% 
  filter(Date %in% seq.Date(max(nhs_pathways$Date) -(52*7), max(nhs_pathways$Date), by = 7)) %>% 
  select(Date) %>% 
  mutate(Date = format(Date, '%d %b'))
  
pathways_dates$Date %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/NHS_pathways_dates.json'))

data.frame(Date = c('23 Apr', '18 May'), lab_1 = c('111 online reinstated', 'Pathway case'), lab_2 = c( 'for 5-18 year olds', 'definition change'), direction = c('blue', 'red'), Triage_count = c(292,334)) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/pathways_changes.json'))

# Mortality ####

# Weekly death figures provide provisional counts of the number of deaths registered in England and Wales for which data are available.	From 31 March 2020 these figures also show the number of deaths involving coronavirus (COVID-19), based on any mention of COVID-19 on the death certificate.											
# The tables include deaths that occurred up to the Friday before last but were registered up to last Friday. Figures by place of death may differ to previously published figures due to improvements in the way we code place of death.											
# These figures do not include deaths of those residents outside England and Wales or those records where the place of residence is either missing or not yet fully coded. For this reason counts may differ to published figures when summed. These figures represent death occurrences and registrations, there can be a delay between the date a death occurred and the date a death was registered. More information can be found in our impact of registration delays release. 	

# For this data, the week ends on a friday (so friday is the cut off date for each week of data). It might be helpful for us to say as at this date this is the number of deaths. To do this we need to convert each week number into a 'friday date'.
set_week_start('Friday')

week_ending <- data.frame(Week_ending = get_date(week = 1:52, year = 2020)) %>% 
  mutate(Week_number = row_number())

# Boo! but we can get around it with some date hackery. This will probably not work on Tuesday morning next week
# download.file(paste0('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek',substr(as.character(as.aweek(Sys.Date()-11)), 7,8), 'finalcodes.xlsx'), paste0(github_repo_dir, '/ons_mortality.xlsx'), mode = 'wb')

# download.file('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek43.xlsx', paste0(github_repo_dir, '/Source files/ons_mortality.xlsx'), mode = 'wb')

download.file(paste0('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek',substr(as.character(as.aweek(Sys.Date()-11)), 7,8), '1.xlsx'),  paste0(github_repo_dir, '/Source files/ons_mortality.xlsx'), mode = 'wb')

# # if the downlaod does fail, it wipes out the old one, which we can use to our advantage
if(!file.exists(paste0(github_repo_dir, '/Source files/ons_mortality.xlsx'))){
download.file(paste0('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek',substr(as.character(as.aweek(Sys.Date()-12)), 7,8), '1.xlsx'),  paste0(github_repo_dir, '/Source files/ons_mortality.xlsx'), mode = 'wb')
}

# Use occurrences, be mindful that the most recent week of occurrence data may not be complete if the death is not registered within 7 days (there is a week lag in reporting to allow up to seven days for registration to take place), this will be updated each week. Estimates suggest around 74% of deaths in England and Wales are registered within seven calendar days of occurrence, with the proportion as low as 68% in the South East region. It is difficult to know what impact Covid-19 has on length of time taken to register a death. 

# Occurrences data is produced at ltla level and we would probably find it useful to aggregate to utla and region for our analysis

Occurrences_ltla <- read_excel(paste0(github_repo_dir, '/Source files/ons_mortality.xlsx'), sheet = 'Occurrences - All data', skip = 2) %>% 
  rename(Name = `Area name`,
         Cause = `Cause of death`,
         Week_number = `Week number`,
         Place_of_death = `Place of death`,
         Deaths = `Number of deaths`) %>% 
  filter(Name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>% 
  select(Name, Week_number, Cause, Place_of_death, Deaths) %>% 
  left_join(week_ending, by = 'Week_number') %>% 
  ungroup()

# Occurrences data is produced at ltla level and we would probably find it useful to aggregate to utla and region for our analysis
Occurrences_wsx <- Occurrences_ltla %>% 
  group_by(Cause, Week_number, Place_of_death) %>% 
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>% 
  mutate(Name = 'West Sussex') %>% 
  select(Name, Week_number, Cause, Place_of_death, Deaths) %>% 
  left_join(week_ending, by = 'Week_number') %>% 
  ungroup()

Occurrences<- Occurrences_ltla %>% 
  bind_rows(Occurrences_wsx)

rm(Occurrences_ltla, Occurrences_wsx)

deaths_labels <- Occurrences %>% 
  arrange(Week_number) %>% 
  select(Week_ending) %>% 
  unique() %>% 
  mutate(deaths_label = paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')))

weekly_all_place_all_deaths <- Occurrences %>% 
  filter(Cause == 'All causes') %>% 
  arrange(Week_number) %>% 
  group_by(Name, Week_ending) %>% 
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>% 
  select(Name, Week_ending, Deaths) %>% 
  mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label)) %>% 
  rename(All_deaths = Deaths)

All_settings_occurrences <- Occurrences %>% 
  group_by(Name, Week_number, Week_ending, Cause) %>% 
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>% 
  group_by(Name, Cause) %>% 
  arrange(Cause, Week_number) %>% 
  mutate(Cumulative_deaths = cumsum(Deaths))  %>% 
  ungroup()

weekly_all_place_deaths <- All_settings_occurrences %>% 
  filter(Name %in% areas_to_loop) %>%
  arrange(Week_number) %>% 
  select(Name, Cause, Week_ending, Deaths) %>% 
  mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label)) %>% 
  pivot_wider(id_cols = c(Name, Week_ending),
              names_from = Cause,
              values_from = Deaths) %>% 
  mutate(`Non-Covid` = `All causes` - `COVID 19`) %>%
  select(-`All causes`) %>% 
  gather(key = 'Cause', value = 'Deaths', `COVID 19`:`Non-Covid`) %>% 
  mutate(Cause = factor(Cause, levels = rev(c('Non-Covid', 'COVID 19')))) %>% 
  mutate(lab_posit = ifelse(Cause == 'Non-Covid', 1.5, -1)) %>% 
  left_join(weekly_all_place_all_deaths, by = c('Name', 'Week_ending'))

for(i in 1:length(areas_to_loop)){

area_x <- areas_to_loop[i]

area_x_cov_non_cov_raw <- weekly_all_place_deaths %>% 
  filter(Name == area_x)

deaths_area_x_cov_non_cov <- area_x_cov_non_cov_raw %>% 
  group_by(Cause) %>% 
  summarise(Deaths_to_date = sum(Deaths, na.rm = TRUE))

area_x_cov_non_cov <- area_x_cov_non_cov_raw %>%
  left_join(deaths_area_x_cov_non_cov, by = 'Cause') %>% 
  mutate(Cause_1 = paste0(ifelse(Cause == 'Non-Covid', 'COVID not mentioned', gsub(' ', '-', Cause)), ' (', format(Deaths_to_date, big.mark = ',', trim = TRUE), ' deaths)')) %>% 
  mutate(Cause_1 = factor(Cause_1, levels = unique(Cause_1)))

max_deaths_limit <- ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 50, round_any(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE), 5, ceiling), ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 100, round_any(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE), 10, ceiling), ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 250, round_any(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE), 25, ceiling), ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 500, round_any(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE), 50, ceiling), round_any(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE), 100, ceiling)))))

max_deaths_break <- ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 20, 2, ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 50, 5, ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 100, 10, ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 250, 25, ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 500, 50, 100)))))

area_x_wk_cause_deaths_plot <-  ggplot(area_x_cov_non_cov,
                                      aes(x = Week_ending, 
                                          y = Deaths,
                                          fill = Cause_1,
                                          colour = Cause_1,
                                          label = Deaths)) +
  geom_bar(stat = 'identity',
           width = .8) +
  labs(title = paste0('Weekly deaths; ', area_x,'; w/e 3rd Jan 2020 - ', deaths_labels$deaths_label[length(deaths_labels$deaths_label)]),
       subtitle = paste0('By week of occurrence and by Covid-19 mentioned on death certificate (deaths registered by ', format(max(deaths_labels$Week_ending) + 8, '%d %B %Y'), ')'),
       x = 'Week',
       y = 'Number of deaths') +
  scale_fill_manual(values = c('#2F5597','#BDD7EE'),
                    name = 'Number of deaths since\nweek ending 3rd Jan 2020') +
  scale_colour_manual(values = c('#2F5597','#BDD7EE')) +
  scale_y_continuous(breaks = seq(0,max_deaths_limit, max_deaths_break),
                     limits = c(0,max_deaths_limit)) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        legend.position = 'right')  +
  guides(colour = FALSE)

png(paste0(output_directory_x, '/Figure_6_wkly_deaths_', gsub(' ', '_', area_x), '.png'),
    width = 1280,
    height = 400,
    res = 110)
print(area_x_wk_cause_deaths_plot)
dev.off()

care_home_ons_all_deaths <- Occurrences %>%
  filter(Place_of_death %in% 'Care home') %>% 
  filter(Cause == 'All causes') %>% 
  arrange(Week_number) %>% 
  select(Name, Week_ending, Deaths) %>% 
  mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label)) %>% 
  rename(All_deaths = Deaths)

carehome_weekly_deaths <- Occurrences %>%
  filter(Place_of_death %in% 'Care home') %>% 
  arrange(Week_number) %>% 
  select(Name, Cause, Week_ending, Week_number, Deaths) %>% 
  mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label)) %>% 
  pivot_wider(id_cols = c(Name, Week_ending, Week_number),
              names_from = Cause,
              values_from = Deaths) %>% 
  mutate(`Non-Covid` = `All causes` - `COVID 19`) %>%
  select(-`All causes`) %>% 
  gather(key = 'Cause', value = 'Deaths', `COVID 19`:`Non-Covid`) %>% 
  mutate(Cause = factor(Cause, levels = rev(c('Non-Covid', 'COVID 19')))) %>% 
  mutate(lab_posit = ifelse(Cause == 'Non-Covid', 1.5, -1)) %>% 
  left_join(care_home_ons_all_deaths, by = c('Name', 'Week_ending'))

area_x_cov_non_cov_carehome_raw <- carehome_weekly_deaths %>% 
  filter(Name == area_x)

deaths_area_x_cov_non_cov_carehome <- area_x_cov_non_cov_carehome_raw %>% 
  group_by(Cause) %>% 
  summarise(Deaths_to_date = sum(Deaths, na.rm = TRUE))

area_x_cov_non_cov_carehome <- area_x_cov_non_cov_carehome_raw %>%
  left_join(deaths_area_x_cov_non_cov_carehome, by = 'Cause') %>% 
  mutate(Cause_1 = paste0(ifelse(Cause == 'Non-Covid', 'COVID not mentioned', gsub(' ', '-', Cause)), ' (', format(Deaths_to_date, big.mark = ',', trim = TRUE), ' deaths)')) %>% 
  mutate(Cause_1 = factor(Cause_1, levels = unique(Cause_1)))

area_x_wk_cause_deaths_plot_2 <- ggplot(area_x_cov_non_cov_carehome,
       aes(x = Week_ending, 
           y = Deaths,
           fill = Cause_1,
           colour = Cause_1,
           label = Deaths)) +
  geom_bar(stat = 'identity',
           width = .8) +
  labs(title = paste0('Weekly deaths in care homes; ', area_x,'; w/e 3rd Jan 2020 - ', deaths_labels$deaths_label[length(deaths_labels$deaths_label)]),
       subtitle = paste0('By week of occurrence and by Covid-19 mentioned on death certificate (deaths registered by ', format(max(deaths_labels$Week_ending) + 8, '%d %B %Y'), ')'),
       x = 'Week',
       y = 'Number of deaths') +
  scale_fill_manual(values = c('#ED7D31','#FFD966'),
                    name = 'Number of deaths since\nweek ending 3rd Jan 2020)') +
  scale_colour_manual(values = c('#ED7D31','#FFD966')) +
  scale_y_continuous(breaks = seq(0,max_deaths_limit, max_deaths_break),
                     limits = c(0,max_deaths_limit)) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        legend.position = 'right')  +
  guides(colour = FALSE)

png(paste0(output_directory_x, '/Figure_7_wkly_deaths_carehomes_', gsub(' ', '_', area_x), '.png'),
    width = 1280,
    height = 400,
    res = 110)
print(area_x_wk_cause_deaths_plot_2)
dev.off()

}

ltla_deaths_df <- weekly_all_place_deaths %>% 
  filter(Name != 'West Sussex')

max_deaths_limit <- ifelse(max(ltla_deaths_df$All_deaths, na.rm = TRUE) < 50, round_any(max(ltla_deaths_df$All_deaths, na.rm = TRUE), 5, ceiling), ifelse(max(ltla_deaths_df$All_deaths, na.rm = TRUE) < 100, round_any(max(ltla_deaths_df$All_deaths, na.rm = TRUE), 10, ceiling), ifelse(max(ltla_deaths_df$All_deaths, na.rm = TRUE) < 250, round_any(max(ltla_deaths_df$All_deaths, na.rm = TRUE), 25, ceiling), ifelse(max(ltla_deaths_df$All_deaths, na.rm = TRUE) < 500, round_any(max(ltla_deaths_df$All_deaths, na.rm = TRUE), 50, ceiling), round_any(max(ltla_deaths_df$All_deaths, na.rm = TRUE), 100, ceiling)))))

max_deaths_break <- ifelse(max(ltla_deaths_df$All_deaths, na.rm = TRUE) < 20, 2, ifelse(max(ltla_deaths_df$All_deaths, na.rm = TRUE) < 50, 5, ifelse(max(ltla_deaths_df$All_deaths, na.rm = TRUE) < 100, 10, ifelse(max(ltla_deaths_df$All_deaths, na.rm = TRUE) < 250, 25, ifelse(max(ltla_deaths_df$All_deaths, na.rm = TRUE) < 500, 50, 100)))))

ltla_deaths_plot_1 <- ggplot(ltla_deaths_df,
                          aes(x = Week_ending, 
                              y = Deaths,
                              fill = Cause,
                              colour = Cause,
                              label = Deaths)) +
  geom_bar(stat = 'identity',
           colour = '#ffffff') +
  # geom_text(data = subset(area_x_cov_non_cov, Deaths > 0),
  #           position = 'stack',
  #           size = 3, 
  #           fontface = "bold",
  #           aes(vjust = lab_posit)) +
  labs(title = paste0('Weekly deaths; w/e 3rd Jan 2020 - ', deaths_labels$deaths_label[length(deaths_labels$deaths_label)], '; West Sussex lower tier Local Authorities'),
       subtitle = paste0('By week of occurrence and by Covid-19 mentioned on death certificate (deaths registered by ', format(max(deaths_labels$Week_ending) + 8, '%d %B %Y'), ')'),
       x = 'Week',
       y = 'Number of deaths') +
    scale_fill_manual(values = c('#2F5597','#BDD7EE'),
                      labels = c('COVID-19', 'COVID not mentioned')) +
  scale_colour_manual(values = c('#000000', '#ffffff')) +
  scale_y_continuous(breaks = seq(0,max_deaths_limit, max_deaths_break),
                     limits = c(0,max_deaths_limit)) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        legend.position = 'bottom')  +
  guides(colour = FALSE) +
  facet_wrap(~Name, ncol = 3)

png(paste0(output_directory_x, '/Figure_9_covid_19_deaths_wsx_ltlas.png'),
    width = 1580,
    height = 1050,
    res = 120)
print(ltla_deaths_plot_1)
dev.off()

ltla_deaths_df_2 <- carehome_weekly_deaths %>% 
  filter(Name != 'West Sussex')

ltla_deaths_plot_2 <- ggplot(ltla_deaths_df_2,
                             aes(x = Week_ending, 
                                 y = Deaths,
                                 fill = Cause,
                                 colour = Cause,
                                 label = Deaths)) +
  geom_bar(stat = 'identity',
           colour = '#ffffff') +
  # geom_text(data = subset(area_x_cov_non_cov, Deaths > 0),
  #           position = 'stack',
  #           size = 3, 
  #           fontface = "bold",
  #           aes(vjust = lab_posit)) +
  labs(title = paste0('Weekly deaths in care homes; w/e 3rd Jan 2020 - ', deaths_labels$deaths_label[length(deaths_labels$deaths_label)], '; West Sussex lower tier Local Authorities'),
       subtitle = paste0('By week of occurrence and by Covid-19 mentioned on death certificate (deaths registered by ', format(max(deaths_labels$Week_ending) + 8, '%d %B %Y'), ')'),
       x = 'Week',
       y = 'Number of deaths') +
  scale_fill_manual(values = c('#ED7D31','#FFD966'),
                    labels = c('COVID-19', 'COVID not mentioned')) +
  scale_colour_manual(values = c('#000000', '#ffffff')) +
  scale_y_continuous(breaks = seq(0,max_deaths_limit, max_deaths_break),
                     limits = c(0,max_deaths_limit)) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        legend.position = 'bottom')  +
  guides(colour = FALSE) +
  facet_wrap(~Name, ncol = 3)

png(paste0(output_directory_x, '/Covid_19_deaths_in_carehomes_wsx_ltlas.png'),
    width = 1580,
    height = 1050,
    res = 120)
print(ltla_deaths_plot_2)
dev.off()

# Exporting for figures #

 all_deaths_json_export <- All_settings_occurrences %>%
  ungroup() %>%
  select(Name, Week_number, Week_ending, Cause, Deaths) %>%
  group_by(Name, Week_number, Week_ending) %>%
  spread(Cause, Deaths) %>%
  mutate(`Not attributed to Covid-19` = `All causes` - `COVID 19`) %>%
  rename(`Covid-19` = `COVID 19`) %>%
  mutate(Deaths_in_week_label = paste0('The total number of deaths occurring in the week ending ', format(Week_ending, '%d %B %Y'), ' in ', Name, ' was<b> ', format(`All causes`, big.mark = ',', trim = TRUE), '</b>. Of these, <b>', format(`Covid-19`, big.mark = ',', trim = TRUE), ifelse(`Covid-19` == 1, ' death</b> was', ' deaths</b> were'), ' attributed to Covid-19. This is ',  round((`Covid-19`/`All causes`) * 100, 1), '% of deaths occuring in this week.')) %>%
  group_by(Name) %>%
  mutate(Cumulative_deaths_all_cause = cumsum(`All causes`)) %>%
  mutate(Cumulative_covid_deaths = cumsum(`Covid-19`)) %>%
  mutate(Cumulative_deaths_label = paste0('As at ', format(Week_ending, '%d %B %Y'), ' in ', Name, ' the total cumulative number of deaths for 2020 was<b> ', format(Cumulative_deaths_all_cause, big.mark = ',', trim = TRUE), '</b>. The cumulative number of deaths where Covid-19 is recorded as a cause by this date was ', format(Cumulative_covid_deaths, big.mark = ',', trim = TRUE), '. This is ', round((Cumulative_covid_deaths/Cumulative_deaths_all_cause) * 100, 1), '% of deaths occuring by this week.')) %>% 
   mutate(Date_label = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label))
     
all_deaths_json_export %>% 
  select(Name, Week_number, Date_label, `Covid-19`, `Not attributed to Covid-19`, Deaths_in_week_label, Cumulative_deaths_label, Cumulative_covid_deaths, Cumulative_deaths_all_cause) %>% 
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/deaths_all_settings.json'))

all_deaths_json_export %>% 
  select(Name, `All causes`) %>% 
  group_by(Name) %>% 
  filter(`All causes` == max(`All causes`)) %>% 
  mutate(Limit = ifelse(`All causes`<= 50, round_any(`All causes`, 5, ceiling), ifelse(`All causes` <= 100, round_any(`All causes`, 10, ceiling), ifelse(`All causes` <= 250, round_any(`All causes`, 25, ceiling), ifelse(`All causes` <= 500, round_any(`All causes`, 50, ceiling), round_any(`All causes`, 100, ceiling)))))) %>% 
  ungroup() %>% 
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/deaths_limits_by_area.json'))

carehome_deaths_json_export <- Occurrences %>% 
  filter(Place_of_death %in% 'Care home') %>% 
  arrange(Week_number) %>% 
  select(Name, Cause, Week_ending, Week_number, Deaths) %>% 
  mutate(Date_label = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label)) %>% 
  pivot_wider(id_cols = c(Name, Week_ending, Week_number, Date_label),
              names_from = Cause,
              values_from = Deaths) %>% 
  mutate(`Not attributed to Covid-19` = `All causes` - `COVID 19`) %>%
  gather(key = 'Cause', value = 'Deaths', `All causes`:`Not attributed to Covid-19`) %>% 
  select(Name, Week_number, Week_ending, Cause, Date_label, Deaths) %>%
  group_by(Name, Week_number, Week_ending, Date_label) %>%
  spread(Cause, Deaths) %>%
  rename(`Covid-19` = `COVID 19`) %>%
  mutate(Deaths_in_week_label = paste0('The total number of deaths occurring in care homes in the week ending ', format(Week_ending, '%d %B %Y'), ' in ', Name, ' was<b> ', format(`All causes`, big.mark = ',', trim = TRUE), '</b>. Of these, <b>', format(`Covid-19`, big.mark = ',', trim = TRUE), ifelse(`Covid-19` == 1, ' death</b> was', ' deaths</b> were'), ' attributed to Covid-19. This is ',  round((`Covid-19`/`All causes`) * 100, 1), '% of deaths occuring in care homes in this week.')) %>%
  group_by(Name) %>%
  mutate(Cumulative_deaths_all_cause = cumsum(`All causes`)) %>%
  mutate(Cumulative_covid_deaths = cumsum(`Covid-19`)) %>%
  mutate(Cumulative_deaths_label = paste0('As at ', format(Week_ending, '%d %B %Y'), ' in ', Name, ' the total cumulative number of deaths in care homes for 2020 was<b> ', format(Cumulative_deaths_all_cause, big.mark = ',', trim = TRUE), '</b>. The cumulative number of care home deaths where Covid-19 is recorded as a cause by this date was ', format(Cumulative_covid_deaths, big.mark = ',', trim = TRUE), '. This is ', round((Cumulative_covid_deaths/Cumulative_deaths_all_cause) * 100, 1), '% of deaths occuring by this week.')) 

carehome_deaths_json_export %>% 
  select(Name, Week_number, Date_label, `Covid-19`, `Not attributed to Covid-19`, Deaths_in_week_label, Cumulative_deaths_label, Cumulative_covid_deaths, Cumulative_deaths_all_cause) %>% 
  toJSON() %>%
  write_lines(paste0(output_directory_x, '/deaths_carehomes.json'))
 
Occurrences %>% 
  select(Week_ending) %>% 
  unique() %>% 
  filter(Week_ending == max(Week_ending)) %>% 
  rename(Occurring_week_ending = Week_ending) %>% 
  mutate(Reported_week_ending = format(Occurring_week_ending + 8, '%B %d %Y')) %>% 
  mutate(Occurring_week_ending = format(Occurring_week_ending, '%B %d %Y')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/ons_weekly_mortality_dates.json'))

# Export to powerpoint ####

# wkly_template <- read_pptx(paste0(github_repo_dir, '/West Sussex C19 weekly datapack template.pptx'))
# # 
# layout_summary(wkly_template)
# layout_properties(wkly_template, 'mortality')

# Start powerpoint
wkly_template <- read_pptx(paste0(github_repo_dir, '/West Sussex C19 weekly datapack template.pptx')) %>%  
  add_slide(layout = "Intro_page", master = "Office Theme") %>% 
  ph_with(value = paste0('Pack date: ',format(Sys.Date(), '%d %B %Y')), 
          location = ph_location_label(ph_label = 'Date Placeholder 2')) %>% 
  ph_with(value = 'West Sussex COVID-19 Data Summary', 
          location = ph_location_label(ph_label = 'Text Placeholder 9')) %>% 
  ph_with(value = paste0('This pack brings together information relating to COVID-19 in West Sussex.\nWest Sussex County Council Public Health Department monitors information on a daily basis and produce this summary pack as well as showing more detailed analysis on the following website: https://wsx-c19-weekly-supplement.netlify.app. Links are provided to the public data sources available and a summary of current sources is provided at the end of this pack.\nLocal authorities have access to some information that is not in the public domain, this may be due to small numbers or data being provisional.'), 
          location = ph_location_label(ph_label = 'Text Placeholder 11')) %>% 
  ph_with(value = 'Contact', 
          location = ph_location_label(ph_label = 'Text Placeholder 13')) %>% 
  ph_with(value = 'publichealth@westsussex.gov.uk', 
          location = ph_location_label(ph_label = 'Text Placeholder 15')) %>% 
  ph_with(value = paste0('Slide ', length(.)-1), 
          location = ph_location_label(ph_label = 'Text Placeholder 12'))
  
for(i in 1:length(areas_to_loop)){
  
area_x <- areas_to_loop[i]

wkly_template <- wkly_template %>% 
  add_slide(layout = "Daily_case_layout", master = "Office Theme") %>% 
  ph_with(value = paste0('Pack date: ', format(Sys.Date(), '%d %B %Y')), 
          location = ph_location_label(ph_label = 'Date Placeholder 2')) %>% 
  ph_with(value = external_img(src = paste0(github_repo_dir, '/Outputs/Figure_1_', gsub(' ' , '_', area_x), '_confirmed_daily_cases.png')), 
          location = ph_location_label(ph_label = 'Picture Placeholder 8')) %>% 
  ph_with(value = 'Source: https://coronavirus.data.gov.uk/', 
          href = 'https://coronavirus.data.gov.uk',
          location = ph_location_label(ph_label = 'Text Placeholder 15')) %>% 
  ph_with(value = paste0('Slide ', length(.) -1), 
          location = ph_location_label(ph_label = 'Text Placeholder 12'))
}

# Middle plots

wkly_template <- wkly_template %>% 
  add_slide(layout = "Heatmap_layout", master = "Office Theme") %>% # Heatmap
  ph_with(value = paste0('Pack date: ', format(Sys.Date(), '%d %B %Y')), 
          location = ph_location_label(ph_label = 'Date Placeholder 2')) %>% 
  ph_with(value = external_img(src = paste0(github_repo_dir, '/Outputs/Figure_2_confirmed_heatmap_rate.png')), 
          location = ph_location_label(ph_label = 'Picture Placeholder 8')) %>% 
  ph_with(value = 'Source: https://coronavirus.data.gov.uk/', 
          href = 'https://coronavirus.data.gov.uk',
          location = ph_location_label(ph_label = 'Text Placeholder 15')) %>% 
  ph_with(value = paste0('Slide ', length(.) -1), 
          location = ph_location_label(ph_label = 'Text Placeholder 12')) %>% 
  add_slide(layout = "rate_map_layout", master = "Office Theme") %>% # UTLA Map
  ph_with(value = external_img(src = paste0(github_repo_dir, '/Outputs/Figure_3_cumulative_rate_utla_latest.png')), 
          location = ph_location_label(ph_label = 'Picture Placeholder 8')) %>% 
  ph_with(value = paste0('Pack date: ', format(Sys.Date(), '%d %B %Y')), 
          location = ph_location_label(ph_label = 'Date Placeholder 2')) %>% 
  ph_with(value = 'Source: https://coronavirus.data.gov.uk/', 
          href = 'https://coronavirus.data.gov.uk',
          location = ph_location_label(ph_label = 'Text Placeholder 15')) %>% 
  ph_with(value = paste0('Slide ', length(.) -1), 
          location = ph_location_label(ph_label = 'Text Placeholder 19')) %>% 
  ph_with(value = paste0('Upper Tier Local Authority cumulative cases as at ', format(last_date, '%d %B')), 
          location = ph_location_label(ph_label = 'Text Placeholder 3')) %>% 
  ph_with(value = 'What is a decile?',
          location = ph_location_label(ph_label = 'Text Placeholder 12')) %>%
  ph_with(value = paste0('Every area in England is ranked from highest to lowest rate of confirmed COVID-19 cases per 100,000 population and then areas are divided 10 groups each representing 10% of the areas in England.'), 
          location = ph_location_label(ph_label = 'Text Placeholder 14')) %>% 
  ph_with(value = paste0('West Sussex is in the ', ifelse(ordinal(as.numeric(subset(utla_rate, Name == 'West Sussex', select = 'Cumulative_Rate_decile_actual'))) == '1st', 'highest 10% of local authorities.', ifelse(ordinal(as.numeric(subset(utla_rate, Name == 'West Sussex', select = 'Cumulative_Rate_decile_actual')))== '10th', 'lowest 10% of local authorities.', paste0(ordinal(as.numeric(subset(utla_rate, Name == 'West Sussex', select = 'Cumulative_Rate_decile_actual'))), ' decile.')))), 
          location = ph_location_label(ph_label = 'Text Placeholder 6')) %>% 
  ph_with(value = ft_utla_rate_wsx, 
          location = ph_location_label(ph_label = 'Table Placeholder 7')) %>% 
  add_slide(layout = "rate_map_rolling_layout", master = "Office Theme") %>% # UTLA Map
  ph_with(value = external_img(src = paste0(github_repo_dir, '/Outputs/Figure_3_rolling_rate_utla_latest.png')), 
          location = ph_location_label(ph_label = 'Picture Placeholder 8')) %>% 
  ph_with(value = paste0('Pack date: ', format(Sys.Date(), '%d %B %Y')), 
          location = ph_location_label(ph_label = 'Date Placeholder 2')) %>% 
  ph_with(value = 'Source: https://coronavirus.data.gov.uk/', 
          href = 'https://coronavirus.data.gov.uk',
          location = ph_location_label(ph_label = 'Text Placeholder 15')) %>% 
  ph_with(value = paste0('Slide ', length(.) -1), 
          location = ph_location_label(ph_label = 'Text Placeholder 19')) %>% 
  ph_with(value = paste0('Upper Tier Local Authority rolling 7 day cases as at ', format(last_date, '%d %B')), 
          location = ph_location_label(ph_label = 'Text Placeholder 3')) %>% 
  ph_with(value = 'What is a decile?',
          location = ph_location_label(ph_label = 'Text Placeholder 12')) %>%
  ph_with(value = paste0('Every area in England is ranked from highest to lowest rate of confirmed COVID-19 cases per 100,000 population and then areas are divided 10 groups each representing 10% of the areas in England.'), 
          location = ph_location_label(ph_label = 'Text Placeholder 14')) %>%
  ph_with(value = paste0('Note that the very recent days are excluded in the 7-day total as these are thought to be incomplete. As such, this data represents cases in the ', rolling_period_x), 
          location = ph_location_label(ph_label = 'Text Placeholder 6')) %>%
  ph_with(value = ft_utla_rolling_rate_wsx,
          location = ph_location_label(ph_label = 'Table Placeholder 7')) %>%
add_slide(layout = "rate_map_layout", master = "Office Theme") %>%  # LTLA map
  ph_with(value = external_img(src = paste0(github_repo_dir, '/Outputs/Figure_4_cumulative_rate_ltla_latest.png')), 
          location = ph_location_label(ph_label = 'Picture Placeholder 8')) %>% 
  ph_with(value = paste0('Pack date: ', format(Sys.Date(), '%d %B %Y')), 
          location = ph_location_label(ph_label = 'Date Placeholder 2')) %>% 
  ph_with(value = 'Source: https://coronavirus.data.gov.uk/', 
          href = 'https://coronavirus.data.gov.uk',
          location = ph_location_label(ph_label = 'Text Placeholder 15')) %>% 
  ph_with(value = paste0('Slide ', length(.) -1), 
          location = ph_location_label(ph_label = 'Text Placeholder 19')) %>% 
  ph_with(value = paste0('Lower Tier Local Authority cumulative cases as at ', format(last_date, '%d %B')), 
          location = ph_location_label(ph_label = 'Text Placeholder 3')) %>% 
  ph_with(value = 'What is a decile?',
          location = ph_location_label(ph_label = 'Text Placeholder 12')) %>%
  ph_with(value = paste0('Every area in England is ranked from highest to lowest rate of confirmed COVID-19 cases per 100,000 population and then areas are divided 10 groups each representing 10% of the areas in England.'), 
          location = ph_location_label(ph_label = 'Text Placeholder 14')) %>% 
  ph_with(value = ft_ltla_rate_wsx, 
          location = ph_location_label(ph_label = 'Table Placeholder 7')) %>% 
  add_slide(layout = "rate_map_rolling_layout", master = "Office Theme") %>%  # LTLA map
  ph_with(value = external_img(src = paste0(github_repo_dir, '/Outputs/Figure_4_rolling_rate_ltla_latest.png')), 
          location = ph_location_label(ph_label = 'Picture Placeholder 8')) %>% 
  ph_with(value = paste0('Pack date: ', format(Sys.Date(), '%d %B %Y')), 
          location = ph_location_label(ph_label = 'Date Placeholder 2')) %>% 
  ph_with(value = 'Source: https://coronavirus.data.gov.uk/', 
          href = 'https://coronavirus.data.gov.uk',
          location = ph_location_label(ph_label = 'Text Placeholder 15')) %>% 
  ph_with(value = paste0('Slide ', length(.) -1), 
          location = ph_location_label(ph_label = 'Text Placeholder 19')) %>% 
  ph_with(value = paste0('Lower Tier Local Authority rolling cases as at ', format(last_date, '%d %B')), 
          location = ph_location_label(ph_label = 'Text Placeholder 3')) %>% 
  ph_with(value = 'What is a decile?',
          location = ph_location_label(ph_label = 'Text Placeholder 12')) %>%
  ph_with(value = paste0('Every area in England is ranked from highest to lowest rate of confirmed COVID-19 cases per 100,000 population and then areas are divided 10 groups each representing 10% of the areas in England.'), 
          location = ph_location_label(ph_label = 'Text Placeholder 14')) %>% 
  ph_with(value = paste0('Note that the very recent days are excluded in the 7-day total as these are thought to be incomplete. As such, this data represents cases in the ', rolling_period_x), 
          location = ph_location_label(ph_label = 'Text Placeholder 6')) %>%
  ph_with(value = ft_ltla_rolling_rate_wsx,
          location = ph_location_label(ph_label = 'Table Placeholder 7')) %>%
  add_slide(layout = "pathways_layout", master = "Office Theme") %>%
  ph_with(value = external_img(src = paste0(github_repo_dir, '/Outputs/Figure_5_complete_triages_nhs_pathways.png')), 
          location = ph_location_label(ph_label = 'Picture Placeholder 10')) %>% 
  ph_with(value = paste0('Pack date: ', format(Sys.Date(), '%d %B %Y')), 
          location = ph_location_label(ph_label = 'Date Placeholder 2')) %>% 
  ph_with(value = 'This data is based on potential COVID-19 symptoms reported by members of the public to NHS Pathways through NHS 111 or 999 and 111 online.\nIt provides a view of service contacts and an early view of people concerned about their symptoms. It is not based on any outcomes of tests for COVID-19.\nThis is also not a count of people as a user can repeat the triage process several times.\nIn 111 online, any user that starts the COVID-19 assessment service is indicating that the may have symptoms of coronavirus.', 
          location = ph_location_label(ph_label = 'Text Placeholder 20')) %>% 
  ph_with(value = 'Source: NHS Digital', 
          # href = 'https://coronavirus.data.gov.uk',
          location = ph_location_label(ph_label = 'Text Placeholder 22')) %>% 
  ph_with(value = paste0('Slide ', length(.) -1), 
          location = ph_location_label(ph_label = 'Text Placeholder 19')) %>% 
  ph_with(value = nhs_pways_text_1, 
          location = ph_location_label(ph_label = 'Text Placeholder 13')) %>% 
  ph_with(value = nhs_pways_text_2, 
          location = ph_location_label(ph_label = 'Text Placeholder 17'))

# Death plots
for(i in 1:length(areas_to_loop)){
  
  area_x <- areas_to_loop[i]
  
  wkly_template <- wkly_template %>% 
    add_slide(layout = "mortality", master = "Office Theme") %>% 
    ph_with(value = paste0('Pack date: ', format(Sys.Date(), '%d %B %Y')), 
            location = ph_location_label(ph_label = 'Date Placeholder 2')) %>% 
    ph_with(value = external_img(src = paste0(github_repo_dir, '/Outputs/Figure_6_wkly_deaths_', gsub(' ' , '_', area_x), '.png')), 
            location = ph_location_label(ph_label = 'Picture Placeholder 7')) %>% 
    ph_with(value = external_img(src = paste0(github_repo_dir, '/Outputs/Figure_7_wkly_deaths_carehomes_', gsub(' ' , '_', area_x), '.png')), 
            location = ph_location_label(ph_label = 'Picture Placeholder 3')) %>% 
    ph_with(value = 'Source: Office for National Statistics', 
            # href = 'https://coronavirus.data.gov.uk',
            location = ph_location_label(ph_label = 'Text Placeholder 22')) %>% 
    ph_with(value = paste0('Slide ', length(.) -1), 
            location = ph_location_label(ph_label = 'Text Placeholder 19'))
}

# Last one - move the data source slide
wkly_template <- wkly_template %>% 
  move_slide(index = 1, to = length(.))

wkly_template %>%  
  print(paste0(github_repo_dir, '/Latest_West_Sussex_C19_slide_deck.pptx'))

# Restrictions ####

# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/914840/LA_watchlist_data_file_week35.XLSX

# download.file('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/919133/Contain_framework_lower_tier_local_authority_watchlist_-_maps_by_Lower_Super_Output_Area_data_-_18_September_2020.xlsx', paste0(github_repo_dir, '/ltla_watchlist.xlsx'), mode = 'wb')
# 
# 
# #               
# 
# lookup <- read_csv('https://opendata.arcgis.com/datasets/3e4f4af826d343349c13fb7f0aa2a307_0.csv')
# 
# # read_csv('https://visual.parliament.uk/research/visualisations/coronavirus-restrictions-map/commonslibrary-coronavirus-restrictions-data.csv') %>% 
#   # View()
# 
# rest_of_england_restrictions <- read_csv('https://visual.parliament.uk/research/visualisations/coronavirus-restrictions-map/commonslibrary-coronavirus-restrictions-data.csv') %>% 
#   filter(Category == 'Rest of England') %>% 
#   mutate(l_national_ruleofsix = 1,
#          l_national_raves = 1,
#          l_national_openinghours = 1)
# 
# ltla_restrictions <-  geojson_read('https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson',  what = "sp") 
# 
# if(exists('ltla_restrictions') == FALSE) {
#   ltla_restrictions <- geojson_read(paste0(output_directory_x, '/failsafe_ltla_boundary.geojson'),  what = "sp") 
# }
# 
# ltla_restrictions <- ltla_restrictions  %>% 
#   left_join(lookup, by = c('lad19nm' = 'LTLA19NM')) %>% 
#   filter(substr(lad19cd, 1, 1) == 'E') %>% 
#   left_join(read_csv('https://visual.parliament.uk/research/visualisations/coronavirus-restrictions-map/commonslibrary-coronavirus-restrictions-data.csv'), by = c('lad19cd' = 'lacode')) %>% 
#   select(!c(Country, lad19nmw)) %>% 
#   rename(Restriction_type = restrictions) %>% 
#   rename(l_tier = tier) %>% 
#   mutate(label = paste0('<Strong>', lad19nm, '</Strong><br><br>This area is currently in the <Strong>', l_tier, ' </Strong>level of restrictions.<br>', ifelse(Restriction_type == 'National', 'This areas is currently subject to <b>national</b> restrictions only. ', ifelse(Restriction_type == 'Local', 'This area is currently subject to <b>local</b> restrictions in addition to the national legislation.', NA)), ' Restrictions for this area include:', '<ul>', ifelse(local_ruleofsix == 1, '<li>Not gathering socially in groups larger than six unless it is for an exempted purpose.</li>', ''), ifelse(local_householdmixing == 1, '<li>Not mixing with other households (e.g. visiting each other inside their homes).</li>', ''), ifelse(local_raves == 1, '<li>Not gathering in large groups sometimes called raves, of typically 30 or more people (some exceptions of large gatherings apply).</li>', ''), ifelse(local_stayinglocal == 1, '<li>Not leaving the local area or entering a protected area without a reasonable excuse.</li>', ''), ifelse(local_notstayingaway == 1, '<li>Not staying overnight somewhere other than their home without a reasonable excuse.</li>', ''), ifelse(local_businessclosures == 1, '<li>Certain businesses must close physical premises and move to online/delivery/take away only services.</li>', ''), ifelse(local_openinghours == 1, '<li>Certain businesses must restrict openning hours (e.g. close early).</li>', ''), '<li>Wearing a face covering is required in many public venues, in shops and when using public transport. This includes customers and staff members. Some exemptions apply.</li></ul>For further information see: ', url_local)) %>%
#   select(objectid, lad19nm, l_tier, label)
# 
# # leaflet(ltla_restrictions) %>% 
# #   addTiles() %>%
# #   addPolygons(stroke = FALSE, 
# #               smoothFactor = 0.3, 
# #               fillOpacity = .6,
# #               fillColor = ~ifelse(Restriction_type == 'National', '#ffb400', '#9762a2'),
# #               label = ~label)
# 
# geojson_write(geojson_json(ltla_restrictions), file = paste0(output_directory_x, '/ltla_covid_restrictions_hcl_latest.geojson'))

#paste0('Information provided by the House of Commons Library Coronavirus Restrictions Tool (available: https://visual.parliament.uk/research/visualisations/coronavirus-restrictions-map/). Contains Parliamentary information licensed under the Open Parliament Licence v3.0.')

# incidence and growth rate ####

# Exact Poisin confidence intervals are calculated using the pois.exact function from the epitools package (see https://www.rdocumentation.org/packages/epitools/versions/0.09/topics/pois.exact for details)

library(epitools)

growth_rate <- p12_test_df %>% 
  filter(Data_completeness == 'Complete') %>% 
  select(Name, Code, Type, Date, Rolling_7_day_new_cases, Perc_change_on_rolling_7_days_actual, Perc_change_on_rolling_7_days_tidy, Cumulative_cases, Cumulative_per_100000, Population, Label_3) %>%
  mutate(Rolling_7_day_new_cases = replace_na(Rolling_7_day_new_cases, 0)) %>% 
  mutate(Rolling_7_day_rate = pois.exact(Rolling_7_day_new_cases, Population)[[3]]*100000) %>% 
  mutate(Rolling_rate_lcl = pois.exact(Rolling_7_day_new_cases, Population)[[4]]*100000) %>% 
  mutate(Rolling_rate_ucl = pois.exact(Rolling_7_day_new_cases, Population)[[5]]*100000) %>% 
  arrange(Name, Date) %>% 
  group_by(Name) %>% 
  mutate(Last_week_incidence_rate = lag(Rolling_7_day_rate, 7),
         Last_week_date = lag(Date, 7)) %>% 
  ungroup() %>% 
  mutate(Change_actual_by_week = ifelse(Perc_change_on_rolling_7_days_tidy == 'No change (zero cases)', 0, ifelse(Perc_change_on_rolling_7_days_tidy == '0 cases in previous 7 days', 1, Perc_change_on_rolling_7_days_actual))) %>% 
  rename(Label_2 = Label_3)

growth_rate_england <- growth_rate %>% 
  filter(Name == 'England') %>% 
  rename(Eng_rate = Rolling_7_day_rate,
         Eng_lcl = Rolling_rate_lcl,
         Eng_ucl = Rolling_rate_ucl) 

growth_rate_ltla <- growth_rate %>%
  left_join(growth_rate_england[c('Date', 'Eng_rate', 'Eng_lcl', 'Eng_ucl')], by = 'Date') %>%
  mutate(Label_1 = paste0('The 7 day incidence rate (',round(Rolling_7_day_rate, 1), ' per 100,000 population) is', ifelse(Rolling_rate_lcl > Eng_ucl, ' significantly higher than '  , ifelse(Rolling_rate_ucl < Eng_lcl, ' significantly lower than ', ' similar to ')), 'the national rate (', round(Eng_rate,1),')')) %>%
  mutate(Label_3 = paste0('As at ', format(Date, '%d %B'), ' there have been a total of ', format(Cumulative_cases, big.mark = ',', trim = TRUE), ' cases (', format(round(Cumulative_per_100000,1), big.mark = ',', trim = TRUE), ' cases per 100,000).')) %>%
  mutate(Label = paste(Label_1, Label_3, sep = '<br><br>')) %>%
  filter(Type %in% c('Lower Tier Local Authority', 'Unitary Authority') | Name == 'England')

growth_rate_ltla %>%
  filter(Date >= '2020-09-01') %>%
  mutate(Name = factor(Name, levels = c(setdiff(unique(growth_rate_ltla$Name), c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex','Worthing', 'England')), c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex','Worthing', 'England')))) %>% 
  arrange(Name) %>% 
  select(Name, Date, Rolling_7_day_rate, Change_actual_by_week, Perc_change_on_rolling_7_days_tidy, Label) %>% 
  toJSON() %>%
  write_lines(paste0(output_directory_x,'/ltla_growth_since_september.json'))

# growth_rate_ltla %>%
#   filter(Date == complete_date) %>%
#   select(Name, Date, Rolling_7_day_rate, Change_actual_by_week, Perc_change_on_rolling_7_days_tidy, Label) %>%
#   toJSON() %>%
#   write_lines(paste0(output_directory_x,'/ltla_growth_latest.json'))

growth_rate_utla <- growth_rate %>% 
  left_join(growth_rate_england[c('Date', 'Eng_rate', 'Eng_lcl', 'Eng_ucl')], by = 'Date') %>% 
  mutate(Label_1 = ifelse(Name == 'England', paste0('The 7 day incidence rate (', round(Rolling_7_day_rate, 1), ' per 100,000 population) and there have been a total of ', format(Cumulative_cases, big.mark = ',', trim = TRUE), ' cases (', format(round(Cumulative_per_100000,1), big.mark = ',', trim = TRUE), ' cases per 100,000).'), paste0('The 7 day incidence rate (',round(Rolling_7_day_rate, 1), ' per 100,000 population) is', ifelse(Rolling_rate_lcl > Eng_ucl, ' significantly higher than '  , ifelse(Rolling_rate_ucl < Eng_lcl, ' significantly lower than ', ' similar to ')), 'the national rate (', round(Eng_rate,1),')'))) %>% 
  mutate(Label_3 = paste0('As at ', format(Date, '%d %B'), ' there have been a total of ', format(Cumulative_cases, big.mark = ',', trim = TRUE), ' cases (', format(round(Cumulative_per_100000, 1), big.mark = ',', trim = TRUE), ' cases per 100,000).')) %>% 
  mutate(Label = paste(Label_1, Label_2, Label_3, sep = '<br><br>')) %>% 
  filter(Type %in% c('Upper Tier Local Authority', 'Unitary Authority') | Name == 'England')

growth_rate_utla %>% 
  filter(Date >= '2020-09-01') %>%
  mutate(Name = factor(Name, levels = c(setdiff(unique(growth_rate_utla$Name), c('Brighton and Hove', 'East Sussex', 'West Sussex', 'England')), c('Brighton and Hove', 'East Sussex', 'West Sussex', 'England')))) %>% 
  arrange(Name) %>% 
  select(Name, Date, Rolling_7_day_rate, Change_actual_by_week, Perc_change_on_rolling_7_days_tidy, Label_1, Label_2, Label_3) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/utla_growth_since_september.json'))

# growth_rate_utla %>% 
#   filter(Date == complete_date) %>% 
#   select(Name, Date, Rolling_7_day_rate, Change_actual_by_week, Perc_change_on_rolling_7_days_tidy, Label_1, Label_2, Label_3) %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x,'/utla_growth_latest.json'))

  # growth_rate_utla %>% 
  # filter(Date >= '2020-09-01') %>%
  # summarise(Max_rolling_rate = max(Rolling_7_day_rate, na.rm = TRUE),
  #           Max_change_week = max(Change_actual_by_week, na.rm =TRUE)) %>% 
  # mutate(Max_rolling_rate = ifelse(Max_rolling_rate <= 50, round_any(Max_rolling_rate, 5, ceiling), ifelse(Max_rolling_rate <= 100, round_any(Max_rolling_rate, 10, ceiling), ifelse(Max_rolling_rate <= 250, round_any(Max_rolling_rate, 25, ceiling), ifelse(Max_rolling_rate <= 500, round_any(Max_rolling_rate, 50, ceiling), ifelse(Max_rolling_rate <= 1000, round_any(Max_rolling_rate, 100, ceiling),  round_any(Max_rolling_rate, 200, ceiling))))))) %>%   mutate(Max_change_week = ifelse(Max_change_week <= 10, round_any(Max_change_week, 1, ceiling), ifelse(Max_change_week <= 20, round_any(Max_change_week, 2, ceiling), ifelse(Max_change_week <= 50, round_any(Max_change_week, 5, ceiling), round_any(Max_change_week, 10, ceiling))))) %>% 
  #   toJSON() %>% 
  #   write_lines(paste0(output_directory_x,'/utla_growth_limits.json'))
  
# growth_rate_utla %>% 
#   filter(Date == complete_date) %>%
#   summarise(Max_rolling_rate = max(Rolling_7_day_rate, na.rm = TRUE),
#             Max_change_week = max(Change_actual_by_week, na.rm =TRUE)) %>% 
#     mutate(Max_rolling_rate = ifelse(Max_rolling_rate <= 50, round_any(Max_rolling_rate, 5, ceiling), ifelse(Max_rolling_rate <= 100, round_any(Max_rolling_rate, 10, ceiling), ifelse(Max_rolling_rate <= 250, round_any(Max_rolling_rate, 25, ceiling), ifelse(Max_rolling_rate <= 500, round_any(Max_rolling_rate, 50, ceiling), ifelse(Max_rolling_rate <= 1000, round_any(Max_rolling_rate, 100, ceiling),  round_any(Max_rolling_rate, 200, ceiling))))))) %>%   mutate(Max_change_week = ifelse(Max_change_week <= 10, round_any(Max_change_week, 1, ceiling), ifelse(Max_change_week <= 20, round_any(Max_change_week, 2, ceiling), ifelse(Max_change_week <= 50, round_any(Max_change_week, 5, ceiling), round_any(Max_change_week, 10, ceiling))))) %>% 
#     toJSON() %>% 
#     write_lines(paste0(output_directory_x,'/utla_growth_limits_complete_date.json'))
  
growth_rate_utla %>% 
  filter(Date >= '2020-09-01') %>%
  select(Date) %>% 
  unique() %>% 
  arrange(Date) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/growth_limits_dates.json'))

# age specific rates ####

mye_ages <- read_csv('https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=2092957699,2013265921...2013265932,1816133633...1816133848,1820327937...1820328318&date=latest&gender=0&c_age=1,3...18,210&measures=20100&select=geography_name,geography_code,c_age_name,obs_value,geography_type') %>% 
  rename(Population = OBS_VALUE,
         Code = GEOGRAPHY_CODE,
         Name = GEOGRAPHY_NAME,
         Type = GEOGRAPHY_TYPE,
         Age = C_AGE_NAME) %>% 
  group_by(Code, Name, Age) %>% 
  mutate(Count = n()) %>% 
  unique() %>% 
  mutate(Type = ifelse(Count == 2, 'Unitary Authoritory', ifelse(Type == 'local authorities: county / unitary (as of April 2019)', 'Upper Tier Local Authority', ifelse(Type == 'local authorities: district / unitary (as of April 2019)', 'Lower Tier Local Authority', ifelse(Type == 'regions', 'Region', ifelse(Type == 'countries', 'Country', Type)))))) %>% 
  group_by(Name, Code) %>% 
  ungroup() %>% 
  select(-Count) %>% 
  unique() %>% 
  mutate(Age = gsub('Aged ', '', Age)) %>% 
  mutate(Age = gsub('Age', '', Age)) %>% 
  mutate(Age = gsub(' 0 - ', '0-', Age)) %>% 
  mutate(Age = paste0(Age, ' years')) %>% 
  mutate(Age = ifelse(Age %in% c('80-84 years', '85+ years'), '80+ years', Age)) %>% 
  group_by(Name, Code, Age, Type) %>% 
  summarise(Population = sum(Population, na.rm = TRUE)) %>% 
  ungroup()

# We need to back fill the missing days!! #####

age_spec <- read_csv('https://coronavirus.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-unstacked.csv') %>% 
  filter(areaName %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East', 'England')) %>% 
  select(areaCode, areaName, areaType, date, `newCasesBySpecimenDate-0_4`,`newCasesBySpecimenDate-5_9`,`newCasesBySpecimenDate-10_14`,`newCasesBySpecimenDate-15_19`,`newCasesBySpecimenDate-20_24`,`newCasesBySpecimenDate-25_29`,`newCasesBySpecimenDate-30_34`,`newCasesBySpecimenDate-35_39`,`newCasesBySpecimenDate-40_44`,`newCasesBySpecimenDate-45_49`,`newCasesBySpecimenDate-50_54`,`newCasesBySpecimenDate-55_59`,`newCasesBySpecimenDate-60_64`,`newCasesBySpecimenDate-65_69`,`newCasesBySpecimenDate-70_74`,`newCasesBySpecimenDate-75_79`,`newCasesBySpecimenDate-80_84`,`newCasesBySpecimenDate-85_89`,`newCasesBySpecimenDate-90+`,`newCasesBySpecimenDate-unassigned`) %>%
  pivot_longer(cols = c(`newCasesBySpecimenDate-0_4`,`newCasesBySpecimenDate-5_9`,`newCasesBySpecimenDate-10_14`,`newCasesBySpecimenDate-15_19`,`newCasesBySpecimenDate-20_24`,`newCasesBySpecimenDate-25_29`,`newCasesBySpecimenDate-30_34`,`newCasesBySpecimenDate-35_39`,`newCasesBySpecimenDate-40_44`,`newCasesBySpecimenDate-45_49`,`newCasesBySpecimenDate-50_54`,`newCasesBySpecimenDate-55_59`,`newCasesBySpecimenDate-60_64`,`newCasesBySpecimenDate-65_69`,`newCasesBySpecimenDate-70_74`,`newCasesBySpecimenDate-75_79`,`newCasesBySpecimenDate-80_84`,`newCasesBySpecimenDate-85_89`,`newCasesBySpecimenDate-90+`,`newCasesBySpecimenDate-unassigned`),
               names_to = 'Age') %>% 
  mutate(Age = gsub('newCasesBySpecimenDate-', '', Age)) %>% 
  mutate(Age = ifelse(Age == 'unassigned', 'Unknown', paste0(Age, ' years'))) %>% 
  mutate(Age = gsub('_', '-', Age)) %>% 
  mutate(Age = ifelse(Age %in% c('80-84 years', '85-89 years', '90+ years'), '80+ years', Age)) %>%
  filter(areaType != 'overview') %>% 
  mutate(areaType = ifelse(areaType == 'ltla', 'Lower Tier Local Authority', ifelse(areaType == 'utla', 'Upper Tier Local Authority', ifelse(areaType == 'region', 'Region', ifelse(areaType == 'nation' , 'Nation', NA))))) %>% 
  rename(Name = areaName,
         Code = areaCode,
         Type = areaType,
         Cases = value,
         Date = date) %>% 
  group_by(Name, Age, Date) %>% 
  summarise(Cases = sum(Cases, na.rm = TRUE)) 

Areas <- c('Adur', 'Arun', 'Chichester','Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East', 'England')
Ages <- data.frame(Age = c('0-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years' , '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years' ,'55-59 years', '60-64 years' , '65-69 years', '70-74 years', '75-79 years', '80+ years'))
Dates <- seq.Date(min(age_spec$Date), max(age_spec$Date), by = '1 day')

age_df_daily_combined <- data.frame(Name = character(), Age = character(), Date = character())

for(i in 1:length(Areas)){
  
  area_x = Areas[i]
  
  df_x <- data.frame(Age = rep(Ages$Age, length(Dates))) %>%
    arrange(Age) %>%
    group_by(Age) %>%
    mutate(Date = seq.Date(min(age_spec$Date), max(age_spec$Date), by = '1 day')) %>%
    mutate(Name = area_x) %>%
    mutate(Date = as.character(Date)) %>%
    ungroup()
  
  age_df_daily_combined <- age_df_daily_combined %>%
    bind_rows(df_x)
  
}

case_age_df_daily <- age_df_daily_combined %>%
  mutate(Date = as.Date(Date)) %>%
  left_join(age_spec, by = c('Name', 'Date', 'Age')) %>%
  mutate(Cases = replace_na(Cases, 0)) %>%
  group_by(Name, Age) %>%
  arrange(Name, Age, Date) %>%
  ungroup() %>% 
  left_join(mye_ages, by = c('Name', 'Age')) %>% 
  group_by(Name, Age) %>% 
  arrange(Name, Age, Date) %>% 
  mutate(Cumulative_cases = cumsum(Cases)) %>% 
  mutate(Rolling_7_day_new_cases = rollapply(Cases, 7, sum, align = 'right', fill = NA, partial = TRUE)) %>%
  mutate(Rolling_7_day_new_cases = replace_na(Rolling_7_day_new_cases, 0)) %>% 
  mutate(Perc_change_on_rolling_7_days_actual = round((Rolling_7_day_new_cases - lag(Rolling_7_day_new_cases, 7))/ lag(Rolling_7_day_new_cases, 7), 2)) %>% 
  mutate(Perc_change_on_rolling_7_days_tidy = factor(ifelse(Rolling_7_day_new_cases == 0 & lag(Rolling_7_day_new_cases, 7) == 0, 'No change (zero cases)', ifelse(Perc_change_on_rolling_7_days_actual == Inf, '0 cases in previous 7 days', ifelse(Perc_change_on_rolling_7_days_actual == -1, '100% fewer cases (now zero cases in recent period)', ifelse(Perc_change_on_rolling_7_days_actual <= -.5, '50%+ fewer cases', ifelse(Perc_change_on_rolling_7_days_actual <= -.5, '50% fewer cases', ifelse(Perc_change_on_rolling_7_days_actual < 0, 'Up to 50% fewer cases', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'No change', ifelse(Perc_change_on_rolling_7_days_actual > 0  & Perc_change_on_rolling_7_days_actual <= .5, 'Up to 50% more cases', ifelse(Perc_change_on_rolling_7_days_actual > .5 & Perc_change_on_rolling_7_days_actual <= 1, 'Up to 100% more cases (double the cases from the previous 7 days)', ifelse(Perc_change_on_rolling_7_days_actual > 1 & Perc_change_on_rolling_7_days_actual <= 2, 'Up to 200% more cases (3x the cases in previous 7 days)', ifelse(Perc_change_on_rolling_7_days_actual > 2 & Perc_change_on_rolling_7_days_actual <= 5, 'Up to 5x the cases in previous 7 days', 'More than 5x the cases in the previous 7 days'))))))))))), levels = c("No change (zero cases)", "100% fewer cases (now zero cases in recent period)",   "50%+ fewer cases" ,  "Up to 50% fewer cases" ,  "No change" ,   "Up to 50% more cases" ,      "Up to 100% more cases (double the cases from the previous 7 days)", "Up to 200% more cases (3x the cases in previous 7 days)","Up to 5x the cases in previous 7 days", "More than 5x the cases in the previous 7 days", "0 cases in previous 7 days"))) %>% 
  mutate(Change_actual_by_week = ifelse(Perc_change_on_rolling_7_days_tidy == 'No change (zero cases)', 0, ifelse(Perc_change_on_rolling_7_days_tidy == '0 cases in previous 7 days', 1, Perc_change_on_rolling_7_days_actual))) %>% 
  ungroup() %>% 
  mutate(ASR = pois.exact(Rolling_7_day_new_cases, Population)[[3]]*100000)

# case_age_df_daily %>%
#   filter(Name == 'West Sussex') %>%
#   View()

case_age_df_daily %>% 
  ungroup() %>% 
  filter(Date %in% seq.Date(complete_date -(52*7), complete_date, by = 14)) %>% 
  arrange(Date) %>% 
  mutate(Date_label = format(Date, '%d %b')) %>% 
  select(Date_label) %>% 
  unique() %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/age_specific_rate_dates.json'))

age_spec_10 <- case_age_df_daily %>% 
  mutate(Age = ifelse(Age %in% c('0-4 years', '5-9 years'), '0-9 years', ifelse(Age %in% c('10-14 years', '15-19 years'), '10-19 years',ifelse(Age %in% c('20-24 years', '25-29 years'), '20-29 years',ifelse(Age %in% c('30-34 years', '35-39 years'), '30-39 years',ifelse(Age %in% c('40-44 years', '45-49 years'), '40-49 years',ifelse(Age %in% c('50-54 years', '55-59 years'), '50-59 years',ifelse(Age %in% c('60-64 years', '65-69 years'), '60-69 years',ifelse(Age %in% c('70-74 years', '75-79 years'), '70-79 years', '80+ years'))))))))) %>% 
  group_by(Name, Age, Date) %>% 
  summarise(Cases = sum(Cases, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE)) %>% 
  group_by(Name, Age) %>% 
  arrange(Name, Age, Date) %>% 
  mutate(Cumulative_cases = cumsum(Cases)) %>% 
  mutate(Rolling_7_day_new_cases = rollapply(Cases, 7, sum, align = 'right', fill = NA, partial = TRUE)) %>%
  mutate(Rolling_7_day_new_cases = replace_na(Rolling_7_day_new_cases, 0)) %>% 
  mutate(Perc_change_on_rolling_7_days_actual = round((Rolling_7_day_new_cases - lag(Rolling_7_day_new_cases, 7))/ lag(Rolling_7_day_new_cases, 7), 2)) %>% 
  mutate(Perc_change_on_rolling_7_days_tidy = factor(ifelse(Rolling_7_day_new_cases == 0 & lag(Rolling_7_day_new_cases, 7) == 0, 'No change (zero cases)', ifelse(Perc_change_on_rolling_7_days_actual == Inf, '0 cases in previous 7 days', ifelse(Perc_change_on_rolling_7_days_actual == -1, '100% fewer cases (now zero cases in recent period)', ifelse(Perc_change_on_rolling_7_days_actual <= -.5, '50%+ fewer cases', ifelse(Perc_change_on_rolling_7_days_actual <= -.5, '50% fewer cases', ifelse(Perc_change_on_rolling_7_days_actual < 0, 'Up to 50% fewer cases', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'No change', ifelse(Perc_change_on_rolling_7_days_actual > 0  & Perc_change_on_rolling_7_days_actual <= .5, 'Up to 50% more cases', ifelse(Perc_change_on_rolling_7_days_actual > .5 & Perc_change_on_rolling_7_days_actual <= 1, 'Up to 100% more cases (double the cases from the previous 7 days)', ifelse(Perc_change_on_rolling_7_days_actual > 1 & Perc_change_on_rolling_7_days_actual <= 2, 'Up to 200% more cases (3x the cases in previous 7 days)', ifelse(Perc_change_on_rolling_7_days_actual > 2 & Perc_change_on_rolling_7_days_actual <= 5, 'Up to 5x the cases in previous 7 days', 'More than 5x the cases in the previous 7 days'))))))))))), levels = c("No change (zero cases)", "100% fewer cases (now zero cases in recent period)",   "50%+ fewer cases" ,  "Up to 50% fewer cases" ,  "No change" ,   "Up to 50% more cases" ,      "Up to 100% more cases (double the cases from the previous 7 days)", "Up to 200% more cases (3x the cases in previous 7 days)","Up to 5x the cases in previous 7 days", "More than 5x the cases in the previous 7 days", "0 cases in previous 7 days"))) %>% 
  mutate(Change_actual_by_week = ifelse(Perc_change_on_rolling_7_days_tidy == 'No change (zero cases)', 0, ifelse(Perc_change_on_rolling_7_days_tidy == '0 cases in previous 7 days', 1, Perc_change_on_rolling_7_days_actual))) %>% 
  ungroup() %>% 
  mutate(ASR = pois.exact(Rolling_7_day_new_cases, Population)[[3]]*100000) %>% 
  mutate(Date_label = format(Date, '%d %b')) %>% 
  arrange(Date) %>% 
  select(Name, Age, Date_label, Cases, Cumulative_cases, Rolling_7_day_new_cases, Change_actual_by_week, ASR) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/age_specific_rates_10_years_by_date.json'))

case_age_df_daily %>% 
  filter(Age %in% c('0-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years')) %>% 
  mutate(Date_label = format(Date, '%d %b')) %>% 
  select(Name, Age, Date_label, Cases, Cumulative_cases, Rolling_7_day_new_cases, Change_actual_by_week, ASR) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/age_specific_rates_u30_by_date.json'))

case_age_df_daily %>% 
  filter(Age %in% c('60-64 years', '65-69 years', '70-74 years', '75-79 years', '80+ years')) %>% 
  mutate(Age = '60+ years') %>% 
  group_by(Name, Age, Date) %>% 
  summarise(Cases = sum(Cases, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE)) %>% 
  group_by(Name) %>% 
  arrange(Name, Age, Date) %>% 
  mutate(Cumulative_cases = cumsum(Cases)) %>% 
  mutate(Rolling_7_day_new_cases = rollapply(Cases, 7, sum, align = 'right', fill = NA, partial = TRUE)) %>%
  mutate(Rolling_7_day_new_cases = replace_na(Rolling_7_day_new_cases, 0)) %>% 
  mutate(Perc_change_on_rolling_7_days_actual = round((Rolling_7_day_new_cases - lag(Rolling_7_day_new_cases, 7))/ lag(Rolling_7_day_new_cases, 7), 2)) %>% 
  mutate(Perc_change_on_rolling_7_days_tidy = factor(ifelse(Rolling_7_day_new_cases == 0 & lag(Rolling_7_day_new_cases, 7) == 0, 'No change (zero cases)', ifelse(Perc_change_on_rolling_7_days_actual == Inf, '0 cases in previous 7 days', ifelse(Perc_change_on_rolling_7_days_actual == -1, '100% fewer cases (now zero cases in recent period)', ifelse(Perc_change_on_rolling_7_days_actual <= -.5, '50%+ fewer cases', ifelse(Perc_change_on_rolling_7_days_actual <= -.5, '50% fewer cases', ifelse(Perc_change_on_rolling_7_days_actual < 0, 'Up to 50% fewer cases', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'No change', ifelse(Perc_change_on_rolling_7_days_actual > 0  & Perc_change_on_rolling_7_days_actual <= .5, 'Up to 50% more cases', ifelse(Perc_change_on_rolling_7_days_actual > .5 & Perc_change_on_rolling_7_days_actual <= 1, 'Up to 100% more cases (double the cases from the previous 7 days)', ifelse(Perc_change_on_rolling_7_days_actual > 1 & Perc_change_on_rolling_7_days_actual <= 2, 'Up to 200% more cases (3x the cases in previous 7 days)', ifelse(Perc_change_on_rolling_7_days_actual > 2 & Perc_change_on_rolling_7_days_actual <= 5, 'Up to 5x the cases in previous 7 days', 'More than 5x the cases in the previous 7 days'))))))))))), levels = c("No change (zero cases)", "100% fewer cases (now zero cases in recent period)",   "50%+ fewer cases" ,  "Up to 50% fewer cases" ,  "No change" ,   "Up to 50% more cases" ,      "Up to 100% more cases (double the cases from the previous 7 days)", "Up to 200% more cases (3x the cases in previous 7 days)","Up to 5x the cases in previous 7 days", "More than 5x the cases in the previous 7 days", "0 cases in previous 7 days"))) %>% 
  mutate(Change_actual_by_week = ifelse(Perc_change_on_rolling_7_days_tidy == 'No change (zero cases)', 0, ifelse(Perc_change_on_rolling_7_days_tidy == '0 cases in previous 7 days', 1, Perc_change_on_rolling_7_days_actual))) %>% 
  ungroup() %>% 
  mutate(ASR = pois.exact(Rolling_7_day_new_cases, Population)[[3]]*100000) %>% 
  mutate(Date_label = format(Date, '%d %b')) %>% 
  select(Name, Age, Date_label, Cases, Cumulative_cases, Rolling_7_day_new_cases, Change_actual_by_week, ASR) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/age_specific_rates_60_plus_by_date.json'))

# case_age_df_daily %>% 
#   mutate(Age = ifelse(Age %in% c('0-4 years', '5-9 years'), '0-9 years', ifelse(Age %in% c('10-14 years', '15-19 years'), '10-19 years',ifelse(Age %in% c('20-24 years', '25-29 years'), '20-29 years',ifelse(Age %in% c('30-34 years', '35-39 years'), '30-39 years',ifelse(Age %in% c('40-44 years', '45-49 years'), '40-49 years',ifelse(Age %in% c('50-54 years', '55-59 years'), '50-59 years',ifelse(Age %in% c('60-64 years', '65-69 years'), '60-69 years',ifelse(Age %in% c('70-74 years', '75-79 years'), '70-79 years', '80+ years'))))))))) %>%
#   select(Age) %>% 
#   unique()

# MSOA map ####
if(!file.exists(paste0(github_repo_dir, '/Source files/msoa_lookup_local.csv'))) {
  
  oa_region <- read_csv('https://opendata.arcgis.com/datasets/180c271e84fc400d92ca6dcc7f6ff780_0.csv') %>% 
    select(OA11CD, RGN11NM)
  
  msoa_names <- read_csv('https://visual.parliament.uk/msoanames/static/MSOA-Names-1.8.csv') %>% 
    select(msoa11cd, msoa11hclnm) %>% 
    rename(MSOA11CD = msoa11cd)
  
  msoa_lookup <- read_csv('https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv') %>%
    left_join(read_csv('https://opendata.arcgis.com/datasets/180c271e84fc400d92ca6dcc7f6ff780_0.csv')[c('OA11CD', 'RGN11NM')], by = 'OA11CD') %>% 
    select(MSOA11CD, MSOA11NM, LAD11NM, RGN11NM) %>% 
    unique() %>% 
    left_join(msoa_names, by = 'MSOA11CD')
  
  lsoa_to_msoa <- read_csv('https://opendata.arcgis.com/datasets/a46c859088a94898a7c462eeffa0f31a_0.csv') %>% 
    select(LSOA11CD, MSOA11CD, MSOA11NM) %>% 
    unique()
  
  msoa_to_utla <- read_csv('https://opendata.arcgis.com/datasets/4c6f3314565e43c5ac7885fd71347548_0.csv') %>% 
    left_join(lsoa_to_msoa, by = 'LSOA11CD') %>% 
    select(MSOA11CD, UTLA19NM) %>% 
    mutate(UTLA19NM = ifelse(UTLA19NM %in% c('City of London', 'Hackney'), 'Hackney and City of London', ifelse(UTLA19NM %in% c('Cornwall', 'Isles of Scilly'), 'Cornwall and Isles of Scilly', UTLA19NM))) %>% 
    filter(!is.na(MSOA11CD)) %>% 
    unique()
  
  msoa_lookup %>% 
    left_join(msoa_to_utla, by = 'MSOA11CD') %>% 
    write.csv(., paste0(github_repo_dir, '/Source files/msoa_lookup_local.csv'), row.names = FALSE)
  
}

msoa_lookup <- read_csv(paste0(github_repo_dir, '/Source files/msoa_lookup_local.csv'))

se_msoas <- msoa_lookup %>% 
  filter(RGN11NM == 'South East')

wsx_msoas <- msoa_lookup %>% 
  filter(LAD11NM %in% c('Adur', 'Arun', 'Chichester','Crawley', 'Horsham', 'Mid Sussex', 'Worthing'))

# Weekly rolling sums and population-based rates of new cases by specimen date time series data are available to download for English MSOAs via the following links. The data are updated each day, and show the latest 7 days for which near-complete data release date minus 5 days are available, and historic non-overlapping 7-day blocks. Dates are the final day in the relevant 7-day block, and counts between 0 and 2 are blank in the CSV or NULL in the other formats.

msoa_cases_1 <-read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingSum&metric=newCasesBySpecimenDateRollingRate&format=csv') %>% 
  select(areaCode, areaName, date, newCasesBySpecimenDateRollingSum, newCasesBySpecimenDateRollingRate) %>% 
  # filter(areaCode %in% msoa_lookup$MSOA11CD) %>% 
  filter(date %in% c(max(date))) %>% 
  select(areaCode, date, newCasesBySpecimenDateRollingRate) %>% 
  rename(Latest_rate = newCasesBySpecimenDateRollingRate) %>% 
  mutate(Latest_rate_key = factor(ifelse(is.na(Latest_rate), 'Less than 3 cases', ifelse(Latest_rate <= 50, 'Up to 50 per 100,000', ifelse(Latest_rate <= 100, '51-100 cases per 100,000', ifelse(Latest_rate <= 150, '101-150 cases per 100,000', ifelse(Latest_rate <= 200, '151-200 cases per 100,000', 'More than 200 cases per 100,000'))))), levels = c('Less than 3 cases', 'Up to 50 cases per 100,000', '51-100 cases per 100,000', '101-150 cases per 100,000', '151-200 cases per 100,000', 'More than 200 cases per 100,000')))

msoa_cases_raw <- as.data.frame(read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingSum&metric=newCasesBySpecimenDateRollingRate&format=csv') %>% 
                                  select(areaCode, areaName, date, newCasesBySpecimenDateRollingSum, newCasesBySpecimenDateRollingRate) %>% 
                                  # filter(areaCode %in% msoa_lookup$MSOA11CD) %>% 
                                  group_by(areaCode, areaName) %>% 
                                  arrange(areaCode, areaName, date) %>% 
                                  filter(date %in% c(max(date), max(date) - 7)) %>% 
                                  select(areaCode, areaName, date, newCasesBySpecimenDateRollingSum) %>% 
                                  mutate(date = ifelse(date == max(date), 'This_week', ifelse(date == max(date)-7, 'Last_week', NA))) %>% 
                                  pivot_wider(names_from = 'date', values_from = 'newCasesBySpecimenDateRollingSum') %>% 
                                  mutate(Change_actual = This_week - Last_week) %>% 
                                  mutate(Change_label = ifelse(is.na(Last_week) & is.na(This_week), 'Cases below 3 in both weeks', ifelse(is.na(Last_week), 'Cases below 3 in previous week but have risen', ifelse(is.na(This_week), 'Cases below 3 in the latest 7 days but have fallen', ifelse(Change_actual == 0, 'No change in case numbers', ifelse(Change_actual < 0, 'Cases have fallen', ifelse(Change_actual>0, 'Cases have risen', NA))))))) %>% 
                                  mutate(Case_key = ifelse(is.na(This_week), '0-2 cases', ifelse(This_week <= 5, '3-5 cases', ifelse(This_week <= 10, '6-10 cases', ifelse(This_week <= 15, '11-15 cases', ifelse(This_week <= 20, '16-20 cases', ifelse(This_week <= 25, '21-25 cases','More than 25 cases'))))))) %>% 
                                  left_join(msoa_cases_1, by = 'areaCode') %>% 
                                  left_join(msoa_lookup, by = c('areaCode' = 'MSOA11CD')) %>% 
                                  mutate(Label = paste0('<b>', MSOA11NM,' (', msoa11hclnm,')</b><br><br>In the seven days to ', format(date, '%A %d %B'), ' there were ', ifelse(is.na(This_week), ' less than three new cases.', paste0(format(This_week, big.mark = ',', trim = TRUE), ' new cases, this is a rate of ', round(Latest_rate, 1), ' cases per 100,000 population.')), '<br><br>', ifelse(is.na(Last_week) & is.na(This_week), 'Cases have been below 3 in the last two 7 day periods.', ifelse(is.na(Last_week), paste0('Cases were below 3 in the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,') but have risen this week.'), ifelse(is.na(This_week), paste0('Cases are now below 3 in the latest 7 days but have fallen since the previous 7 day period (up to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual == 0, 'There is no change in case numbers over the last two weeks', ifelse(Change_actual < 0, paste0('Cases have fallen in the last 7 days compared to the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual >0, paste0('Cases have risen in the last 7 days compared to the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,').'), NA)))))))) %>%
                                  ungroup()) 

msoa_cases <- msoa_cases_raw %>% 
  select(MSOA11NM, Case_key, Latest_rate_key, Change_label, Label) %>% 
  #  filter(MSOA11NM %in% se_msoas$MSOA11NM) %>%
  arrange(MSOA11NM)

msoa_boundaries_json <- geojson_read("https://opendata.arcgis.com/datasets/23cdb60ee47e4fef8d72e4ee202accb0_0.geojson",  what = "sp") %>% 
  filter(MSOA11NM %in% msoa_cases$MSOA11NM) %>%
  arrange(MSOA11NM)

df <- data.frame(ID = character())

# Get the IDs of spatial polygon
for (i in msoa_boundaries_json@polygons ) { df <- rbind(df, data.frame(ID = i@ID, stringsAsFactors = FALSE))  }

# and set rowname = ID
row.names(msoa_cases) <- df$ID

# Then use df as the second argument to the spatial dataframe conversion function:
msoa_boundaries_json <- SpatialPolygonsDataFrame(msoa_boundaries_json, msoa_cases)  

# geojson_write(ms_simplify(geojson_json(utla_ua_boundaries_rate_geo), keep = 0.2), file = paste0(output_directory_x, '/utla_covid_rate_latest.geojson'))

geojson_write(ms_simplify(geojson_json(msoa_boundaries_json), keep = 0.2), file = paste0(output_directory_x, '/msoa_covid_rate_latest.geojson'))

msoa_cases_raw %>% 
  select(MSOA11NM, msoa11hclnm, Latest_rate, LAD11NM, This_week, Last_week, Change_label, date, Change_actual) %>% 
  mutate(Change_label = paste0(ifelse(is.na(Last_week) & is.na(This_week), 'Cases have been below 3 in the last two 7 day periods.', ifelse(is.na(Last_week), paste0('Cases were below 3 in the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,') but have risen this week.'), ifelse(is.na(This_week), paste0('Cases are now below 3 in the latest 7 days but have fallen since the previous 7 day period (up to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual == 0, 'There is no change in case numbers over the last two weeks', ifelse(Change_actual < 0, paste0('<b class = "cases_go_down">', Change_actual, '</b> cases compared to the previous 7 days (', Last_week,  ' cases in the 7 days to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual >0, paste0('<b class = "cases_go_up">+', Change_actual, '</b> cases compared to the previous 7 days (', Last_week, ' cases in the 7 days to ', format(max(date)-7, '%A %d %B') ,').'), NA)))))))) %>% 
  mutate(This_week = ifelse(is.na(This_week), '0-2', format(This_week, big.mark = ',', trim = TRUE)),
         Last_week = ifelse(is.na(Last_week), '0-2', format(Last_week, big.mark = ',', trim = TRUE)),
         Latest_rate = ifelse(is.na(Latest_rate), 'No rate available', Latest_rate)) %>%
  select(!c('date', 'Change_actual')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/msoa_summary.json'))

# 
# # MSOA map ####
# oa_region <- read_csv('https://opendata.arcgis.com/datasets/180c271e84fc400d92ca6dcc7f6ff780_0.csv')[c('OA11CD', 'RGN11MM')]
# 
# msoa_names <- read_csv('https://visual.parliament.uk/msoanames/static/MSOA-Names-1.8.csv') %>% 
#   select(msoa11cd, msoa11hclnm) %>% 
#   rename(MSOA11CD = msoa11cd)
# 
# 
# if(exists('msoa_names') == FALSE) {
#   msoa_names <- read_csv(paste0(github_repo_dir,'/Source files/msoa_names_backup.csv')) 
# }
# 
# msoa_names %>% 
#   write.csv(., paste0(github_repo_dir, '/Source files/msoa_names_backup.csv'), row.names = FALSE)
# 
# msoa_lookup <- read_csv('https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv') %>%
#   left_join(read_csv('https://opendata.arcgis.com/datasets/180c271e84fc400d92ca6dcc7f6ff780_0.csv')[c('OA11CD', 'RGN11NM')], by = 'OA11CD') %>% 
#   select(MSOA11CD, MSOA11NM, LAD11NM, RGN11NM) %>% 
#   unique() %>% 
#   left_join(msoa_names, by = 'MSOA11CD')
# 
# if(exists('msoa_lookup') == FALSE) {
#   msoa_lookup <- read_csv(paste0(github_repo_dir,'/Source files/msoa_lookup_backup.csv')) 
# }
# 
# msoa_lookup %>% 
#   write.csv(., paste0(github_repo_dir, '/Source files/msoa_lookup_backup.csv'), row.names = FALSE)
# 
# se_msoas <- msoa_lookup %>% 
#   filter(RGN11NM == 'South East')
# 
# wsx_msoas <- msoa_lookup %>% 
#   filter(LAD11NM %in% c('Adur', 'Arun', 'Chichester','Crawley', 'Horsham', 'Mid Sussex', 'Worthing'))
#   
# 
# # Weekly rolling sums and population-based rates of new cases by specimen date time series data are available to download for English MSOAs via the following links. The data are updated each day, and show the latest 7 days for which near-complete data release date minus 5 days are available, and historic non-overlapping 7-day blocks. Dates are the final day in the relevant 7-day block, and counts between 0 and 2 are blank in the CSV or NULL in the other formats.
# 
# 
# msoa_cases_1 <- read_csv('https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv') %>% 
#   # filter(areaCode %in% msoa_lookup$MSOA11CD) %>% 
#   filter(date %in% c(max(date))) %>% 
#   select(areaCode, date, newCasesBySpecimenDateRollingRate) %>% 
#   rename(Latest_rate = newCasesBySpecimenDateRollingRate) %>% 
#   mutate(Latest_rate_key = factor(ifelse(is.na(Latest_rate), 'Less than 3 cases', ifelse(Latest_rate <= 50, 'Up to 50 per 100,000', ifelse(Latest_rate <= 100, '51-100 cases per 100,000', ifelse(Latest_rate <= 150, '101-150 cases per 100,000', ifelse(Latest_rate <= 200, '151-200 cases per 100,000', 'More than 200 cases per 100,000'))))), levels = c('Less than 3 cases', 'Up to 50 cases per 100,000', '51-100 cases per 100,000', '101-150 cases per 100,000', '151-200 cases per 100,000', 'More than 200 cases per 100,000')))
# 
# msoa_cases_raw <- as.data.frame(read_csv('https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv') %>% 
#   # filter(areaCode %in% msoa_lookup$MSOA11CD) %>% 
#   group_by(areaCode, areaName) %>% 
#   arrange(areaCode, areaName, date) %>% 
#   filter(date %in% c(max(date), max(date) - 7)) %>% 
#   select(areaCode, areaName, date, newCasesBySpecimenDateRollingSum) %>% 
#   mutate(date = ifelse(date == max(date), 'This_week', ifelse(date == max(date)-7, 'Last_week', NA))) %>% 
#   pivot_wider(names_from = 'date', values_from = 'newCasesBySpecimenDateRollingSum') %>% 
#   mutate(Change_actual = This_week - Last_week) %>% 
#   mutate(Change_label = ifelse(is.na(Last_week) & is.na(This_week), 'Cases below 3 in both weeks', ifelse(is.na(Last_week), 'Cases below 3 in previous week but have risen', ifelse(is.na(This_week), 'Cases below 3 in the latest 7 days but have fallen', ifelse(Change_actual == 0, 'No change in case numbers', ifelse(Change_actual < 0, 'Cases have fallen', ifelse(Change_actual>0, 'Cases have risen', NA))))))) %>% 
#   mutate(Case_key = ifelse(is.na(This_week), '0-2 cases', ifelse(This_week <= 5, '3-5 cases', ifelse(This_week <= 10, '6-10 cases', ifelse(This_week <= 15, '11-15 cases', 'More than 15 cases'))))) %>% 
#   left_join(msoa_cases_1, by = 'areaCode') %>% 
#   left_join(msoa_lookup, by = c('areaCode' = 'MSOA11CD')) %>% 
#   mutate(Label = paste0('<b>', MSOA11NM,' (', msoa11hclnm,')</b><br><br>In the seven days to ', format(date, '%A %d %B'), ' there were ', ifelse(is.na(This_week), ' less than three new cases.', paste0(format(This_week, big.mark = ',', trim = TRUE), ' new cases, this is a rate of ', round(Latest_rate, 1), ' cases per 100,000 population.')), '<br><br>', ifelse(is.na(Last_week) & is.na(This_week), 'Cases have been below 3 in the last two 7 day periods.', ifelse(is.na(Last_week), paste0('Cases were below 3 in the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,') but have risen this week.'), ifelse(is.na(This_week), paste0('Cases are now below 3 in the latest 7 days but have fallen since the previous 7 day period (up to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual == 0, 'There is no change in case numbers over the last two weeks', ifelse(Change_actual < 0, paste0('Cases have fallen in the last 7 days compared to the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual >0, paste0('Cases have risen in the last 7 days compared to the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,').'), NA)))))))) %>%
#   ungroup()) 
#   
# msoa_cases <- msoa_cases_raw %>% 
#   select(MSOA11NM, Case_key, Latest_rate_key, Change_label, Label) %>% 
#   filter(MSOA11NM %in% se_msoas$MSOA11NM) %>% 
#   arrange(MSOA11NM)
# 
# msoa_boundaries_json <- geojson_read("https://opendata.arcgis.com/datasets/23cdb60ee47e4fef8d72e4ee202accb0_0.geojson",  what = "sp") %>% 
#   filter(MSOA11NM %in% se_msoas$MSOA11NM) %>%
#   arrange(MSOA11NM)
# 
# df <- data.frame(ID = character())
# 
# # Get the IDs of spatial polygon
# for (i in msoa_boundaries_json@polygons ) { df <- rbind(df, data.frame(ID = i@ID, stringsAsFactors = FALSE))  }
# 
# # and set rowname = ID
# row.names(msoa_cases) <- df$ID
# 
# # Then use df as the second argument to the spatial dataframe conversion function:
# msoa_boundaries_json <- SpatialPolygonsDataFrame(msoa_boundaries_json, msoa_cases)  
# 
# 
# # geojson_write(ms_simplify(geojson_json(utla_ua_boundaries_rate_geo), keep = 0.2), file = paste0(output_directory_x, '/utla_covid_rate_latest.geojson'))
# 
# geojson_write(geojson_json(msoa_boundaries_json), file = paste0(output_directory_x, '/msoa_covid_rate_latest.geojson'))
# 
# msoa_cases_raw %>% 
#   select(MSOA11NM, msoa11hclnm, Latest_rate,  This_week, Last_week, Change_label, date, Change_actual) %>% 
#   mutate(Change_label = paste0(ifelse(is.na(Last_week) & is.na(This_week), 'Cases have been below 3 in the last two 7 day periods.', ifelse(is.na(Last_week), paste0('Cases were below 3 in the previous 7 days (up to ', format(max(date)-7, '%A %d %B') ,') but have risen this week.'), ifelse(is.na(This_week), paste0('Cases are now below 3 in the latest 7 days but have fallen since the previous 7 day period (up to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual == 0, 'There is no change in case numbers over the last two weeks', ifelse(Change_actual < 0, paste0('<b class = "cases_go_down">', Change_actual, '</b> cases compared to the previous 7 days (', Last_week,  ' cases in the 7 days to ', format(max(date)-7, '%A %d %B') ,').'), ifelse(Change_actual >0, paste0('<b class = "cases_go_up">+', Change_actual, '</b> cases compared to the previous 7 days (', Last_week, ' cases in the 7 days to ', format(max(date)-7, '%A %d %B') ,').'), NA)))))))) %>% 
#   mutate(This_week = ifelse(is.na(This_week), '0-2', format(This_week, big.mark = ',', trim = TRUE)),
#          Last_week = ifelse(is.na(Last_week), '0-2', format(Last_week, big.mark = ',', trim = TRUE)),
#          Latest_rate = ifelse(is.na(Latest_rate), 'No rate available', Latest_rate)) %>%
#   select(!c('date', 'Change_actual')) %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x,'/msoa_summary.json'))

ltla_summary_1 <- p12_test_df %>% 
  filter(Type %in% c('Unitary Authority', 'Lower Tier Local Authority')) %>% 
  filter(Date == last_case_date) %>% 
  select(Name, Date, Cumulative_cases, Cumulative_per_100000) %>% 
  rename(Cumulative_date = Date)

ltla_summary_2 <- p12_test_df %>% 
  filter(Date %in% c(complete_date)) %>% 
  rename(Rolling_7_day_average_new_cases = Seven_day_average_new_cases) %>% 
  select(Name, Date, New_cases, New_cases_per_100000, Rolling_7_day_new_cases, Rolling_7_day_new_cases_per_100000, Rolling_7_day_average_new_cases, Perc_change_on_rolling_7_days_actual, Previous_7_days_sum) %>% 
  mutate(Change_direction = ifelse(Perc_change_on_rolling_7_days_actual <0, 'Down', ifelse(Perc_change_on_rolling_7_days_actual == 0, 'Same', ifelse(Perc_change_on_rolling_7_days_actual > 0, 'Up', NA)))) %>% 
  rename(Rate_date = Date)

ltla_summary <- ltla_summary_1 %>% 
  left_join(ltla_summary_2, by = 'Name') %>% 
  left_join(read_csv(paste0(github_repo_dir, '/Source files/ltla_tiers.csv')), by = 'Name') %>% 
  mutate(Change_label = ifelse(Change_direction == 'Down', paste0('decreased by ', format(abs(Previous_7_days_sum - Rolling_7_day_new_cases), big.mark = ',', trim = TRUE), ' cases (', round(abs(Perc_change_on_rolling_7_days_actual) * 100,1), '%)'),ifelse(Change_direction == 'Up', paste0('increased by ',   format(abs(Previous_7_days_sum - Rolling_7_day_new_cases), big.mark = ',', trim = TRUE), ' cases (', round(abs(Perc_change_on_rolling_7_days_actual) * 100,1), '%)'), 'stayed the same.')))

ltla_summary %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/ltla_summary.json'))

ltla_boundaries <- geojson_read('https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson',  what = "sp") 

#download.file('https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson', paste0(github_repo_dir, '/Source files/failsafe_ltla_boundary.geojson'), mode = 'wb')

if(exists('ltla_boundaries') == FALSE) {
  ltla_boundaries <- geojson_read(paste0(github_repo_dir, '/Source files/failsafe_ltla_boundary.geojson'),  what = "sp") 
}

# utla_restrictions_geojson <-geojson_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson",  what = "sp") %>% 
#   filter(substr(ctyua19cd, 1,1 ) == 'E') %>% 
#   mutate(ctyua19nm = ifelse(ctyua19nm %in% c('Cornwall', 'Isles of Scilly'), 'Cornwall and Isles of Scilly', ifelse(ctyua19nm %in% c('City of London', 'Hackney'), 'Hackney and City of London', ctyua19nm))) %>% 
#   mutate(ctyua19cd = ifelse(ctyua19cd %in% c('E06000053', 'E06000052'), 'E06000052', ifelse(ctyua19cd %in% c('E09000001', 'E09000012'), 'E09000012', ctyua19cd))) %>% 
#   group_by(ctyua19cd, ctyua19nm) %>% 
#   summarise() %>% 
#   arrange(ctyua19cd) %>% 
#   left_join(utla_summary, by = c('ctyua19nm' = 'Name')) 

# geojson_write(geojson_json(utla_restrictions_geojson), file = paste0(output_directory_x, '/utla_covid_latest.geojson'))

ltla_restrictions_geojson <- ltla_boundaries %>% 
  filter(lad19cd %in% ltla_summary$`Area code`) %>% 
  arrange(lad19nm)

ltla_summary <- ltla_summary %>% 
  arrange(Name)
#left_join(ltla_summary, by = c('lad19nm' = 'Name')) 

df <- data.frame(ID = character())

# Get the IDs of spatial polygon
for (i in ltla_restrictions_geojson@polygons ) { df <- rbind(df, data.frame(ID = i@ID, stringsAsFactors = FALSE))  }

# and set rowname = ID
row.names(ltla_summary) <- df$ID

# Then use df as the second argument to the spatial dataframe conversion function:
ltla_restrictions_geojson <- SpatialPolygonsDataFrame(ltla_restrictions_geojson, ltla_summary)  

geojson_write(geojson_json(ltla_restrictions_geojson), file = paste0(output_directory_x, '/ltla_covid_latest.geojson'))
