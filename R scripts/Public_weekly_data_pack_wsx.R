# Public facing data pack - West Sussex and LTLAs
library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'fingertipsR', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid'))

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

# 2018 MYE
mye_total <- read_csv('http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1816133633...1816133848,1820327937...1820328318,2092957697...2092957703,2013265921...2013265932&date=latest&gender=0&c_age=200&measures=20100&select=date_name,geography_name,geography_type,geography_code,obs_value') %>% 
  rename(Population = OBS_VALUE,
         Code = GEOGRAPHY_CODE,
         Name = GEOGRAPHY_NAME,
         Type = GEOGRAPHY_TYPE) %>% 
  unique() %>% 
  group_by(Name, Code) %>% 
  mutate(Count = n()) %>% 
  mutate(Type = ifelse(Count == 2, 'Unitary Authority', ifelse(Type == 'local authorities: county / unitary (as of April 2019)', 'Upper Tier Local Authority', ifelse(Type == 'local authorities: district / unitary (as of April 2019)', 'Lower Tier Local Authority', ifelse(Type == 'regions', 'Region', ifelse(Type == 'countries', 'Country', Type)))))) %>% 
  ungroup() %>% 
  select(-Count) %>% 
  unique()

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
  filter(!(`Area type` == 'Lower tier local authority' & Count == 2)) %>% 
  select(-c(`Area type`, Count)) %>% 
  left_join(mye_total, by = 'Code') %>% 
  select(-DATE_NAME) %>% 
  ungroup()

# If no specimens are taken on a day, there is no row for it, and it would be missing data. Indeed, the only zeros are on the latest day. We need to therefore backfill and say if no date exists where it should, then add it, with the cumulative total and zero for new cases.

# One way to do this is to create a new dataframe with a row for each area and date, and left join the daily_cases data to it.
first_date <- min(daily_cases$Date)
last_date <- max(daily_cases$Date)

Areas = daily_cases %>% 
  select(Name, Code, Type) %>% 
  unique()

Dates = seq.Date(first_date, last_date, by = '1 day')

daily_cases_reworked <- data.frame(Name = rep(Areas$Name, length(Dates)), Code = rep(Areas$Code, length(Dates)), Type = rep(Areas$Type, length(Dates)), check.names = FALSE) %>% 
  arrange(Name) %>% 
  group_by(Name) %>% 
  mutate(Date = seq.Date(first_date, last_date, by = '1 day')) %>% 
  left_join(daily_cases, by = c('Name', 'Code', 'Type', 'Date')) %>% 
  mutate(New_cases = ifelse(is.na(New_cases), 0, New_cases)) %>% 
  mutate(New_cumulative = cumsum(New_cases)) %>% 
  filter(!is.na(Cumulative_cases)) %>% 
  mutate(Calculated_same_as_original = ifelse(Cumulative_cases == New_cumulative, 'Yaas', 'Negative'))

# PHE say the last five data points are incomplete (perhaps they should not publish them). Instead, we need to make sure we account for this so that it is not misinterpreted.
complete_date <- max(daily_cases_reworked$Date) - 5

p12_test_df_raw <- data.frame(Name = rep(Areas$Name, length(Dates)), Code = rep(Areas$Code, length(Dates)), Type = rep(Areas$Type, length(Dates)), check.names = FALSE) %>% 
  arrange(Name) %>% 
  group_by(Name) %>% 
  mutate(Date = seq.Date(first_date, last_date, by = '1 day')) %>% 
  mutate(Data_completeness = ifelse(Date >= max(Date) - 4, 'Considered incomplete', 'Complete')) %>% 
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
  mutate(new_case_per_100000_key = factor(ifelse(New_cases_per_100000 < 0, 'Data revised down', ifelse(round(New_cases_per_100000,0) == 0, 'No new cases', ifelse(round(New_cases_per_100000,0) > 0 & round(New_cases_per_100000, 0) <= 5, '1-5 new cases per 100,000', ifelse(round(New_cases_per_100000,0) >= 6 & round(New_cases_per_100000,0) <= 10, '6-10 new cases per 100,000', ifelse(round(New_cases_per_100000,0) >= 11 & round(New_cases_per_100000, 0) <= 15, '11-15 new cases per 100,000', ifelse(round(New_cases_per_100000,0) >= 16 & round(New_cases_per_100000,0) <= 20, '16-20 new cases per 100,000', ifelse(round(New_cases_per_100000,0) > 20, 'More than 20 new cases per 100,000', NA))))))), levels =  c('No new cases', '1-5 new cases per 100,000', '6-10 new cases per 100,000', '11-15 new cases per 100,000', '16-20 new cases per 100,000', 'More than 20 new cases per 100,000'))) %>% 
  ungroup() %>% 
  mutate(Name = ifelse(Name == 'South East', 'South East region', Name))  %>% 
  mutate(Test_pillar = 'Pillars 1 and 2') %>% 
  group_by(Name) %>% 
  mutate(Ten_day_average_new_cases = rollapply(New_cases, 10, mean, align = 'right', fill = NA)) %>% 
  mutate(Fourteen_day_average_new_cases = rollapply(New_cases, 14, mean, align = 'right', fill = NA))

rm(daily_cases, Areas, Dates, first_date, mye_total, area_code_names, daily_cases_reworked)

# Small multiples plot with daily cases bars and rolling average.

p12_test_df_2 <- p12_test_df_raw %>% 
  group_by(Name) %>% 
  filter(Date %in% c(complete_date, complete_date - 7)) %>% 
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
               breaks = seq.Date(max(area_x_df_1$Date) -(52*7), max(area_x_df_1$Date), by = 7),
               limits = c(min(area_x_df_1$Date), max(area_x_df_1$Date)),
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
           label = 'Last 5 days\nnot considered\ncomplete:',
           size = 2.5,
           hjust = 1) +
  labs(x = 'Date',
       y = 'Number of daily confirmed cases',
       title = paste0('Daily number of confirmed Covid-19 cases; Pillar 1 and 2 combined; ', area_x),
       subtitle = paste0('Confirmed cases by specimen date; 01 March - ', format(max(area_x_df_1$Date), '%d %B %Y')),
       caption = 'The black line represents the average number of new cases confirmed in the previous seven days.') +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  theme(legend.position = 'none')

png(paste0(output_directory_x, '/Covid_19_confirmed_cases_', gsub(' ', '_', area_x), '.png'),
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
               breaks = seq.Date(max(ltla_p12_test_df$Date) -(52*7), max(ltla_p12_test_df$Date), by = 7),
               limits = c(min(ltla_p12_test_df$Date), max(ltla_p12_test_df$Date)),
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
           label = 'Last 5 days\nnot considered\ncomplete:',
           size = 2.5,
           hjust = 1) +
  labs(x = 'Date',
       y = 'Number of daily confirmed cases',
       title = paste0('Daily number of confirmed Covid-19 cases; Pillar 1 and 2 combined; West Sussex lower tier Local Authorities'),
       subtitle = paste0('Confirmed cases by specimen date; 01 March - ', format(max(ltla_p12_test_df$Date), '%d %B %Y')),
       caption = 'The black line represents the average number of new cases confirmed in the previous seven days.') +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  facet_wrap(~Name, ncol = 3) +
  theme(legend.position = 'none')

png(paste0(output_directory_x, '/Covid_19_confirmed_cases_wsx_ltlas.png'),
    width = 1580,
    height = 1050,
    res = 120)
print(total_cases_reported_plot_2)
dev.off()

total_cases_reported_plot_3 <-  ggplot(ltla_p12_test_df) +
  geom_bar(data = ltla_p12_test_df,
           aes(x = Date,
               y = New_cases,
               fill = Colour_key),
           width = 1,
           stat = 'identity',
           position = 'stack',
           colour = '#ffffff') + 
  geom_line(data = ltla_p12_test_df,
            aes(x = Date,
                y = Seven_day_average_new_cases,
                colour = Colour_key),
            group = 1) +
  scale_fill_manual(values = c('#aaaaaa','#721606', '#005bd6', '#1fbfbb', '#c2f792'),
                    name = 'Change in average new cases',
                    drop = FALSE) +
  scale_colour_manual(values = c('#aaaaaa','#721606', '#005bd6', '#1fbfbb', '#c2f792'),
                    name = 'Change in average new cases',
                    drop = FALSE) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date(max(ltla_p12_test_df$Date) -(52*7), max(ltla_p12_test_df$Date), by = 7),
               limits = c(min(ltla_p12_test_df$Date), max(ltla_p12_test_df$Date)),
               expand = c(0.01,0.01)) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     breaks = seq(0,max_daily_case_limit, max_daily_case_break),
                     limits = c(0,max_daily_case_limit)) +
  geom_segment(x = complete_date + 1,
               y = 0,
               xend = complete_date + 1,
               yend = Inf,
               color = "red",
               size = .15,
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
           label = 'Last 5 days\nnot considered\ncomplete:',
           size = 2.25,
           hjust = 1) +
  labs(x = 'Date',
       y = 'Number of daily confirmed cases',
       title = paste0('Daily number of confirmed Covid-19 cases; Pillar 1 and 2 combined; West Sussex lower tier Local Authorities'),
       subtitle = paste0('Confirmed cases by specimen date; 01 March - ', format(max(ltla_p12_test_df$Date), '%d %B %Y')),
       caption = paste0('The line represents the average number of new cases confirmed in the previous seven days.\nA change in cases is identified by comparing the most recent complete 7 day period (average number of new cases between ', format(complete_date - 6, '%d %B') , ' and ', format(complete_date, '%d %B'), ') with the previous 7 day period (', format(complete_date - 13, '%d %B') , '-', format(complete_date - 7, '%d %B'),').')) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        legend.position = c(.7,.125)) +
  facet_wrap(~Name, ncol = 3) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

png(paste0(output_directory_x, '/Covid_19_confirmed_cases_wsx_ltlas_colour_coded.png'),
    width = 1580,
    height = 1050,
    res = 120)
print(total_cases_reported_plot_3)
dev.off()

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
  labs(title = paste0('Summary of new confirmed Covid-19 cases per 100,000 population (all ages); ', format(min(hm_df$Date), '%d %B'), ' to ',format(max(hm_df$Date), '%d %B')),
       x = NULL,
       y = NULL,
       caption = 'Cases for dates after the red dashed line are not considered complete due to a lag in test result reporting.') +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date(max(hm_df$Date) -(52*7), max(hm_df$Date), by = 7),
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

png(paste0(output_directory_x, '/WSx_Covid_19_confirmed_heatmap_rate.png'),
    width = 1480,
    height = 650,
    res = 200)
print(new_case_rate_plot)
dev.off()

# Calendar view ####

# This is the calendar theme I have created
ph_cal_theme = function(){
  theme( 
    legend.position = "bottom", 
    legend.title = element_text(colour = "#000000", size = 10), 
    legend.key.size = unit(0.5, "lines"), 
    legend.background = element_rect(fill = "#ffffff"), 
    legend.key = element_rect(fill = "#ffffff", colour = "#E2E2E3"), 
    legend.text = element_text(colour = "#000000", size = 10), 
    plot.background = element_rect(fill = "white", colour = "#E2E2E3"), 
    panel.background = element_rect(fill = "white"), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour = "#000000", size = 9), 
    plot.title = element_text(colour = "#000000", face = "bold", size = 12, vjust = 1), 
    axis.title = element_text(colour = "#327d9c", face = "bold", size = 10),     
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), 
    strip.text = element_text(colour = "#000000", face = "bold"),
    strip.background = element_rect(fill = "#ffffff"), 
    axis.ticks = element_blank() 
  )}

for(i in 1:length(areas_to_loop)){
  
  area_x <- areas_to_loop[i]
  
  cal_df_raw <- hm_df %>% 
    filter(Name == area_x) %>% 
    select(Name, Date, New_cases_per_100000, new_case_per_100000_key)
  
  # I need to find a way of back filling the dates if the data starts part way through a month.
  # Later we will be getting the days to be plotted on a grid of 42 days (six times seven day weeks) and so the data needs to begin at the beginning of the month to be plotted correctly).
  
  # In this case I think the site went live on the first but had not hits so I'm going to add it in.
  # Add the 1/11/2013 to the dataset (it won't be created otherwise)
  cases_date_add <- data.frame(Date = seq.Date(as.Date('2020-01-01'), min(cal_df_raw$Date) - 1, by = '1 day'), Name = area_x, New_cases = 0, new_case_key = 'No new cases', new_case_per_100000_key = 'No new cases')
  
  cal_df <- cases_date_add %>% 
    bind_rows(cal_df_raw) %>% 
    mutate(Year = format(Date, "%Y"),
           Month_n = format(Date, "%m"),
           Month_name = format(Date, "%b"),
           Weekday = format(Date, "%w"),
           Weekday = ifelse(Weekday == 0, 7, Weekday),
           Weekday_name = format(Date, "%a"),
           Month_year = format(Date, "%m-%Y"))
  
  months_in_between <- data.frame(Date = seq.Date(from = min(cal_df$Date), to = max(cal_df$Date), by = "months"))
  
  # This asks if the first day of the month for the first observation is '01'. If it is then it will skip over the next three lines, if it is not then it will create a new field that concatenates the month-year of the date with 01 at the start, and then overwrites the date field with the dates starting on the 1st of the month. The third line removes the created field.
  
  if (!(format(months_in_between$Date, "%d")[1] == "01")){
    months_in_between$Date_1 <- paste("01", format(months_in_between$Date, "%m-%Y"), sep = "-")
    months_in_between$Date <-as.Date(months_in_between$Date_1, format="%d-%m-%Y")
    months_in_between$Date_1 <- NULL
  }  
  
  # For this data, the week ends on a friday (so friday is the cut off date for each week of data). It might be helpful for us to say as at this date this is the number of deaths. To do this we need to convert each week number into a 'friday date'.
  
  # Day of the week (name and then number)
  months_in_between <- months_in_between %>% 
    mutate(dw = format(Date, "%A"),
           dw_n = format(Date, "%w")) %>% 
    mutate(dw_n = ifelse(dw_n == 0, 7, dw_n),
           Month_year = format(Date, "%m-%Y"))
  
  # To make the calendar plot we are going to need to create a grid of 42 tiles (representing seven days in each week for six weeks, as any outlook calendar shows). From this we can start the data somewhere between tile one and tile seven depending on the day of the week the month starts (e.g. if the month starts on a wednesday, then we want the data to start on tile three).
  
  # Make an empty dataframe with each month of the 'months_in_between' dataframe repeated 42 times
  df_1 <- data.frame(Month_year = rep(months_in_between$Month_year, 42)) %>% 
    group_by(Month_year) %>% 
    mutate(id = row_number())
  
  # Add the information we created about the day each month starts
  cal_df <- cal_df %>% 
    left_join(months_in_between[c("Month_year", "dw_n")], by = "Month_year") %>% 
    mutate(dw_n = as.numeric(dw_n)) %>% 
    group_by(Month_year) %>% 
    mutate(id = row_number()) %>% 
    mutate(id = id + dw_n - 1) %>% # If we add this id number to the dw (day of the week that the month starts) number, the id number becomes the position in our grid of 42 that the date should be. As we can only start a sequence from 1 onwards, and not zero, we need to subtract one from the total otherwise the position is offset too far.
    mutate(Week = ifelse(id <= 7, 1, ifelse(id >= 8 & id <= 14, 2, ifelse(id >= 15 & id <= 21, 3, ifelse(id >= 22 & id <= 28, 4, ifelse(id >= 29 & id <= 35, 5, 6)))))) %>%  #  We can now overwrite the Week field in our dataframe to show what it should be given the grid of 42 days
    left_join(df_1, by = c("Month_year", "id")) %>% 
    mutate(Year = substr(Month_year, 4, 7), # We can now rebuild the artificially created grid with the year, month, and weekday information. Take the four to seventh characters from the Month_year field to create the year
           Month_n = substr(Month_year, 1, 2)) %>% # take the first and second characters from the Month_year field to create the month number
    mutate(Weekday_name = ifelse(id %in% c(1,8,15,22,29,36) & is.na(Date), "Mon", ifelse(id %in% c(2,9,16,23,30,37) & is.na(Date), "Tue", ifelse(id %in% c(3,10,17,24,31,38) & is.na(Date), "Wed", ifelse(id %in% c(4,11,18,25,32,39) & is.na(Date), "Thu", ifelse(id %in% c(5,12,19,26,33,40) & is.na(Date), "Fri", ifelse(id %in% c(6,13,20,27,34,41) & is.na(Date), "Sat", ifelse(id %in% c(7,14,21,28,35,42) & is.na(Date), "Sun", Weekday_name )))))))) %>% # look through the dataframe and where the date is missing (indicating a non-date filler value within our 42 grid) and the id value is 1,8,15,22,29, or 36 (i.e. the monday value in our 42 grid for the month) then add a "Mon" for the day of the week and so on.
    mutate(Weekday = ifelse(id %in% c(1,8,15,22,29,36) & is.na(Date), 1,ifelse(id %in% c(2,9,16,23,30,37) & is.na(Date), 2, ifelse(id %in% c(3,10,17,24,31,38) & is.na(Date), 3, ifelse(id %in% c(4,11,18,25,32,39) & is.na(Date), 4, ifelse(id %in% c(5,12,19,26,33,40) & is.na(Date), 5, ifelse(id %in% c(6,13,20,27,34,41) & is.na(Date), 6, ifelse(id %in% c(7,14,21,28,35,42) & is.na(Date), 7, Weekday )))))))) %>% 
    mutate(Week = factor(ifelse(id >= 1 & id  <= 7, 1,  ifelse(id >= 8 & id <= 14, 2,  ifelse(id >= 15 & id <= 21, 3,  ifelse(id >= 22 & id <= 28, 4,  ifelse(id >= 29 & id <= 35, 5,  ifelse(id >= 36 & id <= 42, 6, NA)))))), levels = c(6,5,4,3,2,1))) %>% # Add a calendar week value for faceting (1-6 from our 42 tile grid). This is not the same as the week of the month and save the levels of this field so R knows how to plot them.
    mutate(Month_name = factor(ifelse(Month_n == "01", "Jan",ifelse(Month_n == "02", "Feb", ifelse(Month_n == "03", "Mar",ifelse(Month_n == "04", "Apr",ifelse(Month_n == "05", "May",ifelse(Month_n == "06", "Jun",ifelse(Month_n == "07", "Jul",ifelse(Month_n == "08", "Aug",ifelse(Month_n == "09", "Sep",ifelse(Month_n == "10", "Oct",ifelse(Month_n == "11", "Nov", ifelse(Month_n == "12", "Dec", NA)))))))))))), levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))) %>% # Fill in the blanks of month name using the value in Month_n
    mutate(Weekday_name = factor(ifelse(Weekday_name == "Mon", "Monday", ifelse(Weekday_name == "Tue", "Tuesday", ifelse(Weekday_name == "Wed", "Wednesday", ifelse(Weekday_name == "Thu", "Thursday", ifelse(Weekday_name == "Fri", "Friday", ifelse(Weekday_name == "Sat", "Saturday", ifelse(Weekday_name == "Sun", "Sunday", Weekday_name))))))), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
    arrange(Month_n, id) %>% 
    mutate(new_case_per_100000_key_v2 = factor(ifelse(is.na(new_case_per_100000_key), 'Not date', new_case_per_100000_key), levels =  c('No new cases', 'Less than 1 case per 100,000', '1-2 new cases per 100,000', '3-4 new cases per 100,000', '5-6 new cases per 100,000', '7-8 new cases per 100,000', '9-10 new cases per 100,000', 'More than 10 new cases per 100,000', 'Not date')))
  
cal_df_comp<- cal_df %>% 
  filter(Date == complete_date)
  

calendar_cases_plot <- ggplot(cal_df, 
                                aes(x = Weekday_name, 
                                    y = Week, 
                                    fill = new_case_per_100000_key_v2)) + 
    geom_tile(colour = "#ffffff") + 
    facet_grid(Year ~ Month_name) +
    scale_x_discrete(expand = c(0,0.1)) +
    scale_fill_manual(values = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026', '#bbc2c8'),
                      breaks = c('No new cases', 'Less than 1 case per 100,000', '1-2 new cases per 100,000', '3-4 new cases per 100,000', '5-6 new cases per 100,000', '7-8 new cases per 100,000', '9-10 new cases per 100,000', 'More than 10 new cases per 100,000'),
                      name = 'Tile\ncolour\nkey',
                      drop = FALSE) +
    labs(title =  paste0('Summary of new confirmed Covid-19 cases per 100,000 population (all ages); ', area_x, '; ', format(min(cal_df$Date, na.rm = TRUE), '%d %B'), ' to ',format(max(cal_df$Date, na.rm = TRUE), '%d %B')),
         x = NULL, 
         y = NULL,
         caption = 'Cases for dates after the red dashed line are not considered complete due to a lag in test result reporting.') +
    geom_segment(data = cal_df_comp,
                 aes(
                   x = Weekday_name,
                   xend = Weekday_name,
                   y = as.numeric(Week) - .5,
                   yend = as.numeric(Week) + .5),
                 color = "red",
                 linetype = "dashed") +
    ph_cal_theme() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE))
  
  png(paste0(output_directory_x, '/Covid_19_calendar_cases_rate', gsub(' ', '_', area_x), '.png'),
      width = 1280,
      height = 550,
      res = 120)
  print(calendar_cases_plot)
  dev.off()
  
}

# Cumulative rate map utla ####

utla_rate <- p12_test_df %>% 
  ungroup() %>% 
  filter(Date == max(Date)) %>%
  # filter(Date == '2020-07-12') %>% 
  filter(Type %in% c('Upper Tier Local Authority', 'Unitary Authority')) %>% 
  select(Code, Name, Date, Cumulative_cases, Cumulative_per_100000, Colour_key) %>% 
  mutate(Cumulate_rate_rank = rank(-Cumulative_per_100000)) %>% 
  mutate(Rate_decile = abs(ntile(Cumulative_per_100000, 10) - 11)) %>% 
  mutate(Rate_decile = factor(ifelse(Rate_decile == 1, '10% of authorities\nwith highest rate', ifelse(Rate_decile == 10, '10% of authorities\nwith lowest rate', paste0('Decile ', Rate_decile))), levels = c('10% of authorities\nwith highest rate','Decile 2','Decile 3','Decile 4','Decile 5','Decile 6','Decile 7','Decile 8','Decile 9','10% of authorities\nwith lowest rate')))

# library(classInt)
# classIntervals(utla_rate$Cumulative_per_100000, n = 10, style = "quantile")

utla_ua_boundaries <- geojson_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson",  what = "sp") %>% 
  fortify(region = "ctyua19cd") %>% 
  rename(ctyua19cd = id) %>% 
  filter(substr(ctyua19cd, 1,1 ) == 'E') %>% 
  left_join(utla_rate, by = c('ctyua19cd' = 'Code')) %>% 
  filter(!is.na(Cumulative_per_100000))

# utla_map_cumulative_rate

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
    plot.title = element_text(colour = "#000000", face = "bold", size = 10), 
    plot.subtitle = element_text(colour = "#000000", size = 9), 
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
                   fill = Rate_decile),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_manual(values = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'),
                    name = 'Decile of cumulative\nrate per 100k') +
  labs(title = paste0('Cumulative rate of confirmed Covid-19 cases per 100,000 population (all ages); Pillar 1 and 2 combined;\nUpper Tier Local and Unitary Authorities'),
       subtitle = paste0('Confirmed cases by specimen date; 01 March - ', format(max(ltla_p12_test_df$Date), '%d %B %Y')))  +
  theme(legend.position = c(.1,.55))

inset_1 <- ggplot() +
coord_fixed(1.5) +
               map_theme() +
               geom_polygon(data = subset(utla_ua_boundaries, Name %in% c(c("Barking and Dagenham", "Barnet", "Bexley","Brent", "Bromley", "Camden", "City of London", "Croydon","Ealing", "Enfield", "Greenwich", "Hackney","Hammersmith and Fulham", "Haringey", "Harrow","Havering", "Hillingdon", "Hounslow", "Islington","Kensington and Chelsea", "Kingston upon Thames", "Lambeth","Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames","Southwark", "Sutton", "Tower Hamlets", "Waltham Forest","Wandsworth", "Westminster"))),
                            aes(x=long,
                                y=lat,
                                group = group,
                                fill = Rate_decile),
                            color="#ffffff",
                            size = .1,
                            alpha = 1,
                            show.legend = FALSE) +
               scale_fill_manual(values = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'),
                                 name = 'Decile of cumulative\nrate per 100k') +
  labs(title = 'London') +
  theme(plot.background  = element_rect(colour = "black", fill=NA, size=.1),
        plot.title = element_text(size = 8))

png(paste0(output_directory_x, '/Covid_19_cumulative_rate_utla_latest.png'),
    width = 1480,
    height = 1480,
    res = 180)
print(map_1)
print(inset_1, vp = viewport(0.2, 0.8, width = 0.22, height = 0.22))
dev.off()

map_2 <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = utla_ua_boundaries,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = Colour_key),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_manual(values = c('#aaaaaa','#721606', '#005bd6', '#1fbfbb', '#c2f792'),
                    name = 'Change in average new cases',
                    drop = FALSE) +
  labs(title = paste0('Recent changes in number of confirmed Covid-19 cases; Pillar 1 and 2 combined;\nUpper Tier Local and Unitary Authorities'),
       subtitle = paste0('Confirmed cases by specimen date; 01 March - ', format(max(ltla_p12_test_df$Date), '%d %B %Y')),
       caption = paste0('A change in cases is identified by comparing the cases in the  most recent complete 7 day\nperiod (average number of new cases between ', format(complete_date - 6, '%d %B') , ' and ', format(complete_date, '%d %B'), ') with the previous 7 day period (', format(complete_date - 13, '%d %B') , '-', format(complete_date - 7, '%d %B'),').'))  +
  theme(legend.position = c(.1,.55))


inset_2 <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = subset(utla_ua_boundaries, Name %in% c(c("Barking and Dagenham", "Barnet", "Bexley","Brent", "Bromley", "Camden", "City of London", "Croydon","Ealing", "Enfield", "Greenwich", "Hackney","Hammersmith and Fulham", "Haringey", "Harrow","Havering", "Hillingdon", "Hounslow", "Islington","Kensington and Chelsea", "Kingston upon Thames", "Lambeth","Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames","Southwark", "Sutton", "Tower Hamlets", "Waltham Forest","Wandsworth", "Westminster"))),
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = Colour_key),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = FALSE) +
  scale_fill_manual(values = c('#aaaaaa','#721606', '#005bd6', '#1fbfbb', '#c2f792'),
                    name = 'Change in average new cases',
                    drop = FALSE) +
  labs(title = 'London') +
  theme(plot.background  = element_rect(colour = "black", fill=NA, size=.1),
        plot.title = element_text(size = 8))

png(paste0(output_directory_x, '/Covid_19_case_change_utla_latest.png'),
    width = 1480,
    height = 1480,
    res = 180)
print(map_2)
print(inset_2, vp = viewport(0.2, 0.8, width = 0.22, height = 0.22))
dev.off()

# utla_json_simplified <- ms_simplify(geojson_json(utla_ua_boundaries), keep = 0.2)
# geojson_write(utla_json_simplified, file = paste0(github_repo_dir, '/utla_covid_cumulative_rate_latest.geojson'))

# NHS Pathways ####

# Mortality ####

deaths_labels <- read_csv(paste0(github_repo_dir, '/All_settings_deaths_occurrences.csv')) %>% 
  arrange(Week_number) %>% 
  select(Week_ending) %>% 
  unique() %>% 
  mutate(deaths_label = paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')))

weekly_all_place_all_deaths <- read_csv(paste0(github_repo_dir, '/All_settings_deaths_occurrences.csv')) %>% 
  filter(Name %in% areas_to_loop) %>% 
  filter(Cause == 'All causes') %>% 
  arrange(Week_number) %>% 
  select(Name, Week_ending, Deaths) %>% 
  mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label)) %>% 
  rename(All_deaths = Deaths)

weekly_all_place_deaths <- read_csv(paste0(github_repo_dir, '/All_settings_deaths_occurrences.csv')) %>% 
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

area_x_cov_non_cov <- weekly_all_place_deaths %>% 
  filter(Name == area_x)

max_deaths_limit <- ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 50, round_any(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE), 5, ceiling), ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 100, round_any(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE), 10, ceiling), ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 250, round_any(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE), 25, ceiling), ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 500, round_any(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE), 50, ceiling), round_any(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE), 100, ceiling)))))

max_deaths_break <- ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 20, 2, ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 50, 5, ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 100, 10, ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 250, 25, ifelse(max(area_x_cov_non_cov$All_deaths, na.rm = TRUE) < 500, 50, 100)))))


area_x_wk_cause_deaths_plot <- ggplot(area_x_cov_non_cov,
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
  labs(title = paste0('Weekly deaths; ', area_x,'; w/e 3rd Jan 2020 - ', deaths_labels$deaths_label[length(deaths_labels$deaths_label)]),
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
  guides(colour = FALSE)

png(paste0(output_directory_x, '/Covid_19_deaths_', gsub(' ', '_', area_x), '.png'),
    width = 1580,
    height = 750,
    res = 200)
print(area_x_wk_cause_deaths_plot)
dev.off()

care_home_ons_all_deaths <- read_csv(paste0(github_repo_dir, '/Care_home_death_occurrences_ONS_weekly.csv')) %>% 
  filter(Name %in% areas_to_loop) %>% 
  filter(Cause == 'All causes') %>% 
  arrange(Week_number) %>% 
  select(Name, Week_ending, Deaths) %>% 
  mutate(Week_ending = factor(paste0('w/e ', ordinal(as.numeric(format(Week_ending, '%d'))), format(Week_ending, ' %b')), levels = deaths_labels$deaths_label)) %>% 
  rename(All_deaths = Deaths)

carehome_weekly_deaths <- read_csv(paste0(github_repo_dir, '/Care_home_death_occurrences_ONS_weekly.csv')) %>% 
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
  left_join(care_home_ons_all_deaths, by = c('Name', 'Week_ending'))

area_x_cov_non_cov_carehome <- carehome_weekly_deaths %>% 
  filter(Name == area_x)

area_x_wk_cause_deaths_plot_2 <- ggplot(area_x_cov_non_cov_carehome,
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
  labs(title = paste0('Weekly deaths in care homes; ', area_x,'; w/e 3rd Jan 2020 - ', deaths_labels$deaths_label[length(deaths_labels$deaths_label)]),
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
  guides(colour = FALSE)

png(paste0(output_directory_x, '/Covid_19_deaths_carehomes_', gsub(' ', '_', area_x), '.png'),
    width = 1580,
    height = 750,
    res = 200)
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

png(paste0(output_directory_x, '/Covid_19_deaths_wsx_ltlas.png'),
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