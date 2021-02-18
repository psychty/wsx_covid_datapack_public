
# Hospital admissions ####

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

#github_repo_dir <- "~/Documents/GitHub/wsx_covid_datapack_public"
github_repo_dir <- '~/GitHub/wsx_covid_datapack_public'
output_directory_x <- paste0(github_repo_dir, '/Outputs')

if(!file.exists(paste0(github_repo_dir, '/Source_files/etr.csv'))){
  download.file('https://files.digital.nhs.uk/assets/ods/current/etr.zip', paste0(github_repo_dir, '/etr.zip'), mode = 'wb')
  unzip(paste0(github_repo_dir, '/etr.zip'), exdir = github_repo_dir)
  file.remove(paste0(github_repo_dir, '/etr.zip'), paste0(github_repo_dir, '/etr.pdf'))
}

calls_hosp_webpage <- read_html('https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/') %>%
  html_nodes("a") %>%
  html_attr("href")

download.file(grep('Weekly-covid', calls_hosp_webpage, value = T), paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'), mode = 'wb')

trust_admission_date <- read_excel( paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
                                    sheet = 'All beds COVID',
                                    skip = 2, 
                                    col_names = FALSE, 
                                    n_max = 5) %>% 
  rename(Item = ...1,
         Description = ...2) %>%
  filter(Item == 'Published:') %>% 
  mutate(Description  = as.Date(as.numeric(Description), origin = "1899-12-30"))

admissions_df_trust <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsTrust&metric=newAdmissions&metric=hospitalCases&metric=covidOccupiedMVBeds&format=csv')

admissions_df_region <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&metric=newAdmissions&metric=hospitalCases&metric=covidOccupiedMVBeds&format=csv')

admissions_df_nation <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=newAdmissions&metric=hospitalCases&metric=covidOccupiedMVBeds&format=csv')

# We need to use data.gov to get cumulative admissions because the NHS digital data only starts on August 1st
admissions_df <- admissions_df_trust %>%
  bind_rows(admissions_df_region) %>% 
  bind_rows(admissions_df_nation) %>% 
  rename(Name = areaName,
         Admissions_or_new_cases_in_last_24hrs = newAdmissions,
         Patients_occupying_beds = hospitalCases,
         Patients_occupying_mv_beds = covidOccupiedMVBeds,
         Date = date) %>% 
  mutate(Admissions_or_new_cases_in_last_24hrs = replace_na(Admissions_or_new_cases_in_last_24hrs, 0),
         Patients_occupying_beds = replace_na(Patients_occupying_beds, 0),
         Patients_occupying_mv_beds = replace_na(Patients_occupying_mv_beds, 0)) %>% 
  mutate(Patients_occupying_non_mv_beds = Patients_occupying_beds - Patients_occupying_mv_beds) %>% 
  select(Name, Date, Admissions_or_new_cases_in_last_24hrs, Patients_occupying_beds, Patients_occupying_mv_beds, Patients_occupying_non_mv_beds) %>% 
  group_by(Name) %>%
  arrange(Name, Date) %>%
  mutate(Rolling_7_day_admissions = rollapply(Admissions_or_new_cases_in_last_24hrs, 7, sum, align = 'right', fill = NA),
         Rolling_average_7_day_admissions = rollapply(Admissions_or_new_cases_in_last_24hrs, 7, mean, align = 'right', fill = NA),
         Total_admissions = cumsum(Admissions_or_new_cases_in_last_24hrs)) %>%
  filter(Name %in%  c('Brighton and Sussex University Hospitals NHS Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Sussex Partnership NHS Foundation Trust', 'Western Sussex Hospitals NHS Foundation Trust', 'South East', 'England')) %>% 
  mutate(Name = factor(Name, levels = c('Brighton and Sussex University Hospitals NHS Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Sussex Partnership NHS Foundation Trust','Western Sussex Hospitals NHS Foundation Trust', 'South East', 'England'))) %>%
  arrange(Name) %>%
  ungroup() %>% 
  add_row(Name = 'Sussex Community NHS Foundation Trust', Date = as.Date('2020-03-19'), Admissions_or_new_cases_in_last_24hrs = NA, Patients_occupying_beds = NA, Patients_occupying_mv_beds = NA, Patients_occupying_non_mv_beds = NA, Rolling_7_day_admissions = NA, Rolling_average_7_day_admissions = NA, Total_admissions = NA) %>% 
  add_row(Name = 'Sussex Community NHS Foundation Trust', Date = as.Date('2020-03-20'), Admissions_or_new_cases_in_last_24hrs = NA, Patients_occupying_beds = NA, Patients_occupying_mv_beds = NA, Patients_occupying_non_mv_beds = NA, Rolling_7_day_admissions = NA, Rolling_average_7_day_admissions = NA, Total_admissions = NA) 

# patients occupying beds as at 8am
admissions_date <- admissions_df_trust %>% 
  filter(!is.na(newAdmissions)) %>% 
  filter(date == max(date)) %>% 
  select(date) %>% 
  unique() %>% 
  mutate(item = 'Admissions')

occupied_date <- admissions_df_trust %>% 
  filter(!is.na(hospitalCases)) %>% 
  filter(date == max(date)) %>% 
  select(date) %>% 
  unique() %>% 
  mutate(item = 'Patients in hospital')

publish_date <- data.frame(date = trust_admission_date$Description,
                           item = 'Publish date')

admissions_date %>% 
  bind_rows(occupied_date) %>% 
  bind_rows(publish_date) %>% 
  mutate(Date_label = format(date, '%A %d %B %Y')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/hospital_meta.json'))

data.frame(Date = seq.Date(admissions_date$date -(52*7), admissions_date$date, by = 7)) %>% 
  filter(Date >= min(admissions_df$Date)) %>% 
  mutate(Date_label = format(Date, '%d %b %y')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/admission_date_labels.json'))

hospital_table_a <- admissions_df %>% 
  filter(Date == admissions_date$date) %>%
  select(Name, Admissions_or_new_cases_in_last_24hrs, Rolling_7_day_admissions, Total_admissions)

hospital_table_b <- admissions_df %>% 
  filter(Date == occupied_date$date) %>%
  select(Name, Patients_occupying_beds, Patients_occupying_mv_beds)

hospital_table <- hospital_table_a %>% 
  left_join(hospital_table_b, by = 'Name')

hospital_table %>% 
  select(!Admissions_or_new_cases_in_last_24hrs) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/latest_hospital_summary.json'))

admissions_export_df <- admissions_df %>% 
  filter(Date <= admissions_date$date) %>% 
  select(Name, Date, Admissions_or_new_cases_in_last_24hrs, Rolling_average_7_day_admissions, Rolling_7_day_admissions, Total_admissions) %>% 
  group_by(Name) %>% 
  arrange(Name, Date) %>% 
  mutate(Previous_Rolling_7_day_admissions = lag(Rolling_7_day_admissions, 7)) %>% 
  mutate(Perc_change_rolling_7_day_admissions = (Rolling_7_day_admissions - lag(Rolling_7_day_admissions, 7))/ lag(Admissions_or_new_cases_in_last_24hrs, 7)) %>% 
  mutate(Perc_change_rolling_7_day_admissions = ifelse(Perc_change_rolling_7_day_admissions == Inf, 1,Perc_change_rolling_7_day_admissions)) %>% 
  mutate(Perc_change_rolling_7_day_admissions = replace_na(Perc_change_rolling_7_day_admissions, 0)) %>% 
  mutate(Perc_change_rolling_7_day_admissions = ifelse(Perc_change_rolling_7_day_admissions <0, 'Down', ifelse(Perc_change_rolling_7_day_admissions == 0, 'Same', ifelse(Perc_change_rolling_7_day_admissions > 0, 'Up', NA)))) %>% 
  mutate(Date_label = format(Date, '%d %b %y')) %>% 
  select(!Date)

admissions_export_df %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/admissions_export_df.json'))

admissions_export_df %>% view()

beds_occupied_export_df <- admissions_df %>% 
  select(Name, Date, Patients_occupying_beds, Patients_occupying_mv_beds, Patients_occupying_non_mv_beds) %>% 
  group_by(Name) %>% 
  arrange(Name, Date) %>% 
  mutate(Previous_occupying_beds = lag(Patients_occupying_beds, 7)) %>% 
  mutate(Perc_change_on_beds_occupied = (Patients_occupying_beds - lag(Patients_occupying_beds, 7))/ lag(Patients_occupying_beds, 7)) %>% 
  mutate(Perc_change_on_beds_occupied = ifelse(Perc_change_on_beds_occupied == Inf, 1, Perc_change_on_beds_occupied)) %>% 
  mutate(Perc_change_on_beds_occupied = replace_na(Perc_change_on_beds_occupied, 0)) %>% 
  mutate(Change_direction = ifelse(Perc_change_on_beds_occupied <0, 'decreased', ifelse(Perc_change_on_beds_occupied == 0, 'stayed the same', ifelse(Perc_change_on_beds_occupied > 0, 'increased', NA)))) %>% 
  mutate(Date_label = format(Date, '%d %b %y')) %>% 
  filter(Date <= occupied_date$date) %>% 
  select(!c(Date))

data.frame(Date = seq.Date(occupied_date$date -(52*7), occupied_date$date, by = 7)) %>% 
  filter(Date >= min(admissions_df$Date)) %>% 
  mutate(Date_label = format(Date, '%d %b %y')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/occupied_date_labels.json'))

beds_occupied_export_df %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/beds_occupied_export_df.json'))

# trust_admissions_1 <- read_excel( paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
#                                   sheet = 'Hosp ads & diag',
#                                   skip = 13) %>% 
#   filter(!is.na(Name)) %>% 
#   mutate(Name = capwords(Name, strict = TRUE)) %>% 
#   mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
#   mutate(Name = gsub(' And ', ' and ', Name)) %>% 
#   mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
#   mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
#   pivot_longer(names_to = 'Date', values_to = 'Admissions_or_new_cases_in_last_24hrs', cols = 5:ncol(.)) %>% 
#   mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))  %>% 
#   filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
#   select(!c('Type 1 Acute?', 'NHS England Region', 'Code')) %>% 
#   mutate(Source_of_admission = 'All')
#
# trust_admissions_1_ch <- read_excel(paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
#                                      sheet = 'Care home ads and diags',
#                                      skip = 13) %>% 
#   filter(!is.na(Name)) %>% 
#   mutate(Name = capwords(Name, strict = TRUE)) %>% 
#   mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
#   mutate(Name = gsub(' And ', ' and ', Name)) %>% 
#   mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
#   mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
#   pivot_longer(names_to = 'Date', values_to = 'Admissions_or_new_cases_in_last_24hrs', cols = 5:ncol(.)) %>% 
#   mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))  %>% 
#   filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
#   select(!c('Type 1 Acute?', 'NHS England Region', 'Code')) %>% 
#   mutate(Source_of_admission = 'Care home')
#
# admissions_source_df <- trust_admissions_1 %>% 
#   bind_rows(trust_admissions_1_ch) %>% 
#   pivot_wider(names_from = Source_of_admission, values_from = Admissions_or_new_cases_in_last_24hrs) %>% 
#   mutate(All = replace_na(All, 0),
#          `Care home` = replace_na(`Care home`, 0)) %>% 
#   mutate(`Not care home` = All - `Care home`) %>% 
#   select(!All) %>% 
#   pivot_longer(cols = c(`Not care home`, `Care home`), names_to = 'Source_of_admission',  values_to = 'Admissions_or_new_cases_in_last_24hrs') %>% 
#   mutate(Source_of_admission = factor(Source_of_admission, levels = c('Care home', 'Not care home'))) %>% 
#   group_by(Name, Source_of_admission) %>% 
#   arrange(Name, Source_of_admission, Date) %>% 
#   mutate(Rolling_7_day_admissions = rollapply(Admissions_or_new_cases_in_last_24hrs, 7, sum, align = 'right', fill = NA),
#          Total_admissions = cumsum(Admissions_or_new_cases_in_last_24hrs)) 

trust_x <- 'England'

admissions_x_df <- admissions_df %>% 
  filter(Name == trust_x)

ggplot(admissions_x_df,
       aes(x = Date,
           y = Admissions_or_new_cases_in_last_24hrs)) +
  geom_bar(stat = 'identity',
           width = .9,
           fill = '#901020') +
  geom_line(aes(x = Date,
                group = 1,
                y = Rolling_average_7_day_admissions),
            colour = '#000000',
            lwd = 1.2) +
  scale_x_date(date_labels = "%d %b %y",
               breaks = seq.Date(admissions_date$date -(52*7), admissions_date$date, by = 7),
               expand = c(0,0.01)) +
  scale_y_continuous(expand = c(0,0.01)) +
  labs(x = 'Date',
       y = 'Number of admissions',
       title = paste0('Number of new admissions with a positive COVID-19 test result; ',trust_x),
       subtitle = paste0('New admissions or inpatients with a new diagnosis; data up to ', format(admissions_date$date, '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y'))) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

ggplot(admissions_x_df,
       aes(x = Date,
           y = Rolling_7_day_admissions)) +
  geom_line(group = 1,
            colour = '#901020') +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date(admissions_date$date -(52*7), admissions_date$date, by = 7),
               expand = c(0.01,0.01)) +
  labs(x = 'Date',
       y = 'Rolling 7 day admissions',
       title = paste0('Rolling 7 day number of new admissions with a positive COVID-19 test result; ',trust_x),
       subtitle = paste0('New admissions or inpatients with a new diagnosis; data up to ', format(admissions_date$date, '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y'))) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

patients_in_beds_df <- admissions_df %>% 
  select(Name, Date, Patients_occupying_mv_beds, Patients_occupying_non_mv_beds) %>% 
  filter(Date <= occupied_date$date) %>% 
  pivot_longer(cols = c(Patients_occupying_non_mv_beds, Patients_occupying_mv_beds), names_to = 'Bed_type', values_to = 'Patients_occupying_beds') %>% 
  mutate(Bed_type = ifelse(Bed_type == 'Patients_occupying_mv_beds', 'Mechanical ventilation', ifelse(Bed_type == 'Patients_occupying_non_mv_beds', 'Not capable of mechanical ventilation', NA)))

patients_in_beds_df_x <- patients_in_beds_df %>% 
  filter(Name == trust_x)

ggplot(patients_in_beds_df_x,
       aes(x = Date,
           y = Patients_occupying_beds)) +
  geom_bar(stat = 'identity',
           colour = '#ffffff',
           aes(fill = Bed_type)) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date(admissions_date$date -(52*7), admissions_date$date, by = 7),
               expand = c(0.01,0.01)) +
  scale_fill_manual(values = c('#006900', '#669900'),
                    name = 'Bed type') +
  labs(x = 'Date',
       y = 'Number of inpatients',
       title = paste0('Number of patients in hospital with positive COVID-19 test result; ',trust_x),
       subtitle = paste0('Data up to ', format(admissions_date$date, '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y'))) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

# Share of inpatient beds occupied ####

trust_c19_patients_occupying_ga_beds <- read_excel( paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
                                                    sheet = 'Adult G&A Beds Occupied COVID',
                                                    skip = 13) %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
  mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
  pivot_longer(names_to = 'Date', values_to = 'c19_patients_occupying_ga_beds', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Sussex Partnership NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

trust_other_patients_occupying_ga_beds <- read_excel( paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
                                                      sheet = 'Adult G&A Bed Occupied NonCOVID',
                                                      skip = 13) %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
  mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
  pivot_longer(names_to = 'Date', values_to = 'other_patients_occupying_ga_beds', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust',  'Sussex Partnership NHS Foundation Trust','Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

trust_vacant_ga_beds <- read_excel( paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
                                    sheet = 'Adult G&A Beds Unoccupied',
                                    skip = 13) %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
  mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
  pivot_longer(names_to = 'Date', values_to = 'vacant_ga_beds', cols = 5:ncol(.)) %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>% 
  filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Sussex Partnership NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
  select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))

bed_used_df <- trust_c19_patients_occupying_ga_beds %>% 
  left_join(trust_other_patients_occupying_ga_beds, by = c('Name', 'Date')) %>% 
  left_join(trust_vacant_ga_beds, by = c('Name', 'Date')) %>%
  pivot_longer(cols = c(c19_patients_occupying_ga_beds, other_patients_occupying_ga_beds, vacant_ga_beds), names_to = 'Bed_status', values_to = 'Beds') %>% 
  group_by(Name, Date) %>% 
  mutate(Total_open_beds = sum(Beds),
         Proportion = Beds/sum(Beds)) %>% 
  mutate(Bed_status = factor(ifelse(Bed_status == 'c19_patients_occupying_ga_beds', 'COVID-19 + patients', ifelse(Bed_status == 'other_patients_occupying_ga_beds', 'Other patients', ifelse(Bed_status == 'vacant_ga_beds', 'Vacant (open) beds', NA))), levels = c('COVID-19 + patients', 'Other patients', 'Vacant (open) beds')))

# trust_admissions_metadata <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
#                                          sheet = 'All beds COVID',
#                                          skip = 2, 
#                                          col_names = FALSE, 
#                                          n_max = 5) %>% 
#   rename(Item = ...1,
#          Description = ...2) %>%
#   mutate(Description = ifelse(Item == 'Published:', as.character(format(as.Date(as.numeric(Description), origin = "1899-12-30"), '%d-%b-%Y')), Description))

beds_used_df_x <- bed_used_df %>% 
  filter(Name == trust_x)

ggplot(beds_used_df_x,
       aes(x = Date,
           y = Proportion)) +
  geom_bar(stat = 'identity',
           position = position_fill(reverse = TRUE),
           colour = '#ffffff',
           aes(fill = Bed_status)) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date(max(bed_used_df$Date) -(52*7), max(bed_used_df$Date), by = 7),
               expand = c(0.01,0.01)) +
  scale_fill_manual(values = c('#12263a', '#c96480', '#abcdef')) +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1, .1),
                     labels = percent_format(accuracy = 1)) +
  labs(x = 'Date',
       y = 'Proportion',
       title = paste0('Proportion of patients occupying adult beds; ', trust_x),
       subtitle = paste0('Share of all adult general and acute beds (%); data available from ',  format(min(bed_used_df$Date), '%d %B %Y'), ' up to ', format(max(bed_used_df$Date), '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y')),
       caption = 'This measure only includes adult inpatient beds. It is estimated that adult beds comprised more than 99% of inpatient beds in NHS hospital trusts.') +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

bed_used_df_export_1 <- bed_used_df %>%
  mutate(Bed_status = ifelse(Bed_status == 'COVID-19 + patients', 'covid_19_patients', ifelse(Bed_status == 'Other patients', 'other', ifelse(Bed_status == 'Vacant (open) beds', 'vacant', NA)))) %>% 
  select(!c(Proportion, Total_open_beds)) %>% 
  pivot_wider(names_from = 'Bed_status', values_from = 'Beds') %>% 
  mutate(Total_open_beds = covid_19_patients + other + vacant) %>% 
  mutate(Label = paste0('As at 8:00am on ', ordinal(as.numeric(format(Date, '%d'))), format(Date, ' %B %Y'), ' there were ', format(covid_19_patients, big.mark = ','), ' patients in adult General and Acute inpatient beds, this is ', round((covid_19_patients / Total_open_beds)*100, 1), '% of the total number of open beds on this day.')) 

bed_used_df_export <- bed_used_df %>%
  mutate(Bed_status = ifelse(Bed_status == 'COVID-19 + patients', 'covid_19_patients', ifelse(Bed_status == 'Other patients', 'other', ifelse(Bed_status == 'Vacant (open) beds', 'vacant', NA)))) %>% 
  select(!c(Beds, Total_open_beds)) %>% 
  pivot_wider(names_from = 'Bed_status', values_from = 'Proportion') %>% 
  left_join(bed_used_df_export_1[c('Name', 'Date', 'Label')], by = c('Name', 'Date')) %>% 
  mutate(Date_label = format(Date, '%d %b %y')) %>% 
  ungroup()

bed_used_df_export %>%
  select(!Date) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/patient_share_occupied_beds.json'))

data.frame(Date = seq.Date(max(bed_used_df_export$Date) -(52*7), max(bed_used_df_export$Date), by = 7)) %>% 
  filter(Date >= min(bed_used_df_export$Date)) %>% 
  mutate(Date_label = format(Date, '%d %b %y')) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/bed_share_ga_date_labels.json'))

# ggplot(beds_used_df_x,
#        aes(x = Date,
#            y = Beds)) +
#   geom_bar(stat = 'identity',
#            colour = '#ffffff',
#            aes(fill = Bed_status)) +
#   scale_x_date(date_labels = "%b %d",
#                breaks = seq.Date(max(bed_used_df$Date) -(52*7), max(bed_used_df$Date), by = 7),
#                expand = c(0.01,0.01)) +
#   scale_fill_manual(values = c('#12263a', '#c96480', '#abcdef')) +
#   labs(x = 'Date',
#        y = 'Number of beds',
#        title = paste0('Number of patients occupying adult beds in NHS hospital trusts'),
#        subtitle = paste0('Share of all adult general and acute beds (%); data up to ', format(max(bed_used_df$Date), '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y')),
#        caption = 'This measure only includes adult inpatient beds. It is estimated that adult beds comprised more than 99% of inpatient beds in NHS hospital trusts.') +
#   ph_theme() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))


# STP/ICS vaccine counts ####

# https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
