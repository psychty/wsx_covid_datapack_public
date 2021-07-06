# Public facing data pack - West Sussex and LTLAshttps://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek01to532020datawk232021.xlsx', paste0(github_repo_dir, '/Source files/ons_mortality_2020.xlsx'), mode = 'wb')

data_dummy_positivity_worked <- positivity_worked %>% 
  rename(dummy_name = Name)

positivity_worked_plotted <- ggplot(positivity_worked,
                                    aes(x = Date,
                                        y = Seven_day_PCR_positivity,
                                        colour = Name)) +
  geom_line(data = data_dummy_positivity_worked,
            aes(x = Date,
                y = Seven_day_PCR_positivity,
                group = dummy_name),
            colour = '#dbdbdb',
            size = .6) +
  geom_line(size = .9) +
  geom_point(size = .5) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = 'none') +
  scale_y_continuous(labels = label_comma(accuracy = 1, suffix = '%'),
                     limits = c(0,40),
                     breaks = seq(0, 40, 5)) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq.Date(complete_date -(52*7), complete_date, by = 7),
               limits = c(min(positivity_worked$Date), complete_date),
               expand = c(0.01,1)) +
  labs(x = '',
       y = '7-day rolling PCR case positivity rate',
       title = paste0('7-day PCR Case positivity rate for Covid-19 in the last 90 days; West Sussex, South East region, and England'),
       subtitle = paste0('Pillar 1 and 2 combined; data as at ', format(last_date, '%d %B %Y')))  +
  theme(axis.text.x = element_text(size = 8)) +
  facet_rep_wrap(. ~ Name, ncol = 4, repeat.tick.labels = TRUE)

png(paste0(output_directory_x, '/Figure_7_day_rolling_positivity_rates_latest_faceted.png'),
    width = 1480,
    height = 880,
    res = 130)
print(positivity_worked_plotted)
dev.off() 

# Hospital admissions ####
# 
# if(!file.exists(paste0(github_repo_dir, '/Source files/etr.csv'))){
#   download.file('https://files.digital.nhs.uk/assets/ods/current/etr.zip', paste0(github_repo_dir, '/Source files/etr.zip'), mode = 'wb')
#   unzip(paste0(github_repo_dir, '/Source files/etr.zip'), exdir = github_repo_dir)
#   file.remove(paste0(github_repo_dir, '/Source files/etr.zip'), paste0(github_repo_dir, '/Source files/etr.pdf'))
# }
# 
# calls_hosp_webpage <- read_html('https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/') %>%
#   html_nodes("a") %>%
#   html_attr("href")
# 
# download.file(grep('Weekly-covid', calls_hosp_webpage, value = T), paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'), mode = 'wb')
# 
# trust_admission_date <- read_excel( paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
#                                     sheet = 'All beds COVID',
#                                     skip = 2, 
#                                     col_names = FALSE, 
#                                     n_max = 5) %>% 
#   rename(Item = ...1,
#          Description = ...2) %>%
#   filter(Item == 'Published:') %>% 
#   mutate(Description  = as.Date(as.numeric(Description), origin = "1899-12-30"))
# 
# admissions_df_trust <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsTrust&metric=newAdmissions&metric=hospitalCases&metric=covidOccupiedMVBeds&format=csv')
# 
# admissions_df_region <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&metric=newAdmissions&metric=hospitalCases&metric=covidOccupiedMVBeds&format=csv')
# 
# admissions_df_nation <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=newAdmissions&metric=hospitalCases&metric=covidOccupiedMVBeds&format=csv')
# 
# # We need to use data.gov to get cumulative admissions because the NHS digital data only starts on August 1st
# admissions_df <- admissions_df_trust %>%
#   bind_rows(admissions_df_region) %>% 
#   bind_rows(admissions_df_nation) %>% 
#   rename(Name = areaName,
#          Admissions_or_new_cases_in_last_24hrs = newAdmissions,
#          Patients_occupying_beds = hospitalCases,
#          Patients_occupying_mv_beds = covidOccupiedMVBeds,
#          Date = date) %>% 
#   mutate(Admissions_or_new_cases_in_last_24hrs = replace_na(Admissions_or_new_cases_in_last_24hrs, 0),
#          Patients_occupying_beds = replace_na(Patients_occupying_beds, 0),
#          Patients_occupying_mv_beds = replace_na(Patients_occupying_mv_beds, 0)) %>% 
#   mutate(Patients_occupying_non_mv_beds = Patients_occupying_beds - Patients_occupying_mv_beds) %>% 
#   select(Name, Date, Admissions_or_new_cases_in_last_24hrs, Patients_occupying_beds, Patients_occupying_mv_beds, Patients_occupying_non_mv_beds) %>% 
#   group_by(Name) %>%
#   arrange(Name, Date) %>%
#   mutate(Rolling_7_day_admissions = rollapply(Admissions_or_new_cases_in_last_24hrs, 7, sum, align = 'right', fill = NA),
#          Rolling_average_7_day_admissions = rollapply(Admissions_or_new_cases_in_last_24hrs, 7, mean, align = 'right', fill = NA),
#          Total_admissions = cumsum(Admissions_or_new_cases_in_last_24hrs)) %>%
#   filter(Name %in%  c('Brighton and Sussex University Hospitals NHS Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Sussex Partnership NHS Foundation Trust', 'Western Sussex Hospitals NHS Foundation Trust', 'South East', 'England')) %>% 
#   mutate(Name = factor(Name, levels = c('Brighton and Sussex University Hospitals NHS Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Sussex Partnership NHS Foundation Trust','Western Sussex Hospitals NHS Foundation Trust', 'South East', 'England'))) %>%
#   arrange(Name) %>%
#   ungroup() %>% 
#   add_row(Name = 'Sussex Community NHS Foundation Trust', Date = as.Date('2020-03-19'), Admissions_or_new_cases_in_last_24hrs = NA, Patients_occupying_beds = NA, Patients_occupying_mv_beds = NA, Patients_occupying_non_mv_beds = NA, Rolling_7_day_admissions = NA, Rolling_average_7_day_admissions = NA, Total_admissions = NA) %>% 
#   add_row(Name = 'Sussex Community NHS Foundation Trust', Date = as.Date('2020-03-20'), Admissions_or_new_cases_in_last_24hrs = NA, Patients_occupying_beds = NA, Patients_occupying_mv_beds = NA, Patients_occupying_non_mv_beds = NA, Rolling_7_day_admissions = NA, Rolling_average_7_day_admissions = NA, Total_admissions = NA) 
# 
# # patients occupying beds as at 8am
# admissions_date <- admissions_df_trust %>% 
#   filter(!is.na(newAdmissions)) %>% 
#   filter(date == max(date, na.rm = TRUE)) %>% 
#   select(date) %>% 
#   unique() %>% 
#   mutate(item = 'Admissions')
# 
# occupied_date <- admissions_df_trust %>% 
#   filter(!is.na(hospitalCases)) %>% 
#   filter(date == max(date, na.rm = TRUE)) %>% 
#   select(date) %>% 
#   unique() %>% 
#   mutate(item = 'Patients in hospital')
# 
# publish_date <- data.frame(date = trust_admission_date$Description,
#                            item = 'Publish date')
# 
# admissions_date %>% 
#   bind_rows(occupied_date) %>% 
#   bind_rows(publish_date) %>% 
#   mutate(Date_label = format(date, '%A %d %B %Y')) %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x,'/hospital_meta.json'))
# 
# data.frame(Date = seq.Date(admissions_date$date -(52*7), admissions_date$date, by = 7)) %>% 
#   filter(Date >= min(admissions_df$Date)) %>% 
#   mutate(Date_label = format(Date, '%d %b %y')) %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x, '/admission_date_labels.json'))
# 
# hospital_table_a <- admissions_df %>% 
#   filter(Date == admissions_date$date) %>%
#   select(Name, Admissions_or_new_cases_in_last_24hrs, Rolling_7_day_admissions, Total_admissions)
# 
# hospital_table_b <- admissions_df %>% 
#   filter(Date == occupied_date$date) %>%
#   select(Name, Patients_occupying_beds, Patients_occupying_mv_beds)
# 
# hospital_table <- hospital_table_a %>% 
#   left_join(hospital_table_b, by = 'Name')
# 
# hospital_table %>% 
#   select(!Admissions_or_new_cases_in_last_24hrs) %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x, '/latest_hospital_summary.json'))
# 
# admissions_export_df <- admissions_df %>% 
#   filter(Date <= admissions_date$date) %>% 
#   select(Name, Date, Admissions_or_new_cases_in_last_24hrs, Rolling_average_7_day_admissions, Rolling_7_day_admissions, Total_admissions) %>% 
#   group_by(Name) %>% 
#   arrange(Name, Date) %>% 
#   mutate(Previous_Rolling_7_day_admissions = lag(Rolling_7_day_admissions, 7)) %>% 
#   mutate(Perc_change_rolling_7_day_admissions = (Rolling_7_day_admissions - lag(Rolling_7_day_admissions, 7))/ lag(Admissions_or_new_cases_in_last_24hrs, 7)) %>% 
#   mutate(Perc_change_rolling_7_day_admissions = ifelse(Perc_change_rolling_7_day_admissions == Inf, 1,Perc_change_rolling_7_day_admissions)) %>% 
#   mutate(Perc_change_rolling_7_day_admissions = replace_na(Perc_change_rolling_7_day_admissions, 0)) %>% 
#   mutate(Perc_change_rolling_7_day_admissions = ifelse(Perc_change_rolling_7_day_admissions <0, 'Down', ifelse(Perc_change_rolling_7_day_admissions == 0, 'Same', ifelse(Perc_change_rolling_7_day_admissions > 0, 'Up', NA)))) %>% 
#   mutate(Date_label = format(Date, '%d %b %y')) %>% 
#   select(!Date)
# 
# admissions_export_df %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x,'/admissions_export_df.json'))
# 
# # admissions_export_df %>% view()
# 
# beds_occupied_export_df <- admissions_df %>% 
#   select(Name, Date, Patients_occupying_beds, Patients_occupying_mv_beds, Patients_occupying_non_mv_beds) %>% 
#   group_by(Name) %>% 
#   arrange(Name, Date) %>% 
#   mutate(Previous_occupying_beds = lag(Patients_occupying_beds, 7)) %>% 
#   mutate(Perc_change_on_beds_occupied = (Patients_occupying_beds - lag(Patients_occupying_beds, 7))/ lag(Patients_occupying_beds, 7)) %>% 
#   mutate(Perc_change_on_beds_occupied = ifelse(Perc_change_on_beds_occupied == Inf, 1, Perc_change_on_beds_occupied)) %>% 
#   mutate(Perc_change_on_beds_occupied = replace_na(Perc_change_on_beds_occupied, 0)) %>% 
#   mutate(Change_direction = ifelse(Perc_change_on_beds_occupied <0, 'decreased', ifelse(Perc_change_on_beds_occupied == 0, 'stayed the same', ifelse(Perc_change_on_beds_occupied > 0, 'increased', NA)))) %>% 
#   mutate(Date_label = format(Date, '%d %b %y')) %>% 
#   filter(Date <= occupied_date$date) %>% 
#   select(!c(Date))
# 
# data.frame(Date = seq.Date(occupied_date$date -(52*7), occupied_date$date, by = 7)) %>% 
#   filter(Date >= min(admissions_df$Date)) %>% 
#   mutate(Date_label = format(Date, '%d %b %y')) %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x, '/occupied_date_labels.json'))
# 
# beds_occupied_export_df %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x,'/beds_occupied_export_df.json'))

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

#trust_x <- 'England'

#admissions_x_df <- admissions_df %>% 
 # filter(Name == trust_x)

# ggplot(admissions_x_df,
#        aes(x = Date,
#            y = Admissions_or_new_cases_in_last_24hrs)) +
#   geom_bar(stat = 'identity',
#            width = .9,
#            fill = '#901020') +
#   geom_line(aes(x = Date,
#                 group = 1,
#                 y = Rolling_average_7_day_admissions),
#             colour = '#000000',
#             lwd = 1.2) +
#   scale_x_date(date_labels = "%d %b %y",
#                breaks = seq.Date(admissions_date$date -(52*7), admissions_date$date, by = 7),
#                expand = c(0,0.01)) +
#   scale_y_continuous(expand = c(0,0.01)) +
#   labs(x = 'Date',
#        y = 'Number of admissions',
#        title = paste0('Number of new admissions with a positive COVID-19 test result; ',trust_x),
#        subtitle = paste0('New admissions or inpatients with a new diagnosis; data up to ', format(admissions_date$date, '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y'))) +
#   ph_theme() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
#
# ggplot(admissions_x_df,
#        aes(x = Date,
#            y = Rolling_7_day_admissions)) +
#   geom_line(group = 1,
#             colour = '#901020') +
#   scale_x_date(date_labels = "%b %d",
#                breaks = seq.Date(admissions_date$date -(52*7), admissions_date$date, by = 7),
#                expand = c(0.01,0.01)) +
#   labs(x = 'Date',
#        y = 'Rolling 7 day admissions',
#        title = paste0('Rolling 7 day number of new admissions with a positive COVID-19 test result; ',trust_x),
#        subtitle = paste0('New admissions or inpatients with a new diagnosis; data up to ', format(admissions_date$date, '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y'))) +
#   ph_theme() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
# 
# patients_in_beds_df <- admissions_df %>% 
#   select(Name, Date, Patients_occupying_mv_beds, Patients_occupying_non_mv_beds) %>% 
#   filter(Date <= occupied_date$date) %>% 
#   pivot_longer(cols = c(Patients_occupying_non_mv_beds, Patients_occupying_mv_beds), names_to = 'Bed_type', values_to = 'Patients_occupying_beds') %>% 
#   mutate(Bed_type = ifelse(Bed_type == 'Patients_occupying_mv_beds', 'Mechanical ventilation', ifelse(Bed_type == 'Patients_occupying_non_mv_beds', 'Not capable of mechanical ventilation', NA)))

#patients_in_beds_df_x <- patients_in_beds_df %>% 
#  filter(Name == trust_x)

# ggplot(patients_in_beds_df_x,
#        aes(x = Date,
#            y = Patients_occupying_beds)) +
#   geom_bar(stat = 'identity',
#            colour = '#ffffff',
#            aes(fill = Bed_type)) +
#   scale_x_date(date_labels = "%b %d",
#                breaks = seq.Date(admissions_date$date -(52*7), admissions_date$date, by = 7),
#                expand = c(0.01,0.01)) +
#   scale_fill_manual(values = c('#006900', '#669900'),
#                     name = 'Bed type') +
#   labs(x = 'Date',
#        y = 'Number of inpatients',
#        title = paste0('Number of patients in hospital with positive COVID-19 test result; ',trust_x),
#        subtitle = paste0('Data up to ', format(admissions_date$date, '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y'))) +
#   ph_theme() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

# Share of inpatient beds occupied ####

# trust_c19_patients_occupying_ga_beds <- read_excel( paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
#                                                     sheet = 'Adult G&A Beds Occupied COVID',
#                                                     skip = 13) %>% 
#   filter(!is.na(Name)) %>% 
#   mutate(Name = capwords(Name, strict = TRUE)) %>% 
#   mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
#   mutate(Name = gsub(' And ', ' and ', Name)) %>% 
#   mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
#   mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
#   pivot_longer(names_to = 'Date', values_to = 'c19_patients_occupying_ga_beds', cols = 5:ncol(.)) %>% 
#   mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>% 
#   filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Sussex Partnership NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
#   select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))
# 
# trust_other_patients_occupying_ga_beds <- read_excel( paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
#                                                       sheet = 'Adult G&A Bed Occupied NonCOVID',
#                                                       skip = 13) %>% 
#   filter(!is.na(Name)) %>% 
#   mutate(Name = capwords(Name, strict = TRUE)) %>% 
#   mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
#   mutate(Name = gsub(' And ', ' and ', Name)) %>% 
#   mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
#   mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
#   pivot_longer(names_to = 'Date', values_to = 'other_patients_occupying_ga_beds', cols = 5:ncol(.)) %>% 
#   mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>% 
#   filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust',  'Sussex Partnership NHS Foundation Trust','Brighton and Sussex University Hospitals NHS Trust')) %>% 
#   select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))
# 
# trust_vacant_ga_beds <- read_excel( paste0(github_repo_dir,'/Source files/trust_admissions.xlsx'),
#                                     sheet = 'Adult G&A Beds Unoccupied',
#                                     skip = 13) %>% 
#   filter(!is.na(Name)) %>% 
#   mutate(Name = capwords(Name, strict = TRUE)) %>% 
#   mutate(Name = gsub('Nhs', 'NHS', Name)) %>% 
#   mutate(Name = gsub(' And ', ' and ', Name)) %>% 
#   mutate(Name = gsub('Cic', 'CIC', Name)) %>% 
#   mutate(Name = gsub('C.i.c', 'C.I.C', Name)) %>% 
#   pivot_longer(names_to = 'Date', values_to = 'vacant_ga_beds', cols = 5:ncol(.)) %>% 
#   mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>% 
#   filter(Name %in% c('England', 'South East', 'Western Sussex Hospitals NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Sussex Community NHS Foundation Trust', 'Sussex Partnership NHS Foundation Trust', 'Brighton and Sussex University Hospitals NHS Trust')) %>% 
#   select(!c('Type 1 Acute?', 'NHS England Region', 'Code'))
# 
# bed_used_df <- trust_c19_patients_occupying_ga_beds %>% 
#   left_join(trust_other_patients_occupying_ga_beds, by = c('Name', 'Date')) %>% 
#   left_join(trust_vacant_ga_beds, by = c('Name', 'Date')) %>%
#   pivot_longer(cols = c(c19_patients_occupying_ga_beds, other_patients_occupying_ga_beds, vacant_ga_beds), names_to = 'Bed_status', values_to = 'Beds') %>% 
#   group_by(Name, Date) %>% 
#   mutate(Total_open_beds = sum(Beds),
#          Proportion = Beds/sum(Beds)) %>% 
#   mutate(Bed_status = factor(ifelse(Bed_status == 'c19_patients_occupying_ga_beds', 'COVID-19 + patients', ifelse(Bed_status == 'other_patients_occupying_ga_beds', 'Other patients', ifelse(Bed_status == 'vacant_ga_beds', 'Vacant (open) beds', NA))), levels = c('COVID-19 + patients', 'Other patients', 'Vacant (open) beds')))

# trust_admissions_metadata <- read_excel( paste0(github_repo_dir,'/Source_files/trust_admissions.xlsx'),
#                                          sheet = 'All beds COVID',
#                                          skip = 2, 
#                                          col_names = FALSE, 
#                                          n_max = 5) %>% 
#   rename(Item = ...1,
#          Description = ...2) %>%
#   mutate(Description = ifelse(Item == 'Published:', as.character(format(as.Date(as.numeric(Description), origin = "1899-12-30"), '%d-%b-%Y')), Description))

# beds_used_df_x <- bed_used_df %>% 
#   filter(Name == trust_x)

# ggplot(beds_used_df_x,
#        aes(x = Date,
#            y = Proportion)) +
#   geom_bar(stat = 'identity',
#            position = position_fill(reverse = TRUE),
#            colour = '#ffffff',
#            aes(fill = Bed_status)) +
#   scale_x_date(date_labels = "%b %d",
#                breaks = seq.Date(max(bed_used_df$Date) -(52*7), max(bed_used_df$Date), by = 7),
#                expand = c(0.01,0.01)) +
#   scale_fill_manual(values = c('#12263a', '#c96480', '#abcdef')) +
#   scale_y_continuous(limits = c(0,1),
#                      breaks = seq(0,1, .1),
#                      labels = percent_format(accuracy = 1)) +
#   labs(x = 'Date',
#        y = 'Proportion',
#        title = paste0('Proportion of patients occupying adult beds; ', trust_x),
#        subtitle = paste0('Share of all adult general and acute beds (%); data available from ',  format(min(bed_used_df$Date), '%d %B %Y'), ' up to ', format(max(bed_used_df$Date), '%d %B %Y'), ' as at ', format(trust_admission_date$Description, '%d %B %Y')),
#        caption = 'This measure only includes adult inpatient beds. It is estimated that adult beds comprised more than 99% of inpatient beds in NHS hospital trusts.') +
#   ph_theme() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

# bed_used_df_export_1 <- bed_used_df %>%
#   mutate(Bed_status = ifelse(Bed_status == 'COVID-19 + patients', 'covid_19_patients', ifelse(Bed_status == 'Other patients', 'other', ifelse(Bed_status == 'Vacant (open) beds', 'vacant', NA)))) %>% 
#   select(!c(Proportion, Total_open_beds)) %>% 
#   pivot_wider(names_from = 'Bed_status', values_from = 'Beds') %>% 
#   mutate(Total_open_beds = covid_19_patients + other + vacant) %>% 
#   mutate(Label = paste0('As at 8:00am on ', ordinal(as.numeric(format(Date, '%d'))), format(Date, ' %B %Y'), ' there were ', format(covid_19_patients, big.mark = ','), ' patients in adult General and Acute inpatient beds, this is ', round((covid_19_patients / Total_open_beds)*100, 1), '% of the total number of open beds on this day.')) 

# bed_used_df_export <- bed_used_df %>%
#   mutate(Bed_status = ifelse(Bed_status == 'COVID-19 + patients', 'covid_19_patients', ifelse(Bed_status == 'Other patients', 'other', ifelse(Bed_status == 'Vacant (open) beds', 'vacant', NA)))) %>% 
#   select(!c(Beds, Total_open_beds)) %>% 
#   pivot_wider(names_from = 'Bed_status', values_from = 'Proportion') %>% 
#   left_join(bed_used_df_export_1[c('Name', 'Date', 'Label')], by = c('Name', 'Date')) %>% 
#   mutate(Date_label = format(Date, '%d %b %y')) %>% 
#   ungroup()
# 
# bed_used_df_export %>%
#   select(!Date) %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x, '/patient_share_occupied_beds.json'))
# 
# data.frame(Date = seq.Date(max(bed_used_df_export$Date) -(52*7), max(bed_used_df_export$Date), by = 7)) %>% 
#   filter(Date >= min(bed_used_df_export$Date)) %>% 
#   mutate(Date_label = format(Date, '%d %b %y')) %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x, '/bed_share_ga_date_labels.json'))

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

# Vaccine time series counts ####
lads <- c("E07000223", "E07000224","E07000225", "E07000226", "E07000227", "E07000228","E07000229")

## build the required structure for the api
# {"date":"date","areaCode":"areaCode","areaName":"areaName","newCasesBySpecimenDateRollingRate":"newCasesBySpecimenDateRollingRate","newCasesBySpecimenDateRollingSum":"newCasesBySpecimenDateRollingSum","uniqueCasePositivityBySpecimenDateRollingSum":"uniqueCasePositivityBySpecimenDateRollingSum","uniquePeopleTestedBySpecimenDateRollingSum":"uniquePeopleTestedBySpecimenDateRollingSum"}

###### read the data
england <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","vaccinationsAgeDemographics":"vaccinationsAgeDemographics"}'

westsussex <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;areaCode=E10000032&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","vaccinationsAgeDemographics":"vaccinationsAgeDemographics"}'

southeast <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=region;areaCode=E12000008&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","vaccinationsAgeDemographics":"vaccinationsAgeDemographics"}'

# create api calls for lads:
baseurl <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=ltla;areaCode=HEREPLEASE&structure={"date":"date","areaCode":"areaCode","areaName":"areaName","vaccinationsAgeDemographics":"vaccinationsAgeDemographics"}'

ltlas <- character()

for(i in lads) {
  new_string <- sub(pattern = "HEREPLEASE", replacement = i, x = baseurl)
  ltlas <- c(ltlas, new_string)
}

# list of apis
urls <- c(england, southeast, westsussex, ltlas)

# empty list
dflist <- list()

# api calls for each geography into list
for(i in urls) {
  response <- httr::GET(url = i)
  
  if (response$status_code >= 400) {
    err_msg = httr::http_status(response)
    stop(err_msg)
  }
  json_text <- content(response, "text")
  data <- jsonlite::fromJSON(json_text)
  
  df <- as.data.frame(data$data)
  df$apisource <- i
  
  dflist[[i]] <- df
}

# bind together (unnest)
vaccine_age_df <- bind_rows(dflist) %>%
  unnest(vaccinationsAgeDemographics) %>% 
  rename(Date = date,
         Code = areaCode,
         Name = areaName,
         Age_group = age,
         Denominator = VaccineRegisterPopulationByVaccinationDate,
         Cumulative_complete = cumPeopleVaccinatedCompleteByVaccinationDate,
         Complete_dose = newPeopleVaccinatedCompleteByVaccinationDate,
         Cumulative_dose_1 = cumPeopleVaccinatedFirstDoseByVaccinationDate,
         Dose_1 = newPeopleVaccinatedFirstDoseByVaccinationDate, 
         Cumulative_dose_2 = cumPeopleVaccinatedSecondDoseByVaccinationDate,
         Dose_2 = newPeopleVaccinatedSecondDoseByVaccinationDate) %>% 
  select(!c(apisource, Code)) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Age_group = factor(paste0(gsub('_', '-', Age_group), ' years'), levels = c("18-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90+ years"))) %>% 
  group_by(Name, Age_group) %>% 
  arrange(Date) %>% 
  mutate(Seven_day_sum_dose_1 = round(rollapplyr(Dose_1, 7, sum, align = 'right', partial = TRUE),0)) %>%  
  mutate(Seven_day_sum_dose_2 = round(rollapplyr(Dose_2, 7, sum, align = 'right', partial = TRUE),0))  %>% 
  mutate(Rolling_age_specific_first_dose_rate_per_100000 = pois.exact(Seven_day_sum_dose_1, Denominator)[[3]]*100000) %>% 
  mutate(Cumulative_age_specific_first_dose_rate_per_100000 = pois.exact(Cumulative_dose_1, Denominator)[[3]]*100000)  %>% 
  mutate(Rolling_age_specific_second_dose_rate_per_100000 = pois.exact(Seven_day_sum_dose_2, Denominator)[[3]]*100000) %>% 
  mutate(Cumulative_age_specific_second_dose_rate_per_100000 = pois.exact(Cumulative_dose_2, Denominator)[[3]]*100000) 

vaccine_ts_df <- vaccine_age_df %>% 
  group_by(Date, Name) %>% 
  summarise(Dose_1 = sum(Dose_1, na.rm = TRUE),
            Dose_2 = sum(Dose_2, na.rm = TRUE),
            Denominator = sum(Denominator, na.rm = TRUE),
            Complete_dose = sum(Complete_dose, na.rm = TRUE)) %>%
  group_by(Name) %>% 
  arrange(Date) %>% 
  mutate(Cumulative_dose_1 = cumsum(Dose_1),
         Cumulative_dose_2 = cumsum(Dose_2),
         Cumulative_complete = cumsum(Complete_dose)) %>% 
  mutate(Seven_day_sum_dose_1 = round(rollapplyr(Dose_1, 7, sum, align = 'right', partial = TRUE),0)) %>%  
  mutate(Seven_day_sum_dose_2 = round(rollapplyr(Dose_2, 7, sum, align = 'right', partial = TRUE),0)) %>% 
  mutate(Rolling_age_specific_first_dose_rate_per_100000 = pois.exact(Seven_day_sum_dose_1, Denominator)[[3]]*100000) %>% 
  mutate(Cumulative_age_specific_first_dose_rate_per_100000 = pois.exact(Cumulative_dose_1, Denominator)[[3]]*100000)  %>% 
  mutate(Rolling_age_specific_second_dose_rate_per_100000 = pois.exact(Seven_day_sum_dose_2, Denominator)[[3]]*100000) %>% 
  mutate(Cumulative_age_specific_second_dose_rate_per_100000 = pois.exact(Cumulative_dose_2, Denominator)[[3]]*100000) 

vaccine_ts_df %>%
  ungroup() %>% 
  filter(Date == max(Date)) %>% 
  select(Date) %>% 
  unique() %>% 
  mutate(Date = paste0(format(Date, '%A '), ordinal(as.numeric(format(Date, '%d'))), format(Date, ' %B'))) %>%   toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_latest_dose_date.json'))

vaccination_area_ts_df_long <- vaccine_ts_df %>% 
  select(Name, Date, Seven_day_sum_dose_1, Seven_day_sum_dose_2) %>% 
  pivot_longer(cols = c('Seven_day_sum_dose_1', 'Seven_day_sum_dose_2'),
               names_to = 'Dose_number',
               values_to = 'Seven_day_rolling_vaccinations') %>% 
  mutate(Dose_number = ifelse(Dose_number == 'Seven_day_sum_dose_1', 'Dose_1', ifelse(Dose_number == 'Seven_day_sum_dose_2', 'Dose_2', NA)))

vaccination_area_ts_rate_df_long <- vaccine_ts_df %>% 
  select(Name, Date, Rolling_age_specific_first_dose_rate_per_100000, Rolling_age_specific_second_dose_rate_per_100000) %>% 
  pivot_longer(cols = c('Rolling_age_specific_first_dose_rate_per_100000', 'Rolling_age_specific_second_dose_rate_per_100000'),
               names_to = 'Dose_number',
               values_to = 'Seven_day_rolling_rate_vaccinations') %>% 
  mutate(Dose_number = ifelse(Dose_number == 'Rolling_age_specific_first_dose_rate_per_100000', 'Dose_1', ifelse(Dose_number == 'Rolling_age_specific_second_dose_rate_per_100000', 'Dose_2', NA)))

vaccination_area_ts_df_long <- vaccination_area_ts_df_long %>% 
  left_join(vaccination_area_ts_rate_df_long, by = c('Name', 'Date', 'Dose_number'))

vaccine_ts_df_x <- vaccination_area_ts_df_long %>% 
  filter(Name == 'West Sussex')

vaccination_area_ts_df_long %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccination_timeseries_overall.json'))

# ggplot(data = vaccine_ts_df_x,
#        aes(x = Date,
#            y = Seven_day_rolling_vaccinations,
#            group = Dose_number,
#            colour = Dose_number)) +
#   geom_line(size = .9) +
#   ph_theme() +
#   theme(axis.text.x = element_text(angle = 90, size = 6)) +
#   scale_y_continuous(labels = label_comma(accuracy = 1)) +
#   scale_x_date(date_labels = "%b %d",
#                breaks = seq.Date(max(vaccine_ts_df_x$Date) - (52*14), max(vaccine_ts_df_x$Date), by = 7),
#                limits = c(min(vaccine_ts_df_x$Date), max(vaccine_ts_df_x$Date) + 2),
#                expand = c(0.01,1)) +
#   scale_colour_manual(values = c('#fa8800','#00563f'),
#                       name = 'Dose',
#                       labels = c('Dose 1', 'Dose 2')) +
#   labs(x = 'Date of administration',
#        y = 'Number of vaccinations\nin previous 7 days',
#        title = paste0('Rolling 7 day number of Covid-19 vaccinations; ', 'West Sussex'),
#        subtitle = paste0('Vaccinations administered to patients registered to addresses in ', 'West Sussex', '; as at ', format(last_date, '%d %B')))  +
#   theme(axis.text.x = element_text(size = 8))

# ggplot(data = vaccine_ts_df_x,
#        aes(x = Date,
#            y = Seven_day_rolling_rate_vaccinations,
#            group = Dose_number,
#            colour = Dose_number)) +
#   geom_line(size = .9) +
#   ph_theme() +
#   theme(axis.text.x = element_text(angle = 90, size = 6)) +
#   scale_y_continuous(labels = label_comma(accuracy = 1)) +
#   scale_x_date(date_labels = "%b %d",
#                breaks = seq.Date(max(vaccine_ts_df_x$Date) - (52*14), max(vaccine_ts_df_x$Date), by = 7),
#                limits = c(min(vaccine_ts_df_x$Date), max(vaccine_ts_df_x$Date) + 2),
#                expand = c(0.01,1)) +
#   scale_colour_manual(values = c('#fa8800','#00563f'),
#                       name = 'Dose',
#                       labels = c('Dose 1', 'Dose 2')) +
#   labs(x = 'Date of administration',
#        y = 'Rate of vaccinations per 100,000\nin previous 7 days',
#        title = paste0('Rolling 7 day rate per 100,000 of Covid-19 vaccinations; ', 'West Sussex'),
#        subtitle = paste0('Vaccinations administered to patients registered to addresses in ', 'West Sussex', '; as at ', format(last_date, '%d %B')))  +
#   theme(axis.text.x = element_text(size = 8))

# vaccine_age_df %>% view()

vaccination_area_ts_df_long %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  filter(Date %in% seq.Date(max(Date) -(104*7), max(Date), by = 7)| Date == min(Date)) %>% 
  mutate(Date_label = format(Date, '%d %b %y')) %>% 
  select(Date_label) %>% 
  unique() %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccination_timeseries_date_labels.json'))
  
vaccination_area_ts_df_long %>%
  mutate(Date_label = format(Date, '%d %b %y')) %>% 
  select(!Date) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccination_timeseries_overall.json'))

vaccine_age_df %>% 
  mutate(Date_label = format(Date, '%d %b %y')) %>% 
  select(Name, Age_group, Date_label, Seven_day_sum_dose_1, Rolling_age_specific_first_dose_rate_per_100000, Seven_day_sum_dose_2, Rolling_age_specific_second_dose_rate_per_100000, Cumulative_dose_1, Cumulative_age_specific_first_dose_rate_per_100000, Cumulative_dose_2, Cumulative_age_specific_second_dose_rate_per_100000) %>%
  mutate(Rolling_age_specific_first_dose_rate_per_100000 = replace_na(Rolling_age_specific_first_dose_rate_per_100000, 0)) %>% 
  mutate(Rolling_age_specific_second_dose_rate_per_100000 = replace_na(Rolling_age_specific_second_dose_rate_per_100000, 0)) %>% 
  mutate(Cumulative_age_specific_first_dose_rate_per_100000 = replace_na(Cumulative_age_specific_first_dose_rate_per_100000, 0)) %>% 
  mutate(Cumulative_age_specific_second_dose_rate_per_100000 = replace_na(Cumulative_age_specific_second_dose_rate_per_100000, 0)) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccination_timeseries_age.json'))

vaccine_ts_age_df_x <- vaccine_age_df %>% 
  filter(Name == 'West Sussex')

library(ggiraph)

# viridis::inferno(15, direction = -1)

# ggplot(data = vaccine_ts_age_df_x,
#        aes(x = Date,
#            y = Seven_day_sum_dose_1,
#            group = Age_group,
#            colour = Age_group)) +
#   geom_line_interactive(aes(tooltip = paste0(Age_group),
#                             data_id = paste0(Age_group)),
#                         size = .9) +
#   ph_theme() +
#   theme(axis.text.x = element_text(angle = 90, size = 6)) +
#   scale_y_continuous(labels = label_comma(accuracy = 1)) +
#   scale_x_date(date_labels = "%b %d",
#                breaks = seq.Date(max(vaccine_ts_age_df_x$Date) -(52*14), max(vaccine_ts_age_df_x$Date), by = 7),
#                limits = c(min(vaccine_ts_age_df_x$Date), max(vaccine_ts_age_df_x$Date)),
#                expand = c(0.01,1)) +
#   # scale_colour_manual(values = age_colours,
#   #                     name = 'Age') +
#   labs(x = 'Date of administration',
#        y = 'Number of first dose\nvaccinations in previous 7 days',
#        title = paste0('Rolling 7 day sum of first dose Covid-19 vaccinations; ', 'West Sussex'),
#        subtitle = paste0('Vaccinations administered to patients registered to addresses in ', 'West Sussex', '; as at ', format(last_date, '%d %B')))  +
#   theme(axis.text.x = element_text(size = 8))
# 
# ggplot(data = vaccine_ts_age_df_x,
#        aes(x = Date,
#            y = Rolling_age_specific_first_dose_rate_per_100000,
#            group = Age_group,
#            colour = Age_group)) +
#   geom_line_interactive(aes(tooltip = paste0(Age_group),
#                             data_id = paste0(Age_group)),
#                         size = .9) +
#   ph_theme() +
#   theme(axis.text.x = element_text(angle = 90, size = 6)) +
#   scale_y_continuous(labels = label_comma(accuracy = 1)) +
#   scale_x_date(date_labels = "%b %d",
#                breaks = seq.Date(max(vaccine_ts_age_df_x$Date) -(52*14), max(vaccine_ts_age_df_x$Date), by = 7),
#                limits = c(min(vaccine_ts_age_df_x$Date), max(vaccine_ts_age_df_x$Date)),
#                expand = c(0.01,1)) +
#   # scale_colour_manual(values = age_colours,
#   #                     name = 'Age') +
#   labs(x = 'Date of administration',
#        y = 'Rate of first dose\nvaccinations per 100,000 population\nin previous 7 days',
#        title = paste0('Rolling 7 day rate per 100,000 population of first dose Covid-19 vaccinations; ', 'West Sussex'),
#        subtitle = paste0('Vaccinations administered to patients registered to addresses in ', 'West Sussex', '; as at ', format(last_date, '%d %B')))  +
#   theme(axis.text.x = element_text(size = 8))
# 
# ggplot(data = vaccine_ts_age_df_x,
#        aes(x = Date,
#            y = Seven_day_sum_dose_2,
#            group = Age_group,
#            colour = Age_group)) +
#   geom_line_interactive(aes(tooltip = paste0(Age_group),
#                             data_id = paste0(Age_group)),
#                         size = .9) +
#   ph_theme() +
#   theme(axis.text.x = element_text(angle = 90, size = 6)) +
#   scale_y_continuous(labels = label_comma(accuracy = 1)) +
#   scale_x_date(date_labels = "%b %d",
#                breaks = seq.Date(max(vaccine_ts_age_df_x$Date) -(52*14), max(vaccine_ts_age_df_x$Date), by = 7),
#                limits = c(min(vaccine_ts_age_df_x$Date), max(vaccine_ts_age_df_x$Date)),
#                expand = c(0.01,1)) +
#   # scale_colour_manual(values = age_colours,
#   #                     name = 'Age') +
#   labs(x = 'Date of administration',
#        y = 'Number of first dose\nvaccinations in previous 7 days',
#        title = paste0('Rolling 7 day sum of first dose Covid-19 vaccinations; ', 'West Sussex'),
#        subtitle = paste0('Vaccinations administered to patients registered to addresses in ', 'West Sussex', '; as at ', format(last_date, '%d %B')))  +
#   theme(axis.text.x = element_text(size = 8))
# 
# ggplot(data = vaccine_ts_age_df_x,
#        aes(x = Date,
#            y = Rolling_age_specific_second_dose_rate_per_100000,
#            group = Age_group,
#            colour = Age_group)) +
#   geom_line_interactive(aes(tooltip = paste0(Age_group),
#                             data_id = paste0(Age_group)),
#                         size = .9) +
#   ph_theme() +
#   theme(axis.text.x = element_text(angle = 90, size = 6)) +
#   scale_y_continuous(labels = label_comma(accuracy = 1)) +
#   scale_x_date(date_labels = "%b %d",
#                breaks = seq.Date(max(vaccine_ts_age_df_x$Date) -(52*14), max(vaccine_ts_age_df_x$Date), by = 7),
#                limits = c(min(vaccine_ts_age_df_x$Date), max(vaccine_ts_age_df_x$Date)),
#                expand = c(0.01,1)) +
#   # scale_colour_manual(values = age_colours,
#   #                     name = 'Age') +
#   labs(x = 'Date of administration',
#        y = 'Rate of first dose\nvaccinations per 100,000 population\nin previous 7 days',
#        title = paste0('Rolling 7 day rate per 100,000 population of first dose Covid-19 vaccinations; ', 'West Sussex'),
#        subtitle = paste0('Vaccinations administered to patients registered to addresses in ', 'West Sussex', '; as at ', format(last_date, '%d %B')))  +
#   theme(axis.text.x = element_text(size = 8))

read_csv(paste0(github_repo_dir, '/Source files/jcvi_dates.csv'),
                              col_types = cols(Opening_date = col_date(format = "%d/%m/%Y"))) %>% 
  mutate(Date_label = format(Opening_date, '%d %b %y')) %>% 
  filter(!is.na(Date_label)) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x,'/jcvi_cohort_key_dates.json'))

# Week by week change ####

set_week_start('Monday')

# Create a dataframe consisting of 52 rows (one for each week) with the field 'Week_start' as the date of the start of each week. Add a number corresponding to the week number, create a string called match_key (which we will use to match the filepath to the week) and then a formatted label for the dates included in the week.
week_starting_a <- data.frame(Week_start = get_date(week = 1:53, year = 2020)) %>%
  mutate(Week_number = paste0(row_number(), ' - 2020'))

week_starting_b <- data.frame(Week_start = get_date(week = 1:52, year = 2021)) %>%
  mutate(Week_number = paste0(row_number(), ' - 2021'))

week_start_vac <- week_starting_a %>%
  bind_rows(week_starting_b) %>%
  mutate(Week_range_label = paste0(format(Week_start, '%d %b'), ' to ', format(Week_start + 6, '%d %b %Y'))) %>%
  mutate(Week_number = factor(Week_number, levels = c("1 - 2020", "2 - 2020",  "3 - 2020", "4 - 2020",  "5 - 2020",  "6 - 2020",  "7 - 2020",  "8 - 2020",  "9 - 2020",  "10 - 2020", "11 - 2020", "12 - 2020", "13 - 2020", "14 - 2020", "15 - 2020", "16 - 2020", "17 - 2020", "18 - 2020", "19 - 2020", "20 - 2020", "21 - 2020", "22 - 2020", "23 - 2020", "24 - 2020", "25 - 2020", "26 - 2020", "27 - 2020", "28 - 2020", "29 - 2020", "30 - 2020", "31 - 2020", "32 - 2020", "33 - 2020", "34 - 2020", "35 - 2020", "36 - 2020", "37 - 2020", "38 - 2020", "39 - 2020", "40 - 2020", "41 - 2020", "42 - 2020", "43 - 2020", "44 - 2020", "45 - 2020", "46 - 2020", "47 - 2020", "48 - 2020", "49 - 2020", "50 - 2020", "51 - 2020", "52 - 2020", "53 - 2020", "1 - 2021", "2 - 2021", "3 - 2021", "4 - 2021",  "5 - 2021",  "6 - 2021",  "7 - 2021", "8 - 2021",  "9 - 2021",  "10 - 2021", "11 - 2021", "12 - 2021", "13 - 2021", "14 - 2021", "15 - 2021", "16 - 2021", "17 - 2021", "18 - 2021", "19 - 2021", "20 - 2021", "21 - 2021", "22 - 2021", "23 - 2021", "24 - 2021", "25 - 2021", "26 - 2021", "27 - 2021", "28 - 2021", "29 - 2021", "30 - 2021", "31 - 2021", "32 - 2021", "33 - 2021", "34 - 2021", "35 - 2021", "36 - 2021", "37 - 2021", "38 - 2021", "39 - 2021", "40 - 2021", "41 - 2021", "42 - 2021", "43 - 2021", "44 - 2021", "45 - 2021", "46 - 2021", "47 - 2021", "48 - 2021", "49 - 2021", "50 - 2021", "51 - 2021", "52 - 2021"))) %>%
  mutate(week_id = row_number())

rm(week_starting_a, week_starting_b)

all_age_vac <- vaccine_ts_df %>% 
  mutate(Age_group = '18 and over') %>% 
  bind_rows(vaccine_age_df) %>% 
  select(!c(cumVaccinationFirstDoseUptakeByVaccinationDatePercentage,cumVaccinationCompleteCoverageByVaccinationDatePercentage,cumVaccinationSecondDoseUptakeByVaccinationDatePercentage)) %>% 
  mutate(Week_number = paste0(date2week(Date, numeric = TRUE), ifelse(Date < '2021-01-04', ' - 2020', ' - 2021' ))) %>% 
  left_join(week_start_vac, by = 'Week_number') %>% 
  mutate(Week_number = factor(Week_number, levels = c("1 - 2020", "2 - 2020",  "3 - 2020", "4 - 2020",  "5 - 2020",  "6 - 2020",  "7 - 2020",  "8 - 2020",  "9 - 2020",  "10 - 2020", "11 - 2020", "12 - 2020", "13 - 2020", "14 - 2020", "15 - 2020", "16 - 2020", "17 - 2020", "18 - 2020", "19 - 2020", "20 - 2020", "21 - 2020", "22 - 2020", "23 - 2020", "24 - 2020", "25 - 2020", "26 - 2020", "27 - 2020", "28 - 2020", "29 - 2020", "30 - 2020", "31 - 2020", "32 - 2020", "33 - 2020", "34 - 2020", "35 - 2020", "36 - 2020", "37 - 2020", "38 - 2020", "39 - 2020", "40 - 2020", "41 - 2020", "42 - 2020", "43 - 2020", "44 - 2020", "45 - 2020", "46 - 2020", "47 - 2020", "48 - 2020", "49 - 2020", "50 - 2020", "51 - 2020", "52 - 2020", "53 - 2020", "1 - 2021", "2 - 2021", "3 - 2021", "4 - 2021",  "5 - 2021",  "6 - 2021",  "7 - 2021", "8 - 2021",  "9 - 2021",  "10 - 2021", "11 - 2021", "12 - 2021", "13 - 2021", "14 - 2021", "15 - 2021", "16 - 2021", "17 - 2021", "18 - 2021", "19 - 2021", "20 - 2021", "21 - 2021", "22 - 2021", "23 - 2021", "24 - 2021", "25 - 2021", "26 - 2021", "27 - 2021", "28 - 2021", "29 - 2021", "30 - 2021", "31 - 2021", "32 - 2021", "33 - 2021", "34 - 2021", "35 - 2021", "36 - 2021", "37 - 2021", "38 - 2021", "39 - 2021", "40 - 2021", "41 - 2021", "42 - 2021", "43 - 2021", "44 - 2021", "45 - 2021", "46 - 2021", "47 - 2021", "48 - 2021", "49 - 2021", "50 - 2021", "51 - 2021", "52 - 2021"))) %>% 
  mutate(Age_group = ifelse(Age_group %in% c('60-64 years','65-69 years','70-74 years','75-79 years','80-84 years','85-89 years','90+ years'), '60+ years', Age_group)) %>% 
  group_by(Name, Age_group, Week_number, Week_start, Week_range_label) %>% 
  summarise(Dose_1 = sum(Dose_1, na.rm = TRUE),
            Dose_2 = sum(Dose_2, na.rm = TRUE)) %>% 
  ungroup()
  
week_x <- ifelse(paste0(date2week(last_date, numeric = TRUE)-1, ifelse(last_date < '2021-01-04', ' - 2020', ' - 2021')) == '0 - 2021', '53 - 2020', paste0(date2week(last_date, numeric = TRUE)-1, ifelse(last_date < '2021-01-04', ' - 2020', ' - 2021')))

date_weeks_x <- week_start_vac %>%
  filter(Week_number == week_x)

weeks_to_keep <- week_start_vac %>%
  filter(Week_start <= date_weeks_x$Week_start) %>%
  arrange(desc(Week_number)) %>%
  filter(week_id %in% seq(date_weeks_x$week_id - 2, date_weeks_x$week_id, 1)) %>% 
  arrange(week_id)

wsx_wk_by_wk_1 <- all_age_vac %>% 
  filter(Week_number %in% weeks_to_keep$Week_number) %>% 
  select(Name, Age_group, Week_range_label, Dose_1) %>% 
  mutate(Week_range_label = paste0('1st doses ', Week_range_label)) %>% 
  pivot_wider(names_from = Week_range_label,
              values_from = Dose_1) 

wsx_wk_by_wk_2 <- all_age_vac %>% 
  filter(Week_number %in% weeks_to_keep$Week_number) %>% 
  select(Name, Age_group, Week_range_label, Dose_2) %>% 
  mutate(Week_range_label = paste0('2nd doses ', Week_range_label)) %>% 
  pivot_wider(names_from = Week_range_label,
              values_from = Dose_2) 

wsx_wk_by_wk <- wsx_wk_by_wk_1 %>% 
  left_join(wsx_wk_by_wk_2,  by = c('Name', 'Age_group')) %>% 
  mutate(Age_group = factor(Age_group, levels = c('18 and over','18-24 years','25-29 years','30-34 years','35-39 years','40-44 years','45-49 years','50-54 years','55-59 years','60+ years'))) %>% 
  arrange(Name, desc(Age_group))

wsx_wk_by_wk %>% 
  names() %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_wk_by_wk_age_headings.json'))

wsx_wk_by_wk %>% 
  rename(Age_group = 2) %>% 
  rename(First_dose_week_minus_3 = 3) %>% 
  rename(First_dose_week_minus_2 = 4) %>%
  rename(First_dose_week_minus_1 = 5) %>%
  rename(Second_dose_week_minus_3 = 6) %>%
  rename(Second_dose_week_minus_2 = 7) %>%
  rename(Second_dose_week_minus_1 = 8) %>%
  mutate(Label = paste0(Name, Age_group)) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_wk_by_wk_age.json'))

age_denominators_1 <- vaccine_age_df %>% 
  filter(Date == max(Date)) %>%
  select(Name, Age_group, Denominator)

age_denominators_2 <- age_denominators_1 %>% 
  group_by(Name) %>% 
  summarise(Denominator = sum(Denominator, na.rm = TRUE)) %>% 
  mutate(Age_group = '18 and over')

age_denominators <- age_denominators_1 %>% 
  bind_rows(age_denominators_2)

# cumulative timeseries by age ####
cumulative_vac <- vaccine_ts_df %>% 
  mutate(Age_group = '18 and over') %>% 
  bind_rows(vaccine_age_df) %>% 
  mutate(Age_group = factor(Age_group, levels = c('18 and over',"18-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90+ years"))) %>% 
  mutate(Date_label = format(Date, '%d %b %y')) 

cumulative_vac %>% 
  filter(Date == max(Date)) %>% 
  select(Name, Age_group, Denominator) %>% 
  unique() %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_age_denominators.json'))  

cumulative_vac %>% 
  select(Name, Age_group, Date_label, Cumulative_dose_1, Cumulative_dose_2) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/cumulative_vaccine_age_data.json'))  

# Week by week percentage

weekly_prop_df <- vaccine_ts_df %>% 
  mutate(Age_group = '18 and over') %>% 
  bind_rows(vaccine_age_df) %>% 
  select(!c(cumVaccinationFirstDoseUptakeByVaccinationDatePercentage,cumVaccinationCompleteCoverageByVaccinationDatePercentage,cumVaccinationSecondDoseUptakeByVaccinationDatePercentage)) %>% 
  mutate(Week_number = paste0(date2week(Date, numeric = TRUE), ifelse(Date < '2021-01-04', ' - 2020', ' - 2021' ))) %>% 
  left_join(week_start_vac, by = 'Week_number') %>% 
  mutate(Week_number = factor(Week_number, levels = c("1 - 2020", "2 - 2020",  "3 - 2020", "4 - 2020",  "5 - 2020",  "6 - 2020",  "7 - 2020",  "8 - 2020",  "9 - 2020",  "10 - 2020", "11 - 2020", "12 - 2020", "13 - 2020", "14 - 2020", "15 - 2020", "16 - 2020", "17 - 2020", "18 - 2020", "19 - 2020", "20 - 2020", "21 - 2020", "22 - 2020", "23 - 2020", "24 - 2020", "25 - 2020", "26 - 2020", "27 - 2020", "28 - 2020", "29 - 2020", "30 - 2020", "31 - 2020", "32 - 2020", "33 - 2020", "34 - 2020", "35 - 2020", "36 - 2020", "37 - 2020", "38 - 2020", "39 - 2020", "40 - 2020", "41 - 2020", "42 - 2020", "43 - 2020", "44 - 2020", "45 - 2020", "46 - 2020", "47 - 2020", "48 - 2020", "49 - 2020", "50 - 2020", "51 - 2020", "52 - 2020", "53 - 2020", "1 - 2021", "2 - 2021", "3 - 2021", "4 - 2021",  "5 - 2021",  "6 - 2021",  "7 - 2021", "8 - 2021",  "9 - 2021",  "10 - 2021", "11 - 2021", "12 - 2021", "13 - 2021", "14 - 2021", "15 - 2021", "16 - 2021", "17 - 2021", "18 - 2021", "19 - 2021", "20 - 2021", "21 - 2021", "22 - 2021", "23 - 2021", "24 - 2021", "25 - 2021", "26 - 2021", "27 - 2021", "28 - 2021", "29 - 2021", "30 - 2021", "31 - 2021", "32 - 2021", "33 - 2021", "34 - 2021", "35 - 2021", "36 - 2021", "37 - 2021", "38 - 2021", "39 - 2021", "40 - 2021", "41 - 2021", "42 - 2021", "43 - 2021", "44 - 2021", "45 - 2021", "46 - 2021", "47 - 2021", "48 - 2021", "49 - 2021", "50 - 2021", "51 - 2021", "52 - 2021"))) %>% 
  # mutate(Age_group = ifelse(Age_group %in% c('60-64 years','65-69 years','70-74 years','75-79 years','80-84 years','85-89 years','90+ years'), '60+ years', Age_group)) %>% 
  group_by(Name, Age_group, Week_number, Week_start, Week_range_label) %>% 
  summarise(Dose_1 = sum(Dose_1, na.rm = TRUE),
            Dose_2 = sum(Dose_2, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(age_denominators, by = c('Name', 'Age_group')) %>% 
  mutate(Proportion_dose_1 = Dose_1 / Denominator) %>% 
  mutate(Proportion_dose_2 = Dose_2 / Denominator) 

weekly_prop_1 <- weekly_prop_df %>% 
  mutate(Week_range_label = paste0('1st doses ', Week_range_label)) %>%
  select(Name, Age_group, Week_range_label, Proportion_dose_1) %>% 
  pivot_wider(names_from = Week_range_label,
              values_from = Proportion_dose_1) %>% 
  mutate(Dose = 'Dose 1')

weekly_prop_2 <- weekly_prop_df %>% 
  select(Name, Age_group, Week_range_label, Proportion_dose_2) %>% 
  mutate(Week_range_label = paste0('2nd doses ', Week_range_label)) %>% 
  pivot_wider(names_from = Week_range_label,
              values_from = Proportion_dose_2) %>% 
  mutate(Dose = 'Dose 2')

wsx_wk_by_wk_prop <- weekly_prop_1 %>% 
  bind_rows(weekly_prop_2) %>% 
  mutate(Age_group = factor(Age_group, levels = c('18 and over', "18-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90+ years"))) %>% 
  arrange(Name, desc(Age_group))


wsx_wk_by_wk_prop %>% 
  write.csv(., paste0(output_directory_x, '/weekly_doses_proportion.csv'), row.names = FALSE)

# recreating vaccine at a glance LTLA ####

latest_denominators_1 <- vaccine_age_df %>% 
  filter(Date == max(Date)) %>%
  select(Name, Age_group, Denominator)

latest_denominators_2 <-  vaccine_age_df %>% 
  filter(Date == max(Date)) %>%
  filter(Age_group %in% c('65-69 years','70-74 years','75-79 years','80-84 years','85-89 years','90+ years')) %>% 
  select(Name, Denominator) %>% 
  group_by(Name) %>% 
  summarise(Denominator = sum(Denominator, na.rm = TRUE)) %>% 
  mutate(Age_group = '65 and over') %>%
  ungroup() 

latest_denominators_3 <-  vaccine_age_df %>% 
  filter(Date == max(Date)) %>%
  filter(Age_group %in% c("18-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",'60-64 years')) %>% 
  select(Name, Denominator) %>% 
  group_by(Name) %>% 
  summarise(Denominator = sum(Denominator, na.rm = TRUE)) %>% 
  mutate(Age_group = '18-64 years') %>%
  ungroup() 

latest_denominators <- latest_denominators_1 %>% 
  group_by(Name) %>% 
  summarise(Denominator = sum(Denominator, na.rm = TRUE)) %>% 
  mutate(Age_group = '18 and over') %>%
  ungroup() %>% 
  bind_rows(latest_denominators_1) %>% 
  bind_rows(latest_denominators_2) %>% 
  bind_rows(latest_denominators_3)
                  
vaccine_df_ltla_1 <- vaccine_age_df %>% 
  group_by(Name, Age_group) %>% 
  summarise(Dose_1 = sum(Dose_1, na.rm = TRUE),
            Dose_2 = sum(Dose_2, na.rm = TRUE)) %>% 
  left_join(latest_denominators, by = c('Name', 'Age_group'))
  
vaccine_df_ltla_2 <- vaccine_age_df %>% 
  filter(Age_group %in% c('65-69 years','70-74 years','75-79 years','80-84 years','85-89 years','90+ years')) %>% 
  group_by(Name) %>% 
  summarise(Dose_1 = sum(Dose_1, na.rm = TRUE),
            Dose_2 = sum(Dose_2, na.rm = TRUE)) %>% 
  mutate(Age_group = '65 and over') %>% 
  left_join(latest_denominators, by = c('Name', 'Age_group'))

vaccine_df_ltla_3 <- vaccine_age_df %>% 
  filter(Age_group %in% c("18-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",'60-64 years')) %>% 
  group_by(Name) %>% 
  summarise(Dose_1 = sum(Dose_1, na.rm = TRUE),
            Dose_2 = sum(Dose_2, na.rm = TRUE)) %>% 
  mutate(Age_group = '18-64 years') %>% 
  left_join(latest_denominators, by = c('Name', 'Age_group'))


vaccine_df_ltla_4 <- vaccine_age_df %>% 
  group_by(Name) %>% 
  summarise(Dose_1 = sum(Dose_1, na.rm = TRUE),
            Dose_2 = sum(Dose_2, na.rm = TRUE)) %>% 
  mutate(Age_group = '18 and over') %>% 
  left_join(latest_denominators, by = c('Name', 'Age_group'))

vaccine_df_ltla <- vaccine_df_ltla_2 %>% 
  bind_rows(vaccine_df_ltla_3) %>% 
  bind_rows(vaccine_df_ltla_4) %>% 
  mutate(Proportion_dose_1 = Dose_1 / Denominator)
  
vaccine_df_ltla_pt_1 <- vaccine_df_ltla%>% 
  select(Name, Dose_1, Age_group) %>% 
  mutate(Age_group = factor(Age_group, levels = c('18 and over', '18-64 years', '65 and over'))) %>% 
  arrange(Age_group) %>% 
  mutate(label = paste0('Number of individuals aged ', Age_group)) %>% 
  select(!Age_group) %>% 
  pivot_wider(names_from = label,
              values_from = Dose_1) 

vaccine_df_ltla_pt_2 <- vaccine_df_ltla%>% 
  select(Name, Proportion_dose_1, Age_group) %>% 
  mutate(Age_group = factor(Age_group, levels = c('18 and over', '18-64 years', '65 and over'))) %>% 
  arrange(Age_group) %>% 
  mutate(label = paste0('Proportion (', Age_group, ')')) %>% 
  select(!Age_group) %>% 
  pivot_wider(names_from = label,
              values_from = Proportion_dose_1) 

vaccine_df_ltla_pt_1 %>% 
  left_join(vaccine_df_ltla_pt_2, by = 'Name') %>% 
  select(Name, `Number of individuals aged 18 and over`, `Proportion (18 and over)`, `Number of individuals aged 18-64 years`, `Proportion (18-64 years)`, `Number of individuals aged 65 and over`, `Proportion (65 and over)`) %>% 
  mutate(Name = factor(Name, levels = c('Adur' ,'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East', 'England'))) %>% 
  arrange(Name) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_at_a_glance.json'))  

# 
# vaccine_df_ltla %>%  toJSON() %>% 
#   write_lines(paste0(mobile_output_directory_x, '/vaccine_at_a_glance.json'))  
# vaccine_df_ltla_pt_1 %>% 
#   left_join(vaccine_df_ltla_pt_2, by = 'Name') %>% 
#   select(Name, `Number of individuals aged 18 and over`, `Proportion (18 and over)`, `Number of individuals aged 18-64 years`, `Proportion (18-64 years)`, `Number of individuals aged 65 and over`, `Proportion (65 and over)`) %>% 
#   mutate(Name = factor(Name, levels = c('Adur' ,'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'South East', 'England'))) %>% 
#   arrange(Name) %>% 
#   toJSON() %>%
#   write_lines(paste0(mobile_output_directory_x, '/vaccine_at_a_glance.json'))
