
# Age is based on age on 31st March 2021 to align with JCVI priority assignments

# Vaccine data
calls_vaccine_webpage <- read_html('https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/') %>%
  html_nodes("a") %>%
  html_attr("href")

# keep only xlsx files
calls_vaccine_webpage <- grep('.xlsx', calls_vaccine_webpage, value = T)

# keep only the top one - this should be read from top to bottom with the most recent at the top of the page - will have to check
calls_vaccine_webpage <- grep('weekly-announced-vaccinations', calls_vaccine_webpage, value = T)[1]

download.file(calls_vaccine_webpage, paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'), mode = 'wb')


vaccine_meta <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
                                      sheet = 'Contents',
                                      skip = 1,
                                      n_max = 7,
                           col_names = c('item', 'description'))

vaccine_published_date <- as.character(vaccine_meta %>% 
  filter(item == 'Published:') %>% 
  select(description)) 

vaccine_published_date %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_update_date.json'))

as.character(vaccine_meta %>% 
               filter(item == 'Period:') %>% 
               select(description)) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_administered_date.json'))

mye_ages_70_plus <- mye_ages %>% 
  filter(Age %in% c('70-74 years', '75-79 years', '80+ years')) %>% 
  group_by(Name, Code, Type) %>% 
  summarise(Population_70plus = sum(Population))

# LTLA vaccine data ####

vaccine_df_ltla <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
                              sheet = 'Vaccinations by LTLA',
                              skip = 15,
                              col_names = c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name', 'Under_70', '70_74', '75_79', '80_and_over')) %>% 
  filter(!is.na(Region_name)) %>% 
  mutate(Total_where_age_known = Under_70 + `70_74` + `75_79` + `80_and_over`) %>% 
  left_join(mye_total, by = c('LTLA_code' = 'Code')) %>% 
  mutate(Rate_age_known_per_100000 = pois.exact(Total_where_age_known, Population)[[3]]*100000) %>% 
  mutate(Rate_age_known_lcl = pois.exact(Total_where_age_known, Population)[[4]]*100000) %>% 
  mutate(Rate_age_known_ucl = pois.exact(Total_where_age_known, Population)[[5]]*100000) %>% 
  rename(Name = LTLA_name)

vaccine_df_ltla_70_plus <- vaccine_df_ltla %>% 
  pivot_longer(cols =  c(Under_70, `70_74`, `75_79`, `80_and_over`),
               names_to = 'Ages',
               values_to = 'Individuals') %>% 
  mutate(Age_banding = ifelse(Ages %in% c('70_74', '75_79', '80_and_over'), '70 and over', Ages)) %>% 
  group_by(LTLA_code, Age_banding) %>% 
  summarise(Individuals = sum(Individuals)) %>% 
  ungroup() %>% 
  filter(Age_banding == '70 and over') %>% 
  left_join(mye_ages_70_plus, by = c('LTLA_code' = 'Code')) %>% 
  mutate(Rate_70_plus_per_100000 = pois.exact(Individuals, Population_70plus)[[3]]*100000) %>% 
  mutate(Rate_70_plus_known_lcl = pois.exact(Individuals, Population_70plus)[[4]]*100000) %>% 
  mutate(Rate_70_plus_known_ucl = pois.exact(Individuals, Population_70plus)[[5]]*100000) %>% 
  rename(Individuals_70_plus = Individuals) %>% 
  mutate(Proportion_70_plus = Individuals_70_plus/ Population_70plus) %>% 
  select(LTLA_code, Individuals_70_plus, Rate_70_plus_per_100000, Proportion_70_plus)

vaccine_df_ltla <- vaccine_df_ltla %>% 
  left_join(vaccine_df_ltla_70_plus, by = 'LTLA_code')
  
vaccine_df_wsx <-vaccine_df_ltla %>% 
  filter(Name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>%
  select(Total_where_age_known, Individuals_70_plus) %>% 
  summarise(Total_where_age_known = sum(Total_where_age_known),
            Individuals_70_plus = sum(Individuals_70_plus)) %>% 
  mutate(Name = 'West Sussex',
         Code = 'E10000032') %>% 
  left_join(mye_total, by = 'Code') %>% 
  mutate(Rate_age_known_per_100000 = pois.exact(Total_where_age_known, Population)[[3]]*100000) %>% 
  mutate(Rate_age_known_lcl = pois.exact(Total_where_age_known, Population)[[4]]*100000) %>% 
  mutate(Rate_age_known_ucl = pois.exact(Total_where_age_known, Population)[[5]]*100000) %>% 
  left_join(mye_ages_70_plus, by = 'Code') %>% 
  mutate(Rate_70_plus_per_100000 = pois.exact(Individuals_70_plus, Population_70plus)[[3]]*100000) %>% 
  mutate(Proportion_70_plus = Individuals_70_plus/ Population_70plus)

vaccine_df_ltla %>% 
  filter(Name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>% 
  bind_rows(vaccine_df_wsx) %>% 
  select(Name, Total_where_age_known, Rate_age_known_per_100000, Individuals_70_plus, Rate_70_plus_per_100000, Proportion_70_plus) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_at_a_glance.json'))

lad_boundary <- geojson_read('https://opendata.arcgis.com/datasets/69cd46d7d2664e02b30c2f8dcc2bfaf7_0.geojson', what = 'sp') %>% 
  filter(LAD19NM %in% c('Brighton and Hove','Eastbourne', 'Hastings', 'Lewes','Rother','Wealden','Adur', 'Arun', 'Chichester', 'Crawley','Horsham','Mid Sussex', 'Worthing')) %>% 
  fortify(region = "LAD19CD") %>%
  rename(LAD19CD = id) %>%
  filter(substr(LAD19CD, 1,1 ) == 'E') %>% 
  left_join(vaccine_df_ltla, by = c('LAD19CD' = 'LTLA_code'))

# This will read in the boundaries (in a geojson format) from Open Geography Portal
County_boundary <- geojson_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson",  what = "sp") %>% 
  filter(ctyua19nm %in% c('West Sussex', 'East Sussex', 'Brighton and Hove')) %>%
  spTransform(CRS("+init=epsg:4326"))

ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = lad_boundary,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = Rate_age_known_per_100000),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_viridis_c(option = "plasma", 
                       label = comma,
                       name = 'Rate (where age known)\nper 100,000 population\n(all ages)') +
  geom_polygon(data = County_boundary,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = '#000000',
               fill = NA,
               size = .2) +
  labs(title = paste0('Cumulative rate of individuals receiving at least one dose of a COVID-19 vaccine per 100,000 population (all ages);\n Includes inviduals where age was recorded; Lower Tier Local and Unitary Authorities'),
       subtitle = paste0('Data as at ', vaccine_published_date))  +
  theme(legend.position = 'right')

ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = lad_boundary,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = Individuals_70_plus),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_viridis_c(option = "plasma", 
                       label = comma,
                       name = 'Rate (where age known)\nper 100,000 population\n(aged 70 and over)') +
  geom_polygon(data = County_boundary,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = '#000000',
               fill = NA,
               size = .2) +
  labs(title = paste0('Cumulative rate of individuals aged 70+ receiving at least one dose of a COVID-19 vaccine per 100,000 population (aged 70+);\n; Includes inviduals where age was recorded; Lower Tier Local and Unitary Authorities'),
       subtitle = paste0('Data as at ', vaccine_published_date))  +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.position = 'bottom')

# MSOA vaccine data ####

msoa_names <- read_csv('https://visual.parliament.uk/msoanames/static/MSOA-Names-Latest.csv') %>% 
  select(msoa11cd, msoa11hclnm, Laname) 

vaccine_df_msoa <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
                              sheet = 'Vaccinations by MSOA',
                              skip = 15,
                              col_names = c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name', 'msoa11cd', 'msoa11nm', 'Under_70', '70_74', '75_79', '80_and_over')) %>% 
  filter(!is.na(Region_name)) %>% 
  mutate(Total_where_age_known = Under_70 + `70_74` + `75_79` + `80_and_over`) %>% 
  left_join(msoa_names[c('msoa11cd', 'msoa11hclnm')], by = 'msoa11cd') %>% 
  filter(LTLA_name %in% c('Brighton and Hove','Eastbourne', 'Hastings', 'Lewes','Rother','Wealden','Adur', 'Arun', 'Chichester', 'Crawley','Horsham','Mid Sussex', 'Worthing')) %>% 
  mutate(Total_banded = factor(ifelse(Total_where_age_known < 2000, 'Up to 1,999', ifelse(Total_where_age_known < 3000, '2,000-2,999', ifelse(Total_where_age_known < 4000, '3,000-3,999', ifelse(Total_where_age_known < 5000, '4,000-4,999',  ifelse(Total_where_age_known < 6000, '5,000-5,999',  ifelse(Total_where_age_known < 7000, '6,000-6,999', '7,000+')))))), levels = c('Up to 1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000-5,999', '6,000-6,999', '7,000+'))) %>% 
  arrange(msoa11cd)

MSOA_boundary <- geojson_read('https://github.com/psychty/wsx_covid_datapack_public/raw/master/Source%20files/failsafe_msoa_boundary.geojson', what = 'sp') %>% 
  filter(MSOA11CD %in% vaccine_df_msoa$msoa11cd) %>% 
  arrange(MSOA11CD)

df <- data.frame(ID = character())

# Get the IDs of spatial polygon
for (i in MSOA_boundary@polygons ) { df <- rbind(df, data.frame(ID = i@ID, stringsAsFactors = FALSE))  }
# and set rowname = ID
row.names(vaccine_df_msoa) <- df$ID
# Then use df as the second argument to the spatial dataframe conversion function:
MSOA_boundary <- SpatialPolygonsDataFrame(MSOA_boundary, vaccine_df_msoa)  

geojson_write(ms_simplify(geojson_json(MSOA_boundary), keep = 0.2), file = paste0(output_directory_x, '/msoa_covid_vaccine_latest.geojson'))

MSOA_boundary_gg <- MSOA_boundary %>%
  fortify(region = "MSOA11CD") %>%
  rename(msoa11cd = id) %>%
  left_join(vaccine_df_msoa, by = 'msoa11cd')

ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = MSOA_boundary_gg,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = Total_banded),
               color="#5d5d5d",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_manual(name = 'Number of individuals\nreceiving at least\none dose',
                    values = c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#0c2c84'),
                    drop = FALSE) +
  geom_polygon(data = County_boundary,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = '#000000',
               fill = NA,
               size = .2) +
  labs(title = paste0('Cumulative number of individuals receiving at least Covid-19 vaccination dose;\nSussex MSOAs;'),
       subtitle =paste0('Data as at ', vaccine_published_date))  +
  # guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.position = 'right')

