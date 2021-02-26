
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

vaccine_published_date <- as.Date('2021-02-25')

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
  mutate(Rate_age_known_ucl = pois.exact(Total_where_age_known, Population)[[5]]*100000) 

vaccine_df_ltla %>% 
  filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'))

lad_boundary <- geojson_read('https://opendata.arcgis.com/datasets/69cd46d7d2664e02b30c2f8dcc2bfaf7_0.geojson', what = 'sp') %>% 
  filter(LAD19NM %in% c('Brighton and Hove','Eastbourne', 'Hastings', 'Lewes','Rother','Wealden','Adur', 'Arun', 'Chichester', 'Crawley','Horsham','Mid Sussex', 'Worthing')) %>% 
  fortify(region = "LAD19CD") %>%
  rename(LAD19CD = id) %>%
  filter(substr(LAD19CD, 1,1 ) == 'E') %>% 
  left_join(vaccine_df_ltla, by = c('LAD19CD' = 'LTLA_code'))

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
                       trans = "sqrt",
                       label = comma,
                       name = 'Rate (where age known)\nper 100,000 population\n(all ages)') +
  geom_polygon(data = County_boundary,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = '#000000',
               fill = NA,
               size = .2) +
  labs(title = paste0('Cumulative rate of individuals receiving at least one dose of a COVID-19 vaccine per 100,000 population (all ages);\n; Includes inviduals where age was recorded; Lower Tier Local and Unitary Authorities'),
       subtitle = paste0('Data as at ', format(vaccine_published_date, '%d %B %Y')))  +
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
  left_join(msoa_names[c('msoa11cd', 'msoa11hclnm')], by = 'msoa11cd')

# This will read in the boundaries (in a geojson format) from Open Geography Portal
County_boundary <- geojson_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson",  what = "sp") %>% 
  filter(ctyua19nm %in% c('West Sussex', 'East Sussex', 'Brighton and Hove')) %>%
  spTransform(CRS("+init=epsg:4326"))


MSOA_boundary <- geojson_read('https://github.com/psychty/wsx_covid_datapack_public/raw/master/Source%20files/failsafe_msoa_boundary.geojson', what = 'sp') %>% 
  filter(MSOA11CD %in% vaccine_df_msoa$msoa11cd)


constituency_boundary <- geojson_read('https://opendata.arcgis.com/datasets/102b0e202e934fc7a13e7906e77b7a1e_0.geojson', what = 'sp')
%>% 
  
view(constituency_boundary)

MSOA_boundary_gg <- MSOA_boundary %>%
  patched_fortify(region = "MSOA11CD") %>% 
  rename(msoa11cd = id)

MSOA_cumulative_map <- MSOA_boundary_gg %>% 
  left_join(vaccine_msoa_cumulative_doses, by = 'msoa11cd')

ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = MSOA_cumulative_map,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = Dose_1_cumulative),
               color="#5d5d5d",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  geom_polygon(data = County_boundary,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = '#000000',
               fill = NA,
               size = .2) +
  labs(title = paste0('Cumulative number of first dose Covid-19 vaccinations administered;\nSussex MSOAs;'),
       subtitle =paste0('Data as at ', format(date_vaccine_latest, '%d %B %Y'), ' for vaccines given up to ', format(date_vaccine_latest - 2, '%d %B %Y')))  +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.position = 'bottom')
