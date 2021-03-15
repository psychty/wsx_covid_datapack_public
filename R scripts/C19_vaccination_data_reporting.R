
library(easypackages)

libraries("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis', 'epitools', 'patchwork')

capwords = function(s, strict = FALSE) {
  cap = function(s) paste(toupper(substring(s, 1, 1)),
                          {s = substring(s, 2); if(strict) tolower(s) else s},sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}

bord_style <- fp_border(color = "black", style = "solid", width = .5)

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
areas_to_loop <- c('West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')

# Age is based on age on 31st March 2021 to align with JCVI priority assignments
map_theme = function(){
  theme( 
    legend.position = "left", 
    legend.key.size = unit(.95,"line"),
    legend.title = element_text(size = 9, face = 'bold'),
    plot.background = element_blank(), 
    plot.title.position = "panel",
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

mye_nims_ltla <- read_csv(paste0(github_repo_dir, '/Source files/ltla_nims_pop_estimates.csv')) %>% 
  mutate(Population = Under_16 + Age_16_and_over) %>% 
  mutate(Population_65_and_over = Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over)

# LTLA vaccine data ####

vaccine_df_ltla <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
                              sheet = 'LTLA',
                              skip = 15,
                              col_names = c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name', 'Under_60', 'Age_60_64', 'Age_65_69', 'Age_70_74', 'Age_75_79', 'Age_80_and_over')) %>% 
  filter(!is.na(Region_name)) %>% 
  mutate(Total_where_age_known = Under_60 + Age_60_64 + Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
  mutate(Age_65_and_over = Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
  left_join(mye_nims_ltla[c('LTLA_code', 'Age_16_and_over', 'Population_65_and_over')], by = 'LTLA_code') %>% 
  mutate(Proportion_age_known = Total_where_age_known/ Age_16_and_over) %>% 
  mutate(Proportion_65_plus = Age_65_and_over/Population_65_and_over) %>% 
  # mutate(Rate_age_known_per_100000 = pois.exact(Total_where_age_known, Age_16_and_over)[[3]]*100000) %>% 
  # mutate(Rate_65_plus_per_100000 = pois.exact(Age_65_and_over, Population_65_and_over)[[3]]*100000) %>% 
  # select(LTLA_code, LTLA_name, Total_where_age_known, Rate_age_known_per_100000, Age_65_and_over, Rate_65_plus_per_100000, Age_16_and_over, Population_65_and_over)
  select(LTLA_code, LTLA_name, Total_where_age_known, Proportion_age_known, Age_65_and_over, Proportion_65_plus, Age_16_and_over, Population_65_and_over)

vaccine_df_wsx <- vaccine_df_ltla %>% 
  filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>%
  summarise(Total_where_age_known = sum(Total_where_age_known),
            Age_65_and_over = sum(Age_65_and_over),
            Age_16_and_over = sum(Age_16_and_over),
            Population_65_and_over = sum(Population_65_and_over)) %>% 
  mutate(LTLA_name = 'West Sussex',
         LTLA_code = 'E10000032') %>%
  mutate(Proportion_age_known = Total_where_age_known/ Age_16_and_over) %>% 
  mutate(Proportion_65_plus = Age_65_and_over/Population_65_and_over) %>%
  # mutate(Rate_age_known_per_100000 = pois.exact(Total_where_age_known, Age_16_and_over)[[3]]*100000) %>% 
  # mutate(Rate_65_plus_per_100000 = pois.exact(Age_65_and_over, Population_65_and_over)[[3]]*100000)
  select(LTLA_code, LTLA_name, Total_where_age_known, Proportion_age_known, Age_65_and_over, Proportion_65_plus, Age_16_and_over, Population_65_and_over) 

vaccine_df_ltla %>% 
  filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>% 
  bind_rows(vaccine_df_wsx) %>% 
  rename(Name = 'LTLA_name') %>% 
  select(Name, Total_where_age_known, Proportion_age_known, Age_65_and_over, Proportion_65_plus) %>% 
  # select(Name, Total_where_age_known, Rate_age_known_per_100000, Age_65_and_over, Rate_65_plus_per_100000) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_at_a_glance.json'))

lad_boundary <- geojson_read('https://opendata.arcgis.com/datasets/69cd46d7d2664e02b30c2f8dcc2bfaf7_0.geojson', what = 'sp') %>% 
  filter(LAD19NM %in% c('Brighton and Hove','Eastbourne', 'Hastings', 'Lewes','Rother','Wealden','Adur', 'Arun', 'Chichester', 'Crawley','Horsham','Mid Sussex', 'Worthing')) %>% 
  fortify(region = "LAD19CD") %>%
  rename(LAD19CD = id) %>%
  filter(substr(LAD19CD, 1,1 ) == 'E') %>% 
  left_join(vaccine_df_ltla, by = c('LAD19CD' = 'LTLA_code')) %>% 
  mutate(Proportion_age_known_banded = factor(ifelse(Proportion_age_known < .3, 'Less than 30%', ifelse(Proportion_age_known < .4, '30-39%', ifelse(Proportion_age_known < .5, '40-49%', ifelse(Proportion_age_known < .6, '50-59%', ifelse(Proportion_age_known < .7, '60-69%', ifelse(Proportion_age_known < .8, '70-79%', ifelse(Proportion_age_known < .9, '80-89%', ifelse(Proportion_age_known < 1, '90-99%', '100% of estimated population')))))))), levels = c('Less than 30%', '30-39%', '40-49%', '50-59%', '60-69%', '70-79%', '80-89%', '90-99%', '100% of estimated population'))) %>% 
  mutate(Proportion_65_plus_banded = factor(ifelse(Proportion_65_plus < .8, 'Less than 80%', ifelse(Proportion_65_plus < .85, '80-84%', ifelse(Proportion_65_plus < .9, '85-89%', ifelse(Proportion_65_plus < .95, '90-94%', ifelse(Proportion_65_plus < 1, '95-99%', '100% of estimated population'))))), levels = c('Less than 80%', '80-84%', '85-89%', '90-94%', '95-99%', '100% of estimated population'))) 

County_boundary <- geojson_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson",  what = "sp") %>% 
  filter(ctyua19nm %in% c('West Sussex', 'East Sussex', 'Brighton and Hove')) %>%
  spTransform(CRS("+init=epsg:4326"))

vac_llim_prop_all_age <- round_any(min(lad_boundary$Proportion_age_known, na.rm = TRUE), .05, floor)
vac_ulim_prop_all_age <- round_any(max(lad_boundary$Proportion_age_known, na.rm = TRUE), .05, ceiling)

ltla_all_age_proportion <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = lad_boundary,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = Proportion_age_known),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_viridis_c(option = "viridis",
                       direction = -1,
                       breaks = seq(vac_llim_prop_all_age, vac_ulim_prop_all_age, .05),
                       limits = c(vac_llim_prop_all_age, vac_ulim_prop_all_age),
                       label = percent,
                       name = 'Proportion of population\n(where age known)\n(aged 16+)') +
  geom_polygon(data = County_boundary,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = '#630436',
               fill = NA,
               size = 1) +
  labs(title = paste0('Proportion of individuals receiving at least one dose of a COVID-19 vaccine (aged 16+);\nData as at ', vaccine_published_date),
       subtitle = paste0('Sussex Lower Tier Local and Unitary Authorities'),
       caption = 'Note: This excludes a small number of individuals where the age was not known.')  +
  theme(legend.position = c(.9,.1))

# all_age_prop_colours <- c("#440154", "#472D7B", "#3B528B", "#2C728E", "#21908C", "#27AD81", "#5DC863", "#AADC32", "#FDE725")
# 
# ltla_all_age_proportion <- ggplot() +
#   coord_fixed(1.5) +
#   map_theme() +
#   geom_polygon(data = lad_boundary,
#                aes(x=long,
#                    y=lat,
#                    group = group,
#                    fill = Proportion_age_known_banded),
#                color="#ffffff",
#                size = .1,
#                alpha = 1,
#                show.legend = TRUE) +
#   scale_fill_manual(values = all_age_prop_colours,
#                     breaks = levels(lad_boundary$Proportion_age_known_banded),
#                     drop = FALSE,
#                     name = 'Proportion of population\n(aged 16+)') +
#   geom_polygon(data = County_boundary,
#                aes(x = long,
#                    y = lat,
#                    group = group),
#                colour = '#630436',
#                fill = NA,
#                size = 1) +
#   labs(title = paste0('Proportion of individuals receiving at least one dose of a COVID-19 vaccine (aged 16+);\nData as at ', vaccine_published_date),
#        subtitle = paste0('Sussex Lower Tier Local and Unitary Authorities; Excludes inviduals where no age was recorded.'),
#        caption = 'Note: This excludes a small number of individuals where the age was not known.\nWe have used the population estimates provided by National Immunisation Management Service (NIMS) for those aged 16 and over.')  +
#     guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
#   theme(legend.position = 'bottom')

png(paste0(output_directory_x, '/Proportion_all_age_vaccination_ltla_latest.png'),
    width = 1280,
    height = 640,
    res = 110)
print(ltla_all_age_proportion + theme(plot.caption = element_text(hjust = 0)))
dev.off()

vac_llim_prop_65_plus <- round_any(min(lad_boundary$Proportion_65_plus, na.rm = TRUE), .05, floor)
vac_ulim_prop_65_plus <- round_any(max(lad_boundary$Proportion_65_plus, na.rm = TRUE), .05, ceiling)

ltla_65_plus_age_proportion <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = lad_boundary,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = Proportion_65_plus),
               color="#ffffff",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_viridis_c(option = "plasma",
                       direction = -1,
                       breaks = seq(vac_llim_prop_65_plus, vac_ulim_prop_65_plus, .025),
                       limits = c(vac_llim_prop_65_plus, vac_ulim_prop_65_plus),
                       label = percent,
                       name = 'Proportion of those aged\n65 years and over\n(where age known)') +
  geom_polygon(data = County_boundary,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = '#630436',
               fill = NA,
               size = 1) +
  labs(title = paste0('Proportion of individuals aged 65+ receiving at least one dose of a COVID-19 vaccine per 100,000 population (aged 65+);\nData as at ', vaccine_published_date),
       subtitle = paste0('Sussex Lower Tier Local and Unitary Authorities'),
       caption = paste0('Note: This excludes a smaller number of individuals where the age was not known.'))  +
  # guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.position = c(.9, .1))

# ltla_65_plus_age_proportion <- ggplot() +
#   coord_fixed(1.5) +
#   map_theme() +
#   geom_polygon(data = lad_boundary,
#                aes(x=long,
#                    y=lat,
#                    group = group,
#                    fill = Proportion_65_plus_banded),
#                color="#ffffff",
#                size = .1,
#                alpha = 1,
#                show.legend = TRUE) +
#   scale_fill_manual(values = all_age_prop_colours,
#                     breaks = levels(lad_boundary$Proportion_65_plus_banded),
#                        name = 'Proportion of those aged\n65 years and over') +
#   geom_polygon(data = County_boundary,
#                aes(x = long,
#                    y = lat,
#                    group = group),
#                colour = '#630436',
#                fill = NA,
#                size = 1) +
#   labs(title = paste0('Proportion of individuals aged 65+ receiving at least one dose of a COVID-19 vaccine per 100,000 population (aged 65+);\nData as at ', vaccine_published_date),
#        subtitle = paste0('Sussex Lower Tier Local and Unitary Authorities; Includes inviduals where age was recorded.'),
#        caption = 'Note: This excludes a small number of individuals where the age was not known.\nWe have used the population estimates provided by National Immunisation Management Service (NIMS) for those aged 16 and over.')  +
#   guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
#   theme(legend.position = 'bottom')

png(paste0(output_directory_x, '/Proportion_age_65_plus_vaccination_ltla_latest.png'),
    width = 1280,
    height = 640,
    res = 110)
print(ltla_65_plus_age_proportion + theme(plot.caption = element_text(hjust = 0)))
dev.off()

png(paste0(output_directory_x, '/Proportion_vaccination_ltla_latest.png'),
    width = 1280,
    height = 1080,
    res = 140)
(ltla_all_age_proportion + theme(legend.position = 'right')) / (ltla_65_plus_age_proportion + theme(legend.position = 'right'))
dev.off()

# MSOA vaccine data ####

mye_nims_msoa <- read_csv(paste0(github_repo_dir, '/Source files/msoa_nims_pop_estimates.csv')) %>% 
  mutate(Population = `Under_16` + `Age_16_and_over`) %>% 
  mutate(Population_65_and_over = Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over)

msoa_names <- read_csv('https://visual.parliament.uk/msoanames/static/MSOA-Names-Latest.csv') %>% 
  select(msoa11cd, msoa11hclnm, Laname) 

vaccine_df_msoa <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
                              sheet = 'MSOA',
                              skip = 15,
                              col_names = c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name', 'msoa11cd', 'msoa11nm', 'Under_60', 'Age_60_64', 'Age_65_69', 'Age_70_74', 'Age_75_79', 'Age_80_and_over')) %>% 
  filter(!is.na(Region_name)) %>% 
  mutate(Total_where_age_known = Under_60 + Age_60_64 + Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
  mutate(Age_65_and_over = Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
  left_join(msoa_names[c('msoa11cd', 'msoa11hclnm')], by = 'msoa11cd') %>% 
  filter(LTLA_name %in% c('Brighton and Hove','Eastbourne', 'Hastings', 'Lewes','Rother','Wealden','Adur', 'Arun', 'Chichester', 'Crawley','Horsham','Mid Sussex', 'Worthing')) %>% 
  mutate(Total_banded = factor(ifelse(Total_where_age_known < 1000, 'Up to 999', ifelse(Total_where_age_known < 2000, '1,000-1,999', ifelse(Total_where_age_known < 3000, '2,000-2,999', ifelse(Total_where_age_known < 4000, '3,000-3,999', ifelse(Total_where_age_known < 5000, '4,000-4,999',  ifelse(Total_where_age_known < 6000, '5,000-5,999',  ifelse(Total_where_age_known < 7000, '6,000-6,999', '7,000+'))))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000-5,999', '6,000-6,999', '7,000+'))) %>% 
  mutate(Total_age_65_banded = factor(ifelse(Age_65_and_over < 1000, 'Up to 999', ifelse(Age_65_and_over < 2000, '1,000-1,999', ifelse(Age_65_and_over < 3000, '2,000-2,999', ifelse(Age_65_and_over < 4000, '3,000-3,999', ifelse(Age_65_and_over < 5000, '4,000-4,999',  '5,000+'))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000+'))) %>%
  arrange(msoa11cd) %>% 
  left_join(mye_nims_msoa[c('msoa11cd', 'Age_16_and_over', 'Population_65_and_over')], by = 'msoa11cd') %>% 
  mutate(Proportion_age_known = Total_where_age_known/ Age_16_and_over) %>% 
  mutate(Proportion_65_plus = Age_65_and_over/Population_65_and_over) %>% 
  # mutate(Rate_age_known_per_100000 = pois.exact(Total_where_age_known, Age_16_and_over)[[3]]*100000) %>% 
  # mutate(Rate_65_plus_per_100000 = pois.exact(Age_65_and_over, Population_65_and_over)[[3]]*100000) %>% 
  # select(LTLA_code, LTLA_name, Total_where_age_known, Rate_age_known_per_100000, Age_65_and_over, Rate_65_plus_per_100000, Age_16_and_over, Population_65_and_over)
  select(msoa11cd, msoa11nm, LTLA_name, Total_where_age_known, Proportion_age_known, Total_banded, Total_age_65_banded, Age_65_and_over, Proportion_65_plus, Age_16_and_over, Population_65_and_over) %>% 
  mutate(Proportion_age_known_banded = factor(ifelse(Proportion_age_known < .1, 'Less than 10%', ifelse(Proportion_age_known < .2, '10-19%',ifelse(Proportion_age_known < .3, '20-29%', ifelse(Proportion_age_known < .4, '30-39%', ifelse(Proportion_age_known < .5, '40-49%', ifelse(Proportion_age_known < .6, '50-59%', ifelse(Proportion_age_known < .7, '60-69%', ifelse(Proportion_age_known < .8, '70-79%', ifelse(Proportion_age_known < .9, '80-89%', ifelse(Proportion_age_known < 1, '90-99%', '100% of estimated population')))))))))), levels = c('Less than 10%', '10-19%', '20-29%', '30-39%', '40-49%', '50-59%', '60-69%', '70-79%', '80-89%', '90-99%', '100% of estimated population'))) %>% 
  mutate(Proportion_65_plus_banded = factor(ifelse(Proportion_65_plus < .7, 'Less than 70%', ifelse(Proportion_65_plus < .75, '70-74%', ifelse(Proportion_65_plus < .8, '75-79%',ifelse(Proportion_65_plus < .85, '80-84%', ifelse(Proportion_65_plus < .9, '85-89%', ifelse(Proportion_65_plus < .95, '90-94%', ifelse(Proportion_65_plus < 1, '95-99%', '100% of estimated population'))))))), levels = c('Less than 70%', '70-74%', '75-79%', '80-84%', '85-89%', '90-94%', '95-99%', '100% of estimated population'))) 

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

geojson_write(ms_simplify(geojson_json(MSOA_boundary), keep = 0.6), file = paste0(output_directory_x, '/msoa_covid_vaccine_latest.geojson'))

MSOA_boundary_gg <- geojson_read('https://github.com/psychty/wsx_covid_datapack_public/raw/master/Source%20files/failsafe_msoa_boundary.geojson', what = 'sp') %>% 
  filter(MSOA11CD %in% vaccine_df_msoa$msoa11cd) %>% 
  arrange(MSOA11CD) %>% 
  fortify(region = "MSOA11CD") %>%
  rename(MSOA11CD = id) %>%
  left_join(vaccine_df_msoa, by = c('MSOA11CD' = 'msoa11cd'))

msoa_total_so_far <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = MSOA_boundary_gg,
               aes(x=long,
                   y=lat,
                   group = MSOA11CD,
                   fill = Total_banded),
               color="#5d5d5d",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_manual(name = 'Number of individuals receiving at least\none dose (where age known)',
                    values = c('#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#08589e'),
                    drop = FALSE) +
  geom_polygon(data = County_boundary,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = '#630436',
               fill = NA,
               size = 1) +
  labs(title = paste0('Cumulative number of individuals receiving at least Covid-19 vaccination dose;\nData as at ', vaccine_published_date),
       subtitle =paste0('Sussex Middle layer Super Output Areas (MSOAs)'),
       caption = 'Note: This excludes a small number of individuals where the age was not known.')  +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme(legend.position = c(.85,.1))

png(paste0(output_directory_x, '/Number_at_least_one_dose_vaccination_msoa_latest.png'),
    width = 1280,
    height = 640,
    res = 100)
print(msoa_total_so_far)
dev.off()

all_age_prop_colours <- c("#440154", "#482576", "#414487", "#35608D", "#2A788E", "#21908C", "#22A884", "#43BF71", "#7AD151", "#BBDF27", "#FDE725")

msoa_all_age_proportion <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = MSOA_boundary_gg,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = Proportion_age_known_banded),
               color="#000000",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_manual(values = all_age_prop_colours,
                    breaks = rev(levels(MSOA_boundary_gg$Proportion_age_known_banded)),
                    drop = FALSE,
                    name = 'Proportion of population\n(aged 16+)') +
  geom_polygon(data = County_boundary,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = '#630436',
               fill = NA,
               size = 1) +
  labs(title = paste0('Proportion of individuals receiving at least one dose of a COVID-19 vaccine (aged 16+);\nData as at ', vaccine_published_date),
       subtitle = paste0('Sussex Middle layer Super Output Areas (MSOAs)'),
       caption = 'Note: This excludes a small number of individuals where the age was not known.\nWe have used the population estimates provided by National Immunisation Management Service (NIMS)\nfor those aged 16 and over.')  +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = 'bottom')

png(paste0(output_directory_x, '/Proportion_at_least_one_dose_vaccination_msoa_latest.png'),
    width = 1280,
    height = 640,
    res = 100)
print(msoa_all_age_proportion)
dev.off()

msoa_total_age_65_plus_so_far <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = MSOA_boundary_gg,
               aes(x=long,
                   y=lat,
                   group = MSOA11CD,
                   fill = Total_age_65_banded),
               color="#000000",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_manual(name = 'Number of individuals aged 65 and over\nreceiving at least one dose',
                    values = c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
                    drop = FALSE) +
  geom_polygon(data = County_boundary,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = '#630436',
               fill = NA,
               size = 1) +
  labs(title = paste0('Cumulative number of individuals aged 65 and over receiving at least Covid-19 vaccination dose;\nData as at ', vaccine_published_date),
       subtitle = paste0('Sussex Middle layer Super Output Areas (MSOAs)'),
       caption = 'Note: This excludes a small number of individuals where the age was not known.\nWe have used the population estimates provided by National Immunisation Management Service (NIMS)\n for those aged 65 and over.') +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme(legend.position = c(.85,.1))

png(paste0(output_directory_x, '/Number_at_least_one_dose_vaccination_65_plus_msoa_latest.png'),
    width = 1280,
    height = 640,
    res = 100)
print(msoa_total_age_65_plus_so_far)
dev.off()

age_65_prop_colours <- c("#0D0887", "#5402A3", "#8B0AA5", "#B93289", "#DB5C68", "#F48849", "#FEBC2A", "#F0F921")

msoa_age_65_proportion <- ggplot() +
  coord_fixed(1.5) +
  map_theme() +
  geom_polygon(data = MSOA_boundary_gg,
               aes(x=long,
                   y=lat,
                   group = group,
                   fill = Proportion_65_plus_banded),
               color="#000000",
               size = .1,
               alpha = 1,
               show.legend = TRUE) +
  scale_fill_manual(values = age_65_prop_colours,
                    breaks = rev(levels(MSOA_boundary_gg$Proportion_65_plus_banded)),
                    drop = FALSE,
                    name = 'Proportion of population\n(aged 65+)') +
  geom_polygon(data = County_boundary,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = '#000000',
               fill = NA,
               size = 1) +
  labs(title = paste0('Proportion of individuals aged 65 and over receiving at least one dose of a COVID-19 vaccine;\nData as at ', vaccine_published_date),
       subtitle = paste0('Sussex Middle layer Super Output Areas (MSOAs)'),
       caption = 'Note: This excludes a small number of individuals where the age was not known.\nWe have used the population estimates provided by National Immunisation Management Service (NIMS)\nfor those aged 65 and over.')  +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = 'bottom')


png(paste0(output_directory_x, '/Proportion_at_least_one_dose_vaccination_aged_65_plus_msoa_latest.png'),
    width = 1280,
    height = 640,
    res = 100)
print(msoa_age_65_proportion)
dev.off()

png(paste0(output_directory_x, '/Number_vaccination_msoa_latest.png'),
    width = 1280,
    height = 1080,
    res = 140)
(msoa_total_so_far + theme(legend.position = 'right')) / (msoa_total_age_65_plus_so_far + theme(legend.position = 'right'))
dev.off()

png(paste0(output_directory_x, '/Proportion_vaccination_msoa_latest.png'),
    width = 1280,
    height = 1080,
    res = 140)
(msoa_all_age_proportion + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right')) / (msoa_age_65_proportion + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right'))
dev.off()

png(paste0(output_directory_x, '/Vaccination_msoa_latest.png'),
    width = 1280,
    height = 1080,
    res = 140)
(msoa_total_so_far +  scale_fill_manual(name = 'Number of individuals\nreceiving at least\none dose (where age known)', values = rev(c('#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#08589e')), breaks = rev(levels(MSOA_boundary_gg$Total_banded)), drop = FALSE) + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right'))/(msoa_all_age_proportion + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right'))
dev.off()

png(paste0(output_directory_x, '/Vaccination_msoa_65_plus_latest.png'),
    width = 1280,
    height = 1080,
    res = 140)
(msoa_total_age_65_plus_so_far +  scale_fill_manual(name = 'Number of individuals\naged 65 and over\nreceiving at least one dose', values = rev(c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026')), breaks = rev(levels(MSOA_boundary_gg$Total_age_65_banded)), drop = FALSE) + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right'))/(msoa_age_65_proportion + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right'))
dev.off()

# What about age groups ####

mye_nims_ltla_age <- mye_nims_ltla %>% 
  select(!c('Population', 'Age_16_and_over', 'Population_65_and_over')) %>% 
  pivot_longer(cols = !c('LTLA_code', 'LTLA_name'), values_to = 'NIMS_population') %>% 
  rename(Age_group = 'name')

# LTLA vaccine data ####

vaccine_df_ltla_age <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
                              sheet = 'LTLA',
                              skip = 15,
                              col_names = c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name', 'Under_60', 'Age_60_64', 'Age_65_69', 'Age_70_74', 'Age_75_79', 'Age_80_and_over')) %>% 
  filter(!is.na(Region_name)) %>% 
  pivot_longer(cols = !c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name'), values_to = 'At_least_one_dose') %>% 
  left_join(mye_nims_ltla_age[c('LTLA_code', 'Age_16_and_over', 'Population_65_and_over')], by = 'LTLA_code')

# What about social care staff and residents ####

# Total eligible due to not having had COVID-19 in last 28 days

vaccine_df_utla_asc <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
                                  sheet = 'Care Homes Vaccinations by UTLA',
                                  skip = 16,
                                  col_names = c('Name', 'Null_1','Total_residents', 'Total_eligible_residents', 'Number_eligible_residents_vaccinated_dose_one', 'Proportion_eligible_residents_vaccinated_dose_one', 'Null_2', 'Total_staff', 'Total_eligible_staff', 'Number_staff_vaccinatied_dose_one', 'Proportion_eligible_staff_vaccinated_dose_one')) %>% 
  select(!c('Null_1', 'Null_2')) %>% 
  filter(!is.na(Total_residents)) 

vaccine_df_utla_asc %>% 
  filter(Name %in% c('West Sussex', 'East Sussex', 'Brighton and Hove'))
                                  