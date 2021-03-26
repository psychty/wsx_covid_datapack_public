
# Using the methods described here: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833947/IoD2019_Research_Report.pdf aggregated LSOA scores to MSOA geographies for use with data published at this level.


# Sum the population-weighted scores within each MSOA area and rank them

library(easypackages)

libraries("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis', 'epitools', 'patchwork', 'lemon')

github_repo_dir <- '~/GitHub/wsx_covid_datapack_public'

lookup <- read_csv('https://opendata.arcgis.com/datasets/65664b00231444edb3f6f83c9d40591f_0.csv') %>% 
  select(LSOA11CD, MSOA11CD, MSOA11NM) %>% 
  unique()

IMD_2019 <- read_csv('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv') %>% 
  select("LSOA code (2011)",  "Local Authority District name (2019)", "Index of Multiple Deprivation (IMD) Score",  "Total population: mid 2015 (excluding prisoners)" ) %>% 
  rename(LSOA11CD = 'LSOA code (2011)',
         LTLA = 'Local Authority District name (2019)',
         IMD_2019_score = 'Index of Multiple Deprivation (IMD) Score',
         Population = 'Total population: mid 2015 (excluding prisoners)') %>%
  mutate(Pop_weighted_score = IMD_2019_score * Population) %>% 
  left_join(lookup, by = 'LSOA11CD') %>% 
  group_by(MSOA11CD) %>% 
  summarise(Pop_weighted_imd_score = sum(Pop_weighted_score),
            Population = sum(Population)) %>% 
  mutate(Pop_weighted_imd_score = Pop_weighted_imd_score / Population) %>% 
  arrange(desc(Pop_weighted_imd_score)) %>% 
  mutate(Pop_weighted_rank = rank(desc(Pop_weighted_imd_score))) %>% 
  left_join(read_csv('https://visual.parliament.uk/msoanames/static/MSOA-Names-Latest.csv')[c('msoa11cd', 'msoa11hclnm')], by = c('MSOA11CD' = 'msoa11cd')) %>% 
  select(MSOA11CD, msoa11hclnm, Population, Pop_weighted_imd_score, Pop_weighted_rank) %>% 
  mutate(Pop_weighted_decile = abs(ntile(Pop_weighted_imd_score, 10) - 11)) 

IMD_2019 %>% 
  write.csv(., paste0(github_repo_dir,'/Source files/msoa_pop_weighted_imd.csv'), row.names = FALSE)


