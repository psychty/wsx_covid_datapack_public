library(easypackages)

libraries("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis', 'epitools', 'patchwork', 'lemon', 'PostcodesioR')

capwords = function(s, strict = FALSE) {aa
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

# github_repo_dir <- "~/Documents/GitHub/wsx_covid_datapack_public"
github_repo_dir <- '~/GitHub/wsx_covid_datapack_public'
output_directory_x <- paste0(github_repo_dir, '/Outputs')
areas_to_loop <- c('West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')

mobile_github_repo_dir <- "~/GitHub/wsx_covid_public_mobile_site"
mobile_output_directory_x <- paste0(mobile_github_repo_dir, '/Outputs')

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

msoa_names <- read_csv('https://visual.parliament.uk/msoanames/static/MSOA-Names-Latest.csv') %>% 
  select(msoa11cd, msoa11hclnm, Laname) 

imd_msoa <- read_csv(paste0(github_repo_dir,'/Source files/msoa_pop_weighted_imd.csv')) %>% 
  left_join(msoa_names[c('msoa11cd', 'Laname')], by = c('MSOA11CD' = 'msoa11cd')) %>%  filter(Laname %in% c('Brighton and Hove', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden')) %>% 
  arrange(desc(Pop_weighted_imd_score)) %>% 
  mutate(Rank_in_Sussex = rank(desc(Pop_weighted_imd_score))) %>% 
  mutate(Decile_in_Sussex = abs(ntile(Pop_weighted_imd_score, 10) - 11))  %>% 
  rename(National_pop_weighted_rank = Pop_weighted_rank,
         National_pop_weighted_decile = Pop_weighted_decile) %>% 
  left_join(read_csv('https://opendata.arcgis.com/datasets/0af77205a4644af3af2175d21a4ef4db_0.csv')[c('MSOA11CD', 'RUC11CD', 'RUC11')], by = 'MSOA11CD') %>% 
  select(!Laname)

# imd_msoa %>% 
#   group_by(Laname, RUC11) %>% 
#   summarise(Areas = n()) %>% 
#   pivot_wider(names_from = RUC11, values_from = Areas) %>% 
#   view()

IMD_2019 <- read_csv('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv') %>% 
  select("LSOA code (2011)",  "Local Authority District name (2019)", "Index of Multiple Deprivation (IMD) Score", "Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)", "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)") %>% 
  rename(lsoa_code = 'LSOA code (2011)',
         LTLA = 'Local Authority District name (2019)',
         IMD_2019_score = 'Index of Multiple Deprivation (IMD) Score',
         IMD_2019_rank = "Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)", 
         IMD_2019_decile = "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)") %>% 
  mutate(IMD_2019_decile = factor(ifelse(IMD_2019_decile == 1, '10% most deprived',  ifelse(IMD_2019_decile == 2, 'Decile 2',  ifelse(IMD_2019_decile == 3, 'Decile 3',  ifelse(IMD_2019_decile == 4, 'Decile 4',  ifelse(IMD_2019_decile == 5, 'Decile 5',  ifelse(IMD_2019_decile == 6, 'Decile 6',  ifelse(IMD_2019_decile == 7, 'Decile 7',  ifelse(IMD_2019_decile == 8, 'Decile 8',  ifelse(IMD_2019_decile == 9, 'Decile 9',  ifelse(IMD_2019_decile == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  filter(LTLA %in% c('Brighton and Hove', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden')) %>% 
  arrange(desc(IMD_2019_score)) %>% 
  mutate(Rank_in_Sussex = rank(desc(IMD_2019_score))) %>% 
  mutate(Decile_in_Sussex = abs(ntile(IMD_2019_score, 10) - 11)) %>% 
  mutate(Decile_in_Sussex = factor(ifelse(Decile_in_Sussex == 1, '10% most deprived',  ifelse(Decile_in_Sussex == 2, 'Decile 2',  ifelse(Decile_in_Sussex == 3, 'Decile 3',  ifelse(Decile_in_Sussex == 4, 'Decile 4',  ifelse(Decile_in_Sussex == 5, 'Decile 5',  ifelse(Decile_in_Sussex == 6, 'Decile 6',  ifelse(Decile_in_Sussex == 7, 'Decile 7',  ifelse(Decile_in_Sussex == 8, 'Decile 8',  ifelse(Decile_in_Sussex == 9, 'Decile 9',  ifelse(Decile_in_Sussex == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>%   mutate(UTLA = ifelse(LTLA %in% c('Brighton and Hove'),'Brighton and Hove', ifelse(LTLA %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'), 'West Sussex', ifelse(LTLA %in% c('Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden'), 'East Sussex', NA)))) %>% 
  group_by(UTLA) %>% 
  arrange(UTLA, desc(IMD_2019_score)) %>% 
  mutate(Rank_in_UTLA = rank(desc(IMD_2019_score))) %>% 
  mutate(Decile_in_UTLA = abs(ntile(IMD_2019_score, 10) - 11)) %>% 
  mutate(Decile_in_UTLA = factor(ifelse(Decile_in_UTLA == 1, '10% most deprived',  ifelse(Decile_in_UTLA == 2, 'Decile 2',  ifelse(Decile_in_UTLA == 3, 'Decile 3',  ifelse(Decile_in_UTLA == 4, 'Decile 4',  ifelse(Decile_in_UTLA == 5, 'Decile 5',  ifelse(Decile_in_UTLA == 6, 'Decile 6',  ifelse(Decile_in_UTLA == 7, 'Decile 7',  ifelse(Decile_in_UTLA == 8, 'Decile 8',  ifelse(Decile_in_UTLA == 9, 'Decile 9',  ifelse(Decile_in_UTLA == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  mutate(UTLA = ifelse(LTLA %in% c('Brighton and Hove'),'Brighton and Hove', ifelse(LTLA %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'), 'West Sussex', ifelse(LTLA %in% c('Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden'), 'East Sussex', NA)))) %>% 
  rename(LSOA11CD = lsoa_code) %>% 
  arrange(LSOA11CD)

if(file.exists(paste0(output_directory_x, '/lsoa_deprivation_2019_sussex.geojson')) == FALSE){

# Read in the lsoa geojson boundaries for our lsoas (actually this downloads all 30,000+ and then we filter)
lsoa_spdf <- geojson_read('https://opendata.arcgis.com/datasets/8bbadffa6ddc493a94078c195a1e293b_0.geojson',  what = "sp") %>%
  filter(LSOA11CD %in% IMD_2019$LSOA11CD) %>% 
  arrange(LSOA11CD)

df <- data.frame(ID = character())

# Get the IDs of spatial polygon
for (i in lsoa_spdf@polygons ) { df <- rbind(df, data.frame(ID = i@ID, stringsAsFactors = FALSE))  }

# and set rowname = ID
row.names(IMD_2019) <- df$ID

# Then use df as the second argument to the spatial dataframe conversion function:
lsoa_spdf_json <- SpatialPolygonsDataFrame(lsoa_spdf, IMD_2019)  

geojson_write(geojson_json(lsoa_spdf), file = paste0(output_directory_x, '/lsoa_deprivation_2019_sussex.geojson'))

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

substr(vaccine_published_date, 1, nchar(vaccine_published_date) - 5) %>% 
  toJSON() %>% 
  write_lines(paste0(mobile_output_directory_x, '/vaccine_update_date.json'))

as.character(vaccine_meta %>% 
               filter(item == 'Period:') %>% 
               select(description)) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_administered_date.json'))

as.character(vaccine_meta %>% 
               filter(item == 'Period:') %>% 
               select(description)) %>% 
  toJSON() %>% 
  write_lines(paste0(mobile_output_directory_x, '/vaccine_administered_date.json'))

County_boundary <- geojson_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson",  what = "sp") %>% 
  filter(ctyua19nm %in% c('West Sussex', 'East Sussex', 'Brighton and Hove')) %>%
  spTransform(CRS("+init=epsg:4326"))

# MSOA vaccine data ####
mye_nims_msoa <- read_csv(paste0(github_repo_dir, '/Source files/msoa_nims_pop_estimates.csv')) %>% 
  mutate(Population_12_and_over = Age_12_and_over) %>%
  mutate(Population_16_and_over = Age_16_and_over) %>% 
  mutate(Population_18_and_over = Age_18_and_over) %>% 
  mutate(Population_12_17 = Age_12_15 + Age_16_17) %>% 
  mutate(Population_12_49 = Age_12_15 + Age_16_17 + Age_18_24 + Age_25_29 + Age_30_34 + Age_35_39 + Age_40_44 + Age_45_49) %>% 
  mutate(Population_50_and_over = Age_50_54 + Age_55_59 + Age_60_64 + Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
  mutate(Population_65_and_over = Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
  mutate(Population_18_64 = Age_18_24 + Age_25_29 + Age_30_34 + Age_35_39 + Age_40_44 + Age_45_49 + Age_50_54 + Age_55_59 + Age_60_64)

vaccine_df_msoa <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
                              sheet = 'MSOA',
                              skip = 15,
                              col_names = c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name', 'msoa11cd', 'msoa11nm', 'First_dose_12_15', 'First_dose_16_17',  'First_dose_age_18_24', 'First_dose_age_25_29', 'First_dose_age_30_34', 'First_dose_age_35_39', 'First_dose_age_40_44', 'First_dose_age_45_49', 'First_dose_age_50_54', 'First_dose_age_55_59', 'First_dose_age_60_64', 'First_dose_age_65_69', 'First_dose_age_70_74', 'First_dose_age_75_79', 'First_dose_age_80_and_over', 'Null_1', 'Second_dose_under_18', 'Second_dose_age_18_24',  'Second_dose_age_25_29', 'Second_dose_age_30_34', 'Second_dose_age_35_39', 'Second_dose_age_40_44', 'Second_dose_age_45_49', 'Second_dose_age_50_54', 'Second_dose_age_55_59', 'Second_dose_age_60_64', 'Second_dose_age_65_69', 'Second_dose_age_70_74', 'Second_dose_age_75_79', 'Second_dose_age_80_and_over', 'Null_2','Third_dose_under_50', 'Third_dose_50_54', 'Third_dose_55_59', 'Third_dose_60_64', 'Third_dose_65_69', 'Third_dose_70_74', 'Third_dose_75_79', 'Third_dose_80_and_over', 'Null_3','Total_doses')) %>% 
  filter(!is.na(Region_name)) %>% 
  mutate(Age_12_and_over = as.numeric(First_dose_12_15) + as.numeric(First_dose_16_17) + as.numeric(First_dose_age_18_24) + First_dose_age_25_29 + First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64 + First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
  mutate(Age_16_and_over =  as.numeric(First_dose_16_17) + as.numeric(First_dose_age_18_24) + First_dose_age_25_29 + First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64 + First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
  mutate(Age_18_and_over =  as.numeric(First_dose_age_18_24) + First_dose_age_25_29 + First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64 + First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
  mutate(Age_50_and_over = First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64 + First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
  mutate(Age_65_and_over = First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
  mutate(Age_18_64 = as.numeric(First_dose_age_18_24) + First_dose_age_25_29 + First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64) %>% 
  left_join(imd_msoa, by = c('msoa11cd' = 'MSOA11CD')) %>% 
  select(!c(Region_code, Region_name, LTLA_code, msoa11nm, RUC11CD)) %>% 
  filter(LTLA_name %in% c('Brighton and Hove','Eastbourne', 'Hastings', 'Lewes','Rother','Wealden','Adur', 'Arun', 'Chichester', 'Crawley','Horsham','Mid Sussex', 'Worthing')) %>% 
  mutate(Age_12_and_over_banded = factor(ifelse(Age_12_and_over < 1000, 'Up to 999', ifelse(Age_12_and_over < 2000, '1,000-1,999', ifelse(Age_12_and_over < 3000, '2,000-2,999', ifelse(Age_12_and_over < 4000, '3,000-3,999', ifelse(Age_12_and_over < 5000, '4,000-4,999',  ifelse(Age_12_and_over < 6000, '5,000-5,999',  ifelse(Age_12_and_over < 7000, '6,000-6,999', '7,000+'))))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000-5,999', '6,000-6,999', '7,000+'))) %>% 
  mutate(Age_16_and_over_banded = factor(ifelse(Age_16_and_over < 1000, 'Up to 999', ifelse(Age_16_and_over < 2000, '1,000-1,999', ifelse(Age_16_and_over < 3000, '2,000-2,999', ifelse(Age_16_and_over < 4000, '3,000-3,999', ifelse(Age_16_and_over < 5000, '4,000-4,999',  ifelse(Age_16_and_over < 6000, '5,000-5,999',  ifelse(Age_16_and_over < 7000, '6,000-6,999', '7,000+'))))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000-5,999', '6,000-6,999', '7,000+'))) %>% 
  mutate(Age_18_and_over_banded = factor(ifelse(Age_18_and_over < 1000, 'Up to 999', ifelse(Age_18_and_over < 2000, '1,000-1,999', ifelse(Age_18_and_over < 3000, '2,000-2,999', ifelse(Age_18_and_over < 4000, '3,000-3,999', ifelse(Age_18_and_over < 5000, '4,000-4,999',  '5,000+'))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000+'))) %>%
  mutate(Age_50_and_over_banded = factor(ifelse(Age_50_and_over < 1000, 'Up to 999', ifelse(Age_50_and_over < 2000, '1,000-1,999', ifelse(Age_50_and_over < 3000, '2,000-2,999', ifelse(Age_50_and_over < 4000, '3,000-3,999', ifelse(Age_50_and_over < 5000, '4,000-4,999',  '5,000+'))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000+'))) %>%
  mutate(Age_65_and_over_banded = factor(ifelse(Age_65_and_over < 1000, 'Up to 999', ifelse(Age_65_and_over < 2000, '1,000-1,999', ifelse(Age_65_and_over < 3000, '2,000-2,999', ifelse(Age_65_and_over < 4000, '3,000-3,999', ifelse(Age_65_and_over < 5000, '4,000-4,999',  '5,000+'))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000+'))) %>%
  mutate(Total_Age_18_64_banded = factor(ifelse(Age_18_64 < 1000, 'Up to 999', ifelse(Age_18_64 < 2000, '1,000-1,999', ifelse(Age_18_64 < 3000, '2,000-2,999', ifelse(Age_18_64 < 4000, '3,000-3,999', ifelse(Age_18_64 < 5000, '4,000-4,999',  '5,000+'))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000+'))) %>%
  arrange(msoa11cd) %>% 
  left_join(mye_nims_msoa[c('msoa11cd', 'Population_12_and_over', 'Population_16_and_over','Population_18_and_over',  'Population_50_and_over', 'Population_65_and_over', 'Population_18_64')], by = 'msoa11cd') %>% 
  mutate(Proportion_12_and_over = Age_12_and_over/ Population_12_and_over) %>% 
  mutate(Proportion_16_and_over = Age_16_and_over/Population_16_and_over) %>% 
  mutate(Proportion_18_and_over = Age_18_and_over/Population_18_and_over) %>% 
  mutate(Proportion_50_and_over = Age_18_and_over/Population_50_and_over) %>% 
  mutate(Proportion_65_and_over = Age_65_and_over/Population_65_and_over) %>% 
  mutate(Proportion_18_64 = Age_18_64/Population_18_64) %>% 
  mutate(Proportion_12_and_over_banded = factor(ifelse(Proportion_12_and_over < .1, 'Less than 10%', ifelse(Proportion_12_and_over < .2, '10-19%',ifelse(Proportion_12_and_over < .3, '20-29%', ifelse(Proportion_12_and_over < .4, '30-39%', ifelse(Proportion_12_and_over < .5, '40-49%', ifelse(Proportion_12_and_over < .6, '50-59%', ifelse(Proportion_12_and_over < .7, '60-69%', ifelse(Proportion_12_and_over < .8, '70-79%', ifelse(Proportion_12_and_over < .9, '80-89%', ifelse(Proportion_12_and_over < 1, '90-99%', '100% of estimated population')))))))))), levels = c('Less than 10%', '10-19%', '20-29%', '30-39%', '40-49%', '50-59%', '60-69%', '70-79%', '80-89%', '90-99%', '100% of estimated population'))) %>% 
  mutate(Proportion_16_and_over_banded = factor(ifelse(Proportion_16_and_over < .1, 'Less than 10%', ifelse(Proportion_16_and_over < .2, '10-19%',ifelse(Proportion_16_and_over < .3, '20-29%', ifelse(Proportion_16_and_over < .4, '30-39%', ifelse(Proportion_16_and_over < .5, '40-49%', ifelse(Proportion_16_and_over < .6, '50-59%', ifelse(Proportion_16_and_over < .7, '60-69%', ifelse(Proportion_16_and_over < .8, '70-79%', ifelse(Proportion_16_and_over < .9, '80-89%', ifelse(Proportion_16_and_over < 1, '90-99%', '100% of estimated population')))))))))), levels = c('Less than 10%', '10-19%', '20-29%', '30-39%', '40-49%', '50-59%', '60-69%', '70-79%', '80-89%', '90-99%', '100% of estimated population'))) %>% 
  mutate(Proportion_18_and_over_banded = factor(ifelse(Proportion_18_and_over < .7, 'Less than 70%', ifelse(Proportion_18_and_over < .75, '70-74%', ifelse(Proportion_18_and_over < .8, '75-79%',ifelse(Proportion_18_and_over < .85, '80-84%', ifelse(Proportion_18_and_over < .9, '85-89%', ifelse(Proportion_18_and_over < .95, '90-94%', ifelse(Proportion_18_and_over < 1, '95-99%', '100% of estimated population'))))))), levels = c('Less than 70%', '70-74%', '75-79%', '80-84%', '85-89%', '90-94%', '95-99%', '100% of estimated population'))) %>% 
  mutate(Proportion_50_and_over_banded = factor(ifelse(Proportion_50_and_over < .7, 'Less than 70%', ifelse(Proportion_50_and_over < .75, '70-74%', ifelse(Proportion_50_and_over < .8, '75-79%',ifelse(Proportion_50_and_over < .85, '80-84%', ifelse(Proportion_50_and_over < .9, '85-89%', ifelse(Proportion_50_and_over < .95, '90-94%', ifelse(Proportion_50_and_over < 1, '95-99%', '100% of estimated population'))))))), levels = c('Less than 70%', '70-74%', '75-79%', '80-84%', '85-89%', '90-94%', '95-99%', '100% of estimated population'))) %>%
  mutate(Proportion_65_and_over_banded = factor(ifelse(Proportion_65_and_over < .7, 'Less than 70%', ifelse(Proportion_65_and_over < .75, '70-74%', ifelse(Proportion_65_and_over < .8, '75-79%',ifelse(Proportion_65_and_over < .85, '80-84%', ifelse(Proportion_65_and_over < .9, '85-89%', ifelse(Proportion_65_and_over < .95, '90-94%', ifelse(Proportion_65_and_over < 1, '95-99%', '100% of estimated population'))))))), levels = c('Less than 70%', '70-74%', '75-79%', '80-84%', '85-89%', '90-94%', '95-99%', '100% of estimated population'))) %>%
  mutate(Proportion_18_64_banded = factor(ifelse(Proportion_18_64 < .7, 'Less than 70%', ifelse(Proportion_18_64 < .75, '70-74%', ifelse(Proportion_18_64 < .8, '75-79%',ifelse(Proportion_18_64 < .85, '80-84%', ifelse(Proportion_18_64 < .9, '85-89%', ifelse(Proportion_18_64 < .95, '90-94%', ifelse(Proportion_18_64 < 1, '95-99%', '100% of estimated population'))))))), levels = c('Less than 70%', '70-74%', '75-79%', '80-84%', '85-89%', '90-94%', '95-99%', '100% of estimated population'))) %>% 
  mutate(Estimated_left_to_vaccinate_12_and_over = Population_12_and_over - Age_12_and_over,
         Estimated_left_to_vaccinate_16_and_over = Population_16_and_over - Age_16_and_over,
         Estimated_left_to_vaccinate_18_and_over = Population_18_and_over - Age_18_and_over,
         Estimated_left_to_vaccinate_50_and_over = Population_50_and_over - Age_50_and_over,
         Estimated_left_to_vaccinate_65_and_over = Population_65_and_over - Age_65_and_over,
         Estimated_left_to_vaccinate_18_64 = Population_18_64 - Age_18_64) %>%
  select(msoa11cd, msoa11hclnm, LTLA_name, Age_12_and_over, Proportion_12_and_over, Age_12_and_over_banded, Proportion_12_and_over_banded, Estimated_left_to_vaccinate_12_and_over, Age_16_and_over, Proportion_16_and_over, Age_16_and_over_banded, Proportion_16_and_over_banded, Estimated_left_to_vaccinate_16_and_over, Age_18_and_over, Proportion_18_and_over, Age_18_and_over_banded, Proportion_18_and_over_banded, Estimated_left_to_vaccinate_18_and_over,Age_50_and_over, Proportion_50_and_over, Age_50_and_over_banded, Proportion_50_and_over_banded, Estimated_left_to_vaccinate_50_and_over, Age_65_and_over, Proportion_65_and_over, Age_65_and_over_banded, Proportion_65_and_over_banded, Estimated_left_to_vaccinate_65_and_over, Age_18_64, Proportion_18_64, Total_Age_18_64_banded, Proportion_18_64_banded, Estimated_left_to_vaccinate_18_64, Pop_weighted_imd_score, National_pop_weighted_rank, National_pop_weighted_decile, Rank_in_Sussex, Decile_in_Sussex, RUC11, Total_doses) 
  
vaccine_df_msoa %>% 
  group_by(LTLA_name) %>% 
  mutate(Proportion_rank_12_and_over_within_LTLA = rank(desc(Proportion_12_and_over))) %>% 
  mutate(Proportion_rank_16_and_over_within_LTLA = rank(desc(Proportion_16_and_over))) %>% 
  mutate(Proportion_rank_18_and_over_within_LTLA = rank(desc(Proportion_18_and_over))) %>% 
  mutate(Proportion_rank_50_and_over_within_LTLA = rank(desc(Proportion_50_and_over))) %>% 
  mutate(Proportion_rank_65_and_over_within_LTLA = rank(desc(Proportion_65_and_over))) %>% 
  mutate(Proportion_rank_18_64_within_LTLA = rank(desc(Proportion_18_64))) %>% 
  mutate(MSOA_name = paste0(msoa11hclnm, ' (', msoa11cd, ')')) %>% 
  select(LTLA_name, MSOA_name, Age_12_and_over, Proportion_rank_12_and_over_within_LTLA, Proportion_12_and_over, Estimated_left_to_vaccinate_12_and_over, Age_16_and_over, Proportion_rank_16_and_over_within_LTLA, Proportion_16_and_over, Estimated_left_to_vaccinate_16_and_over, Age_18_and_over, Proportion_rank_18_and_over_within_LTLA, Proportion_18_and_over, Estimated_left_to_vaccinate_18_and_over, Age_50_and_over, Proportion_rank_50_and_over_within_LTLA, Proportion_50_and_over, Estimated_left_to_vaccinate_50_and_over, Age_65_and_over, Proportion_rank_65_and_over_within_LTLA, Proportion_65_and_over, Estimated_left_to_vaccinate_65_and_over, Age_18_64, Proportion_rank_18_64_within_LTLA, Proportion_18_64, Estimated_left_to_vaccinate_18_64, Pop_weighted_imd_score, National_pop_weighted_decile, Decile_in_Sussex, National_pop_weighted_rank, Rank_in_Sussex, msoa11cd) %>% 
  mutate(LTLA_name_ns = gsub(' ', '_', LTLA_name)) %>% 
  mutate(National_pop_weighted_decile = factor(ifelse(National_pop_weighted_decile == 1, '10% most deprived',  ifelse(National_pop_weighted_decile == 2, 'Decile 2',  ifelse(National_pop_weighted_decile == 3, 'Decile 3',  ifelse(National_pop_weighted_decile == 4, 'Decile 4',  ifelse(National_pop_weighted_decile == 5, 'Decile 5',  ifelse(National_pop_weighted_decile == 6, 'Decile 6',  ifelse(National_pop_weighted_decile == 7, 'Decile 7',  ifelse(National_pop_weighted_decile == 8, 'Decile 8',  ifelse(National_pop_weighted_decile == 9, 'Decile 9',  ifelse(National_pop_weighted_decile == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  mutate(Decile_in_Sussex = factor(ifelse(Decile_in_Sussex == 1, '10% most deprived',  ifelse(Decile_in_Sussex == 2, 'Decile 2',  ifelse(Decile_in_Sussex == 3, 'Decile 3',  ifelse(Decile_in_Sussex == 4, 'Decile 4',  ifelse(Decile_in_Sussex == 5, 'Decile 5',  ifelse(Decile_in_Sussex == 6, 'Decile 6',  ifelse(Decile_in_Sussex == 7, 'Decile 7',  ifelse(Decile_in_Sussex == 8, 'Decile 8',  ifelse(Decile_in_Sussex == 9, 'Decile 9',  ifelse(Decile_in_Sussex == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  select(LTLA_name, MSOA_name, Age_12_and_over, Proportion_rank_12_and_over_within_LTLA, Proportion_12_and_over, Estimated_left_to_vaccinate_12_and_over, Age_16_and_over, Proportion_rank_16_and_over_within_LTLA, Proportion_16_and_over, Estimated_left_to_vaccinate_16_and_over, Age_18_64, Proportion_rank_18_64_within_LTLA, Proportion_18_64, Estimated_left_to_vaccinate_18_64, Age_65_and_over, Proportion_rank_65_and_over_within_LTLA,Proportion_65_and_over, Estimated_left_to_vaccinate_65_and_over, Pop_weighted_imd_score, Rank_in_Sussex, National_pop_weighted_rank, LTLA_name_ns) %>% 
  mutate(UTLA = ifelse(LTLA_name %in% c('Eastbourne', 'Hastings', 'Lewes','Rother','Wealden') , 'East Sussex', ifelse(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley','Horsham','Mid Sussex', 'Worthing'), 'West Sussex', LTLA_name))) %>% 
  arrange(LTLA_name, Proportion_rank_12_and_over_within_LTLA) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_msoa_explore_data.json'))

vaccine_df_msoa %>% 
 write.csv(. , paste0(output_directory_x, '/vaccine_msoa_data.csv'), row.names = FALSE)

MSOA_boundary <- geojson_read('https://github.com/psychty/wsx_covid_datapack_public/raw/master/Source%20files/failsafe_msoa_boundary.geojson', what = 'sp') %>% 
  filter(MSOA11CD %in% vaccine_df_msoa$msoa11cd) %>% 
  arrange(MSOA11CD)

plasma(11, direction = -1)

vaccine_df_msoa <- vaccine_df_msoa %>% 
  mutate(National_pop_weighted_decile = factor(ifelse(National_pop_weighted_decile == 1, '10% most deprived',  ifelse(National_pop_weighted_decile == 2, 'Decile 2',  ifelse(National_pop_weighted_decile == 3, 'Decile 3',  ifelse(National_pop_weighted_decile == 4, 'Decile 4',  ifelse(National_pop_weighted_decile == 5, 'Decile 5',  ifelse(National_pop_weighted_decile == 6, 'Decile 6',  ifelse(National_pop_weighted_decile == 7, 'Decile 7',  ifelse(National_pop_weighted_decile == 8, 'Decile 8',  ifelse(National_pop_weighted_decile == 9, 'Decile 9',  ifelse(National_pop_weighted_decile == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  mutate(Decile_in_Sussex = factor(ifelse(Decile_in_Sussex == 1, '10% most deprived',  ifelse(Decile_in_Sussex == 2, 'Decile 2',  ifelse(Decile_in_Sussex == 3, 'Decile 3',  ifelse(Decile_in_Sussex == 4, 'Decile 4',  ifelse(Decile_in_Sussex == 5, 'Decile 5',  ifelse(Decile_in_Sussex == 6, 'Decile 6',  ifelse(Decile_in_Sussex == 7, 'Decile 7',  ifelse(Decile_in_Sussex == 8, 'Decile 8',  ifelse(Decile_in_Sussex == 9, 'Decile 9',  ifelse(Decile_in_Sussex == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived')))

df <- data.frame(ID = character())

# Get the IDs of spatial polygon
for (i in MSOA_boundary@polygons ) { df <- rbind(df, data.frame(ID = i@ID, stringsAsFactors = FALSE))  }

# and set rowname = ID
row.names(vaccine_df_msoa) <- df$ID

# Then use df as the second argument to the spatial dataframe conversion function:
MSOA_boundary <- SpatialPolygonsDataFrame(MSOA_boundary, vaccine_df_msoa)  

geojson_write(ms_simplify(geojson_json(MSOA_boundary), keep = 0.6), file = paste0(output_directory_x, '/msoa_covid_vaccine_latest.geojson'))

# MSOA_boundary_gg <- geojson_read('https://github.com/psychty/wsx_covid_datapack_public/raw/master/Source%20files/failsafe_msoa_boundary.geojson', what = 'sp') %>% 
#   filter(MSOA11CD %in% vaccine_df_msoa$msoa11cd) %>% 
#   arrange(MSOA11CD) %>% 
#   fortify(region = "MSOA11CD") %>%
#   rename(MSOA11CD = id) %>%
#   left_join(vaccine_df_msoa, by = c('MSOA11CD' = 'msoa11cd'))

# MSOA by age ####
# mye_nims_msoa_age <- mye_nims_msoa %>% 
#   select(!c('Population', 'Age_18_and_over', 'Population_18_and_over')) %>% 
#   pivot_longer(cols = !c('msoa11cd', 'msoa11nm'), values_to = 'NIMS_population') %>% 
#   rename(Age_group = 'name') %>% 
#   filter(Age_group != 'Under_18')
# 
# vaccine_df_msoa_age <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
#                                   sheet = 'MSOA',
#                                   skip = 15,
#                                   col_names = c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name', 'msoa11cd', 'msoa11nm', 'First_dose_under_18',  'First_dose_age_18_24', 'First_dose_age_25_29', 'First_dose_age_30_34', 'First_dose_age_35_39', 'First_dose_age_40_44', 'First_dose_age_45_49', 'First_dose_age_50_54', 'First_dose_age_55_59', 'First_dose_age_60_64', 'First_dose_age_65_69', 'First_dose_age_70_74', 'First_dose_age_75_79', 'First_dose_age_80_and_over', 'Null_1', 'Second_dose_under_18', 'Second_dose_age_18_24',  'Second_dose_age_25_29', 'Second_dose_age_30_34', 'Second_dose_age_35_39', 'Second_dose_age_40_44', 'Second_dose_age_45_49', 'Second_dose_age_50_54', 'Second_dose_age_55_59', 'Second_dose_age_60_64', 'Second_dose_age_65_69', 'Second_dose_age_70_74', 'Second_dose_age_75_79', 'Second_dose_age_80_and_over', 'Null_2','Third_dose_under_50', 'Third_dose_50_54', 'Third_dose_55_59', 'Third_dose_60_64', 'Third_dose_65_69', 'Third_dose_70_74', 'Third_dose_75_79', 'Third_dose_80_and_over', 'Null_3','Total_doses')) %>% 
#   mutate(First_dose_under_18 = as.numeric(First_dose_under_18),
#          First_dose_age_18_24 = as.numeric(First_dose_age_18_24)) %>% 
#   filter(!is.na(Region_name)) %>% 
#   select(!c( 'Null_1', 'Second_dose_under_18', 'Second_dose_age_18_24','Second_dose_age_25_29', 'Second_dose_age_30_34', 'Second_dose_age_35_39',  'Second_dose_age_40_44', 'Second_dose_age_45_49', 'Second_dose_age_50_54', 'Second_dose_age_55_59', 'Second_dose_age_60_64', 'Second_dose_age_65_69', 'Second_dose_age_70_74', 'Second_dose_age_75_79', 'Second_dose_age_80_and_over', 'Null_2','Third_dose_under_50', 'Third_dose_50_54', 'Third_dose_55_59', 'Third_dose_60_64', 'Third_dose_65_69', 'Third_dose_70_74', 'Third_dose_75_79', 'Third_dose_80_and_over', 'Null_3', 'Total_doses')) %>% 
#   pivot_longer(cols = !c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name', 'msoa11cd', 'msoa11nm'), values_to = 'At_least_one_dose') %>% 
#   rename(Age_group = name) %>% 
#   filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>% 
#   left_join(mye_nims_msoa_age, by = c('msoa11cd', 'msoa11nm', 'Age_group')) %>%
#   mutate(Age_group = factor(ifelse(Age_group == 'Under_18', 'Age under 18*', ifelse(Age_group == 'Age_18_24', 'Age 18-24', ifelse(Age_group == 'Age_25_29', 'Age 25-29',ifelse(Age_group == 'Age_30_34', 'Age 30-34',ifelse(Age_group == 'Age_35_39', 'Age 35-39', ifelse(Age_group == 'Age_40_44', 'Age 40-44', ifelse(Age_group == 'Age_45_49', 'Age 45-49', ifelse(Age_group == 'Age_50_54', 'Age 50-54', ifelse(Age_group == 'Age_55_59', 'Age 55-59', ifelse(Age_group == 'Age_60_64', 'Age 60-64', ifelse(Age_group == 'Age_65_69', 'Age 65-69', ifelse(Age_group == 'Age_70_74', 'Age 70-74', ifelse(Age_group == 'Age_75_79', 'Age 75-79', ifelse(Age_group == 'Age_80_and_over', 'Age 80 and over', Age_group)))))))))))))), levels = c('Age under 18*','Age 18-24','Age 25-29', 'Age 30-39', 'Age 35-39', 'Age 40-44', 'Age_45_49', 'Age 50-54', 'Age 55-59', 'Age 60-64', 'Age 65-69', 'Age 70-74', 'Age 75-79', 'Age 80 and over'))) %>%
#   mutate(Individuals_not_vaccinated = NIMS_population - At_least_one_dose)
# 
# vaccine_df_wsx_age_msoa_wide <- vaccine_df_msoa_age %>% 
#   filter(msoa11cd %in% vaccine_df_msoa$msoa11cd) %>% 
#   select('LTLA_name', 'msoa11cd', 'msoa11nm', 'Age_group', 'At_least_one_dose', 'Individuals_not_vaccinated', 'NIMS_population') 
# 
# vaccine_df_wsx_age_msoa_wide %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x, '/vaccine_msoa_age.json'))