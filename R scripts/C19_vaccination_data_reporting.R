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

# Vaccine sites ####

# List updated every Friday
calls_vaccine_sites_webpage <- read_html('https://www.england.nhs.uk/coronavirus/publication/vaccination-sites/') %>%
  html_nodes("a") %>%
  html_attr("href")

# keep only xlsx files
calls_vaccine_sites_webpage <- unique(grep('.xlsx', calls_vaccine_sites_webpage, value = T))

download.file(calls_vaccine_sites_webpage, paste0(github_repo_dir,'/Source files/nhs_e_vaccine_sites.xlsx'), mode = 'wb')

nhs_vaccine_sites_hospital_hub <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccine_sites.xlsx'),
                                sheet = 'Hospital Hubs') %>% 
  rename(Site = `Trust or Site Name`) %>% 
  mutate(Type = 'Hospital Hub') 

nhs_vaccine_sites_gp_led <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccine_sites.xlsx'),
                                             sheet = 'GP-led vaccination services') %>% 
  mutate(Type = 'GP led service') %>% 
  rename(Site = 'Site Name')

nhs_vaccine_sites_pharm <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccine_sites.xlsx'),
                                       sheet = 'Pharmacies') %>% 
  mutate(Type = 'Pharmacies') %>% 
  rename(Site = 'Site Name')

nhs_vaccine_sites_centre <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccine_sites.xlsx'),
                                       sheet = 'Vaccination centres') %>% 
  mutate(Type = 'Vaccination centre') %>% 
  rename(Site = 'Site name')

vaccine_sites <- nhs_vaccine_sites_hospital_hub %>% 
  bind_rows(nhs_vaccine_sites_gp_led) %>% 
  bind_rows(nhs_vaccine_sites_pharm) %>% 
  bind_rows(nhs_vaccine_sites_centre) %>% 
  select(Type, Site, Address, Postcode) %>% 
  mutate(Postcode = gsub(' ', '', Postcode)) %>% 
  mutate(Postcode = gsub(',','', Postcode)) %>% 
  mutate(Postcode = ifelse(Postcode == 'OX169A', 'OX169AL', ifelse(Postcode == ' SO160YU', 'SO160YU', Postcode))) %>% 
  filter(Postcode != 'L234RE')

lookup_result <- data.frame(postcode = character(), longitude = double(), latitude = double(), lsoa_code = character(), msoa_code = character(), msoa = character())

for(i in 1:nrow(vaccine_sites)){
  lookup_result_x <- postcode_lookup(vaccine_sites$Postcode[i]) %>% 
    select(postcode, longitude, latitude, lsoa_code, msoa_code, msoa)
  
  lookup_result <- lookup_result_x %>% 
    bind_rows(lookup_result)
}

uncaught_postcodes <- lookup_result %>% 
  filter(is.na(latitude))

lookup_result <- lookup_result %>% 
  filter(!postcode %in% uncaught_postcodes$postcode)

lookup_result_uncaught <- data.frame(postcode = character(), longitude = double(), latitude = double())

if(nrow(uncaught_postcodes != 0)){
for(i in 1:nrow(uncaught_postcodes)){

lookup_result_x <- terminated_postcode(uncaught_postcodes$postcode[i]) %>%
  select(postcode, longitude, latitude)

  lookup_result_uncaught <- lookup_result_x %>%
    bind_rows(lookup_result_uncaught)
}
}  
  
lookup_result_final <- lookup_result %>% 
  bind_rows(lookup_result_uncaught) %>% 
  rename(Postcode = postcode) %>% 
  mutate(Postcode = gsub(' ', '', Postcode))

vaccine_sites_final <- vaccine_sites %>% 
  left_join(lookup_result_final, by = 'Postcode') %>% 
  unique()

rm(nhs_vaccine_sites_hospital_hub, nhs_vaccine_sites_gp_led, nhs_vaccine_sites_pharm, nhs_vaccine_sites_centre, vaccine_sites, lookup_result, lookup_result_final, lookup_result_uncaught, lookup_result_x, uncaught_postcodes)

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

Sussex_vaccine_sites <- vaccine_sites_final %>% 
  left_join(IMD_2019, by = c('lsoa_code' = 'LSOA11CD')) %>% 
  filter(LTLA %in% c('Brighton and Hove', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden')) 

# Check Appletree Centre (Hindu Temple), Crawley RH110AF - this is not appearing in the .gov list updated Friday 26th March

WSx_vaccine_sites <- Sussex_vaccine_sites %>% 
  filter(LTLA %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>% 
  group_by(Type) %>% 
  summarise(Sites = n()) %>% 
  mutate(LTLA = 'West Sussex')

ESx_vaccine_sites <- Sussex_vaccine_sites %>% 
  filter(LTLA %in% c('Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden')) %>% 
  group_by(Type) %>% 
  summarise(Sites = n()) %>% 
  mutate(LTLA = 'East Sussex')

Total_sites_by_district <- Sussex_vaccine_sites %>% 
  group_by(LTLA, Type) %>% 
  summarise(Sites = n()) %>% 
  bind_rows(WSx_vaccine_sites) %>% 
  bind_rows(ESx_vaccine_sites) %>% 
  pivot_wider(names_from = Type, values_from = Sites) %>% 
  mutate(`GP led service` = replace_na(`GP led service`, 0),
         `Hospital Hub` = replace_na(`Hospital Hub`, 0),
         `Vaccination centre` = replace_na(`Vaccination centre`, 0),
         Pharmacies = replace_na(Pharmacies, 0)) %>% 
  mutate(Total = `GP led service` + `Hospital Hub` + `Vaccination centre` + Pharmacies) %>% 
  rename(Area = LTLA) %>% 
  mutate(Area = factor(Area, levels = c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex', 'Brighton and Hove', 'Eastbourne', 'Hastings', 'Lewes', 'Rother', 'Wealden','East Sussex'))) %>% 
  arrange(Area)

Total_sites_by_district %>% 
  rename(GP_led = `GP led service`,
         Hospital_hub = `Hospital Hub`,
         Vaccination_centre = `Vaccination centre`) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/total_vaccination_sites_summary_table.json'))

Total_sites_by_district %>% 
  write.csv(., paste0(output_directory_x, '/total_vaccination_sites_summary_table.csv'), row.names = FALSE)

Sussex_vaccine_sites %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/Sussex_vaccination_sites.json'))

Sussex_vaccine_sites %>% 
  write.csv(., paste0(output_directory_x, '/Sussex_vaccination_sites.csv'), row.names = FALSE)

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
  mutate(Population = `Under_16` + `Age_16_and_over`) %>% 
  mutate(Population_25_and_over = Age_25_29 + Age_30_34 + Age_40_44 + Age_45_49 + Age_50_54 + Age_55_59 + Age_60_64 + Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
  mutate(Population_65_and_over = Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
  mutate(Population_25_64 = Age_25_29 + Age_30_34 + Age_35_39 + Age_40_44 + Age_45_49 + Age_50_54 + Age_55_59 + Age_60_64)

vaccine_df_msoa <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
                              sheet = 'MSOA',
                              skip = 15,
                              col_names = c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name', 'msoa11cd', 'msoa11nm', 'First_dose_under_25', 'First_dose_age_25_29', 'First_dose_age_30_34', 'First_dose_age_35_39', 'First_dose_age_40_44', 'First_dose_age_45_49', 'First_dose_age_50_54', 'First_dose_age_55_59', 'First_dose_age_60_64', 'First_dose_age_65_69', 'First_dose_age_70_74', 'First_dose_age_75_79', 'First_dose_age_80_and_over', 'Null_1', 'Second_dose_under_25', 'Second_dose_age_25_29', 'Second_dose_age_30_34', 'Second_dose_age_35_39', 'Second_dose_age_40_44', 'Second_dose_age_45_49', 'Second_dose_age_50_54', 'Second_dose_age_55_59', 'Second_dose_age_60_64', 'Second_dose_age_65_69', 'Second_dose_age_70_74', 'Second_dose_age_75_79', 'Second_dose_age_80_and_over', 'Null_2', 'Total_doses')) %>% 
  filter(!is.na(Region_name)) %>% 
  mutate(Total_where_age_known = First_dose_under_25 + First_dose_age_25_29 + First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64 + First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
  mutate(Age_25_and_over = First_dose_age_25_29 + First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64 + First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
  mutate(Age_65_and_over = First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
  mutate(Age_25_64 = First_dose_age_25_29 + First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64) %>% 
  left_join(imd_msoa, by = c('msoa11cd' = 'MSOA11CD')) %>% 
  select(!c(Region_code, Region_name, LTLA_code, msoa11nm, RUC11CD)) %>% 
  filter(LTLA_name %in% c('Brighton and Hove','Eastbourne', 'Hastings', 'Lewes','Rother','Wealden','Adur', 'Arun', 'Chichester', 'Crawley','Horsham','Mid Sussex', 'Worthing')) %>% 
  mutate(Total_banded = factor(ifelse(Total_where_age_known < 1000, 'Up to 999', ifelse(Total_where_age_known < 2000, '1,000-1,999', ifelse(Total_where_age_known < 3000, '2,000-2,999', ifelse(Total_where_age_known < 4000, '3,000-3,999', ifelse(Total_where_age_known < 5000, '4,000-4,999',  ifelse(Total_where_age_known < 6000, '5,000-5,999',  ifelse(Total_where_age_known < 7000, '6,000-6,999', '7,000+'))))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000-5,999', '6,000-6,999', '7,000+'))) %>% 
  mutate(Total_age_25_banded = factor(ifelse(Age_25_and_over < 1000, 'Up to 999', ifelse(Age_25_and_over < 2000, '1,000-1,999', ifelse(Age_25_and_over < 3000, '2,000-2,999', ifelse(Age_25_and_over < 4000, '3,000-3,999', ifelse(Age_25_and_over < 5000, '4,000-4,999',  '5,000+'))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000+'))) %>%
  mutate(Total_age_65_banded = factor(ifelse(Age_65_and_over < 1000, 'Up to 999', ifelse(Age_65_and_over < 2000, '1,000-1,999', ifelse(Age_65_and_over < 3000, '2,000-2,999', ifelse(Age_65_and_over < 4000, '3,000-3,999', ifelse(Age_65_and_over < 5000, '4,000-4,999',  '5,000+'))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000+'))) %>%
  mutate(Total_age_25_64_banded = factor(ifelse(Age_25_64 < 1000, 'Up to 999', ifelse(Age_25_64 < 2000, '1,000-1,999', ifelse(Age_25_64 < 3000, '2,000-2,999', ifelse(Age_25_64 < 4000, '3,000-3,999', ifelse(Age_25_64 < 5000, '4,000-4,999',  '5,000+'))))), levels = c('Up to 999', '1,000-1,999', '2,000-2,999', '3,000-3,999', '4,000-4,999', '5,000+'))) %>%
  arrange(msoa11cd) %>% 
  left_join(mye_nims_msoa[c('msoa11cd', 'Age_16_and_over', 'Population_25_and_over', 'Population_65_and_over', 'Population_25_64')], by = 'msoa11cd') %>% 
  mutate(Proportion_age_known = Total_where_age_known/ Age_16_and_over) %>% 
  mutate(Proportion_25_plus = Age_25_and_over/Population_25_and_over) %>% 
  mutate(Proportion_65_plus = Age_65_and_over/Population_65_and_over) %>% 
  mutate(Proportion_25_64 = Age_25_64/Population_25_64) %>% 
  mutate(Proportion_age_known_banded = factor(ifelse(Proportion_age_known < .1, 'Less than 10%', ifelse(Proportion_age_known < .2, '10-19%',ifelse(Proportion_age_known < .3, '20-29%', ifelse(Proportion_age_known < .4, '30-39%', ifelse(Proportion_age_known < .5, '40-49%', ifelse(Proportion_age_known < .6, '50-59%', ifelse(Proportion_age_known < .7, '60-69%', ifelse(Proportion_age_known < .8, '70-79%', ifelse(Proportion_age_known < .9, '80-89%', ifelse(Proportion_age_known < 1, '90-99%', '100% of estimated population')))))))))), levels = c('Less than 10%', '10-19%', '20-29%', '30-39%', '40-49%', '50-59%', '60-69%', '70-79%', '80-89%', '90-99%', '100% of estimated population'))) %>% 
  mutate(Proportion_25_plus_banded = factor(ifelse(Proportion_25_plus < .7, 'Less than 70%', ifelse(Proportion_25_plus < .75, '70-74%', ifelse(Proportion_25_plus < .8, '75-79%',ifelse(Proportion_25_plus < .85, '80-84%', ifelse(Proportion_25_plus < .9, '85-89%', ifelse(Proportion_25_plus < .95, '90-94%', ifelse(Proportion_25_plus < 1, '95-99%', '100% of estimated population'))))))), levels = c('Less than 70%', '70-74%', '75-79%', '80-84%', '85-89%', '90-94%', '95-99%', '100% of estimated population'))) %>% 
  mutate(Proportion_65_plus_banded = factor(ifelse(Proportion_65_plus < .7, 'Less than 70%', ifelse(Proportion_65_plus < .75, '70-74%', ifelse(Proportion_65_plus < .8, '75-79%',ifelse(Proportion_65_plus < .85, '80-84%', ifelse(Proportion_65_plus < .9, '85-89%', ifelse(Proportion_65_plus < .95, '90-94%', ifelse(Proportion_65_plus < 1, '95-99%', '100% of estimated population'))))))), levels = c('Less than 70%', '70-74%', '75-79%', '80-84%', '85-89%', '90-94%', '95-99%', '100% of estimated population'))) %>%
  mutate(Proportion_25_64_banded = factor(ifelse(Proportion_25_64 < .7, 'Less than 70%', ifelse(Proportion_25_64 < .75, '70-74%', ifelse(Proportion_25_64 < .8, '75-79%',ifelse(Proportion_25_64 < .85, '80-84%', ifelse(Proportion_25_64 < .9, '85-89%', ifelse(Proportion_25_64 < .95, '90-94%', ifelse(Proportion_25_64 < 1, '95-99%', '100% of estimated population'))))))), levels = c('Less than 70%', '70-74%', '75-79%', '80-84%', '85-89%', '90-94%', '95-99%', '100% of estimated population'))) %>% 
  mutate(Estimated_left_to_vaccinate_25_plus = Population_25_and_over - Age_25_and_over,
         Estimated_left_to_vaccinate_65_plus = Population_65_and_over - Age_65_and_over,
         Estimated_left_to_vaccinate_25_64 = Population_25_64 - Age_25_64) %>%
  select(msoa11cd, msoa11hclnm, LTLA_name, Total_where_age_known, Proportion_age_known, Total_banded, Proportion_age_known_banded, Age_25_and_over, Proportion_25_plus, Total_age_25_banded, Proportion_25_plus_banded, Estimated_left_to_vaccinate_25_plus, Age_65_and_over, Proportion_65_plus, Total_age_65_banded, Proportion_65_plus_banded, Estimated_left_to_vaccinate_65_plus, Age_25_64, Proportion_25_64, Total_age_25_64_banded, Proportion_25_64_banded, Estimated_left_to_vaccinate_25_64, Pop_weighted_imd_score, National_pop_weighted_rank, National_pop_weighted_decile, Rank_in_Sussex, Decile_in_Sussex, RUC11, Total_doses) 

vaccine_df_msoa %>% 
  group_by(LTLA_name) %>% 
  mutate(Proportion_rank_25_plus_within_LTLA = rank(desc(Proportion_25_plus))) %>% 
  mutate(Proportion_rank_65_plus_within_LTLA = rank(desc(Proportion_65_plus))) %>% 
  mutate(Proportion_rank_25_64_within_LTLA = rank(desc(Proportion_25_64))) %>% 
  mutate(MSOA_name = paste0(msoa11hclnm, ' (', msoa11cd, ')')) %>% 
  select(LTLA_name, MSOA_name, Total_where_age_known, Age_25_and_over, Proportion_rank_25_plus_within_LTLA, Proportion_25_plus, Estimated_left_to_vaccinate_25_plus, Age_65_and_over, Proportion_rank_65_plus_within_LTLA,Proportion_65_plus, Estimated_left_to_vaccinate_65_plus, Pop_weighted_imd_score, National_pop_weighted_decile, Decile_in_Sussex, National_pop_weighted_rank, Rank_in_Sussex, msoa11cd, Proportion_age_known, Proportion_25_64, Age_25_64) %>% 
  mutate(LTLA_name_ns = gsub(' ', '_', LTLA_name)) %>% 
  mutate(National_pop_weighted_decile = factor(ifelse(National_pop_weighted_decile == 1, '10% most deprived',  ifelse(National_pop_weighted_decile == 2, 'Decile 2',  ifelse(National_pop_weighted_decile == 3, 'Decile 3',  ifelse(National_pop_weighted_decile == 4, 'Decile 4',  ifelse(National_pop_weighted_decile == 5, 'Decile 5',  ifelse(National_pop_weighted_decile == 6, 'Decile 6',  ifelse(National_pop_weighted_decile == 7, 'Decile 7',  ifelse(National_pop_weighted_decile == 8, 'Decile 8',  ifelse(National_pop_weighted_decile == 9, 'Decile 9',  ifelse(National_pop_weighted_decile == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  mutate(Decile_in_Sussex = factor(ifelse(Decile_in_Sussex == 1, '10% most deprived',  ifelse(Decile_in_Sussex == 2, 'Decile 2',  ifelse(Decile_in_Sussex == 3, 'Decile 3',  ifelse(Decile_in_Sussex == 4, 'Decile 4',  ifelse(Decile_in_Sussex == 5, 'Decile 5',  ifelse(Decile_in_Sussex == 6, 'Decile 6',  ifelse(Decile_in_Sussex == 7, 'Decile 7',  ifelse(Decile_in_Sussex == 8, 'Decile 8',  ifelse(Decile_in_Sussex == 9, 'Decile 9',  ifelse(Decile_in_Sussex == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  # select(LTLA_name, MSOA_name, Total_where_age_known, Age_50_and_over, Proportion_rank_50_plus_within_LTLA, Proportion_50_plus, Estimated_left_to_vaccinate_50_plus, Age_65_and_over, Proportion_rank_65_plus_within_LTLA,Proportion_65_plus, Estimated_left_to_vaccinate_65_plus, Age_50_64, Proportion_rank_50_64_within_LTLA,Proportion_50_64_plus, Estimated_left_to_vaccinate_50_64,Pop_weighted_imd_score) %>% 
  mutate(UTLA = ifelse(LTLA_name %in% c('Eastbourne', 'Hastings', 'Lewes','Rother','Wealden') , 'East Sussex', ifelse(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley','Horsham','Mid Sussex', 'Worthing'), 'West Sussex', LTLA_name))) %>% 
  arrange(LTLA_name, Proportion_rank_25_plus_within_LTLA) %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_msoa_explore_data.json'))

vaccine_df_msoa %>% 
 write.csv(. , paste0(output_directory_x, '/vaccine_msoa_data.csv'), row.names = FALSE)

MSOA_boundary <- geojson_read('https://github.com/psychty/wsx_covid_datapack_public/raw/master/Source%20files/failsafe_msoa_boundary.geojson', what = 'sp') %>% 
  filter(MSOA11CD %in% vaccine_df_msoa$msoa11cd) %>% 
  arrange(MSOA11CD)

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

MSOA_boundary_gg <- geojson_read('https://github.com/psychty/wsx_covid_datapack_public/raw/master/Source%20files/failsafe_msoa_boundary.geojson', what = 'sp') %>% 
  filter(MSOA11CD %in% vaccine_df_msoa$msoa11cd) %>% 
  arrange(MSOA11CD) %>% 
  fortify(region = "MSOA11CD") %>%
  rename(MSOA11CD = id) %>%
  left_join(vaccine_df_msoa, by = c('MSOA11CD' = 'msoa11cd'))

# msoa_total_so_far <- ggplot() +
#   coord_fixed(1.5) +
#   map_theme() +
#   geom_polygon(data = MSOA_boundary_gg,
#                aes(x=long,
#                    y=lat,
#                    group = MSOA11CD,
#                    fill = Total_banded),
#                color="#5d5d5d",
#                size = .1,
#                alpha = 1,
#                show.legend = TRUE) +
#   scale_fill_manual(name = 'Number of individuals receiving at least\none dose (where age known)',
#                     values = c('#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#08589e'),
#                     drop = FALSE) +
#   geom_polygon(data = County_boundary,
#                aes(x = long,
#                    y = lat,
#                    group = group),
#                colour = '#630436',
#                fill = NA,
#                size = 1) +
#   labs(title = paste0('Cumulative number of individuals receiving at least Covid-19 vaccination dose;\nData as at ', vaccine_published_date),
#        subtitle =paste0('Sussex Middle layer Super Output Areas (MSOAs)'),
#        caption = 'Note: This excludes a small number of individuals where the age was not known.')  +
#   guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
#   theme(legend.position = c(.85,.1))
# 
# png(paste0(output_directory_x, '/Number_at_least_one_dose_vaccination_msoa_latest.png'),
#     width = 1280,
#     height = 640,
#     res = 100)
# print(msoa_total_so_far)
# dev.off()

#all_age_prop_colours <- c("#440154", "#482576", "#414487", "#35608D", "#2A788E", "#21908C", "#22A884", "#43BF71", "#7AD151", "#BBDF27", "#FDE725")

# msoa_all_age_proportion <- ggplot() +
#   coord_fixed(1.5) +
#   map_theme() +
#   geom_polygon(data = MSOA_boundary_gg,
#                aes(x=long,
#                    y=lat,
#                    group = group,
#                    fill = Proportion_age_known_banded),
#                color="#000000",
#                size = .1,
#                alpha = 1,
#                show.legend = TRUE) +
#   scale_fill_manual(values = all_age_prop_colours,
#                     breaks = rev(levels(MSOA_boundary_gg$Proportion_age_known_banded)),
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
#        subtitle = paste0('Sussex Middle layer Super Output Areas (MSOAs)'),
#        caption = 'Note: This excludes a small number of individuals where the age was not known.\nWe have used the population estimates provided by National Immunisation Management Service (NIMS)\nfor those aged 16 and over.')  +
#   guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
#   theme(legend.position = 'bottom')
# 
# png(paste0(output_directory_x, '/Proportion_at_least_one_dose_vaccination_msoa_latest.png'),
#     width = 1280,
#     height = 640,
#     res = 100)
# print(msoa_all_age_proportion)
# dev.off()
# 
# msoa_total_age_45_plus_so_far <- ggplot() +
#   coord_fixed(1.5) +
#   map_theme() +
#   geom_polygon(data = MSOA_boundary_gg,
#                aes(x=long,
#                    y=lat,
#                    group = MSOA11CD,
#                    fill = Total_age_45_banded),
#                color="#000000",
#                size = .1,
#                alpha = 1,
#                show.legend = TRUE) +
#   scale_fill_manual(name = 'Number of individuals aged 45 and over\nreceiving at least one dose',
#                     values = c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
#                     drop = FALSE) +
#   geom_polygon(data = County_boundary,
#                aes(x = long,
#                    y = lat,
#                    group = group),
#                colour = '#630436',
#                fill = NA,
#                size = 1) +
#   labs(title = paste0('Cumulative number of individuals aged 45 and over receiving at least Covid-19 vaccination dose;\nData as at ', vaccine_published_date),
#        subtitle = paste0('Sussex Middle layer Super Output Areas (MSOAs)'),
#        caption = 'Note: This excludes a small number of individuals where the age was not known.\nWe have used the population estimates provided by National Immunisation Management Service (NIMS)\n for those aged 45 and over.') +
#   guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
#   theme(legend.position = c(.85,.1))
# 
# png(paste0(output_directory_x, '/Number_at_least_one_dose_vaccination_45_plus_msoa_latest.png'),
#     width = 1280,
#     height = 640,
#     res = 100)
# print(msoa_total_age_45_plus_so_far)
# dev.off()
# 
# age_45_prop_colours <- c("#0D0887", "#5402A3", "#8B0AA5", "#B93289", "#DB5C68", "#F48849", "#FEBC2A", "#F0F921")
# 
# msoa_age_45_proportion <- ggplot() +
#   coord_fixed(1.5) +
#   map_theme() +
#   geom_polygon(data = MSOA_boundary_gg,
#                aes(x=long,
#                    y=lat,
#                    group = group,
#                    fill = Proportion_45_plus_banded),
#                color="#000000",
#                size = .1,
#                alpha = 1,
#                show.legend = TRUE) +
#   scale_fill_manual(values = age_45_prop_colours,
#                     breaks = rev(levels(MSOA_boundary_gg$Proportion_45_plus_banded)),
#                     drop = FALSE,
#                     name = 'Proportion of population\n(aged 45+)') +
#   geom_polygon(data = County_boundary,
#                aes(x = long,
#                    y = lat,
#                    group = group),
#                colour = '#000000',
#                fill = NA,
#                size = 1) +
#   labs(title = paste0('Proportion of individuals aged 45 and over receiving at least one dose of a COVID-19 vaccine;\nData as at ', vaccine_published_date),
#        subtitle = paste0('Sussex Middle layer Super Output Areas (MSOAs)'),
#        caption = 'Note: This excludes a small number of individuals where the age was not known.\nWe have used the population estimates provided by National Immunisation Management Service (NIMS)\nfor those aged 45 and over.')  +
#   guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
#   theme(legend.position = 'bottom')
# 
# png(paste0(output_directory_x, '/Proportion_at_least_one_dose_vaccination_aged_45_plus_msoa_latest.png'),
#     width = 1280,
#     height = 640,
#     res = 100)
# print(msoa_age_45_proportion)
# dev.off()
# 
# png(paste0(output_directory_x, '/Number_vaccination_msoa_latest.png'),
#     width = 1280,
#     height = 1080,
#     res = 140)
# (msoa_total_so_far + theme(legend.position = 'right')) / (msoa_total_age_45_plus_so_far + theme(legend.position = 'right'))
# dev.off()
# 
# png(paste0(output_directory_x, '/Proportion_vaccination_msoa_latest.png'),
#     width = 1280,
#     height = 1080,
#     res = 140)
# (msoa_all_age_proportion + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right')) / (msoa_age_45_proportion + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right'))
# dev.off()
# 
# png(paste0(output_directory_x, '/Vaccination_msoa_latest.png'),
#     width = 1280,
#     height = 1080,
#     res = 140)
# (msoa_total_so_far +  scale_fill_manual(name = 'Number of individuals\nreceiving at least\none dose (where age known)', values = rev(c('#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#08589e')), breaks = rev(levels(MSOA_boundary_gg$Total_banded)), drop = FALSE) + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right'))/(msoa_all_age_proportion + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right'))
# dev.off()
# 
# png(paste0(output_directory_x, '/Vaccination_msoa_45_plus_latest.png'),
#     width = 1280,
#     height = 1080,
#     res = 140)
# (msoa_total_age_45_plus_so_far +  scale_fill_manual(name = 'Number of individuals\naged 45 and over\nreceiving at least one dose', values = rev(c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026')), breaks = rev(levels(MSOA_boundary_gg$Total_age_45_banded)), drop = FALSE) + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right'))/(msoa_age_45_proportion + guides(fill = guide_legend(ncol = 1, byrow = TRUE)) + theme(legend.position = 'right'))
# dev.off()
# 
# fourtyfive_sixtyfour_prop_colours <- rev(c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177'))
# 
# ggplot() +
#   coord_fixed(1.5) +
#   map_theme() +
#   geom_polygon(data = MSOA_boundary_gg,
#                aes(x=long,
#                    y=lat,
#                    group = group,
#                    fill = Proportion_45_64_banded),
#                color="#000000",
#                size = .1,
#                alpha = 1,
#                show.legend = TRUE) +
#   scale_fill_manual(values = fourtyfive_sixtyfour_prop_colours,
#                     breaks = rev(levels(MSOA_boundary_gg$Proportion_45_64_banded)),
#                     drop = FALSE,
#                     name = 'Proportion of population\n(aged 45-64 years)') +
#   geom_polygon(data = County_boundary,
#                aes(x = long,
#                    y = lat,
#                    group = group),
#                colour = '#000000',
#                fill = NA,
#                size = 1) +
#   labs(title = paste0('Proportion of individuals aged 45 to 64 years receiving at least one dose of a COVID-19 vaccine;\nData as at ', vaccine_published_date),
#        subtitle = paste0('Sussex Middle layer Super Output Areas (MSOAs)'),
#        caption = 'Note: This excludes a small number of individuals where the age was not known.\nWe have used the population estimates provided by National Immunisation Management Service (NIMS)\nfor those aged 45-64 years.')  +
#   guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
#   theme(legend.position = 'bottom')
# 
# summary(MSOA_boundary_gg$Proportion_45_64)

# LTLA vaccine data ####

# mye_nims_ltla <- read_csv(paste0(github_repo_dir, '/Source files/ltla_nims_pop_estimates.csv')) %>% 
#   mutate(Population = Under_16 + Age_16_and_over) %>% 
#   mutate(Population_30_and_over = Age_30_34 + Age_35_39 + Age_40_44 + Age_45_49 + Age_50_54 + Age_55_59 + Age_60_64 + Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
#   mutate(Population_65_and_over = Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
#   mutate(Population_30_64 = Age_30_34 + Age_35_39 +Age_40_44 + Age_45_49 + Age_50_54 + Age_55_59 + Age_60_64)
# 
# vaccine_df_ltla <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
#                               sheet = 'LTLA',
#                               skip = 15,
#                               col_names = c('Region_code', 'Region_name', 'UTLA_code', 'UTLA_name', 'LTLA_code', 'LTLA_name', 'First_dose_under_30', 'First_dose_age_30_34', 'First_dose_age_35_39','First_dose_age_40_44', 'First_dose_age_45_49', 'First_dose_age_50_54', 'First_dose_age_55_59', 'First_dose_age_60_64', 'First_dose_age_65_69', 'First_dose_age_70_74', 'First_dose_age_75_79', 'First_dose_age_80_and_over', 'Null_1', 'Second_dose_under_30', 'Second_dose_age_30_34', 'Second_dose_age_35_39', 'Second_dose_age_40_44', 'Second_dose_age_45_49', 'Second_dose_age_50_54', 'Second_dose_age_55_59', 'Second_dose_age_60_64', 'Second_dose_age_65_69', 'Second_dose_age_70_74', 'Second_dose_age_75_79', 'Second_dose_age_80_and_over', 'Null_2', 'Total_doses')) %>% 
#   filter(!is.na(Region_name)) %>% 
#   mutate(Total_first_dose_where_age_known = First_dose_under_30 + First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64 + First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
#   mutate(First_dose_age_30_and_over = First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64 + First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
#   mutate(First_dose_age_65_and_over = First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
#   mutate(First_dose_age_30_64 = First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64) %>% 
#   left_join(mye_nims_ltla[c('LTLA_code', 'Age_16_and_over', 'Population_30_and_over', 'Population_65_and_over', 'Population_30_64')], by = 'LTLA_code') %>% 
#   mutate(First_dose_proportion_age_known = Total_first_dose_where_age_known/ Age_16_and_over) %>% 
#   mutate(First_dose_proportion_30_plus = First_dose_age_30_and_over/Population_30_and_over) %>% 
#   mutate(First_dose_proportion_65_plus = First_dose_age_65_and_over/Population_65_and_over) %>% 
#   mutate(First_dose_proportion_30_64 = First_dose_age_30_64/Population_30_64) %>% 
#   select(LTLA_code, LTLA_name, Total_first_dose_where_age_known, First_dose_proportion_age_known, First_dose_age_30_and_over, First_dose_proportion_30_plus, Age_16_and_over, Population_30_and_over, First_dose_age_65_and_over, First_dose_proportion_65_plus, Population_65_and_over, First_dose_age_30_64, First_dose_proportion_30_64, Population_30_64) %>% 
#   rename(Population_16_and_over = Age_16_and_over)
# 
# vaccine_df_wsx <- vaccine_df_ltla %>% 
#   filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>%
#   summarise(Total_first_dose_where_age_known = sum(Total_first_dose_where_age_known),
#             First_dose_age_30_and_over = sum(First_dose_age_30_and_over),
#             First_dose_age_65_and_over = sum(First_dose_age_65_and_over),
#             First_dose_age_30_64 = sum(First_dose_age_30_64),
#             Population_16_and_over = sum(Population_16_and_over),
#             Population_30_and_over = sum(Population_30_and_over),
#             Population_65_and_over = sum(Population_65_and_over),
#             Population_30_64 = sum(Population_30_64)) %>% 
#   mutate(LTLA_name = 'West Sussex',
#          LTLA_code = 'E10000032') %>%
#   mutate(First_dose_proportion_age_known = Total_first_dose_where_age_known/ Population_16_and_over) %>% 
#   mutate(First_dose_proportion_30_plus = First_dose_age_30_and_over/Population_30_and_over) %>%
#   mutate(First_dose_proportion_65_plus = First_dose_age_65_and_over/Population_65_and_over) %>%
#   mutate(First_dose_proportion_30_64 = First_dose_age_30_64/Population_30_64) %>% 
#   select(LTLA_code, LTLA_name, Total_first_dose_where_age_known, First_dose_proportion_age_known, First_dose_age_30_and_over, First_dose_proportion_30_plus, Population_16_and_over, Population_30_and_over, First_dose_age_65_and_over, First_dose_proportion_65_plus, Population_65_and_over, First_dose_age_30_64, First_dose_proportion_30_64, Population_30_64) 
# 
# # Add SE and England
# 
# mye_ons_region <- read_csv(paste0(github_repo_dir, '/Source files/region_ons_pop_estimates.csv')) %>% 
#   mutate(Population = Under_16 + Age_16_and_over) %>% 
#   mutate(Population_30_and_over = Age_30_34 + Age_35_39 + Age_40_44 + Age_45_49 + Age_50_54 + Age_55_59 + Age_60_64 + Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
#   mutate(Population_65_and_over = Age_65_69 + Age_70_74 + Age_75_79 + Age_80_and_over) %>% 
#   mutate(Population_30_64 = Age_30_34 + Age_35_39 + Age_40_44 + Age_45_49 + Age_50_54 + Age_55_59 + Age_60_64) %>% 
#   rename(Region_name = Area) %>% 
#   mutate(Region_name = ifelse(Region_name == 'England', 'England', paste0(Region_name, ' region')))
# 
# vaccine_df_region <- read_excel("GitHub/wsx_covid_datapack_public/Source files/nhs_e_vaccines.xlsx", 
#                                 sheet = "NHS Region", 
#                                 range = "B14:O22",
#                                 col_names = c('Region_name', 'Null_1','First_dose_under_30', 'First_dose_age_30_34', 'First_dose_age_35_39','First_dose_age_40_44', 'First_dose_age_45_49', 'First_dose_age_50_54', 'First_dose_age_55_59', 'First_dose_age_60_64', 'First_dose_age_65_69', 'First_dose_age_70_74', 'First_dose_age_75_79', 'First_dose_age_80_and_over')) %>%
#   select(!Null_1) %>% 
#   filter(!is.na(Region_name)) %>%
#   mutate(Region_name = ifelse(Region_name == 'Total4', 'England', paste0(Region_name, ' region'))) %>% 
#   mutate(Total_first_dose_where_age_known = First_dose_under_30 + First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64 + First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
#   mutate(First_dose_age_30_and_over = First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49  + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64 + First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
#   mutate(First_dose_age_65_and_over = First_dose_age_65_69 + First_dose_age_70_74 + First_dose_age_75_79 + First_dose_age_80_and_over) %>% 
#   mutate(First_dose_age_30_64 = First_dose_age_30_34 + First_dose_age_35_39 + First_dose_age_40_44 + First_dose_age_45_49 + First_dose_age_50_54 + First_dose_age_55_59 + First_dose_age_60_64) %>% 
#   left_join(mye_ons_region[c('Region_name', 'Age_16_and_over', 'Population_30_and_over', 'Population_65_and_over', 'Population_30_64')], by = 'Region_name') %>% 
#   mutate(First_dose_proportion_age_known = Total_first_dose_where_age_known/ Age_16_and_over) %>% 
#   mutate(First_dose_proportion_30_plus = First_dose_age_30_and_over/Population_30_and_over) %>% 
#   mutate(First_dose_proportion_65_plus = First_dose_age_65_and_over/Population_65_and_over) %>% 
#   mutate(First_dose_proportion_30_64 = First_dose_age_30_64/Population_30_64) %>% 
#   select(Region_name, Total_first_dose_where_age_known, First_dose_proportion_age_known, First_dose_age_30_and_over, First_dose_proportion_30_plus, Age_16_and_over, Population_30_and_over, First_dose_age_65_and_over, First_dose_proportion_65_plus, Population_65_and_over, First_dose_age_30_64, First_dose_proportion_30_64, Population_30_64) %>% 
#   rename(Population_16_and_over = Age_16_and_over) %>% 
#   filter(Region_name %in% c('South East region', 'England')) %>% 
#   rename(Name = Region_name) %>% 
#   mutate(Name = factor(Name, levels = c('South East region', 'England'))) %>% 
#   arrange(Name)
# 
# vaccine_df_ltla %>% 
#   filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>% 
#   bind_rows(vaccine_df_wsx) %>% 
#   rename(Name = 'LTLA_name') %>% 
#   bind_rows(vaccine_df_region) %>% 
#   select(Name, Total_first_dose_where_age_known, First_dose_proportion_age_known, First_dose_age_30_and_over, First_dose_proportion_30_plus, Population_16_and_over, Population_30_and_over, First_dose_age_65_and_over, First_dose_proportion_65_plus, Population_65_and_over, First_dose_age_30_64, First_dose_proportion_30_64, Population_30_64) %>% 
#   toJSON() %>% 
#   write_lines(paste0(mobile_output_directory_x, '/vaccine_at_a_glance.json'))
# 
# vaccine_df_ltla %>% 
#   filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>% 
#   bind_rows(vaccine_df_wsx) %>% 
#   rename(Name = 'LTLA_name') %>% 
#  # bind_rows(vaccine_df_region) %>% 
#   select(Name, Total_first_dose_where_age_known, First_dose_proportion_age_known, First_dose_age_30_and_over, First_dose_proportion_30_plus, Population_16_and_over, Population_30_and_over, First_dose_age_65_and_over, First_dose_proportion_65_plus, Population_65_and_over, First_dose_age_30_64, First_dose_proportion_30_64, Population_30_64) %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x, '/vaccine_at_a_glance.json'))

# lad_boundary <- geojson_read('https://opendata.arcgis.com/datasets/69cd46d7d2664e02b30c2f8dcc2bfaf7_0.geojson', what = 'sp') %>% 
#   filter(LAD19NM %in% c('Brighton and Hove','Eastbourne', 'Hastings', 'Lewes','Rother','Wealden','Adur', 'Arun', 'Chichester', 'Crawley','Horsham','Mid Sussex', 'Worthing')) %>% 
#   fortify(region = "LAD19CD") %>%
#   rename(LAD19CD = id) %>%
#   filter(substr(LAD19CD, 1,1 ) == 'E') %>% 
#   left_join(vaccine_df_ltla, by = c('LAD19CD' = 'LTLA_code')) %>% 
#   mutate(First_dose_proportion_age_known_banded = factor(ifelse(First_dose_proportion_age_known < .3, 'Less than 30%', ifelse(First_dose_proportion_age_known < .4, '30-39%', ifelse(First_dose_proportion_age_known < .5, '40-49%', ifelse(First_dose_proportion_age_known < .6, '50-59%', ifelse(First_dose_proportion_age_known < .7, '60-69%', ifelse(First_dose_proportion_age_known < .8, '70-79%', ifelse(First_dose_proportion_age_known < .9, '80-89%', ifelse(First_dose_proportion_age_known < 1, '90-99%', '100% of estimated population')))))))), levels = c('Less than 30%', '30-39%', '40-49%', '50-59%', '60-69%', '70-79%', '80-89%', '90-99%', '100% of estimated population'))) %>% 
#   mutate(First_dose_proportion_30_plus_banded = factor(ifelse(First_dose_proportion_30_plus < .8, 'Less than 80%', ifelse(First_dose_proportion_30_plus < .85, '80-84%', ifelse(First_dose_proportion_30_plus < .9, '85-89%', ifelse(First_dose_proportion_30_plus < .95, '90-94%', ifelse(First_dose_proportion_30_plus < 1, '95-99%', '100% of estimated population'))))), levels = c('Less than 80%', '80-84%', '85-89%', '90-94%', '95-99%', '100% of estimated population'))) %>% 
#   mutate(First_dose_proportion_30_64_banded = factor(ifelse(First_dose_proportion_30_64 < .1, 'Less than 10%', ifelse(First_dose_proportion_30_64 < .2, '10-19%', ifelse(First_dose_proportion_30_64 < .3, '20-29%', ifelse(First_dose_proportion_30_64 < .4, '30-39%', ifelse(First_dose_proportion_30_64 < .5, '40-49%', ifelse(First_dose_proportion_30_64 < .6, '50-59%', ifelse(First_dose_proportion_30_64 < .7, '60-69%', ifelse(First_dose_proportion_30_64 < .8, '70-79%', ifelse(First_dose_proportion_30_64 < .9, '80-89%', ifelse(First_dose_proportion_30_64 < 1, '90-99%', '100% of estimated population')))))))))), levels = c('Less than 10%', '10-19%', '20-29%', '30-39%', '40-49%', '50-59%', '60-69%', '70-79%', '80-89%', '90-99%', '100% of estimated population')))
# 
# lad_boundary_export <- geojson_read('https://opendata.arcgis.com/datasets/69cd46d7d2664e02b30c2f8dcc2bfaf7_0.geojson', what = 'sp') %>% 
#   filter(LAD19NM %in% c('Brighton and Hove','Eastbourne', 'Hastings', 'Lewes','Rother','Wealden','Adur', 'Arun', 'Chichester', 'Crawley','Horsham','Mid Sussex', 'Worthing'))
# 
# geojson_write(ms_simplify(geojson_json(lad_boundary_export), keep = 1), file = paste0(output_directory_x, '/lad_boundary_export.geojson'))
# 
# vac_llim_prop_all_age <- round_any(min(lad_boundary$First_dose_proportion_age_known, na.rm = TRUE), .05, floor)
# vac_ulim_prop_all_age <- round_any(max(lad_boundary$First_dose_proportion_age_known, na.rm = TRUE), .05, ceiling)

# ltla_all_age_proportion <- ggplot() +
#   coord_fixed(1.5) +
#   map_theme() +
#   geom_polygon(data = lad_boundary,
#                aes(x=long,
#                    y=lat,
#                    group = group,
#                    fill = First_dose_proportion_age_known),
#                color="#ffffff",
#                size = .1,
#                alpha = 1,
#                show.legend = TRUE) +
#   scale_fill_viridis_c(option = "viridis",
#                        direction = -1,
#                        breaks = seq(vac_llim_prop_all_age, vac_ulim_prop_all_age, .05),
#                        limits = c(vac_llim_prop_all_age, vac_ulim_prop_all_age),
#                        label = percent,
#                        name = 'Proportion of population\n(where age known)\n(aged 16+)') +
#   geom_polygon(data = County_boundary,
#                aes(x = long,
#                    y = lat,
#                    group = group),
#                colour = '#630436',
#                fill = NA,
#                size = 1) +
#   labs(title = paste0('Proportion of individuals receiving at least one dose of a COVID-19 vaccine (aged 16+);\nData as at ', vaccine_published_date),
#        subtitle = paste0('Sussex Lower Tier Local and Unitary Authorities'),
#        caption = 'Note: This excludes a small number of individuals where the age was not known.')  +
#   theme(legend.position = c(.9,.1))
# 
# png(paste0(output_directory_x, '/Proportion_all_age_vaccination_ltla_latest.png'),
#     width = 1280,
#     height = 640,
#     res = 110)
# print(ltla_all_age_proportion + theme(plot.caption = element_text(hjust = 0)))
# dev.off()
# 
# vac_llim_prop_45_plus <- round_any(min(lad_boundary$First_dose_proportion_45_plus, na.rm = TRUE), .05, floor)
# vac_ulim_prop_45_plus <- round_any(max(lad_boundary$First_dose_proportion_45_plus, na.rm = TRUE), .05, ceiling)
# 
# ltla_45_plus_age_proportion <- ggplot() +
#   coord_fixed(1.5) +
#   map_theme() +
#   geom_polygon(data = lad_boundary,
#                aes(x=long,
#                    y=lat,
#                    group = group,
#                    fill = First_dose_proportion_45_plus),
#                color="#ffffff",
#                size = .1,
#                alpha = 1,
#                show.legend = TRUE) +
#   scale_fill_viridis_c(option = "plasma",
#                        direction = -1,
#                        breaks = seq(vac_llim_prop_45_plus, vac_ulim_prop_45_plus, .025),
#                        limits = c(vac_llim_prop_45_plus, vac_ulim_prop_45_plus),
#                        label = percent,
#                        name = 'Proportion of those aged\n45 years and over\n(where age known)') +
#   geom_polygon(data = County_boundary,
#                aes(x = long,
#                    y = lat,
#                    group = group),
#                colour = '#630436',
#                fill = NA,
#                size = 1) +
#   labs(title = paste0('Proportion of individuals aged 45+ receiving at least one dose of a COVID-19 vaccine per 100,000 population (aged 45+);\nData as at ', vaccine_published_date),
#        subtitle = paste0('Sussex Lower Tier Local and Unitary Authorities'),
#        caption = paste0('Note: This excludes a smaller number of individuals where the age was not known.'))  +
#   # guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
#   theme(legend.position = c(.9, .1))
# 
# png(paste0(output_directory_x, '/Proportion_age_45_plus_vaccination_ltla_latest.png'),
#     width = 1280,
#     height = 640,
#     res = 110)
# print(ltla_45_plus_age_proportion + theme(plot.caption = element_text(hjust = 0)))
# dev.off()
# 
# png(paste0(output_directory_x, '/Proportion_vaccination_ltla_latest.png'),
#     width = 1280,
#     height = 1080,
#     res = 140)
# (ltla_all_age_proportion + theme(legend.position = 'right')) / (ltla_45_plus_age_proportion + theme(legend.position = 'right'))
# dev.off()

# What about age groups ####
# mye_nims_ltla_age <- mye_nims_ltla %>% 
#   select(!c('Population', 'Age_16_and_over', 'Population_30_and_over')) %>% 
#   pivot_longer(cols = !c('LTLA_code', 'LTLA_name'), values_to = 'NIMS_population') %>% 
#   rename(Age_group = 'name') %>% 
#   mutate(Age_group = ifelse(Age_group == 'Age_16_29', 'Under_30', Age_group)) %>% 
#   filter(Age_group != 'Under_16')
# 
# vaccine_df_ltla_age <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
#                               sheet = 'LTLA',
#                               skip = 15,
#                               col_names = c('Region_code', 'Region_name', 'UTLA_code', 'UTLA_name', 'LTLA_code', 'LTLA_name', 'First_dose_under_30', 'First_dose_age_30_34', 'First_dose_age_35_39','First_dose_age_40_44', 'First_dose_age_45_49', 'First_dose_age_50_54', 'First_dose_age_55_59', 'First_dose_age_60_64', 'First_dose_age_65_69', 'First_dose_age_70_74', 'First_dose_age_75_79', 'First_dose_age_80_and_over', 'Null_1', 'Second_dose_under_30', 'Second_dose_age_30_34', 'Second_dose_age_35_39', 'Second_dose_age_40_44', 'Second_dose_age_45_49', 'Second_dose_age_50_54', 'Second_dose_age_55_59', 'Second_dose_age_60_64', 'Second_dose_age_65_69', 'Second_dose_age_70_74', 'Second_dose_age_75_79', 'Second_dose_age_80_and_over', 'Null_2', 'Total_doses')) %>% 
#   filter(!is.na(Region_name)) %>% 
#   select(!c('UTLA_code', 'UTLA_name', 'Null_1', 'Second_dose_under_30', 'Second_dose_age_30_34', 'Second_dose_age_35_39', 'Second_dose_age_40_44', 'Second_dose_age_45_49', 'Second_dose_age_50_54', 'Second_dose_age_55_59', 'Second_dose_age_60_64', 'Second_dose_age_65_69', 'Second_dose_age_70_74', 'Second_dose_age_75_79', 'Second_dose_age_80_and_over', 'Null_2', 'Total_doses')) %>% 
#   pivot_longer(cols = !c('Region_code', 'Region_name',  'LTLA_code', 'LTLA_name'), values_to = 'At_least_one_dose') %>% 
#   rename(Age_group = name) %>% 
#   mutate(Age_group = gsub('First_dose_u', 'U', gsub('First_dose_a', 'A', Age_group))) %>%  
#   left_join(mye_nims_ltla_age, by = c('LTLA_code', 'LTLA_name', 'Age_group')) %>% 
#   mutate(Age_group = factor(ifelse(Age_group == 'Under_30', 'Age under 30*', ifelse(Age_group == 'Age_30_34', 'Age 30-34', ifelse(Age_group == 'Age_35_39', 'Age 35-39',  ifelse(Age_group == 'Age_40_44', 'Age 40-44', ifelse(Age_group == 'Age_45_49', 'Age 45-49',  ifelse(Age_group == 'Age_50_54', 'Age 50-54', ifelse(Age_group == 'Age_55_59', 'Age 55-59', ifelse(Age_group == 'Age_60_64', 'Age 60-64', ifelse(Age_group == 'Age_65_69', 'Age 65-69', ifelse(Age_group == 'Age_70_74', 'Age 70-74', ifelse(Age_group == 'Age_75_79', 'Age 75-79', ifelse(Age_group == 'Age_80_and_over', 'Age 80 and over', Age_group)))))))))))), levels = c('Age under 30*', 'Age 30-34', 'Age 35-39', 'Age 40-44','Age 45-49', 'Age 50-54', 'Age 55-59', 'Age 60-64', 'Age 65-69', 'Age 70-74', 'Age 75-79', 'Age 80 and over'))) %>% 
#   mutate(Individuals_not_vaccinated = NIMS_population - At_least_one_dose)
# 
# vaccine_df_wsx_age <- vaccine_df_ltla_age %>% 
#   filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>% 
#   group_by(Age_group) %>% 
#   summarise(At_least_one_dose = sum(At_least_one_dose, na.rm = TRUE),
#             NIMS_population = sum(NIMS_population, na.rm = TRUE)) %>% 
#   mutate(Individuals_not_vaccinated = NIMS_population - At_least_one_dose) %>% 
#   ungroup() %>% 
#   mutate(LTLA_name = 'West Sussex',
#          LTLA_code = 'E10000032') %>% 
#   bind_rows(vaccine_df_ltla_age) %>% 
#   select('LTLA_name', 'LTLA_code', 'Age_group', 'At_least_one_dose', 'Individuals_not_vaccinated', 'NIMS_population') %>% 
#   filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex')) %>% 
#   mutate(LTLA_name = factor(LTLA_name, levels = c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex'))) %>% 
#   pivot_longer(cols = !c('LTLA_code', 'LTLA_name', 'Age_group', 'NIMS_population'), values_to = 'Individuals', names_to = 'Type') %>% 
#   rename(Name = LTLA_name) %>% 
#   mutate(Type = factor(Type, levels = rev(c('At_least_one_dose', 'Individuals_not_vaccinated')))) %>% 
#   arrange(Type)
# 
# ggplot(data = subset(vaccine_df_wsx_age, Name != 'West Sussex'),
#        aes(x = Age_group,
#            y = Individuals,
#            fill = Type)) +
#   geom_bar(position = 'stack', 
#            stat = 'identity') +
#   scale_fill_manual(values = c('#ff4f03', '#8b9dc3'),
#                     breaks = rev(levels(vaccine_df_wsx_age$Type)),
#                     labels = c('At least one dose', 'Not vaccinated')) +
#   ph_theme() +
#   theme(axis.text.x = element_text(size = 8, hjust = 0.5),
#         panel.grid.major.x = element_line(colour = "#E7E7E7", size = .3),
#         panel.grid.major.y = element_blank()) +
#   facet_rep_wrap(. ~ Name, ncol = 4, repeat.tick.labels = TRUE) +
#   coord_flip() 
# 
# ggplot(data = subset(vaccine_df_wsx_age, Name != 'West Sussex'),
#        aes(x = Age_group,
#            y = Individuals,
#            fill = Type)) +
#   geom_bar(position = 'fill', 
#            stat = 'identity') +
#   scale_y_continuous(labels = percent,
#                      breaks = seq(0,1,.2)) +
#   scale_fill_manual(values = c('#ff4f03', '#8b9dc3'),
#                     breaks = rev(levels(vaccine_df_wsx_age$Type)),
#                     labels = c('At least one dose', 'Not vaccinated')) +
#   ph_theme() +
#   theme(axis.text.x = element_text(size = 8, hjust = 0.5),
#         panel.grid.major.x = element_line(colour = "#E7E7E7", size = .3),
#         panel.grid.major.y = element_blank()) +
#   facet_rep_wrap(. ~ Name, ncol = 4, repeat.tick.labels = TRUE) +
#   coord_flip() 
# 
# vaccine_df_wsx_age_wide <- vaccine_df_ltla_age %>% 
#   filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>% 
#   group_by(Age_group) %>% 
#   summarise(At_least_one_dose = sum(At_least_one_dose, na.rm = TRUE),
#             NIMS_population = sum(NIMS_population, na.rm = TRUE)) %>% 
#   mutate(Individuals_not_vaccinated = NIMS_population - At_least_one_dose) %>% 
#   ungroup() %>% 
#   mutate(LTLA_name = 'West Sussex',
#          LTLA_code = 'E10000032') %>% 
#   bind_rows(vaccine_df_ltla_age) %>% 
#   select('LTLA_name', 'LTLA_code', 'Age_group', 'At_least_one_dose', 'Individuals_not_vaccinated', 'NIMS_population') %>% 
#   filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex')) %>% 
#   mutate(LTLA_name = factor(LTLA_name, levels = c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex'))) %>% 
#   rename(Name = LTLA_name)
# 
# vaccine_df_wsx_age_wide %>% 
#   mutate(At_least_one_dose_prop = At_least_one_dose / NIMS_population,
#          Individuals_not_vaccinated_prop = Individuals_not_vaccinated / NIMS_population) %>% 
#   toJSON() %>% 
#   write_lines(paste0(output_directory_x, '/vaccine_ltla_age.json'))
# 
# vaccine_df_wsx_age_wide %>% 
#   mutate(At_least_one_dose_prop = At_least_one_dose / NIMS_population,
#          Individuals_not_vaccinated_prop = Individuals_not_vaccinated / NIMS_population) %>% 
#   toJSON() %>% 
#   write_lines(paste0(mobile_output_directory_x, '/vaccine_ltla_age.json'))

# MSOA by age ####
mye_nims_msoa_age <- mye_nims_msoa %>% 
  select(!c('Population', 'Age_16_and_over', 'Population_25_and_over')) %>% 
  pivot_longer(cols = !c('msoa11cd', 'msoa11nm'), values_to = 'NIMS_population') %>% 
  rename(Age_group = 'name') %>% 
  mutate(Age_group = ifelse(Age_group == 'Age_16_29', 'Under_25', Age_group)) %>% 
  filter(Age_group != 'Under_16')

vaccine_df_msoa_age <- read_excel(paste0(github_repo_dir,'/Source files/nhs_e_vaccines.xlsx'),
                                  sheet = 'MSOA',
                                  skip = 15,
                                  col_names = c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name', 'msoa11cd', 'msoa11nm', 'First_dose_under_25', 'First_dose_age_25_29', 'First_dose_age_30_34', 'First_dose_age_35_39', 'First_dose_age_40_44', 'First_dose_age_45_49', 'First_dose_age_50_54', 'First_dose_age_55_59', 'First_dose_age_60_64', 'First_dose_age_65_69', 'First_dose_age_70_74', 'First_dose_age_75_79', 'First_dose_age_80_and_over', 'Null_1', 'Second_dose_under_25', 'Second_dose_age_25_29', 'Second_dose_age_30_34', 'Second_dose_age_35_39', 'Second_dose_age_40_44', 'Second_dose_age_45_49', 'Second_dose_age_50_54', 'Second_dose_age_55_59', 'Second_dose_age_60_64', 'Second_dose_age_65_69', 'Second_dose_age_70_74', 'Second_dose_age_75_79', 'Second_dose_age_80_and_over', 'Null_2', 'Total_doses')) %>% 
  filter(!is.na(Region_name)) %>% 
  select(!c( 'Null_1', 'Second_dose_under_25','Second_dose_age_25_29', 'Second_dose_age_30_34', 'Second_dose_age_35_39',  'Second_dose_age_40_44', 'Second_dose_age_45_49', 'Second_dose_age_50_54', 'Second_dose_age_55_59', 'Second_dose_age_60_64', 'Second_dose_age_65_69', 'Second_dose_age_70_74', 'Second_dose_age_75_79', 'Second_dose_age_80_and_over', 'Null_2', 'Total_doses')) %>% 
  pivot_longer(cols = !c('Region_code', 'Region_name', 'LTLA_code', 'LTLA_name', 'msoa11cd', 'msoa11nm'), values_to = 'At_least_one_dose') %>% 
  rename(Age_group = name) %>% 
  
  filter(LTLA_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>% 
  left_join(mye_nims_msoa_age, by = c('msoa11cd', 'msoa11nm', 'Age_group')) %>%
  mutate(Age_group = factor(ifelse(Age_group == 'Under_25', 'Age under 25*',ifelse(Age_group == 'Age_25_29', 'Age 25-29',ifelse(Age_group == 'Age_30_34', 'Age 30-34',ifelse(Age_group == 'Age_35_39', 'Age 35-39', ifelse(Age_group == 'Age_40_44', 'Age 40-44', ifelse(Age_group == 'Age_45_49', 'Age 45-49', ifelse(Age_group == 'Age_50_54', 'Age 50-54', ifelse(Age_group == 'Age_55_59', 'Age 55-59', ifelse(Age_group == 'Age_60_64', 'Age 60-64', ifelse(Age_group == 'Age_65_69', 'Age 65-69', ifelse(Age_group == 'Age_70_74', 'Age 70-74', ifelse(Age_group == 'Age_75_79', 'Age 75-79', ifelse(Age_group == 'Age_80_and_over', 'Age 80 and over', Age_group))))))))))))), levels = c('Age under 25*','Age 25-29', 'Age 30-39', 'Age 35-39', 'Age 40-44', 'Age_45_49', 'Age 50-54', 'Age 55-59', 'Age 60-64', 'Age 65-69', 'Age 70-74', 'Age 75-79', 'Age 80 and over'))) %>%
  mutate(Individuals_not_vaccinated = NIMS_population - At_least_one_dose)

vaccine_df_wsx_age_msoa_wide <- vaccine_df_msoa_age %>% 
  filter(msoa11cd %in% vaccine_df_msoa$msoa11cd) %>% 
  select('LTLA_name', 'msoa11cd', 'msoa11nm', 'Age_group', 'At_least_one_dose', 'Individuals_not_vaccinated', 'NIMS_population') 

vaccine_df_wsx_age_msoa_wide %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory_x, '/vaccine_msoa_age.json'))


