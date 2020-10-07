
library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'fingertipsR', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis'))

ltla_boundaries <- geojson_read('https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson',  what = "sp") 

lookup <- read_csv('https://opendata.arcgis.com/datasets/3e4f4af826d343349c13fb7f0aa2a307_0.csv')

# We want a dataframe with all ltlas in England on. left join with the restrictions df and say whether an area is subject to local restrictions above national ones.

restrictions <- read_csv('https://visual.parliament.uk/research/visualisations/coronavirus-restrictions-map/commonslibrary-coronavirus-restrictions-data.csv') %>% 
  filter(l_Country == 'E') %>% 
  left_join(lookup, by = c('l_Category' = 'LTLA19NM')) %>% 
  rename(LTLA19NM = l_Category) %>% 
  select(LTLA19NM, LTLA19CD, UTLA19CD, UTLA19NM, l_restrictions, l_url_local, l_url_national, l_local_ruleofsix, l_local_householdmixing, l_local_raves, l_local_stayinglocal, l_local_stayinghome, l_local_notstayingaway, l_local_businessclosures, l_local_openinghours, l_national_ruleofsix, l_national_householdmixing, l_national_raves, l_national_stayinglocal, l_national_stayinghome, l_national_notstayingaway, l_national_businessclosures, l_national_openinghours, l_national_gatherings) %>% 
  mutate(label = paste0('<Strong>',LTLA19NM, '</Strong><br><br>', ifelse(l_national_ruleofsix == 1, 'It is currently prohibited for people to gather socially in groups larger than six unless it is for an exempted purpose.', ''), ifelse(l_national_householdmixing == 1, 'In England, people are currently prohibited from mixing with other households (e.g. visiting each other inside their homes).', ''),'In addition to national restrictions, this area is subject to further local restrictions including: <br><ul>', ifelse(l_local_ruleofsix == 1, '<li>not meeting with more than six people socially</li>', '')))

# TODO add ifelse for any rules 

View(restrictions)

restrictions <- rlookup %>% 
  filter(LTLA19NM %in% restrictions$l_Category)

paste0('Information provided by the House of Commons Library Coronavirus Restrictions Tool (available: https://visual.parliament.uk/research/visualisations/coronavirus-restrictions-map/). Contains Parliamentary information licensed under the Open Parliament Licence v3.0.')
