
library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'zoo', 'stats',"rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'fingertipsR', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis'))

lookup <- read_csv('https://opendata.arcgis.com/datasets/3e4f4af826d343349c13fb7f0aa2a307_0.csv')

rest_of_england_restrictions <- read_csv('https://visual.parliament.uk/research/visualisations/coronavirus-restrictions-map/commonslibrary-coronavirus-restrictions-data.csv') %>% 
  filter(l_Category == 'Rest of England')

ltla_restrictions <-  geojson_read('https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson',  what = "sp")  %>% 
  left_join(lookup, by = c('lad19nm' = 'LTLA19NM')) %>% 
  filter(substr(lad19cd, 1, 1) == 'E') %>% 
  left_join(read_csv('https://visual.parliament.uk/research/visualisations/coronavirus-restrictions-map/commonslibrary-coronavirus-restrictions-data.csv'), by = c('lad19nm' = 'l_Category')) %>% 
  select(!c(l_Country, lad19nmw)) %>% 
  rename(Restriction_type = l_restrictions) %>% 
  mutate(Restriction_type = ifelse(is.na(Restriction_type), 'National', Restriction_type)) %>%
  mutate(l_url_national = "https://www.gov.uk/government/publications/coronavirus-outbreak-faqs-what-you-can-and-cant-do/coronavirus-outbreak-faqs-what-you-can-and-cant-do") %>% 
  mutate(l_national_ruleofsix = ifelse(is.na(l_national_ruleofsix), rest_of_england_restrictions$l_national_ruleofsix, l_national_ruleofsix)) %>% 
  mutate(l_national_householdmixing = ifelse(is.na(l_national_householdmixing), rest_of_england_restrictions$l_national_householdmixing, l_national_householdmixing)) %>%  mutate(l_national_raves = ifelse(is.na(l_national_raves), rest_of_england_restrictions$l_national_raves, l_national_raves)) %>%
  mutate(l_national_stayinglocal = ifelse(is.na(l_national_stayinglocal), rest_of_england_restrictions$l_national_stayinglocal, l_national_stayinglocal)) %>%   
  mutate(l_national_stayinghome = ifelse(is.na(l_national_stayinghome), rest_of_england_restrictions$l_national_stayinghome, l_national_stayinghome)) %>% 
  mutate(l_national_notstayingaway = ifelse(is.na(l_national_notstayingaway), rest_of_england_restrictions$l_national_notstayingaway, l_national_notstayingaway)) %>% 
  mutate(l_national_businessclosures = ifelse(is.na(l_national_businessclosures), rest_of_england_restrictions$l_national_businessclosures, l_national_businessclosures)) %>% 
  mutate(l_national_openinghours = ifelse(is.na(l_national_openinghours), rest_of_england_restrictions$l_national_openinghours, l_national_openinghours)) %>% 
  mutate(l_national_gatherings = ifelse(is.na(l_national_gatherings), rest_of_england_restrictions$l_national_gatherings, l_national_gatherings)) %>% 
  mutate(l_local_ruleofsix = ifelse(is.na(l_local_ruleofsix), 0, l_local_ruleofsix))%>% 
  mutate(l_local_householdmixing = ifelse(is.na(l_local_householdmixing), 0, l_local_householdmixing))%>% 
  mutate(l_local_raves = ifelse(is.na(l_local_raves), 0, l_local_raves))%>% 
  mutate(l_local_stayinglocal = ifelse(is.na(l_local_stayinglocal), 0, l_local_stayinglocal))%>% 
  mutate(l_local_stayinghome = ifelse(is.na(l_local_stayinghome), 0, l_local_stayinghome))%>% 
  mutate(l_local_notstayingaway = ifelse(is.na(l_local_notstayingaway), 0, l_local_notstayingaway)) %>% 
  mutate(l_local_businessclosures = ifelse(is.na(l_local_businessclosures), 0, l_local_businessclosures)) %>% 
  mutate(l_local_openinghours = ifelse(is.na(l_local_openinghours), 0, l_local_openinghours)) %>% 
  mutate(label = paste0('<Strong>', lad19nm, '</Strong><br><br>', ifelse(Restriction_type == 'National', 'This areas is currently subject to <b>national</b> restrictions only. These include:', ifelse(Restriction_type == 'Local', 'This area is currently subject to national as well as further <b>local</b> restrictions. National restrictions include:', '')), '<ul>', ifelse(l_national_ruleofsix == 1, '<li>People are prohibited from gathering socially in groups larger than six unless it is for an exempted purpose.</li>', ''), ifelse(l_national_householdmixing == 1, '<li>People are currently prohibited from mixing with other households (e.g. visiting each other inside their homes).</li>', ''), ifelse(l_national_raves == 1, '<li>People are currently prohibited from gathering in large groups sometimes called raves, of typically 30 or more people (some exceptions of large gatherings apply). Many forms of raves are already prohibited in England and Wales.</li>', ''), ifelse(l_national_stayinglocal == 1, '<li>People are currently prohibited from leaving their local area or entering a protected area without a reasonable excuse.</li>', ''), ifelse(l_local_stayinghome == 1, '<li>People are currently prohibited from leaving their home without a reasonable excuse.</li>', ''), ifelse(l_national_notstayingaway == 1, '<li>People are currently prohibited from staying overnight somewhere other than their home without a reasonable excuse.</li>', ''), ifelse(l_national_businessclosures == 1, '<li>Certain businesses (often retail) must close physical shops and move to online/delivery/take away only services.</li>', ''), ifelse(l_national_openinghours == 1, '<li>Certain businesses (hospitality) must restrict openning hours (e.g. close early).</li>', ''), paste0('<li>Wearing a face covering is required in many public venues, in shops and when using public transport. This includes customers and staff members. Some exemptions apply.</li></ul>For further information see: ', l_url_national), ifelse(Restriction_type == 'Local', paste0('In addition to national restrictions, this area is subject to further local restrictions including: <br><ul>', ifelse(l_local_ruleofsix == 1, '<li>Not meeting with more than six people socially</li>', ''), ifelse(l_local_householdmixing == 1, '<li>Not mixing with people in other households</li>', ''),  ifelse(l_local_raves == 1, '<li>Not attending illegal raves</li>', ''), ifelse(l_local_stayinglocal == 1, '<li>Not leaving or entering a protected areas without reasonal excuse</li>', ''), ifelse(l_local_stayinghome == 1, '<li>Not leaving home without reasonal excuse</li>', ''), ifelse(l_local_notstayingaway == 1, '<li>Not staying overnight somewhere other than home without reasonal excuse</li>', ''),  ifelse(l_local_businessclosures == 1, '<li>Some businesses are not permitted to open</li>', ''), ifelse(l_local_openinghours == 1, '<li>Some businesses are required to restrict openning times</li>', ''), '</ul>For further information see: ', l_url_local), ''))) %>% 
  select(!c('lad19cd','LTLA19CD', 'UTLA19CD', 'UTLA19NM'))


leaflet(ltla_restrictions) %>% 
  addTiles() %>%
  addPolygons(stroke = FALSE, 
              smoothFactor = 0.3, 
              fillOpacity = .6,
              fillColor = ~ifelse(Restriction_type == 'National', '#ffb400', '#9762a2'),
              label = ~label)

geojson_write(geojson_json(ltla_restrictions), file = paste0(output_directory_x, '/ltla_covid_restrictions_hcl_latest.geojson'))

paste0('Information provided by the House of Commons Library Coronavirus Restrictions Tool (available: https://visual.parliament.uk/research/visualisations/coronavirus-restrictions-map/). Contains Parliamentary information licensed under the Open Parliament Licence v3.0.')


# scatter plot ltla high cases high increase vs low cases low increase. and by time.