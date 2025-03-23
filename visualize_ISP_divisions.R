
library(dplyr)
library(ggplot2)
library(sf)

CISP_sf <- 
  read_sf(here::here('data','CISPshp','lm_cisp_bd.shp'))

UPP_sf <- 
  read_sf(here::here('data','UPPshp','lm_upp_edit.shp'))

crimes_rio <- 
  read.csv(here::here('data','BaseDPEvolucaoMensalCisp.csv'), sep = ';') %>%
  left_join(CISP_sf, by = 'cisp')

# -

all_neighborhoods_2022 <- 
  geobr::read_neighborhood(year = 2022)

rio_neighborhoods_2022 <- 
  all_neighborhoods_2022 %>% 
  filter(code_muni == 3304557)

rio_neighborhoods_2022 <- 
  st_transform(rio_neighborhoods_2022, st_crs(UPP_sf)) 

# ----

ggplot(data = crimes_rio$geometry) +
  geom_sf(aes(color = cisp)) +
  ggtitle('CISP')

ggplot() +
  geom_sf(data = rio_neighborhoods_2022, fill = "gray80", color = "gray8") +
  geom_sf(data = UPP_sf, fill = 'navy', color = 'blue') +
  ggtitle('UPPs em 2017 - sobre bairros do Rio')
