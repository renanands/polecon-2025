
library(dplyr)
library(geobr)
library(ggplot2)
library(sf)

RISP_sf <- read_sf('./data/RISPshp/Limite_RISP_WGS.shp')
AISP_sf <- read_sf('./data/AISPshp/lm_aisp_072024.shp')
CISP_sf <- read_sf('./data/CISPshp/lm_cisp_bd.shp')

REGIOES_sf <- read_sf('./data/REGIOESshp/LM_REGIAO_SESEG.shp')

UPP_sf <- read_sf('./data/UPPshp/lm_upp_edit.shp')

#
# all TRUE
#
# st_crs(RISP_sf) == st_crs(AISP_sf)
# st_crs(AISP_sf) == st_crs(CISP_sf)
# st_crs(CISP_sf) == st_crs(REGIOES_sf)
# st_crs(REGIOES_sf) == st_crs(UPP_sf)

# -

all_neighborhoods_2010 <- read_neighborhood(year = 2010)
all_neighborhoods_2022 <- read_neighborhood(year = 2022)

rio_neighborhoods_2010 <- all_neighborhoods_2010 %>% filter(code_muni == 3304557)
rio_neighborhoods_2022 <- all_neighborhoods_2022 %>% filter(code_muni == 3304557)

rio_neighborhoods_2010 <- st_transform(rio_neighborhoods_2010, st_crs(UPP_sf)) 
rio_neighborhoods_2022 <- st_transform(rio_neighborhoods_2022, st_crs(UPP_sf)) 

# ----

ggplot() +
  geom_sf(data=RISP_sf) +
  ggtitle('RISP')
ggplot() +
  geom_sf(data=AISP_sf) +
  ggtitle('AISP')
ggplot() +
  geom_sf(data=CISP_sf) +
  ggtitle('CISP')
ggplot() +
  geom_sf(data=REGIOES_sf) +
  ggtitle('Grandes regiões')

ggplot() +
  geom_sf(data = REGIOES_sf, fill = "gray80", color = "gray8") +
  geom_sf(data=UPP_sf, fill='navy', color='blue') +
  ggtitle('UPPs em 2017 - sobre Grandes Regiões')

ggplot() +
  geom_sf(data = rio_neighborhoods_2010, fill = "gray80", color = "gray8") +
  geom_sf(data=UPP_sf, fill='navy', color='blue') +
  ggtitle('UPPs em 2017 - sobre bairros do Rio')
