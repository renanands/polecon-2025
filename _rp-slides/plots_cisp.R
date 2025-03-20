
library(dplyr)
library(ggplot2)
library(sf)

# -

CISP_sf <- 
  read_sf(here::here('data','CISPshp','lm_cisp_bd.shp'))

UPP_sf <- 
  read_sf(here::here('data','UPPshp','lm_upp_edit.shp'))

crimes_rj <- 
  read.csv(here::here('data','BaseDPEvolucaoMensalCisp.csv'), sep = ';') %>%
  left_join(CISP_sf, by = 'cisp') %>%
  st_as_sf() %>%
  st_make_valid()

# -

all_neighborhoods_2022 <- 
  geobr::read_neighborhood(year = 2022)

rio_neighborhoods_2022 <- 
  all_neighborhoods_2022 %>% 
  filter(code_muni == 3304557)

rio_neighborhoods_2022 <- 
  st_transform(rio_neighborhoods_2022, st_crs(UPP_sf)) 

crimes_rio$regiao %>% unique

# ----

crimes_plot_data <-
  crimes_rj %>% 
  filter(ano == 2022) %>%
  filter(regiao == 'Capital' | regiao == 'Baixada Fluminense') %>%
  summarise(.by = cisp,
            letal_sum = sum(letalidade_violenta))
  

lggplot(data = crimes_plot_data) +
  geom_sf(fill = 'gray80', color = 'gray8') +
  geom_sf(aes(fill = letalidade_violenta)) +
  scale_fill_gradient(low = "gray8", high = "cyan") +
  ggtitle('CISP')

ggplot() +
  geom_sf(data = rio_neighborhoods_2022, fill = "gray80", color = "gray8") +
  geom_sf(data = UPP_sf, fill = 'navy', color = 'blue') +
  ggtitle('UPPs em 2017 - sobre bairros do Rio')
