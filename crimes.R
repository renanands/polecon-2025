
library(dplyr)
library(ggplot2)
library(sf)

# ----------------------------------------------------------------------------
# ---------------                DATA PROCESSING               ---------------
# ----------------------------------------------------------------------------

# read shps

CISP_sf <- 
  read_sf(here::here('data','CISPshp','lm_cisp_bd.shp')) %>%
  st_make_valid()

UPP_sf <- 
  read_sf(here::here('data','UPPshp','lm_upp_edit.shp'))

# ---------------

# read crime data

crimes_rj <- 
  read.csv(here::here('data','BaseDPEvolucaoMensalCisp.csv'), sep = ';') %>%
  left_join(CISP_sf, by = 'cisp') %>%
  st_as_sf() 

# ---------------

# read geobr sf

all_muni_2022 <- 
  geobr::read_municipality(year = 2022)

# -

all_neighborhoods_2022 <- 
  geobr::read_neighborhood(year = 2022)

rio_neighborhoods_2022 <- 
  all_neighborhoods_2022 %>% 
  filter(code_muni == 3304557) %>%
  st_transform(., st_crs(UPP_sf)) 

# ----------------------------------------------------------------------------
# ------------------                PLOTTING               -------------------
# ----------------------------------------------------------------------------

crimes_plot_data <-
  crimes_rj %>% 
  filter(ano == 2024) %>%
  filter(regiao == 'Capital' | regiao == 'Baixada Fluminense') %>%
  summarise(.by = cisp,
            across(geometry, st_union),
            letal_sum = sum(letalidade_violenta))
  
crimes_plot <-
  ggplot(data = crimes_plot_data) +
  geom_sf(fill = 'gray80', color = 'gray8') +
  geom_sf(aes(fill = letal_sum)) +
  scale_fill_gradient(low = "midnightblue", high = "cyan") +
  labs(fill = 'Counts\n') +
  ggtitle('Violent deaths in 2024 by CISP')

ggsave(here::here('out', 'crimes_plot.png'), crimes_plot)

# -

upp_plot <-
  ggplot() +
  geom_sf(data = all_muni_2022 %>% st_crop(xmin = -43.6,
                                           xmax = -43.0,
                                           ymin = -23.5,
                                           ymax = -22.7),
          fill = "gray15", color = "gray50") +
  geom_sf(data = UPP_sf, fill = 'dodgerblue', color = 'navy') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('UPPs in 2017')


ggsave(here::here('out', 'upp_plot.png'), upp_plot)
