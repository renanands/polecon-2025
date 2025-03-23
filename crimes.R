
library(dplyr)
library(ggplot2)
library(sf)

# -------------------------------------------------------------------------
# -----------------            DATA PROCESSING             ----------------
# -------------------------------------------------------------------------

# read shps

CISP_sf <- 
  read_sf(here::here('data','CISPshp','lm_cisp_bd.shp')) %>%
  st_make_valid()

UPP_sf <- 
  read_sf(here::here('data','UPPshp','lm_upp_edit.shp'))

# ---------------------------------------------------------------------

# read crime (cisp) data

crimes_rj <- 
  read.csv(here::here('data','BaseDPEvolucaoMensalCisp.csv'), sep = ';') %>%
  left_join(CISP_sf, by = 'cisp') %>%
  st_as_sf() 

# ---------------------------------------------------------------------

# read shootings (rio metropol) data

shootings_metropol_rio <- 
  read.csv(here::here('data','shootings_metropol_rio.csv')) %>%
  st_as_sf(., coords = c('longitude', 'latitude'), crs = 4326)

# ---------------------------------------------------------------------

# read gini (muni) data

gini_indices <-
  readODS::read_ods(here::here('data', 'ginibr.ods')) %>%
  # split 1st col by the 1st white space
  tidyr::separate(., Município,
                  into = c("code_muni", "name_muni"),
                  sep = "^\\S*\\K\\s+") %>%
  # remove some of name_muni's text fmt 
  mutate(name_muni =
           stringi::stri_trans_general(str = .$name_muni,
                                       id = "Latin-ASCII") %>% toupper()) %>%
  # triggers warnings on purpose: I'm coercing '...' -> NS
  mutate(`1991` = as.numeric(.$`1991`),
         `2000` = as.numeric(.$`2000`),
         `2010` = as.numeric(.$`2010`)) %>%
  # tidying data
  tidyr::pivot_longer(
    .,
    cols = c(`1991`, `2000`, `2010`), 
    names_to = "gini_period",
    values_to = "gini_index"
  ) %>%
  mutate(gini_period = as.numeric(gini_period)) %>% 
  mutate(code_muni = as.numeric(code_muni))

# ---------------------------------------------------------------------

# read homicide rates (muni) data

homicide_rates <-
  read.csv(here::here('data', 'taxa-homicidios.csv'), sep=';') %>%
  rename(code_muni = cod,
         name_muni = nome,
         year = `período`,
         rate = valor) %>%
  mutate(name_muni =
           stringi::stri_trans_general(str = .$name_muni,
                                       id = "Latin-ASCII") %>% toupper())

mean_homicide_rates <-
  homicide_rates %>%
  mutate(
    gini_period = case_when(
      year <= 1991 ~ 1991,
      year > 1991 & year <= 2000 ~ 2000,
      year > 2000 & year <= 2010 ~ 2010
    )
  ) %>%
  summarise(.by = c(name_muni, gini_period), # code_muni not uniform
            mean_rate = mean(rate)) %>% 
  # mutate(code_muni = as.numeric(code_muni)) %>%
  mutate(gini_period = as.numeric(gini_period))

# ---------------------------------------------------------------------

# read pop (muni) 2010

muni_pop_2010 <-
  read.csv(here::here('data', 'muni_pop_2010.csv'), sep=';') %>%
  # remove some of name_muni's text fmt 
  mutate(name_muni =
           stringi::stri_trans_general(str = .$name_muni,
                                       id = "Latin-ASCII") %>% toupper())

# ---------------------------------------------------------------------

# read geobr sf

all_muni <- 
  geobr::read_municipality(year = 2010, simplified = FALSE) %>%
  st_make_valid() %>%
  mutate(name_muni =
           stringi::stri_trans_general(str = .$name_muni,
                                       id = "Latin-ASCII") %>% toupper())

# ---

all_neighborhoods <- 
  geobr::read_neighborhood(year = 2010, simplified = FALSE) %>%
  mutate(name_muni =
           stringi::stri_trans_general(str = .$name_muni,
                                       id = "Latin-ASCII") %>% toupper())

rio_neighborhoods <- 
  all_neighborhoods %>% 
  filter(code_muni == 3304557) %>%
  st_transform(., st_crs(UPP_sf)) 

# -------------------------------------------------------------------------
# -----------------                PLOTTING                ----------------
# -------------------------------------------------------------------------

# rio plots

crimes_plot_data <-
  crimes_rj %>% 
  filter(ano == 2024) %>%
  st_crop(xmin = -44.0,
          xmax = -42.6,
          ymin = -23.1,
          ymax = -22.4) %>%
  summarise(.by = cisp,
            across(geometry, st_union),
            letal_sum = sum(letalidade_violenta))

# ---
  
viol_deaths_plot <-
  ggplot(data = crimes_plot_data) +
  theme_minimal() +
  geom_sf(fill = 'gray80', color = 'gray8') +
  geom_sf(aes(fill = letal_sum)) +
  scale_fill_gradient(low = "midnightblue", high = "cyan") +
  labs(fill = 'Counts\n')
#   ggtitle('Violent deaths in 2024 by CISP') +
#   theme(plot.title = element_text(hjust = 0.5))
 
# crimes_plot
ggsave(here::here('out', 'viol_deaths_plot.png'), viol_deaths_plot)

# ---

shootings_plot <-
  ggplot() +
  theme_minimal() +
  geom_sf(data = crimes_plot_data,
          fill = "gray8", color = "gray50") +
  geom_sf(data = shootings_metropol_rio %>% filter(data > '01-01-2024'),
          alpha = 0.1, size = 0.1, color = 'magenta')
#   ggtitle('Shootings: Greater Rio (2024)') +
#   theme(plot.title = element_text(hjust = 0.5))
 
# shootings_plot
ggsave(here::here('out', 'shootings_plot.png'), shootings_plot)

# ---

upp_plot <-
  ggplot() +
  theme_minimal() +
  geom_sf(data = crimes_plot_data$geometry %>% st_crop(xmin = -43.6,
                                           xmax = -43.0,
                                           ymin = -23.5,
                                           ymax = -22.7),
          fill = "gray15", color = "gray50") +
  geom_sf(data = UPP_sf, fill = 'dodgerblue', color = 'navy')
#   ggtitle('UPPs in 2017') +
#   theme(plot.title = element_text(hjust = 0.5))
 
# upp_plot
ggsave(here::here('out', 'upp_plot.png'), upp_plot)

# ---------------------------------------------------------------------

# reg data (muni)

reg_data <-
  left_join(mean_homicide_rates,
            gini_indices,
            by = c('name_muni', 'gini_period')) %>%
  left_join(muni_pop_2010 %>% filter(pop_count > 500000),
            by = 'name_muni') %>%
  mutate(is_big = ifelse(!is.na(pop_count), 1, 0))

head(reg_data)

# ---------------------------------------------------------------------

# -- muni plots

muni_plot_data <-
  left_join(reg_data,
            all_muni,
            by = 'name_muni') %>%
  filter(gini_period == 2010) %>%
  st_as_sf() %>%
  st_crop(xmin = -44.0,
          xmax = -42.6,
          ymin = -23.1,
          ymax = -22.4)

# - homicide rates

homicide_plot <-
  ggplot() +
  theme_minimal() +
  geom_sf(data = muni_plot_data, aes(fill = mean_rate), color = NA) +
  scale_fill_gradient(low = 'navy', high = 'magenta') +
  labs(fill = 'Mean rate per 100k')
  # ggtitle('Homicide rate by municipality in 2010') +
  # theme(plot.title = element_text(hjust = 0.5))

# homicide_plot
ggsave(here::here('out', 'homicide_plot.png'), homicide_plot)

# - gini indices

gini_plot <-
  ggplot(data = muni_plot_data) +
  theme_minimal() +
  geom_sf(aes(fill = gini_index), color = NA) +
  scale_fill_gradient(low = 'navy', high = 'magenta') +
  labs(fill = 'Index')
  # ggtitle('GINI index by municipality in 2010') +
  # theme(plot.title = element_text(hjust = 0.5))

# gini_plot
ggsave(here::here('out', 'gini_plot.png'), gini_plot)

# -------------------------------------------------------------------------
# -------------------            REGRESSIONS             ------------------
# -------------------------------------------------------------------------

no_FE <- fixest::feols(mean_rate ~ gini_index,
              data = reg_data)

ols <- fixest::feols(mean_rate ~ gini_index | code_muni + gini_period,
              data = reg_data)

ols500k <- fixest::feols(mean_rate ~ gini_index*is_big | code_muni + gini_period,
                     data = reg_data)

gm <- fixest::feglm(mean_rate ~ gini_index | code_muni + gini_period,
              data = reg_data)
