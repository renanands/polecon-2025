library(dplyr)
library(ggplot2)
library(sf)

# --------------------------------------------------------------------------

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

# --

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

# --------------------------------------------------------------------------

# reg

all_muni <- # maybe useful for FEs later (or mesoregs, pop arrangms etc)
  geobr::read_municipality(year = 2010) %>%
  mutate(name_muni =
           stringi::stri_trans_general(str = .$name_muni,
                                       id = "Latin-ASCII") %>% toupper())

reg_data <-
  left_join(mean_homicide_rates,
            gini_indices,
            by = c('name_muni', 'gini_period')) 

fixest::feols(mean_rate ~ gini_index,
              data = reg_data)

# --------------------------------------------------------------------------

# 2010 plots

plot_data <-
  left_join(reg_data,
            all_muni,
            by = 'name_muni') %>%
  filter(gini_period == 2010) %>%
  st_as_sf()

homicide_plot <-
  ggplot() +
  geom_sf(data = plot_data, aes(fill = mean_rate), color = NA) + # , color='gray8' ;;; lw
  scale_fill_gradient(low = 'gray8', high = 'darkolivegreen1') +
  ggtitle('Homicide rate by municipality in 2010')

ggsave(here::here('out', 'BUGGED_homicide_plot.png'), homicide_plot)

gini_plot <-
  ggplot(data = plot_data) +
  geom_sf(aes(fill = gini_index), color = NA) +
  scale_fill_gradient(low = 'gray8', high = 'magenta') +
  ggtitle('GINI index by municipality in 2010')

ggsave(here::here('out', 'BUGGED_gini_plot.png'), gini_plot)

