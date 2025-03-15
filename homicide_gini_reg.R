library(dplyr)

# --

gini_indices <-
  readODS::read_ods(here::here('data', 'ginibr.ods')) %>%
  # split 1st col by the 1st white space
  tidyr::separate(., Município,
                  into = c("code_muni", "name_muni"),
                  sep = "^\\S*\\K\\s+") %>%
  # # remove some of name_muni's text fmt 
  # mutate(name_muni = 
  #          stringi::stri_trans_general(str = .$name_muni,
  #                                      id = "Latin-ASCII") %>% toupper()) %>%
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
  mutate(gini_period = as.numeric(gini_period))

# --

homicide_rates <-
  read.csv(here::here('data', 'taxa-homicidios.csv'), sep=';') %>%
  rename(code_muni = cod,
         name_muni = nome,
         year = `período`,
         rate = valor)

mean_homicide_rates <-
  homicide_rates %>%
  mutate(
    gini_period = case_when(
      year <= 1991 ~ 1991,
      year > 1991 & year <= 2000 ~ 2000,
      year > 2000 & year <= 2010 ~ 2010
    )
  ) %>%
  summarise(.by = c(name_muni, gini_period),
            mean_rate = mean(rate)) %>%
  mutate(gini_period = as.numeric(gini_period))

# --

reg_data <-
  left_join(mean_homicide_rates,
            gini_indices,
            by = c('name_muni', 'gini_period'))

fixest::feols(mean_rate ~ gini_index,
              data = reg_data)
