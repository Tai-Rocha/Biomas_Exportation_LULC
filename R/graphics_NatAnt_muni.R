#############################################################################
## Code to build graphic for antrhopic and natural vegetation from Mapbiomas(1997 - 2022)
## Author : Tainá Rocha
## R verison 4.2.2
## Rstudio version RStudio 2022.12.0+353 "Elsbeth Geranium" Release 
## Date: 08 February 2023
## Last updated: 28/03/2023
#############################################################################

## Packages

library(dplyr)
library(geobr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(readr)
library(sf)
library(stringr)
library(tidyr)
library(tmap)
library(viridis)


## read mapbiomas data 

mapbiomas = readr::read_csv("data-raw/mapBiomas_raw/TABELA_GERAL_COL7_MAPBIOMAS_DESMAT_VEGSEC_UF_Muni_Biome_aba.csv") 

# Biome analysis 

# split the 'city' column into three separate columns
mapbiomasmuni = mapbiomas |> 
  separate(city, into = c("city", "state", "biome"), sep = " - ") |>
  rename("name_muni" = city) |> 
  filter(level_1 == "2. Non Forest Natural Formation")  |> 
  select(-color) |>
  # mutate_all(~replace_na(.,0)) |> This is wrong
  gather(year, hectare, starts_with("19"), starts_with("20")) |> 
  group_by(YEAR = year, name_muni) |> 
  summarize(Ha = sum(hectare)) |> 
  mutate_at('YEAR', as.numeric)  |> 
  na.omit()


# Read municipalities

muni = geobr::read_municipality(
  code_muni = "all",
  year = 2020,
  simplified = TRUE,
  showProgress = TRUE
)

## Join geobr and mapbiomasmuni 

mapmuniJOIN= left_join(mapbiomasmuni, muni, by = "name_muni", multiple = "all") |> 
  st_as_sf()


mapmuniJOIN |>
  filter(YEAR >= 2019 & YEAR <= 2021) |> 
  ggplot() +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "_",
       #subtitle = "1997-2000",
       fill = "Ha")  +
  theme_void()