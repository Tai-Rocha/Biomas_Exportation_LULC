#############################################################################
## Code to build graphic for LULC changes Mapbiomas(1997 - 2022)
## Author : Tainá Rocha
## R verison 4.2.2 e 4.3 (26 April 2023)
## Rstudio version RStudio 2022.12.0+353 "Elsbeth Geranium" Release 
## Date: 11 May 2023
## Last updated: 11/05/2023
#############################################################################
## Packages

library(dplyr)
library(geobr)
library(ggplot2)
library(ggthemes)
library(geomtextpath)
library(RColorBrewer)
library(readr)
library(sf)
library(stringr)
library(tidyr)
library(tmap)
library(viridis)

## READ mapbiomas data

#RAWmapbiomas = readr::read_csv("data-raw/mapBiomas_raw/TABELA_GERAL_COL7_MAPBIOMAS_DESMAT_VEGSEC_UF_Muni_Biome_aba.csv")

mapbiomasraw = readr::read_csv("data/mapBiomas/TABELA-GERAL-COL71-MAPBIOMAS-BIOMASxMUNICIPIOS-v2.csv")

## Read Biome shape

biomeshape = geobr::read_biomes(year = 2019, simplified = TRUE, showProgress = TRUE) |>
  na.omit()
#mutate(across('name_biome', str_replace, 'Amazônia', 'AMAZONIA')) |> 
#mutate(across('name_biome', str_replace, 'Mata Atlântica', 'MATA ATLNTICA')) |>
#mutate(across('name_biome', str_replace, 'Cerrado', 'CERRADO')) |>
#mutate(across('name_biome', str_replace, 'Caatinga', 'CAATINGA')) |>
#mutate(across('name_biome', str_replace, 'Pantanal', 'PANTANAL')) |>
#mutate(across('name_biome', str_replace, 'Pampa', 'PAMPA')) |>
#rename(biome = name_biome) |> 


## Read Muni shape

Muni = geobr::read_municipality(code_muni = "all", year = 2020, simplified = TRUE)

## Manipulations 

# Biome analysis 

## Mapbiomas Anthropic areas in ha

pasture = mapbiomasraw |>
  separate(city, into = c("city", "state", "biome"), sep = " - ") |>
  rename(name_muni = city) |> 
  filter(level_2 == "Pasture")  |> 
  select(-color) |>
  pivot_longer(cols = starts_with(c("19", "20")), names_to = "year", values_to = "hectare") |> 
  group_by(YEAR = year, name_muni) |> 
  na.omit() |> 
  summarize(Ha = sum(hectare)) |> 
  mutate_at('YEAR', as.numeric)

## Join mapbiomas 

pasturebidnded = left_join(pasture, Muni, by = "name_muni") |> 
  st_as_sf() |> 
  st_make_valid()

## 1997 - 2021

png(file="Pasture_Muni.png",
    width=21, height=20, units="in", res=300)
pasturebidnded |>
  filter(YEAR >= 1997 & YEAR <= 2021) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 5) +
  labs(title = "Pasture",
       #subtitle = "1997-2021",
       fill = "Ha")  +
  theme_void()
dev.off()
########## End

