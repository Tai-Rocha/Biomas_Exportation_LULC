######################################################################
## Code to build graphic biomes lulc changes data from 1997 - 2021
## Author : Tainá Rocha
## R verison 4.2.2
## Rstudio version RStudio 2022.12.0+353 "Elsbeth Geranium" Release 
## Date: 31 MArch 2023
## Last updated: 01/04/2023
######################################################################

## Packages
library(directlabels)
library(dplyr)
library(geobr)
library(ggplot2)
library(ggthemes)
library(networkD3)
library(RColorBrewer)
library(readr)
library(sf)
library(stringr)
library(tidyr)
library(tmap)
library(viridis)
library(ggrepel)

######################################################################


## READ data 

## Municipalities shape

munishape = geobr::read_municipality(code_muni = "all", year = 2020, simplified = TRUE, showProgress = TRUE)

## Biome shape

biomeshape = geobr::read_biomes(year = 2019, simplified = TRUE, showProgress = TRUE) |>
  na.omit()


##### MapBiomas 

mapbiomas = readr::read_csv("data/MAPBIOMAS_DESMAT_VEGSEC_CityUfBiome.csv") 

# split the 'city' column into three separate columns
mapbiomas <- separate(mapbiomas, city, into = c("city", "state", "biome"), sep = " - ")

map_bioma_ha = mapbiomas |>  
  select(-color) |>
  filter(level_0 == "Natural") |> 
  #mutate_all(~replace_na(.,0)) |> 
  #separate(city, c("city", "state", "biome"), sep = "-") |> 
  gather(year, hectare, starts_with("19"), starts_with("20")) |> 
  group_by(YEAR = year, biome) |> 
  summarize(Ha = sum(hectare)) |> 
  mutate_at('YEAR', as.numeric)


ggplot(map_bioma_ha, aes(x = YEAR, y = Ha, color = biome)) +
  geom_line() +
  labs(x = "Year", y = "Hectare") +
  scale_color_discrete(name = "Biome")