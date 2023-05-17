#############################################################################
## Code to build graphic for LULC changes Mapbiomas(1997 - 2022)
## Author : Tainá Rocha
## R verison 4.2.2 e 4.3 (26 April 2023)
## Rstudio version RStudio 2022.12.0+353 "Elsbeth Geranium" Release 
## Date: 01 April 2023
## Last updated: 05/04/2023
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

AntroMapbiomas = mapbiomasraw |>
  separate(city, into = c("city", "state", "biome"), sep = " - ") |>
  rename(name_muni = city) |> 
  filter(level_0 == "Anthropic")  |> 
  select(-color) |>
  #mutate_all(~replace_na(.,0)) |> 
  pivot_longer(cols = starts_with(c("19", "20")), names_to = "year", values_to = "hectare") |> 
  group_by(YEAR = year, name_muni) |> 
  na.omit() |> 
  summarize(Ha = sum(hectare)) |> 
  mutate_at('YEAR', as.numeric)

## Join mapbiomas 

bindedAnthropic = left_join(AntroMapbiomas, Muni, by = "name_muni") |> 
  st_as_sf() |> 
  st_make_valid()

## 1997 - 2021

png(file="Anthropic_Muni.png",
    width=21, height=20, units="in", res=300)
bindedAnthropic |>
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
  labs(title = "Anthropization",
       #subtitle = "1997-2000",
       fill = "Ha")  +
  theme_void()
dev.off()
########## End

# 1997_2000

png(file="anthropic_1997_2000.png",
    width=20, height=8, units="in", res=300)
bindedAnthropic |>
  filter(YEAR >= 1997 & YEAR <= 2000) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "1997-2000",
       fill = "Ha")  +
  theme_void()
dev.off()

# 1997_2000

png(file="anthropic_1997_2000.png",
    width=20, height=8, units="in", res=300)
bindedAnthropic |>
  filter(YEAR >= 1997 & YEAR <= 2000) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "1997-2000",
       fill = "Ha")  +
  theme_void()
dev.off()


# 2001_2003

png(file="anthropic_2001_2003.png",
    width=20, height=8, units="in", res=300)
bindedAnthropic |>
  filter(YEAR >= 2001 & YEAR <= 2003) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2001-2003",
       fill = "Ha")  +
  theme_void()
dev.off()

# 2004_2006

png(file="anthropic_2004_2006.png",
    width=20, height=8, units="in", res=300)
bindedAnthropic |>
  filter(YEAR >= 2004 & YEAR <= 2006) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2004-2006",
       fill = "Ha")  +
  theme_void()
dev.off()

# 2007_2009

png(file="anthropic_2007_2009.png",
    width=20, height=8, units="in", res=300)
bindedAnthropic |>
  filter(YEAR >= 2007 & YEAR <= 2009) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2007-2009",
       fill = "Ha")  +
  theme_void()
dev.off()


# 2010_2012

png(file="anthropic_2010_2012.png",
    width=20, height=8, units="in", res=300)
bindedAnthropic |>
  filter(YEAR >= 2010 & YEAR <= 2012) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2010-2012",
       fill = "Ha")  +
  theme_void()
dev.off()


# 2013_2015

png(file="anthropic_2013_2015.png",
    width=20, height=8, units="in", res=300)
bindedAnthropic |>
  filter(YEAR >= 2013 & YEAR <= 2015) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2013-2015",
       fill = "Ha")  +
  theme_void()
dev.off()


# 2016_2018

png(file="anthropic_2016_2018.png",
    width=20, height=8, units="in", res=300)
bindedAnthropic |>
  filter(YEAR >= 2016 & YEAR <= 2018) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2016-2018",
       fill = "Ha")  +
  theme_void()
dev.off()

# 2019_2021

png(file="anthropic_2019_2021.png",
    width=20, height=8, units="in", res=300)
bindedAnthropic |>
  filter(YEAR >= 2019 & YEAR <= 2021) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2019-2021",
       fill = "Ha")  +
  theme_void()
dev.off()
