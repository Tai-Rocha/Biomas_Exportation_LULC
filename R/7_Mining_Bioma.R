#############################################################################
## Code to build graphic for Mining (1997 - 2022)
## Author : Tainá Rocha
## R verison 4.2.2
## Rstudio version RStudio 2022.12.0+353 "Elsbeth Geranium" Release 
## Date: 18 April 2023
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

## READ Mining data

BiomaM = readr::read_csv("data/mapBiomas/Mining_Biome.csv")

## Manipulation 

## Mining in ha

Mining = BiomaM |>
  filter(level_3 == "2.2.01 Ferro")  |> 
  pivot_longer(cols = starts_with(c("19", "20")), names_to = "year", values_to = "hectare") |> 
  group_by(YEAR = year, biome, level_1) |> 
  na.omit() |> 
  summarize(Ha = sum(hectare)) |> 
  mutate_at('YEAR', as.numeric)


# Plot
ferro = ggplot(Mining) +
  geom_textline(aes(
    x = YEAR, y = Ha, group = biome, colour = biome, label = biome
  ),
  hjust = 1
  ) +
  theme(legend.position = "none")

ferro +  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
