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

MM = readr::read_csv("data/mapBiomas/Mining_Muni.csv")


## Manipulation 

## Mining in ha

MiningMuni = MM |>
  #filter(level_3 == "2.2.01 Ferro")  |> 
  pivot_longer(cols = starts_with(c("19", "20")), names_to = "year", values_to = "hectare") |> 
  group_by(YEAR = year, municipality, level_1) |> 
  na.omit() |> 
  summarize(Ha = sum(hectare)) |> 
  mutate_at('YEAR', as.numeric)



biomeshape = geobr::read_biomes(year = 2019, simplified = TRUE, showProgress = TRUE) |>
  na.omit()

Muni = geobr::read_municipality(year = 2020, simplified = TRUE, showProgress = TRUE) |> 
  dplyr::rename(municipality = name_muni)

MuniJoin = left_join(MiningMuni, Muni, by = "municipality") |> 
  st_as_sf() |> 
  st_make_valid()

## Plot

# Create the ggplot object

ggplot(MuniJoin) +
  geom_textline(aes(
    x = YEAR, y = Ha, group = municipality, colour = municipality, label = municipality
  ),
  hjust = 1
  ) +
  theme(legend.position = "none")

## MAP
png(file="Mining.png",
    width=21, height=20, units="in", res=300)
MuniJoin |>
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
  labs(title = "Minig",
       #subtitle = "1997-2000",
       fill = "Ha")  +
  theme_void()
dev.off()

# Plot
ggplot(MuniJoin) +
  geom_textline(aes(
    x = YEAR, y = Ha, group = municipality, colour = municipality, label = municipality
  ),
  hjust = 1
  ) +
  theme(legend.position = "none")
#### End

M = ggplot(MuniJoin, aes(x = YEAR, y = Ha, fill = municipality)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = municipality) +
  facet_wrap(~ biomeshape, ncol = 2) +
  labs(x = " ", y = "Ha", fill = "Municipality") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

M +  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



png(file="Mining_2001_2003.png",
    width=20, height=8, units="in", res=300)
MuniJoin |>
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
  labs(title = "Mining",
       #subtitle = "2001-2003",
       fill = "Ha")  +
  theme_void()
dev.off()


png(file="Mining_2004_2006.png",
    width=20, height=8, units="in", res=300)
MuniJoin |>
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
  labs(title = "Mining",
       #subtitle = "2004-2006",
       fill = "Ha")  +
  theme_void()
dev.off()


png(file="Mining_2007_2009.png",
    width=20, height=8, units="in", res=300)
MuniJoin |>
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
  labs(title = "Mining",
       #subtitle = "2007-2009",
       fill = "Ha")  +
  theme_void()
dev.off()

png(file="Mining_2007_2009.png",
    width=20, height=8, units="in", res=300)
MuniJoin |>
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
  labs(title = "Mining",
       #subtitle = "2007-2009",
       fill = "Ha")  +
  theme_void()
dev.off()