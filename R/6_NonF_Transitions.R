#############################################################################
## Code to build graphic for LULC changes Mapbiomas(1997 - 2022)
## Author : Tainá Rocha
## R verison 4.2.2 e 4.3 (26 April 2023)
## Rstudio version RStudio 2022.12.0+353 "Elsbeth Geranium" Release 
## Date: 09 May 2023
## Last updated: 10/05/2023
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

tranraw = readr::read_csv("data/mapBiomas/Transitions_Muni_Mapbiomas71.csv")

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
  

# Muni

Muni = geobr::read_municipality(code_muni = "all", year = 2020, simplified = TRUE)

## Manipulations 

# Biome analysis 

## Mapbiomas Anthropic areas in ha

remove_values = c("Grassland", "Wetland", "Rocky outcrop", "River, Lake and Ocean", "Salt flat", "Savanna Formation", "Forest Formation", "Wooded Restinga", "Non Observed", "Other Non Forest Natural Formation", "Other Non Vegetated Area", "Mangroove", "Magrove")

NonFtransitions = tranraw |>
  separate(city, into = c("city", "state", "biome"), sep = " - ") |>
  rename(name_muni = city) |> 
  filter(from_level_1== "2. Non Forest Natural Formation")|> 
  select(-c(21:43)) |> 
  select(-c(25:28)) |> 
  select(-c(26, 28, 29)) |>
  select(-c(30, 34, 35, 38, 39, 40, 43, 47, 49, 52)) |>
  na.omit() |>
  #mutate_all(~replace_na(.,0)) |> 
  pivot_longer(cols = starts_with(c("19", "20")), names_to = "year", values_to = "hectare") |> 
  group_by(YEAR = year, to_level_2, name_muni, biome) |> 
  summarize(Ha = sum(hectare)) |> 
  filter(!(to_level_2 %in% remove_values)) 


# Plot

# Define the colors for each "to_level_2" category
#to_level_2_colors = c("Pasture" = "red", 
#                       "Agriculture" = "yellow",
#                       "Forest Plantation" = "green",
#                       "Aquaculture" = "blue",
#                       "Mining" = "gray", 
#                      "Land-Use Mosaic" = "purple", 
#                       "Urban Infrastructure" = "black")

# Create the bar plot

nonF = ggplot(NonFtransitions, aes(x = YEAR, y = Ha, fill = to_level_2)) +
  geom_bar(stat = "identity", position = "stack") +
  #scale_fill_manual(values = to_level_2) +
  facet_wrap(~ biome, ncol = 2) +
  labs(x = " ", y = "Ha", fill = "Land use class") +
  #theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

nonf2 = nonF + scale_fill_manual(values = c("red", "green", "purple", "black","yellow", "gray"))

nonf2 + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# Join

TransJoin =  left_join(NonFtransitions, Muni, by = "name_muni") |> 
  na.omit()
  
# Map
# Convert the 'geom' column to sf geometry
TransJoin_sf = st_as_sf(TransJoin)

# Create a rank variable based on Ha within each year
TransJoin_sf = TransJoin_sf |> 
  group_by(YEAR) |> 
  mutate(rank = rank(Ha))

# Plot the faceted map
png(file="NonF_transintion_Maps.png",
    width=21, height=20, units="in", res=300)
TransJoin_sf|>
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  geom_sf(data = TransJoin_sf, aes(fill = rank)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Ha") +
  facet_wrap(~YEAR, ncol = 5) +
  labs(title = "Transitions from non forest  to anthropic* classes",
       #subtitle = "1997-2000",
       fill = "Ha")  +
  theme_void()
dev.off()
