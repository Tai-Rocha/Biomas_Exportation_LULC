######################################################################
## Code to build graphic Brazil exportation data from 1997 - 2021
## Author : Tainá Rocha
## R verison 4.2.2
## Rstudio version RStudio 2022.12.0+353 "Elsbeth Geranium" Release 
## Date: 08 February 2023
## Last updated: 31/03/2023
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

## BRL exportantion data

export = readr::read_csv("data/comexStat/exportBR_97_22_v2.csv")  |> 
  filter(coAno >= 1997 & coAno <= 2021) |> 
  rename(name_muni = municipality)

## export$coAno = as.character(export$coAno)

## Group the data by year and product
export_grouped = export |> 
  group_by(Year = coAno, noSh4pt, name_muni) |> 
  summarize(Exportation_KGL = sum(kgLiquido))


## Join export_grouped and munishape
expotMuniJoin = left_join(munishape, export_grouped, by = "name_muni", multiple = "all")  |> 
  na.omit() |> 
  st_make_valid()

## Plot 

# 1997_2000

png(file="export_1997_2000.png",
    width=20, height=8, units="in", res=300)
expotMuniJoin |> 
  filter(Year >= 1997 & Year <= 2000) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Exportation_KGL)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Municipality exports",
       #subtitle = "1997-2000",
       fill = "kg")  +
  theme_void()
dev.off()

# 2001_2003
png(file="export_2001_2003.png",
    width=15, height=5, units="in", res=300)
expotMuniJoin |> 
  filter(Year >= 2001 & Year <= 2003) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Exportation_KGL)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Municipality exports",
       #subtitle = "2001-2003",
       fill = "kg")  +
  theme_void()
dev.off()

# 2004_2006
png(file="export_2004_2006.png",
    width=15, height=5, units="in", res=300)
expotMuniJoin |> 
  filter(Year >= 2004 & Year <= 2006) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Exportation_KGL)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Municipality exports",
       #subtitle = "2004-2006",
       fill = "kg")  +
  theme_void()
dev.off()


# 2007_2009
png(file="export_2007_2009.png",
    width=15, height=5, units="in", res=300)
expotMuniJoin |> 
  filter(Year >= 2007 & Year <= 2009) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Exportation_KGL)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Municipality exports",
       #subtitle = "2007-2009",
       fill = "kg")  +
  theme_void()
dev.off()


# 2010_2012
png(file="export_2010_2012.png",
    width=15, height=5, units="in", res=300)
expotMuniJoin |> 
  filter(Year >= 2010 & Year <= 2012) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Exportation_KGL)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Municipality exports",
       #subtitle = "2010-2012",
       fill = "kg")  +
  theme_void()
dev.off()

# 2013_2015
png(file="export_2013_2015.png",
    width=15, height=5, units="in", res=300)
expotMuniJoin |> 
  filter(Year >= 2013 & Year <= 2015) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Exportation_KGL)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Municipality exports",
       #subtitle = "2013-2015",
       fill = "kg")  +
  theme_void()
dev.off()

# 2016_2018
png(file="export_2016_2018.png",
    width=15, height=5, units="in", res=300)
expotMuniJoin |> 
  filter(Year >= 2016 & Year <= 2018) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Exportation_KGL)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Municipality exports",
       #subtitle = "2016-2018",
       fill = "kg")  +
  theme_void()
dev.off()

# 2019_2021
png(file="export_2019_2021.png",
    width=15, height=5, units="in", res=300)
expotMuniJoin |> 
  filter(Year >= 2019 & Year <= 2021) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = biomeshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Exportation_KGL)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Municipality exports",
       #subtitle = "2019-2021",
       fill = "kg")  +
  theme_void()
dev.off()

# End

###########################################################################################

## Plot- Works but not beauty
## Create the map
#expotMuniJoin |>  
#  filter(Year >= 1997 & Year <= 2008) |> 
#ggplot() +
# Use facet_wrap to create a map for each year
#  facet_wrap(~Year, ncol = 3) +
# Add the polygons for each municipality
#  geom_sf(data = expotMuniJoin, aes(fill = Exportation_KGL), color = "black", size = 0.1) +
# Set the color palette
#  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90", 
#                       name = "Rank", guide = guide_legend(reverse = TRUE))


## Not necessary. Create a new variable with the rank of kgLiquido for each year and municipality

#df_2aggJOIN = df_aggJOIN |>  
#  group_by(coAno, name_muni) |>  
#  summarise(kgLiquido_sum = sum(kgLiquido)) |>  
#  ungroup() |>  
#  arrange(desc(kgLiquido_sum))  |>  
#  mutate(rank = rank(desc(kgLiquido_sum), ties.method = "first"))


# Graphic per state... not proper but interest plot - ok
#df_grouped <- export_filtered %>%
#  group_by(state_acronym, coAno) %>%
#  summarise(total_kgLiquido = sum(kgLiquido)) %>%
# arrange(desc(total_kgLiquido))  # Arrange by descending order of total_kgLiquido

#ggplot(df_grouped, aes(x = coAno, y = total_kgLiquido, fill = state_acronym)) +
#  geom_bar(stat = "identity") +
#  labs(x = "Year", y = "Exportation kg", fill = "State", 
#       title = "Total Exportation Weight per State and Year") +
#  theme_minimal()

# Other plot
#ggplot() +
  # Add the biomes lines contours
#  geom_sf(data = biomes, color = "gray", fill = NA, size = 0.1) +
  # Add the municipalities
#  geom_sf(data = df_subset, aes(fill = kgLiquido)) +
  # Add the legend for the kgLiquido scale
#  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
#  facet_wrap(~ coAno, ncol = 5) +
  # Remove axis ticks and labels
#  theme(axis.ticks = element_blank(),
#        axis.text = element_blank(),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank())

