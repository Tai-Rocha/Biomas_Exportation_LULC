######################################################################
## Code to build graphic Cerrado exportation data from 1997 - 2021
## Author : Tainá Rocha
## R verison 4.2.2
## Rstudio version RStudio 2022.12.0+353 "Elsbeth Geranium" Release 
## Date: 29 March 2023
## Last updated: --/--/2023
######################################################################

## Packages

library(dplyr)
library(geobr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(readr)
library(scales)
library(sf)
library(stringr)
library(tidyr)
library(tmap)
library(viridis)


#########################################################################################
############################### Exportation  ############################################
#########################################################################################                                                 


##Graphic Top 5 most exported products by kg - ok


## Geospatial data 

## Municipalities shape

munishape = geobr::read_municipality(code_muni = "all", year = 2020, simplified = TRUE, showProgress = TRUE)

## Biome shape

cerradoshape = geobr::read_biomes(year = 2019, simplified = TRUE, showProgress = TRUE) |>
  filter(name_biome == "Cerrado")

## Join munishape and cerradoshape

CERmunishape = st_join(munishape, cerradoshape, join = st_intersects) |> 
  st_make_valid()

## Read the data 

exportFULL= readr::read_csv("data/exportBR_97_22_v2.csv")  

## export$coAno = as.character(export$coAno)

## Filter the data for the desired years
export = exportFULL |> 
  filter(coAno >= 1997 & coAno <= 2021)

## Group the data by year and product
export_grouped = export |> 
  group_by(Year = coAno, noSh4pt) |> 
  summarize(Exportation_KGL = sum(kgLiquido))

## Select the top 5 most exported products
top_5_export = export_grouped |> 
  top_n(5, Exportation_KGL)


###########################################################################################
## Take rows of top 5 most exported product from main dataframe (with all columns)- Works but it's not proper to build graphics  - ok

top5only = export |> 
  filter(noSh4pt %in% c("Alumínio em formas brutas", "Carnes e miudezas comestíveis, frescas, refrigeradas ou congeladas, das aves da posição 0105","Ferro fundido bruto e ferro spiegel (especular), em lingotes, linguados ou outras formas primárias", "Milho", "Minérios de alumínio e seus concentrados", "Minérios de ferro e seus concentrados, incluídas as pirites de ferro ustuladas (cinzas de pirites)", "Minérios de manganês e seus concentrados", "Soja, mesmo triturada"))

## sort dataframe in decreasing order by kgLiquido column
top5only = top5only[order(-top5only$kgLiquido), ]

# Aggregate data by municipality and year
muniyearkg_top5only = aggregate(kgLiquido ~ municipality + coAno, data = top5only, sum) |>   rename(name_muni = municipality)

## Join AMZmunishape and dataframe muniyearkg_top5only 

exportmunishapeJOIN = left_join(AMZmunishape, muniyearkg_top5only, by = "name_muni") |>
  na.omit()

# Create a plot
exportmunishapeJOIN |> 
  filter(coAno >= 1997 & coAno <= 2000) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = amazonhape, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = kgLiquido)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~coAno, ncol = 4) +
  labs(title = "Municipalities' exportation (kgLiquido)",
       #subtitle = "1997-2008",
       fill = "kgLiquido")  +
  theme_void()


#########################################################################################
############################### MapBiomas  ##############################################
#########################################################################################    


##### MapBiomas Anthropic

mapbiomas = readr::read_csv("data/mapBiomas/TABELA_GERAL_COL7_MAPBIOMAS_DESMAT_VEGSEC_UF_Muni_Biome.csv") 

#mapbiomas6 = readr::read_csv("data/mapBiomas/coleção6") 

# split the 'city' column into three separate columns
mapbiomas = separate(mapbiomas, city, into = c("city", "state", "biome"), sep = " - ") 

mapbiomas6 = separate(mapbiomas6, city, into = c("city", "state", "biome"), sep = " - ") 

# Pay attention: hectare has some NA values for some Years !

## Anthropic areas in Ha - Mapbiomas 7
haAnthropic = mapbiomas |>  
  select(-color) |>
  filter(biome == "CERRADO")  |> 
  filter(level_0 == "Anthropic") |>
  #mutate_all(~replace_na(.,0)) |>  It affects the results. Show a decrease for some years, when actually the data is absent
  gather(year, hectare, starts_with("19"), starts_with("20")) |> 
  group_by(Year = year, city) |>
  rename(name_muni = city) |>
  filter(!is.na(hectare))  |> 
  summarize(Ha = sum(hectare)) |> 
  mutate_at('Year', as.numeric)

## Anthropic areas in Ha - Mapbiomas 6
SIXhaAnthropic = mapbiomas6 |>  
  select(-color) |>
  filter(biome == "CERRADO")  |> 
  filter(level_0 == "Anthropic") |>
  #mutate_all(~replace_na(.,0)) |>  It affects the results. Show a decrease for some years, when actually the data is absent
  gather(year, hectare, starts_with("19"), starts_with("20")) |> 
  group_by(Year = year, city) |>
  rename(name_muni = city) |>
  filter(!is.na(hectare))  |> 
  summarize(Ha = sum(hectare)) |> 
  mutate_at('Year', as.numeric)

## Join CERmunishape and haAnthropic
MuniHAanthropic = left_join(CERmunishape, haAnthropic, by = "name_muni", multiple = "all")  |> 
  na.omit() |> 
  st_make_valid()

# Join CERmunishape and haAnthropic Mapbiomas 6
SIXMuniHAanthropic=left_join(CERmunishape, SIXhaAnthropic, by = "name_muni", multiple = "all")  |> 
  na.omit() |> 
  st_make_valid()

# Create plot

## 1997 - 2000

png(file="anthropic_1997_2000.png",
    width=20, height=8, units="in", res=300)
MuniHAanthropic |> 
  filter(Year >= 1997 & Year <= 2000) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = cerradoshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the Ha per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the Ha scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "1997-2000",
       fill = "Ha")  +
  theme_void()
dev.off()


## 2001 - 2003
png(file="anthropic_2001_2003.png",
    width=15, height=5, units="in", res=300)
MuniHAanthropic |> 
  filter(Year >= 2001 & Year <= 2003) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = cerradoshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the Ha per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the Ha scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2001-2003",
       fill = "Ha")  +
  theme_void()
dev.off()


## 2004 - 2006
png(file="anthropic_2004_2006.png",
    width=15, height=5, units="in", res=300)
MuniHAanthropic |> 
  filter(Year >= 2004 & Year <= 2006) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = cerradoshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the Ha per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the Ha scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2004-2006",
       fill = "Ha")  +
  theme_void()
dev.off()


## 2007 - 2009
png(file="anthropic_2007_2009.png",
    width=15, height=5, units="in", res=300)
MuniHAanthropic |> 
  filter(Year >= 2007 & Year <= 2009) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = cerradoshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the Ha per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the Ha scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2007-2009",
       fill = "Ha")  +
  theme_void()
dev.off()


## 2010 - 2012
png(file="anthropic_2010_2012.png",
    width=15, height=5, units="in", res=300)
MuniHAanthropic |> 
  filter(Year >= 2010 & Year <= 2012) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = cerradoshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the Ha per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the Ha scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2010-2012",
       fill = "Ha")  +
  theme_void()
dev.off()


## 2013 - 2015
png(file="anthropic_2013_2015.png",
    width=15, height=5, units="in", res=300)
MuniHAanthropic |> 
  filter(Year >= 2013 & Year <= 2015) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = cerradoshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the Ha per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the Ha scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2013-2015",
       fill = "Ha")  +
  theme_void()
dev.off()

## 2016 - 2018
png(file="anthropic_2016_2018.png",
    width=15, height=5, units="in", res=300)
MuniHAanthropic |> 
  filter(Year >= 2016 & Year <= 2018) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = cerradoshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the Ha per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the Ha scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2016-2018",
       fill = "Ha")  +
  theme_void()
dev.off()


## 2019 - 2021 
png(file="anthropic_2019_2021.png",
    width=5, height=5, units="in", res=300)
SIXMuniHAanthropic |> 
  filter(Year >= 2019 & Year <= 2021) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = cerradoshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the Ha per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the Ha scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Anthropization",
       #subtitle = "2019-2021",
       fill = "Ha")  +
  theme_void()
dev.off()


##### MapBiomas Natural

## Anthropic areas in Ha
haNatural = mapbiomas |>  
  select(-color) |>
  filter(biome == "CERRADO")  |> 
  filter(level_0 == "Natural") |>
  #mutate_all(~replace_na(.,0)) |>  It affects the results. Show a decrease for some years, when actually the data is absent
  gather(year, hectare, starts_with("19"), starts_with("20")) |> 
  group_by(Year = year, city) |>
  rename(name_muni = city) |>
  filter(!is.na(hectare))  |> 
  summarize(Ha = sum(hectare)) |> 
  mutate_at('Year', as.numeric)

## Join AMZmunishape and haNatural

MuniHAnatural = left_join(CERmunishape, haNatural, by = "name_muni", multiple = "all")  |> 
  na.omit() |> 
  st_make_valid()

# Create plot

MuniHAnatural |> 
  filter(Year >= 2001 & Year <= 2003) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = cerradoshape, color = "gray", fill = NA, size = 0.1)  +
  # Add the HA per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the Ha scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Natural (ha)",
       #subtitle = "1997-2008",
       fill = "Ha")  +
  theme_void()


##### MapBiomas NON FOREST types 

haNonF = mapbiomas |>  
  select(-color) |>
  filter(biome == "CERRADO")  |> 
  filter(level_1 == "2. Non Forest Natural Formation") |>
  #mutate_all(~replace_na(.,0)) |>  It affects the results. Show a decrease for some years, when actually the data is absent
  gather(year, hectare, starts_with("19"), starts_with("20")) |> 
  group_by(year, level_2) |>
  rename(name_muni = city) |>
  filter(!is.na(hectare))  |> 
  summarize(Ha = sum(hectare)) |> 
  mutate_at('year', as.numeric)

## Bar plot
ggplot(haNonF, aes(x=year, y=Ha, fill=level_2)) +
  geom_bar(stat='identity', position='dodge')

# Line plots
cerradoNONforest = ggplot(haNonF, aes(x = year, y = Ha, color = level_2)) +
  geom_line() +
  labs(x = "Year", y = "Ha", color = "Nonforest types")

