######################################################################
## Code to build graphic Brazil exportation data from 1997 - 2021
## Author : Tainá Rocha
## R verison 4.2.2
## Rstudio version RStudio 2022.12.0+353 "Elsbeth Geranium" Release 
## Date: 31 MArch 2023
## Last updated: 15/05/2023
######################################################################

## Packages
library(directlabels)
library(dplyr)
library(geobr)
library(geomtextpath)
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
library(ggalluvial)

######################################################################


## Read the data 

export = readr::read_csv("data/comexStat/exportBR_97_23.csv")  |> 
  filter(coAno >= 1997 & coAno <= 2023) |> 
  rename(name_muni = municipality) |> 
  filter(!grepl('Mor', state_acronym)) |> 
  filter(!grepl('EX', state_acronym)) |> 
  filter(!grepl('ND', state_acronym))|> 
  filter(!grepl('Mor', state_acronym))|> 
  filter(!grepl('Mirim', state_acronym))|> 
  filter(!grepl('Ijuís', state_acronym))|> 
  filter(!grepl('Açu', state_acronym))


export$noSh4pt = str_replace(export$noSh4pt, "Minérios de ferro e seus concentrados, incluídas as pirites de ferro ustuladas \\(cinzas de pirites\\)", "Ferro")

export$noSh4pt = str_replace(export$noSh4pt, "Ferro fundido bruto e ferro spiegel \\(especular\\), em lingotes, linguados ou outras formas primárias", "Ferro bruto e spiegel")

export$noSh4pt = str_replace(export$noSh4pt, "Soja, mesmo triturada", "Soja")

export$noSh4pt = str_replace(export$noSh4pt, "Soja, mesmo triturada", "Soja")

export$noSh4pt = str_replace(export$noSh4pt, "Minérios de alumínio e seus concentrados", "Aluminio")

export$noSh4pt = str_replace(export$noSh4pt, "Minérios de cobre e seus concentrados", "Cobre")

export$noSh4pt = str_replace(export$noSh4pt, "Minérios de cromo e seus concentrados", "Cobre")

export$noSh4pt = str_replace(export$noSh4pt, "Minérios de tungstênio e seus concentrados", "Tungstênio")

export$noSh4pt = str_replace(export$noSh4pt, "Minérios de titânio e seus concentrados", "Titânio")

export$noSh4pt = str_replace(export$noSh4pt, "Minérios de niquel e seus concentrados", "Niquel")

export$noSh4pt = str_replace(export$noSh4pt, "Minérios de molibdénio e seus concentrados", "molibdénio")

export$noSh4pt = str_replace(export$noSh4pt, "Minérios de metais preciosos e seus concentrados", "Metais preciosos")

export$noSh4pt = str_replace(export$noSh4pt, "Minérios de manganês e seus concentrados", "Manganês")

export$noSh4pt = str_replace(export$noSh4pt, "Minérios de nióbio, tântalo, vanádio ou de zircónio, e seus concentrados
", "Outros Minérios")

## export$coAno = as.character(export$coAno)

## Group the data by year and product
export_grouped = export |> 
  group_by(Year = coAno, noSh4pt) |> 
  summarize(Exportation_KGL = sum(kgLiquido))


## Select the top 5 most exported products
#exporttop5 = export_grouped |> 
# top_n(5, Exportation_KGL)

## Plot the data

# Line with names 


ex = ggplot(export_grouped) +
  geom_textline(aes(
    x = Year, y = Exportation_KGL, group = noSh4pt, colour = noSh4pt, label = noSh4pt
  ),
  hjust = 1
  ) +
  theme(legend.position = "none")

ex + scale_x_continuous(breaks = seq(1997, 2023, by = 2)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#ggplot() + 
#  geom_line(data = export_grouped, aes(Year, Exportation_KGL, color = noSh4pt)) +
#  geom_text(data = export_grouped |> 
#              filter(Year == last(Year)), 
#              aes(label = noSh4pt, 
#                  x = Year + 0.5, 
#                  y = Exportation_KGL, 
#                  color = noSh4pt)) + 
#  guides(color = FALSE) + theme_bw() + 
#  scale_x_continuous(breaks = scales::pretty_breaks(10))


# ggplot(export_grouped, aes(x = Year, y = Exportation_KGL, group = noSh4pt, colour = noSh4pt)) + 
# geom_line() +
#  scale_colour_discrete(guide = 'none') +
#  scale_x_continuous(limits = c(1997, 2021), expand = c(0, 1)) +
#  geom_dl(aes(label = noSh4pt), method = list(dl.combine("first.points", "last.points")), cex = 0.8)


#png(file="exportProduct.png",
#    width=20, height=10, units="in", res=300)
#ggplot(export_grouped, aes(x = Year, y = Exportation_KGL, color = noSh4pt, group = noSh4pt)) +
#  geom_line() +
#  labs(x = "Year", y = "Exportation kg", color = "Product", title = "Temporal Series of Most Exported Products") +
#theme_minimal()
#dev.off()

## Bar plot
#ggplot(export_grouped, aes(x=Year, y=Exportation_KGL, fill = noSh4pt)) +
#  geom_bar(stat='identity', position='dodge', width = 1)


####################### Network States

# Aggregate the data by state and economic bloc

net_export = export |> 
  group_by(state_acronym, noBlocopt) |> 
  summarize(total_value = sum(kgLiquido)) |> 
  ungroup() |> 
  na.omit()

# Create the nodes and links data frames for the Sankey diagram
nodes = data.frame(name = unique(c(as.character(net_export$state_acronym), as.character(net_export$noBlocopt))))

links = data.frame(
  source = match(net_export$state_acronym, nodes$name) - 1,
  target = match(net_export$noBlocopt, nodes$name) - 1,
  value = net_export$total_value
)

# Create the Sankey diagram
sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 14,
  nodeWidth = 30,
  sinksRight = FALSE
)


####################### Network Biomes

biomemuni = sf::read_sf("/home/tai-rocha/Documents/Radom_Data/Shapefile/Muni_Biomes/Bioma_Muni.shp") |> 
  dplyr::rename(name_muni = nome)


JoiNN = dplyr::left_join(export, biomemuni, by = "name_muni")
#JoiNN$kgLiquido

# Aggregate the data by state and economic bloc
net_export = JoiNN |> 
  group_by(Biome, noBlocopt) |> 
  summarize(total_value = sum(kgLiquido)) |> 
  ungroup()  |> 
  na.omit()

# Create the nodes and links data frames for the Sankey diagram
nodes = data.frame(name = unique(c(as.character(net_export$Biome), as.character(net_export$noBlocopt))))

links = data.frame(
  source = match(net_export$Biome, nodes$name) - 1,
  target = match(net_export$noBlocopt, nodes$name) - 1,
  value = net_export$total_value
)

# Create the Sankey diagram
sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 14,
  nodeWidth = 30,
  sinksRight = FALSE
)

###########################################################################################
## Take rows of top 5 most exported product from main dataframe (with all columns)- Works but it's not proper to build graphics  - ok

#top5products = export_filtered |> 
#  filter(noSh4pt %in% c("Alumínio em formas brutas", "Carnes e miudezas comestíveis, frescas, refrigeradas ou congeladas, das aves da posição 0105","Ferro fundido bruto e ferro spiegel (especular), em lingotes, linguados ou outras formas primárias", "Milho", "Minérios de alumínio e seus concentrados", "Minérios de ferro e seus concentrados, incluídas as pirites de ferro ustuladas (cinzas de pirites)", "Minérios de manganês e seus concentrados", "Soja, mesmo triturada"))

## sort dataframe in decreasing order by kgLiquido column
#top5products_sorted = top5products[order(-top5products$kgLiquido), ]

# Aggregate data by municipality and year
#top5_agg = aggregate(kgLiquido ~ municipality + coAno, data = top5products_sorted, sum) |> rename(name_muni = municipality)