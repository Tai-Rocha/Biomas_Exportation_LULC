############################################################################
## Code to to spacialize (G space) Brazil exportation data from 1997 - 2022
## Author : Tainá Rocha
## R verison 4.2.2
## Rstudio version RStudio 2022.12.0+353 "Elsbeth Geranium" Release 
## Date: 03 February 2023
## Last update: -
#############################################################################


## Read muni shape and export 2022 

export_2022 = sf::st_read("data-raw/2022_R_comexstat.csv") |> 
  dplyr::rename(NM_MUN = municipality)

muni = sf::st_read("data-raw/br_municipios_20200807/BR_Municipios_2019.shp")


join_test = dplyr::left_join(export_2022, muni, by = "NM_MUN")   
plot(join_test$geometry)

sf::st_write(join_test,"data/export_2022.shp")
