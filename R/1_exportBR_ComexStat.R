######################################################################
## Code to get Brazil exportation data from 1997 - 2022
## Author : Tainá Rocha
## R verison 4.2.2
## Rstudio version RStudio 2022.12.0+353 "Elsbeth Geranium" Release 
## Date: 03 February 2023
## Last update: 08 February 2023
######################################################################

library(ComexstatR)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tidyr)

######

names = c("Carnes", "Soja", "Milho", "Arroz", "Minérios", "Ferro", "Alumínio")


############### Year 1997
# Define the years and months you want to process
year_1997 = 1997:1997
months_1997 = 1:12

# Define the function that processes each year and month
process_ym_1997 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_1997 = map2(year_1997, months_1997, process_ym_1997)

# Combine the results into a single data frame
result_1997 = dplyr::bind_rows(result_list_1997)

result_1997_split_mun_state = result_1997 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_1997 =  result_1997_split_mun_state[str_detect(result_1997_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 1998
# Define the years and months you want to process
year_1998 = 1998:1998
months_1998 = 1:12

# Define the function that processes each year and month
process_ym_1998 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_1998 = map2(year_1998, months_1998, process_ym_1998) 

# Combine the results into a single data frame

result_1998 = dplyr::bind_rows(result_list_1998) |> 
  as.data.frame()

result_1998_split_mun_state = result_1998 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")

final_1998 =  result_1998_split_mun_state[str_detect(result_1998_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]

############### Year 1999
# Define the years and months you want to process
year_1999 = 1999:1999
months_1999 = 1:12

# Define the function that processes each year and month
process_ym_1999 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_1999 = map2(year_1999, months_1999, process_ym_1999)

# Combine the results into a single data frame
result_1999 = dplyr::bind_rows(result_list_1999)

result_1999_split_mun_state = result_1999 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_1999 =  result_1999_split_mun_state[str_detect(result_1999_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2000
# Define the years and months you want to process
year_2000 = 2000:2000
months_2000 = 1:12

# Define the function that processes each year and month
process_ym_2000 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2000 = map2(year_2000, months_2000, process_ym_2000)

# Combine the results into a single data frame
result_2000 = dplyr::bind_rows(result_list_2000)

result_2000_split_mun_state = result_2000 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2000 =  result_2000_split_mun_state[str_detect(result_2000_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2001
# Define the years and months you want to process
year_2001 = 2001:2001
months_2001 = 1:12

# Define the function that processes each year and month
process_ym_2001 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2001 = map2(year_2001, months_2001, process_ym_2001)

# Combine the results into a single data frame
result_2001 = dplyr::bind_rows(result_list_2001)

result_2001_split_mun_state = result_2001 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2001 =  result_2001_split_mun_state[str_detect(result_2001_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2002
# Define the years and months you want to process
year_2002 = 2002:2002
months_2002 = 1:12

# Define the function that processes each year and month
process_ym_2002 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2002 = map2(year_2002, months_2002, process_ym_2002)

# Combine the results into a single data frame
result_2002 = dplyr::bind_rows(result_list_2002)

result_2002_split_mun_state = result_2002 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2002 =  result_2002_split_mun_state[str_detect(result_2002_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2003
# Define the years and months you want to process
year_2003 = 2003:2003
months_2003 = 1:12

# Define the function that processes each year and month
process_ym_2003 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2003 = map2(year_2003, months_2003, process_ym_2003)

# Combine the results into a single data frame
result_2003 = dplyr::bind_rows(result_list_2003)

result_2003_split_mun_state = result_2003 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2003 =  result_2003_split_mun_state[str_detect(result_2003_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2004
# Define the years and months you want to process
year_2004 = 2004:2004
months_2004 = 1:12

# Define the function that processes each year and month
process_ym_2004 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2004 = map2(year_2004, months_2004, process_ym_2004)

# Combine the results into a single data frame
result_2004 = dplyr::bind_rows(result_list_2004)

result_2004_split_mun_state = result_2004 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")

final_2004 =  result_2004_split_mun_state[str_detect(result_2004_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]


############### Year 2005
# Define the years and months you want to process
year_2005 = 2005:2005
months_2005 = 1:12

# Define the function that processes each year and month
process_ym_2005 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2005 = map2(year_2005, months_2005, process_ym_2005)

# Combine the results into a single data frame
result_2005 = dplyr::bind_rows(result_list_2005)

result_2005_split_mun_state = result_2005 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2005 =  result_2005_split_mun_state[str_detect(result_2005_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2006
# Define the years and months you want to process
year_2006 = 2006:2006
months_2006 = 1:12

# Define the function that processes each year and month
process_ym_2006 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2006 = map2(year_2006, months_2006, process_ym_2006)

# Combine the results into a single data frame
result_2006 = dplyr::bind_rows(result_list_2006)

result_2006_split_mun_state = result_2006 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2006 =  result_2006_split_mun_state[str_detect(result_2006_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2007
# Define the years and months you want to process
year_2007 = 2007:2007
months_2007 = 1:12

# Define the function that processes each year and month
process_ym_2007 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2007 = map2(year_2007, months_2007, process_ym_2007)

# Combine the results into a single data frame
result_2007 = dplyr::bind_rows(result_list_2007)

result_2007_split_mun_state = result_2007 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2007 =  result_2007_split_mun_state[str_detect(result_2007_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]

############### Year 2008
# Define the years and months you want to process
year_2008 = 2008:2008
months_2008 = 1:12

# Define the function that processes each year and month
process_ym_2008 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2008 = map2(year_2008, months_2008, process_ym_2008)

# Combine the results into a single data frame
result_2008 = dplyr::bind_rows(result_list_2008)

result_2008_split_mun_state = result_2008 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2008 =  result_2008_split_mun_state[str_detect(result_2008_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]

############### Year 2009
# Define the years and months you want to process
year_2009 = 2009:2009
months_2009 = 1:12

# Define the function that processes each year and month
process_ym_2009 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2009 = map2(year_2009, months_2009, process_ym_2009)

# Combine the results into a single data frame
result_2009 = dplyr::bind_rows(result_list_2009)

result_2009_split_mun_state = result_2009 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2009 =  result_2009_split_mun_state[str_detect(result_2009_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]


############### Year 2010
# Define the years and months you want to process
year_2010 = 2010:2010
months_2010 = 1:12

# Define the function that processes each year and month
process_ym_2010 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2010 = map2(year_2010, months_2010, process_ym_2010)

# Combine the results into a single data frame
result_2010 = dplyr::bind_rows(result_list_2010)

result_2010_split_mun_state = result_2010 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2010 =  result_2010_split_mun_state[str_detect(result_2010_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2011
# Define the years and months you want to process
year_2011 = 2011:2011
months_2011 = 1:12

# Define the function that processes each year and month
process_ym_2011 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2011 = map2(year_2011, months_2011, process_ym_2011)

# Combine the results into a single data frame
result_2011 = dplyr::bind_rows(result_list_2011)

result_2011_split_mun_state = result_2011 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2011 =  result_2011_split_mun_state[str_detect(result_2011_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]


############### Year 2012
# Define the years and months you want to process
year_2012 = 2012:2012
months_2012 = 1:12

# Define the function that processes each year and month
process_ym_2012 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2012 = map2(year_2012, months_2012, process_ym_2012)

# Combine the results into a single data frame
result_2012 = dplyr::bind_rows(result_list_2012)

result_2012_split_mun_state = result_2012 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2012 =  result_2012_split_mun_state[str_detect(result_2012_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2013
# Define the years and months you want to process
year_2013 = 2013:2013
months_2013 = 1:12

# Define the function that processes each year and month
process_ym_2013 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2013 = map2(year_2013, months_2013, process_ym_2013)

# Combine the results into a single data frame
result_2013 = dplyr::bind_rows(result_list_2013)

result_2013_split_mun_state = result_2013 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2013 =  result_2013_split_mun_state[str_detect(result_2013_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2014
# Define the years and months you want to process
year_2014 = 2014:2014
months_2014 = 1:12

# Define the function that processes each year and month
process_ym_2014 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2014 = map2(year_2014, months_2014, process_ym_2014)

# Combine the results into a single data frame
result_2014 = dplyr::bind_rows(result_list_2014)

result_2014_split_mun_state = result_2014 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2014 =  result_2014_split_mun_state[str_detect(result_2014_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2015
# Define the years and months you want to process
year_2015 = 2015:2015
months_2015 = 1:12

# Define the function that processes each year and month
process_ym_2015 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2015 = map2(year_2015, months_2015, process_ym_2015)

# Combine the results into a single data frame
result_2015 = dplyr::bind_rows(result_list_2015)

result_2015_split_mun_state = result_2015 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2015 =  result_2015_split_mun_state[str_detect(result_2015_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2016
# Define the years and months you want to process
year_2016 = 2016:2016
months_2016 = 1:12

# Define the function that processes each year and month
process_ym_2016 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2016 = map2(year_2016, months_2016, process_ym_2016)

# Combine the results into a single data frame
result_2016 = dplyr::bind_rows(result_list_2016)

result_2016_split_mun_state = result_2016 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2016 =  result_2016_split_mun_state[str_detect(result_2016_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2017
# Define the years and months you want to process
year_2017 = 2017:2017
months_2017 = 1:12

# Define the function that processes each year and month
process_ym_2017 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2017 = map2(year_2017, months_2017, process_ym_2017)

# Combine the results into a single data frame
result_2017 = dplyr::bind_rows(result_list_2017)

result_2017_split_mun_state = result_2017 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2017 =  result_2017_split_mun_state[str_detect(result_2017_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2018
# Define the years and months you want to process
year_2018 = 2018:2018
months_2018 = 1:12

# Define the function that processes each year and month
process_ym_2018 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2018 = map2(year_2018, months_2018, process_ym_2018)

# Combine the results into a single data frame
result_2018 = dplyr::bind_rows(result_list_2018)

result_2018_split_mun_state = result_2018 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2018 =  result_2018_split_mun_state[str_detect(result_2018_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2019
# Define the years and months you want to process
year_2019 = 2019:2019
months_2019 = 1:12

# Define the function that processes each year and month
process_ym_2019 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2019 = map2(year_2019, months_2019, process_ym_2019)

# Combine the results into a single data frame
result_2019 = dplyr::bind_rows(result_list_2019)

result_2019_split_mun_state = result_2019 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2019 =  result_2019_split_mun_state[str_detect(result_2019_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2020
# Define the years and months you want to process
year_2020 = 2020:2020
months_2020 = 1:12

# Define the function that processes each year and month
process_ym_2020 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2020 = map2(year_2020, months_2020, process_ym_2020)

# Combine the results into a single data frame
result_2020 = dplyr::bind_rows(result_list_2020)

result_2020_split_mun_state = result_2020 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2020 =  result_2020_split_mun_state[str_detect(result_2020_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2021
# Define the years and months you want to process
year_2021 = 2021:2021
months_2021 = 1:12

# Define the function that processes each year and month
process_ym_2021 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2021 = map2(year_2021, months_2021, process_ym_2021)

# Combine the results into a single data frame
result_2021 = dplyr::bind_rows(result_list_2021)

result_2021_split_mun_state = result_2021 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2021 =  result_2021_split_mun_state[str_detect(result_2021_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]



############### Year 2022
# Define the years and months you want to process
year_2022 = 2022:2022
months_2022 = 1:12

# Define the function that processes each year and month
process_ym_2022 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2022 = map2(year_2022, months_2022, process_ym_2022)

# Combine the results into a single data frame
result_2022 = dplyr::bind_rows(result_list_2022)

result_2022_split_mun_state = result_2022 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2022 =  result_2022_split_mun_state[str_detect(result_2022_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]


############### Year 2023
# Define the years and months you want to process
year_2023 = 2023:2023
months_2023 = 1:11

# Define the function that processes each year and month
process_ym_2023 = function(year, month) {
  ComexstatR::pesquisar_comex_stat_mun(
    ano_inicial = year, ano_final = year, mes_inicial = month, mes_final = month,
    detalha_mes = FALSE, tipo_op = 'exp', tipo_ord = 'val',
    detalhamentos = c('pais','blocos','mun', 'sh4'),
    filtros = c(), filtros_esp = c()
  )
}

# Apply the function to each year and month combination
result_list_2023 = map2(year_2023, months_2023, process_ym_2023)

# Combine the results into a single data frame
result_2023 = dplyr::bind_rows(result_list_2023)

result_2023_split_mun_state = result_2023 |> 
  separate(noMunMinsgUf, into = c("municipality", "state_acronym"), sep = "-")


final_2023 =  result_2023_split_mun_state[str_detect(result_2023_split_mun_state$noSh4pt, paste(names, collapse = "|")), ]


## Bind all
All_97_23 = bind_rows(final_1997, final_1998, final_1999, final_2000, final_2001, final_2002, final_2003, final_2004, final_2005, final_2006, final_2007, final_2008, final_2009, final_2010, final_2011, final_2012, final_2013, final_2014, final_2015, final_2016, final_2017, final_2018, final_2019, final_2020, final_2021, final_2022, final_2023)

## Write

readr::write_csv(All_97_23, "data/exportBR_97_23.csv")

