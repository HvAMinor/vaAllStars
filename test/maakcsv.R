library(eurostat)
library(tidyverse)
gasPerCapita.code <- search_eurostat('Greenhouse gas emissions per capita', type='table')$code[1]
gasPerCapita.df <- get_eurostat(gasPerCapita.code, stringsAsFactors = FALSE) %>% 
  mutate(jaar = format(time, '%Y')) %>% 
  dplyr::select(geo, gas=values, jaar)

gdpPerCapita.code <- search_eurostat('Real GDP per capita', type='table')$code[1]
gdpPerCapita.df <- get_eurostat(gdpPerCapita.code, stringsAsFactors = FALSE) %>%
    filter(unit == 'CLV10_EUR_HAB') %>%
    mutate(jaar = format(time, '%Y')) %>%
    dplyr::select(geo, gdp=values, jaar)

gasGdpPerCapita.df <- inner_join(gasPerCapita.df, gdpPerCapita.df) %>%
    filter(str_length(geo) == 2) %>%
    dplyr::select(jaar, geo, gdp, gas)

gasGdpPerCapita.df %>% dplyr::filter(is.na(geo) | is.na(gdp))
gasGdpPerCapita.df[which(gasGdpPerCapita.df$jaar %in% c(2001,2000) & gasGdpPerCapita.df$geo == 'RO'),]$gdp <- gasGdpPerCapita.df[which(gasGdpPerCapita.df$jaar == 2002 & gasGdpPerCapita.df$geo == 'RO'),]$gdp 
gasGdpPerCapita.df %>% dplyr::filter(is.na(geo) | is.na(gdp))

write.csv(gasGdpPerCapita.df, 'gasGdpCapita.csv')
gasGdpPerCapita <- read.csv('gasGdpCapita.csv', header=TRUE, row.names='X', stringsAsFactors=FALSE)

