#This line of code installs the pacman page if you do not have it installed - 
#if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")
devtools::install_github('HvAMinor/bbplot')
pacman::p_load('rstudioapi','dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt', 'eurostat',
               'forcats', 'R.utils', 'png', 'glue', 
               'grid', 'ggpubr', 'scales', 'colorspace',
               'bbplot')

windowsFonts("Helvetica" = windowsFont("Helvetica Light"))

# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))
# you can make sure you are in the right directory
print(getwd())

gasGdpCapita <- read.csv('gasGdpCapita.csv', row.names='X', header=TRUE, stringsAsFactors = FALSE)

#########
top_lijst <- "7"
klassen <- "3"
keuzejaar <- "2006"
keuzejaar.min <- "2006"
keuzejaar.max <- "2016"
bronnen <-" Eurostat (2019) Real GDP per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/sdg_08_10 \r\n Eurostat (2019) Greenhouse gas emissions per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/t2020_rd300"
######### hieronder staan geen instellingen meer
geodata_N0 <- get_eurostat_geospatial(year = "2016",
                                      nuts_level = 0,
                                      resolution = "60")

 # TOEVOEGEN - COUNTRY
geodata_N0 <- geodata_N0 %>%
  mutate(country = countrycode(sourcevar = CNTR_CODE,
                               origin = 'eurostat',
                               destination = 'country.name') )


 # SELECTEREN - GEOMETRY, GEO
geodata_N0 <- geodata_N0 %>%
  select("geometry","geo", "country")


 # DATASETS JOINEN - geodata_N0, gasGdpCapita
map_data <- gasGdpCapita %>%
  inner_join(y = geodata_N0, by = "geo")


 # DATAFRAME MAP_DATA_GDP (KOLOM BRUTO BINNENLANDS PRODUCT PER CAPITA) 
map_data_gas <- map_data %>%
  dplyr::select(-gdp) %>%
  mutate(cat = cut_to_classes(x = gas, 
                              n = 7,
                              style = "quantile" ))


ggplot(data = map_data_gas) +
  
  # LANDEN EEN ONDERLIGGENDE GRIJZE KLEUR GEVEN (LANDEN ZONDER DATA WORDEN GRIJS)
  geom_sf(data = geodata_N0,
          fill = "gray85",
          alpha = 1) +
  
  # LANDEN KLEUREN OP BASIS VAN CATEGORIE
  geom_sf(data = map_data_gas[map_data_gas$jaar %in% c(2010),],
          aes(fill = cat),
          color = "gray40",
          size = 0.2) +
  
  # RANDEN VAN LANDEN KLEUREN OP BASIS VAN GRENZEN
  geom_sf(data = geodata_N0,
          color = "gray30",
          size = 0.703,
          alpha = 0) +
  
  # CO?RDINATEN VAN DE MAP VERKLEINEN
  coord_sf(xlim = c(-25, 45),
           ylim = c(35, 72)) +
  
  # PASSEND KLEURENPALETTE, ZODAT ALLE KLASSEN DUIDELIJK ZICHTBAAR WORDEN
  scale_fill_discrete_sequential(name = "<AANVULLEN>", "ag_GrnYl") +
  
  # POSITIE VAN LEGENDA NAAR LINKS
  theme(legend.position = "left",
        legend.title.align = 0) +
  
  # VOLGORDE VAN LEGENDA - HOOG > LAAG
  guides(fill = guide_legend(reverse = TRUE))+
  
  # TITEL, ONDERTITEL, CAPTION, X-AS, Y-AS, LEGENDATITEL
  labs(title = paste("<AANVULLEN>"),
       subtitle = paste("<AANVULLEN>"),
       caption = "<AANVULLEN>",
       x = "Lengtegraad",
       y = "Breedtegraad")


 # DATAFRAME MAP_DATA_GDP (KOLOM BRUTO BINNENLANDS PRODUCT PER CAPITA) 
map_data_gdp <- map_data %>%
  select(-gas) %>%
  mutate(cat = cut_to_classes(x = gdp, 
                              n = 3,
                              style = "quantile" ))


 # PLOT MAP_DATA_GDP
ggplot(data = map_data_gdp) +
  
  # LANDEN EEN ONDERLIGGENDE GRIJZE KLEUR GEVEN (LANDEN ZONDER DATA WORDEN GRIJS)
  geom_sf(data = geodata_N0,
          fill = "gray85",
          alpha = 1) +
  
  # LANDEN KLEUREN OP BASIS VAN CATEGORIE
  geom_sf(data = map_data_gdp[map_data_gdp$jaar %in% c(2010),],
          aes(fill = cat),
          color = "gray40",
          size = 0.2) +
  
  # RANDEN VAN LANDEN KLEUREN OP BASIS VAN GRENZEN
  geom_sf(data = geodata_N0,
          color = "gray30",
          size = 0.703,
          alpha = 0) +
  
  coord_sf(xlim = c(-25, 45),
           ylim = c(35, 72)) +
  
  
  # PASSEND KLEURENPALETTE, ZODAT ALLE KLASSEN DUIDELIJK ZICHTBAAR WORDEN
  scale_fill_discrete_sequential(name = "<AANVULLEN>", "ag_GrnYl") +
  
  # POSITIE VAN LEGENDA NAAR LINKS
  theme(legend.position = "left",
        legend.title.align = 0) +
  
  # VOLGORDE VAN LEGENDA - HOOG > LAAG
  guides(fill = guide_legend(reverse = TRUE))+
  
  # TITEL, ONDERTITEL, CAPTION, X-AS, Y-AS, LEGENDATITEL
  labs(title = paste("<AANVULLEN>"),
       subtitle = paste("<AANVULLEN>"),
       caption = "<AANVULLEN>",
       x = "Lengtegraad",
       y = "Breedtegraad")


  
