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

# Scatterplot

gasGdpCapita %>% 
  mutate(klass = cut_to_classes(gdp, 5)) %>%
  filter(jaar %in% c(2006, 2016), geo %in% c("NL","DE")) %>%
    ggplot(aes(x = gdp,
               y = gas,
               fill = geo)) +
    
    # Scatterplot met vorm, kleur en grootte
    geom_point(shape = 21,
               color = "black",
               size = 1.5) +
    
    stat_ellipse(aes(color = geo),
                 color = "black",
                 geom = "polygon",
                 level = 0.95,
                 alpha = 0.5)
    
    geom_text(label = gasGdpCapita$jaar,
              hjust = ,
              vjust = 0.2,
              size = 2.3,
              color = "black",
              check_overlap = T)
