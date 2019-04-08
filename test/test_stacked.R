#This line of code installs the pacman page if you do not have it installed - 
#if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")
devtools::install_github('HvAMinor/bbplot')
pacman::p_load('rstudioapi','dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt', 'eurostat',
               'forcats', 'R.utils', 'png', 'glue', 
               'grid', 'ggpubr', 'scales',
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

stacked.gdp <- gasGdpCapita %>% 
  filter(jaar %in% seq(keuzejaar.min, keuzejaar.max)) %>%
  mutate(range_gdp = cut_to_classes(gdp, n=klassen, style="quantile", decimals=0)) %>%
  dplyr::select(jaar, gdp=range_gdp, gas) %>%
  group_by(gdp, jaar) %>% 
  summarize(gas = mean(gas), count = n()) %>%
  ungroup() %>%
  dplyr::select(jaar, gdp, gas, count)

######### hieronder staan geen instellingen meer
  


) +
  geom_bar(stat="identity",position="fill", aes(y=prop.gas))

ggplot(stacked.gdp, aes(x=jaar, y=prop.gas, fill=gas)) +
  geom_area(alpha=0.6, size=1, colour="black")

ggplot(stacked.gdp, aes(x=jaar, y=prop.gas, fill=gdp)) +
  geom_area(alpha=0.6, size=1, colour="black")

  geom_area(alpha=0.3, aes(x=jaar, y=prop.gas, colour="black", fill=gdp)) +
  bbc_style()
  
