#This line of code installs the pacman page if you do not have it installed - 
#if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")
devtools::install_github('HvAMinor/bbplot')
pacman::p_load('rstudioapi','dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt','eurostat',
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

gasGdpCapita.gemiddelde.gas.fname <- glue("gemiddelde uitstoot {klassen} welvaartsgroepen {keuzejaar.min} - {keuzejaar.max}.png")
gasGdpCapita.gemiddelde.gas <- gasGdpCapita %>% 
  filter(jaar %in% seq(keuzejaar.min, keuzejaar.max)) %>%
  mutate(range_gdp = cut_to_classes(gdp, n=klassen, style="quantile", decimals=0)) %>%
  dplyr::select(jaar, gdp=range_gdp, gas) %>%
  group_by(gdp, jaar) %>% 
  summarize(
    gas = mean(gas),
    telling = n()
    ) %>%
  ungroup()

gasGdpCapita.gemiddelde.gdp.fname <- glue("gemiddelde gdp {klassen} uitstootgroepen {keuzejaar.min} - {keuzejaar.max}.png")
gasGdpCapita.gemiddelde.gdp <- gasGdpCapita %>% 
  filter(jaar %in% seq(keuzejaar.min, keuzejaar.max)) %>%
  mutate(range_gas = cut_to_classes(gas, n=klassen, style="quantile", decimals=0)) %>%
  dplyr::select(jaar, gas=range_gas, gdp) %>%
  group_by(gas, jaar) %>% 
  summarize(
    gdp = mean(gdp),
    telling = n()
    ) %>%
  ungroup()

######### hieronder staan geen instellingen meer maar de plots
######### 
gasGdpCapita.gemiddelde.gas.plot <- ggplot(gasGdpCapita.gemiddelde.gas, aes(x = jaar, y = gas, colour = gdp)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() + 
  theme(legend.position = "right")

gasGdpCapita.gemiddelde.gas.final <- finalise_plot(
  plot_name = gasGdpCapita.gemiddelde.gas.plot,
  source = bronnen,
  save_filepath = gasGdpCapita.gemiddelde.gas.fname,
  width_pixels = 640, height_pixels = 500, logo_image_path = "dataloog.png")

gasGdpCapita.gemiddelde.gdp.plot <- ggplot(gasGdpCapita.gemiddelde.gdp, aes(x = jaar, y = gdp, colour = gas)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() + 
  theme(legend.position = "right")

gasGdpCapita.gemiddelde.gdp.final <- finalise_plot(
  plot_name = gasGdpCapita.gemiddelde.gdp.plot,
  source = bronnen,
  save_filepath = gasGdpCapita.gemiddelde.gdp.fname,
  width_pixels = 640, height_pixels = 500, logo_image_path = "dataloog.png")

