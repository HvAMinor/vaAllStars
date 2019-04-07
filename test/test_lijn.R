#This line of code installs the pacman page if you do not have it installed - 
#if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")
devtools::install_github('HvAMinor/bbplot')
pacman::p_load('rstudioapi','dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
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
bronnen <-" Eurostat (2019) Real GDP per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/sdg_08_10 \r\n Greenhouse gas emissions per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/t2020_rd300"

######### hieronder staan geen instellingen meer en moeten de plots staan

lijn.gdp <- ""
lijn.gdp.fname <- glue("lijn plot {keuzejaar.min} - {keuzejaar.max}.png")

lijn.gdp.plot <- ggplot(lijn.gdp, aes(x = gdp, y=gas, group=geo)) + 
  geom_nogiets() +
  bbc_style()

finalise_plot(plot_name = lijn.gdp.plot, 
              source = bronnen, 
              save_filepath = lijn.gdp.fname,
              width_pixels = 640, 
              height_pixels = 500, 
              logo_image_path = "dataloog.png")

