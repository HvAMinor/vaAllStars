#This line of code installs the pacman page if you do not have it installed - 
#if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")
devtools::install_github('HvAMinor/bbplot')
pacman::p_load('rstudioapi','dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
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

keuzejaar <- "2006"
keuzejaar.min <- "2006"
keuzejaar.max <- "2016"

bronnen <-" Eurostat (2019) Real GDP per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/sdg_08_10 \r\n Greenhouse gas emissions per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/t2020_rd300"
###########

dumbbell <- gasGdpCapita %>% 
    select(geo, jaar, gas) %>% 
    spread(jaar, gas, convert=TRUE)
dumbbell <- dumbbell %>% mutate(gap = dumbbell[,keuzejaar.max] - dumbbell[,keuzejaar.min])

dumbbell.top <- dumbbell %>% arrange(desc(gap)) %>% head(10)

dumbell.top.plot <- ggplot(dumbbell.top, aes(x = dumbbell.top[,keuzejaar.min], xend = dumbbell.top[,keuzejaar.min],
                                             y = reorder(geo, gap), group = geo)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  bbc_style()

finalise_plot(plot_name = dumbell.top.plot, source = bronnen, save_filepath = "dumbell top 10.png",
              width_pixels = 640, height_pixels = 500, logo_image_path = "dataloog.png")

dumbbell.bottom <- dumbbell %>% arrange(gap) %>% head(10)

dumbell.bottom.plot <- ggplot(dumbbell.bottom, aes(x = dumbbell.bottom[,keuzejaar.min], xend = dumbbell.bottom[,keuzejaar.min],
                                             y = reorder(geo, gap), group = geo)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  bbc_style()

finalise_plot(plot_name = dumbell.bottom.plot, source = bronnen, save_filepath = "dumbell bottom 10.png",
              width_pixels = 640, height_pixels = 500, logo_image_path = "dataloog.png")


