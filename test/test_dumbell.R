#This line of code installs the pacman page if you do not have it installed - 
#if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")
devtools::install_github('HvAMinor/bbplot')
pacman::p_load('rstudioapi','dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt', 'eurostat',
               'forcats', 'R.utils', 'png', 'glue', 
               'grid', 'ggpubr', 'scales')
library(bbplot)

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

dumbbell <- gasGdpCapita %>% 
    select(geo, jaar, gas) %>% 
    spread(jaar, gas, convert=TRUE)
dumbbell <- dumbbell %>% mutate(gap = dumbbell[,keuzejaar.max] - dumbbell[,keuzejaar.min])
######### hieronder staan geen instellingen meer


dumbbell.top <- dumbbell %>% arrange(desc(gap)) %>% head(as.numeric(top_lijst))
dumbbell.top.fname <- glue("dumbell top {top_lijst} {keuzejaar.min} - {keuzejaar.max}.png")

dumbell.top.plot <- ggplot(dumbbell.top, aes(x = dumbbell.top[,keuzejaar.max], 
                                             xend = dumbbell.top[,keuzejaar.min],
                                             y = reorder(geo, gap), group = geo)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  ggtitle(glue('Verandering uitstoot broeikasgassen per hoofd {keuzejaar.min} - {keuzejaar.max}')) +
  bbc_style() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),    # Center title position and size
    plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
    plot.caption = element_text(hjust = 0, face = "italic")# move caption to the left
  )

dumbbell.top.final <- finalise_plot(plot_name = dumbell.top.plot, source_name = bronnen, save_filepath = dumbbell.top.fname,
              width_pixels = 640, height_pixels = 500, logo_image_path = "dataloog.png")

dumbbell.bottom <- dumbbell %>% arrange(gap) %>% head(as.numeric(top_lijst))
dumbbell.bottom.fname <- glue("dumbell bottom {top_lijst} {keuzejaar.min} - {keuzejaar.max}.png")


dumbbell.bottom.plot <- ggplot(dumbbell.bottom, aes(x = dumbbell.bottom[,keuzejaar.max], 
                                                    xend = dumbbell.bottom[,keuzejaar.min],
                                                    y = reorder(geo, gap), group = geo)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  ggtitle(glue('Verandering uitstoot broeikasgassen per hoofd {keuzejaar.min} - {keuzejaar.max}')) +
  bbc_style() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),    # Center title position and size
    plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
    plot.caption = element_text(hjust = 0, face = "italic")# move caption to the left
  )

dumbbell.bottom.final <- finalise_plot(plot_name = dumbbell.bottom.plot, 
                                       source_name = bronnen, 
                                       save_filepath = dumbbell.bottom.fname,
              width_pixels = 640, height_pixels = 500, logo_image_path = "dataloog.png")


