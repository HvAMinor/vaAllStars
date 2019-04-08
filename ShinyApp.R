#This line of code installs the pacman page if you do not have it installed - 
#if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")

devtools::install_github('HvAMinor/bbplot')

pacman::p_load('rstudioapi','dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt', 'eurostat',
               'forcats', 'R.utils', 'png', 'glue', 'countrycode', 
               'grid', 'ggpubr', 'scales')

library(bbplot)
library(shiny)
library(shinydashboard)
#windowsFonts("Arial" = windowsFont("Arial"))
# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))
# you can make sure you are in the right directory
print(getwd())

gasGdpCapita <- read.csv('gasGdpCapita.csv', row.names='X', header=TRUE, stringsAsFactors = FALSE) %>%
  mutate(geo=countrycode::countrycode(geo, origin='eurostat', destination='country.name'))
bronnen <-"Eurostat (2019) Real GDP per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/sdg_08_10 \r\nEurostat (2019) Greenhouse gas emissions per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/t2020_rd300"

dumbell <- gasGdpCapita %>% 
    select(geo, jaar, gas) %>% 
    spread(jaar, gas, convert=TRUE)

ui <- dashboardPage(
  dashboardHeader(
    title = "bruto binnenlands product tegen de uitstoot van broeikasgassen per capita"
  ),
  dashboardSidebar(
    sidebarMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                
                menuItem("Charts", tabName = "Plots", icon = icon("bar-chart-o"),
                         menuSubItem("1D - Histogram", tabName = "Histogram", icon = icon("angle-double-right")),
                         menuSubItem("2D - Lijngrafieken", tabName = "Lijngrafieken", icon = icon("angle-double-right")),
                         menuSubItem("2D - Dumbell charts", tabName = "Dumbell charts", icon = icon("angle-double-right")),
                         menuSubItem("3D - Geospatiale plots", tabName = "Geospatiale plots", icon = icon("angle-double-right"))
                         ),
                
                         
                sliderInput("keuzejaren", "keuzejaren", 2000, 2016, c(2006, 2016)),
                selectInput("keuzejaar", "keuzejaar", choices = seq(2000, 2016)),
                selectInput("klassen", "klassen", choices = seq(2, 8))
                )
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Ontwikkeling broeikas uitstoot per hoofd", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("dumbell", height = 750)
      ),
      box(
        title = "lijn", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("lijn", height = 750)
      )
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content")
      ),
      
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)


server <- function(input, output) {
  output$lijn <- renderPlot({
    klassen <- as.character(input$klassen)
    keuzejaar.min <- as.character(min(input$keuzejaren))
    keuzejaar.max <- as.character(max(input$keuzejaren))
    
    # LIJNGRAFIEK LABS
    dumbell.broeikas.titel <- glue("gemiddelde uitstoot broeikasgassen per hoofd vangdp {klassen} uitstootgroepen {keuzejaar.min} - {keuzejaar.max}.png")
    dumbell.broeikas.x.as <- glue("Landsnamen")
    dumbell.broeikas.y.as <- glue("Uitstoot broeikasgassen (gementen in tonnen CO2 equivalent)")
    
    lijn.data <- gasGdpCapita %>% 
      filter(jaar %in% seq(keuzejaar.min, keuzejaar.max)) %>%
      mutate(range_gdp = cut_to_classes(gdp, n=klassen, style="quantile", decimals=0)) %>%
      dplyr::select(jaar, gdp=range_gdp, gas) %>%
      group_by(gdp, jaar) %>% 
      summarize(
        gas = mean(gas)
        ) %>%
      ungroup()
    lijn.plot <- ggplot(lijn.data, aes(x = jaar, y = gas, colour = gdp)) +
      geom_line(size = 1) +
      geom_hline(yintercept = 0, size = 1, colour="#333333") +
      bbc_style() + 
      
      # LABS KOPPELEN
      labs(title=lijn.titel,
           y = "",
           ) +
      
      # LEGENDA TITEL
      theme(legend.position = "right")

      finalise_plot(plot_name = lijn.plot,
                source_name = bronnen,
                save_filepath = glue("lijn {keuzejaar.min} - {keuzejaar.max}.png"),
                width_pixels = 750, height_pixels = 600, logo_image_path = "dataloog.png")
  })
  output$dumbell <- renderPlot({
    keuzejaar.min <- as.character(min(input$keuzejaren))
    keuzejaar.max <- as.character(max(input$keuzejaren))
    dumbell.data <- dumbell %>% mutate(gap = dumbell[,keuzejaar.max] - dumbell[,keuzejaar.min])
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("verandering uitstoot broeikasgassen per hoofd bevolking {keuzejaar.min} - {keuzejaar.max}")
    dumbell.broeikas.x.as <- glue("Landsnamen")
    dumbell.broeikas.y.as <- glue("Uitstoot broeikasgassen (gementen in tonnen CO2 equivalent)")
    
    dumbell.plot <- ggplot(dumbell.data, aes(x = dumbell.data[,keuzejaar.max],
                                             xend = dumbell.data[,keuzejaar.min],
                                             y = reorder(geo, gap), group = geo)) +
      geom_dumbbell(colour = "#dddddd",
                    size = 3,
                    colour_x = "#FAAB18",
                    colour_xend = "#1380A1") +
      labs(title=dumbell.broeikas.title) +
      bbc_style()

    finalise_plot(plot_name = dumbell.plot,
                  source_name = bronnen,
                  save_filepath = glue("dumbell {keuzejaar.min} - {keuzejaar.max}.png"),
                  width_pixels = 750, height_pixels = 600, logo_image_path = "dataloog.png")
  })
}

shinyApp(ui, server)
