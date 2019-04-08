#This line of code installs the pacman page if you do not have it installed - 
#if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")

devtools::install_github('HvAMinor/bbplot')

pacman::p_load('rstudioapi','dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt','eurostat',
               'forcats', 'R.utils', 'png', 'glue', 
               'grid', 'ggpubr', 'scales')

library(shiny)
library(shinydashboard)
library(bbplot)



# Run the application 

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
        title = "Histogram", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("plot3", height = 250)
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
  output$hist.gdp <- renderPlot({
    bronnen <-" Eurostat (2019) Real GDP per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/sdg_08_10 \r\n Eurostat (2019) Greenhouse gas emissions per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/t2020_rd300"
    p <- gasGdpCapita %>% 
      Categoriseer(input$hist.gdp.verdeling) %>%
      filter(jaar %in% seq(2000, input$hist.gdp.slider)) %>%
      ggplot(aes(x=cat_gdp)) +
      geom_bar(colour="white", fill = "#1380A1", stat = "count") +
      geom_hline(yintercept = 0, size = 1, colour="#333333") +
      bbc_style() +
      labs(title = "Verdeling bruto binnenlands product") +
      scale_y_continuous(breaks=pretty_breaks())
    finalise_plot(
      plot_name = p,
      source_name = bronnen,
      save_filepath = gasGdpCapita.gemiddelde.gdp.fname,
      width_pixels = 640, height_pixels = 500, logo_image_path = "dataloog.png")
  })
}

shinyApp(ui, server)
