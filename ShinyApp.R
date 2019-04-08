library(shiny)
library(shinydashboard)
library(tidyverse)
library(eurostat)

## functies beginnen met een hoofdletter
Keuzejaar <- function(df, vectorJaar) {df %>% filter(jaar %in% vectorJaar)}

Keuzeland <- function(df, vectorGeo) {df %>% filter(geo %in% vectorGeo)}

Categoriseer <- function(df, x) {df %>% mutate(cat_gdp = cut_to_classes(gdp, n=x, style="equal"),
                                               cat_gas = cut_to_classes(gas, n=x, style="equal"))}


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
                
                         
                sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
                
                radioButtons("typeInput", "Product type",
                             choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                             selected = "WINE"),
                
                selectInput("countryInput", "Country",
                            choices = c("CANADA", "FRANCE", "ITALY")))
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Histogram", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("plot3", height = 250)
      )
    )
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


#------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  output$hist.gdp <- renderPlot({
    gasGdpCapita %>% 
      Categoriseer(input$hist.gdp.verdeling) %>%
      filter(jaar %in% seq(2000, input$hist.gdp.slider)) %>%
      ggplot(aes(x=cat_gdp)) +
      geom_bar(colour="white", fill = "#1380A1", stat = "count") +
      geom_hline(yintercept = 0, size = 1, colour="#333333") +
      bbc_style() +
      labs(title = "Verdeling bruto binnenlands product") +
      scale_y_continuous(breaks=pretty_breaks())
  })
}

shinyApp(ui, server)
