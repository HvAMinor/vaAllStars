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

#-------------------------- DASHBOARD SIDEBAR ------------------------------------------    
  dashboardSidebar(
    sidebarMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                
                menuItem("Charts", tabName = "Plots", icon = icon("bar-chart-o"),
                         menuSubItem("1D - Histogrammen", tabName = "histogrammen", icon = icon("angle-double-right")),
                         menuSubItem("2D - Lijngrafieken", tabName = "lijngrafieken", icon = icon("angle-double-right")),
                         menuSubItem("2D - Dumbell charts", tabName = "dumbellcharts", icon = icon("angle-double-right")),
                         menuSubItem("3D - Geospatiale visualisaties", tabName = "geospatialevisualisaties", icon = icon("angle-double-right"))
                         ),
                
                         
                sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
                
                radioButtons("typeInput", "Product type",
                             choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                             selected = "WINE"),
                
                selectInput("countryInput", "Country",
                            choices = c("CANADA", "FRANCE", "ITALY")))
  ),

#-------------------------- DASHBOARD BODY ------------------------------------------  
  dashboardBody(
    tabItems(
      
      ##------------------- TAB ITEM 1 - HISTOGRAMMEN ---------------------
      tabItem(
        tabName = "histogrammen",
        
          fluidRow(
            
            # BOX 1 - Histogram GDP
            box(
              title = "Histogram GDP", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("output$hist.gdp", height = 250)
            ),
            
            # BOX 2 - Histgram Broeikasgassen
            box(
              title = "Histogram Broeikasgassen", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("output$hist.gdp", height = 250)
            )
          )
        ),
      
      ##----------------- TAB ITEM 2 - LIJNGRAFIEKEN --------------
      tabItem(
        tabName = "lijngrafieken",
        
        fluidRow(
          
          # BOX 1 - lijngrafieken GDP
          box(
            title = "Lijngrafieken GDP", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("output$hist.gdp", height = 250)
          ),
          
          # BOX 2 - lijngrafieken Broeikasgassen
          box(
            title = "Lijngrafieken Broeikasgassen", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("output$hist.gdp", height = 250)
          )
        )
      ),
      
      ##--------------- TAB ITEM 3 - DUMBELL CHARTS ---------------
      tabItem(
        tabName = "dumbellcharts",
        
        fluidRow(
          
          # BOX 1 - dumbell charts GDP
          box(
            title = "Dumbell charts GDP", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("output$hist.gdp", height = 250)
          ),
          
          # BOX 2 - dumbell charts Broeikasgassen
          box(
            title = "Dumbell charts Broeikasgassen", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("output$hist.gdp", height = 250)
          )
        )
      ),
      
      ##--------------- TAB ITEM 4 - GEOSPATIALE VISUALISATIES -------------
      tabItem(
        tabName = "geospatialevisualisaties",
        
        fluidRow(
          
          # BOX 1 - Geospatiale GDP
          box(
            title = "Geospatiale GDP", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("output$hist.gdp", height = 250)
          ),
          
          # BOX 2 - Geospatiale Broeikasgassen
          box(
            title = "Geospatiale Broeikasgassen", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("output$hist.gdp", height = 250)
          )
        )
      )
    )
  )
)
  



#-------------------------- SERVER INPUT & OUTPUT-----------------------------------------------------------------------
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
