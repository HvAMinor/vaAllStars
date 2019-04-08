#This line of code installs the pacman page if you do not have it installed - 
#if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")

devtools::install_github('HvAMinor/bbplot', upgrade = c("never"))
pacman::p_load('extrafont')

extrafont::font_import(prompt = FALSE, pattern = "*erdana*")
extrafont::loadfonts(device="win")

pacman::p_load('rstudioapi','dplyr', 'tidyr', 'gapminder',
               'ggalt', 'eurostat', 'colorspace', 'forcats', 'R.utils', 'png', 'glue', 'countrycode', 
               'grid', 'ggpubr', 'scales')

library(ggplot2)
library(bbplot)
library(shiny)
library(shinydashboard)

# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))
# you can make sure you are in the right directory
print(getwd())
bronnen <-"Eurostat (2019) Real GDP per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/sdg_08_10 \r\nEurostat (2019) Greenhouse gas emissions per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/t2020_rd300"

gasGdpCapita <- read.csv('gasGdpCapita.csv', row.names='X', header=TRUE, stringsAsFactors = FALSE) %>%
  mutate(geo=countrycode::countrycode(geo, origin='eurostat', destination='country.name'))

dumbell <- gasGdpCapita %>% 
  select(geo, jaar, gas) %>% 
  spread(jaar, gas, convert=TRUE)

geodata_N0 <- get_eurostat_geospatial(year = "2016",
                                      nuts_level = 0,
                                      resolution = "60")

# TOEVOEGEN - COUNTRY
geodata_N0 <- geodata_N0 %>%
  mutate(geo = countrycode(sourcevar = CNTR_CODE,
                           origin = 'eurostat',
                           destination = 'country.name'))


# DATASETS JOINEN - geodata_N0, gasGdpCapita
map_data <- gasGdpCapita %>%
  inner_join(y = geodata_N0, by = "geo")

#-------------------------- SHINY DASHBOARD APP ------------------------------------------ 
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
                
                         
                sliderInput("keuzejaren", "keuzejaren", 2000, 2016, c(2006, 2016)),
                selectInput("keuzejaar", "keuzejaar", choices = seq(2000, 2016), selected = 2006),
                selectInput("klassen", "klassen", choices = seq(2, 8))
                )
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
              plotOutput("lijn_gas_in_groepen_gdp", height = 250)
            ),
            
            # BOX 2 - Histgram Broeikasgassen
            box(
              title = "Histogram Broeikasgassen", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("lijn_gas_in_groepen_gdp", height = 250)
            )
          )
        ),
      
      ##----------------- TAB ITEM 2 - LIJNGRAFIEKEN --------------
      tabItem(
        tabName = "lijngrafieken",
        
        fluidRow(
          # BOX 2 - lijngrafieken Broeikasgassen
          box(
            title = "Lijngrafieken Broeikasgassen", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("lijn_gas_in_groepen_gdp", height = 750)
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
            plotOutput("lijn_gas_in_groepen_gdp", height = 250)
          ),
          
          # BOX 2 - dumbell charts Broeikasgassen
          box(
            title = "Dumbell charts Broeikasgassen", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("lijn_gas_in_groepen_gdp", height = 250)
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
            plotOutput("lijn_gas_in_groepen_gdp", height = 250)
          ),
          
          # BOX 2 - Geospatiale Broeikasgassen
          box(
            title = "Geospatiale Broeikasgassen", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("lijn_gas_in_groepen_gdp", height = 250)
          )
        )
      )
    )
  )
)
  



#-------------------------- SERVER INPUT & OUTPUT-----------------------------------------------------------------------
server <- function(input, output) {
  
  #--------------- OUTPUT LIJNGRAFIEKEN GAS -------------------------------
    output$lijn_gas_in_groepen_gdp <- renderPlot({
      print("derp")
      klassen <- as.character(input$klassen)
      keuzejaar.min <- as.character(min(input$keuzejaren))
      keuzejaar.max <- as.character(max(input$keuzejaren))
      
      # LABS BENOEMEN
      lijn_broeikas_titel <- glue("Ontwikkeling uitstoot broeikasgassen per hoofd bevolking {keuzejaar.min} - {keuzejaar.max}")
      lijn_broeikas_subtitel <- glue("{keuzejaar.min} - {keuzejaar.max}")
      lijn_broeikas_legendatitel <- glue("Verdeling van GDP in klassen (EUR)")
      lijn_broeikas_x_as <- glue("Landsnamen")
      lijn_broeikas_y_as <- glue("Uitstoot broeikasgassen (gemeten in tonnen CO2 equivalent)")
      
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
        theme(legend.position = "right")
      
      finalise_plot(plot_name = lijn.plot,
                    source_name = bronnen,
                    save_filepath = glue("lijn {keuzejaar.min} - {keuzejaar.max}.png"),
                    width_pixels = 750, height_pixels = 600, logo_image_path = "dataloog.png")
  })
    
    
}

shinyApp(ui, server)
