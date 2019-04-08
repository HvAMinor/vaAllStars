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


ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "histogrammen en verdeling", tabName = "histogrammen"),
      menuItem(text = "de lijngrafieken", tabName = "lijngrafieken"),
      menuItem(text = "dumbell charts", tabName = "dumbells"),
      menuItem(text = "geospatiale plots", tabName = "geodata"),
      
      sliderInput("keuzejaren", "keuzejaren", 2000, 2016, c(2006, 2016)),
      selectInput("keuzejaar", "keuzejaar", choices = seq(2000, 2016), selected = 2006),
      selectInput("klassen", "klassen", choices = seq(2, 8))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "lijngrafieken"),
      tabItem(tabName = "dumbells",
              fluidRow(
                box(
                  title = "Ontwikkeling broeikas uitstoot per hoofd", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("dumbell", height = 750)
                  )                
              )),
      tabItem(tabName = "geodata"),
      tabItem(tabName = "histogrammen",
              fluidRow(
                box(
                  title = "lijnplot", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("lijn", height = 750)
                  )
                )
              ),
      tabItem(tabName = "kpis",
              fluidRow(
                box(title = "lijnplot", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("geoplot", height = 750)
                    )
                )
          )
      )
    )
  )
)


server <- function(input, output) {
  output$geoplot <- renderPlot({
    klassen <- as.character(input$klassen)
    keuzejaar <- as.character(input$keuzejaar)
    map_data_gas <- map_data %>%
      mutate(cat = cut_to_classes(x = gas, n = klassen, style = "quantile" ))

    map_data_gas.plot <- ggplot(data = map_data_gas) +
        geom_sf(data = geodata_N0, fill = "gray85", alpha = 1) +
        geom_sf(data = map_data_gas[map_data_gas$jaar == keuzejaar,], 
                aes(fill = cat),
                color = "gray40", size = 0.2) +
        geom_sf(data = geodata_N0, color = "gray30", size = 0.703, alpha = 0) +
        coord_sf(xlim = c(-25, 45), ylim = c(35, 72)) +
        scale_fill_discrete_sequential(name = "<AANVULLEN>", "ag_GrnYl") +
        theme(legend.position = "left", legend.title.align = 0) +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(title = paste("<AANVULLEN>"),
             subtitle = paste("<AANVULLEN>"),
             caption = "<AANVULLEN>",
             x = "Lengtegraad",
             y = "Breedtegraad") +
        bbc_style()
    finalise_plot(plot_name = map_data_gas.plot,
            source_name = bronnen,
            save_filepath = glue("geo {keuzejaar}.png"),
            width_pixels = 750, height_pixels = 600, logo_image_path = "dataloog.png")
  })
  
  output$lijn <- renderPlot({
    klassen <- as.character(input$klassen)
    keuzejaar.min <- as.character(min(input$keuzejaren))
    keuzejaar.max <- as.character(max(input$keuzejaren))

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
      labs(title=dumbell.broeikas.titel) +
      bbc_style()

    finalise_plot(plot_name = dumbell.plot,
                  source_name = bronnen,
                  save_filepath = glue("dumbell {keuzejaar.min} - {keuzejaar.max}.png"),
                  width_pixels = 750, height_pixels = 600, logo_image_path = "dataloog.png")
  })
}

shinyApp(ui, server)
