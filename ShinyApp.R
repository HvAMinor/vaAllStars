#This line of code installs the pacman page if you do not have it installed - 
#if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")

devtools::install_github('HvAMinor/bbplot', upgrade = c("never"))
pacman::p_load('extrafont')

extrafont::font_import(prompt = FALSE, pattern = "*erdana*")
extrafont::loadfonts(device="win")

pacman::p_load('rstudioapi','dplyr', 'tidyr', 'gapminder',
              'ggalt', 'eurostat', 'colorspace', 'forcats', 'R.utils', 'png', 'glue', 'countrycode', 
               'grid', 'ggpubr', 'scales','ggrepel')

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
      menuItem(text = "de lijngrafieken", tabName = "lijngrafieken"),
      menuItem(text = "histogrammen en verdeling", tabName = "histogrammen"),
      menuItem(text = "dumbell charts", tabName = "dumbells"),
      menuItem(text = "geospatiale plots", tabName = "geodata"),
      menuItem(text = "correlatie plots", tabName = "correlatie"),
      
      sliderInput("keuzejaren", "keuzejaren", 2000, 2016, c(2006, 2016)),
      #selectInput("keuzejaar", "keuzejaar", choices = seq(2000, 2016), selected = 2006),
      selectInput("klassen", "klassen", choices = seq(2, 8), selected=3),
      selectInput("keuzelanden", "keuzelanden", choices = unique(gasGdpCapita$geo), multiple = TRUE, selected = c('Netherlands', 'Germany'))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "lijngrafieken",
              fluidRow(
                box(
                  title = "lijnplot", status = "primary", solidHeader = TRUE, width=12, height=552,
                  collapsible = TRUE,
                  plotOutput("lijn")
                  )
                )
              ),
      tabItem(tabName = "dumbells",
              fluidRow(
                box(width=12,
                  title = "Ontwikkeling broeikas uitstoot per hoofd", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("dumbell")
                  )                
              )),
      tabItem(tabName = "geodata",
              fluidRow(
                box(title = "de geoplot max", status = "primary", solidHeader = TRUE, width=4,
                    collapsible = TRUE,
                    plotOutput("geoplotgdp.max")
                    ),
                box(title = "de geoplot max", status = "primary", solidHeader = TRUE, width=4,
                    collapsible = TRUE,
                    plotOutput("geoplotgas.max")
                    )
              )),
      tabItem(tabName = "correlatie", fluidRow( box( title = "correlatie", status = "primary", solidHeader = TRUE, width=12,
                  collapsible = TRUE, 
                  plotOutput("correlatie"))))
    )
  )
)

server <- function(input, output) {
  output$geoplotgdp.max <- renderPlot({
    klassen <- as.character(input$klassen)
    keuzejaar <- as.character(max(input$keuzejaren))
    map_data_gas <- map_data %>%
      mutate(cat = cut_to_classes(x = gas, n = klassen, style = "quantile" ))

    ggplot(data = map_data_gas) +
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
  })
  output$geoplotgas.max <- renderPlot({
    klassen <- as.character(input$klassen)
    keuzejaar <- as.character(max(input$keuzejaren))
    map_data_gdp <- map_data %>%
      mutate(cat = cut_to_classes(x = gdp, n = klassen, style = "quantile" ))

    ggplot(data = map_data_gdp) +
        geom_sf(data = geodata_N0, fill = "gray85", alpha = 1) +
        geom_sf(data = map_data_gdp[map_data_gdp$jaar == keuzejaar,], 
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
    ggplot(lijn.data, aes(x = jaar, y = gas, colour = gdp)) +
      geom_line(size = 1) +
      labs(x='jaren') +
      bbc_style()

  })
  output$dumbell <- renderPlot({
    keuzejaar.min <- as.character(min(input$keuzejaren))
    keuzejaar.max <- as.character(max(input$keuzejaren))
    dumbell.data <- dumbell %>% mutate(gap = dumbell[,keuzejaar.max] - dumbell[,keuzejaar.min])
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("verandering uitstoot broeikasgassen per hoofd bevolking {keuzejaar.min} - {keuzejaar.max}")
    dumbell.broeikas.x.as <- glue("Landsnamen")
    dumbell.broeikas.y.as <- glue("Uitstoot broeikasgassen (gementen in tonnen CO2 equivalent)")
    
    ggplot(dumbell.data, aes(x = dumbell.data[,keuzejaar.max],
                                             xend = dumbell.data[,keuzejaar.min],
                                             y = reorder(geo, gap), group = geo)) +
      geom_dumbbell(colour = "#dddddd",
                    size = 3,
                    colour_x = "#FAAB18",
                    colour_xend = "#1380A1") +
      labs(title=dumbell.broeikas.titel) +
      bbc_style()
  })
  
  output$correlatie <- renderPlot({
    keuzelanden <- input$keuzelanden
    gasGdpCapita %>% 
      filter(jaar %in% seq(as.character(min(input$keuzejaren)), as.character(max(input$keuzejaren))), geo %in% keuzelanden) %>%
        ggplot(aes(x = gdp,
                   y = gas,
                   fill = geo)) +
        
        # Scatterplot met vorm, kleur en grootte
        geom_point(shape = 21,
                   color = "black",
                   size = 1.5) +
        
        stat_ellipse(aes(color = geo),
                     color = "black",
                     geom = "polygon",
                     level = 0.95,
                     alpha = 0.5) +
      
      geom_label_repel(aes(label = jaar),
                       size = 4,
                       box.padding = 0.5,
                       segment.color = "blue") +
      bbc_style()
  })
}

shinyApp(ui, server)
