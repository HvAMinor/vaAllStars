# This line of code installs the pacman page if you do not have it installed -
# if you do, it simply loads the package
if (!require(pacman))
  install.packages("pacman")

devtools::install_github("HvAMinor/bbplot", upgrade = c("never"))
pacman::p_load("extrafont")

extrafont::font_import(prompt = FALSE, pattern = "*erdana*")
extrafont::loadfonts(device = "win")

#--------- 1. LIBRARIES INLADEN -------------
pacman::p_load(
  "rstudioapi",
  "dplyr",
  "tidyr",
  "gapminder",
  "ggalt",
  "eurostat",
  "colorspace",
  "forcats",
  "R.utils",
  "png",
  "glue",
  "countrycode",
  "grid",
  "ggpubr",
  "scales",
  "ggrepel",
  "scales"
)

library(ggplot2)
library(bbplot)
library(shiny)
library(shinydashboard)

# 2. CONFIGUREREN VAN DE WORKING DIRECTORY --------------

# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path
# The next line set the working directory to the relevant one:
setwd(dirname(current_path))
# you can make sure you are in the right directory
print(getwd())

# 3. BRONNEN - HYPERLINKS --------------------------------
bronnen <-
  "Eurostat (2019) Real GDP per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/sdg_08_10 \r\n
    Eurostat (2019) Greenhouse gas emissions per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/t2020_rd300"

metadata <- 
  "Eurostat (2019) Real GDP per capita [metadata] Retrieved from: https://ec.europa.eu/eurostat/cache/metadata/en/sdg_08_10_esmsip2.htm \r\n
    Eurostat (2019) Greenhouse gas emissions per capita [metadata] Retrieved from: https://ec.europa.eu/eurostat/cache/metadata/en/t2020_rd300_esmsip2.htm"

# 4. BESTANDEN INLADEN -----------------------------------
gasGdpCapita <-
  read.csv("gasGdpCapita.csv", row.names = "X", header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(geo = countrycode::countrycode(geo, origin = "eurostat", destination = "country.name"))

# 5. DATASETS AANMAKEN & JOINEN --------------------------
#dumbbell chart
dumbell <- gasGdpCapita %>%
  select(geo, jaar, gas) %>%
  spread(jaar, gas, convert = TRUE)

#geodata
geodata_N0 <- get_eurostat_geospatial(year = "2016", nuts_level = 0, resolution = "60") %>%
  # landnamen toevoegen
  mutate(geo = countrycode(
    sourcevar = CNTR_CODE,
    origin = "eurostat",
    destination = "country.name"
  ))

#datasets joinen - geodata_N0, gasGdpCapita
map_data <- gasGdpCapita %>%
  inner_join(y = geodata_N0, by = "geo")

# 6. SHINY DASHBOARD -------------------------------------
# 6a. unit interface -------------------------------------
ui <- dashboardPage(
  # 6a1. header ------------------------------------------
  dashboardHeader(title = "Gemiddeld BBP per hoofd VS uitstoot broeikasgassen per hoofd",
                  titleWidth = 750),
  
  # 6a2. sidebar -----------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Verdeling inkomen en uitstoot", tabName = "hist"),
      menuItem(text = "Lijngrafieken", tabName = "lijngrafieken"),
      menuItem(text = "Dumbell charts", tabName = "dumbells"),
      menuItem(text = "Geospatiale plots", tabName = "geodata"),
      menuItem(text = "Correlatie plots", tabName = "correlatie"),
      
      sliderInput("keuzejaren", "Begin- & eindjaar", 2000, 2016, c(2006, 2016)),
      selectInput("klassen", "Klassenverdeling", choices = seq(2, 8), selected = 3),
      selectInput("keuzelanden", "Keuzelanden", choices = unique(gasGdpCapita$geo),
                  multiple = TRUE, selected = c("Netherlands", "Germany"))
    )
  ),
  ## sidebar close
  
  # 6a3. body ---------------------------------------------
  dashboardBody(tabItems(
    
    # `-------- histogrammen ---------
    tabItem(tabName = "hist",
            fluidRow(
              tabBox(
                title = "BEGINJAAR",
                tabPanel("BBP per hoofd", plotOutput("hist_gdp_min") ),
                tabPanel("Broeikasgassen per hoofd", plotOutput("hist_gas_min"))
              ),
              tabBox(
                title = "EINDJAAR",
                tabPanel("BBP per hoofd", plotOutput("hist_gdp_max") ),
                tabPanel("Broeikasgassen per hoofd", plotOutput("hist_gas_max"))
              )
            )
          ),
    
    
    # `-------- lijngrafieken ---------
      tabItem(tabName = "lijngrafieken",
              fluidRow(
                box(
                  title = "lijnplot",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  height = 552,
                  collapsible = TRUE,
                  plotOutput("lijn_gdp")
                ),
                box(
                  title = "lijnplot",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  height = 552,
                  collapsible = TRUE,
                  plotOutput("lijn_gas")
                )
              )),
    
    
    # `-------- dumbbells ---------
      tabItem(tabName = "dumbells",
              fluidRow(
                box(
                  width = 12,
                  title = "Ontwikkeling broeikasgas uitstoot per hoofd",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("dumbell")
                )
              )),
    
    
    
    # `-------- geodata ---------
      tabItem(tabName = "geodata",
              fluidRow(
                box(
                  title = "de geoplot max",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  collapsible = TRUE,
                  plotOutput("geo_gdp_max")
                ),
                box(
                  title = "de geoplot max",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  collapsible = TRUE,
                  plotOutput("geo_gas_max")
                )
              )),
    
    
    # `-------- correlaties ---------
      tabItem(tabName = "correlatie",
              fluidRow(
                box(
                  title = "Correlatie per land",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  collapsible = TRUE,
                  plotOutput("correlatie")
                ),
                box(
                  title = "Correlatie per inkomensklasse",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  collapsible = TRUE,
                  plotOutput("correlatie_gdp")
                )
              )
            )
    )
  )
)

##-------------- server --------------
server <- function(input, output) {
  
  # geodata BBP
  output$geo_gdp_max <- renderPlot({
    klassen <- as.character(input$klassen)
    keuzejaar <- as.character(max(input$keuzejaren))
    map_data_gas <- map_data %>%
      mutate(cat = cut_to_classes(x = gas, n = klassen, style = "quantile"))
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("")
    dumbell.broeikas.subtitel <- glue("")
    dumbell.broeikas.caption <- glue("Eurostat (2019) Real GDP per capita
                                      Eurostat (2019) Greenhouse gas emissions per capita")
    dumbell.broeikas.y.as <- glue("")
    dumbell.broeikas.x.as <- glue("")
    
    ggplot(data = map_data_gas) +
      geom_sf(data = geodata_N0,
              fill = "gray85",
              alpha = 1) +
      geom_sf(data = map_data_gas[map_data_gas$jaar == keuzejaar,],
              aes(fill = cat),
              color = "gray40",
              size = 0.2) +
      geom_sf(
        data = geodata_N0,
        color = "gray30",
        size = 0.703,
        alpha = 0
      ) +
      
      coord_sf(xlim = c(-25, 45), ylim = c(35, 72)) +
      
      scale_fill_discrete_sequential(name = "<AANVULLEN>", "ag_GrnYl") +
      
      theme(legend.position = "right",
            legend.title.align = 0) +
      guides(fill = guide_legend(reverse = TRUE)) +
      
      # labs aanroepen
      labs(title = dumbell.broeikas.titel,
           subtitle = dumbell.broeikas.subtitel,
           caption = dumbell.broeikas.caption) +
      xlab(dumbell.broeikas.x.as) + 
      ylab(dumbell.broeikas.y.as)
  })
  
  # geodata uitstoot
  output$geo_gas_max <- renderPlot({
    klassen <- as.character(input$klassen)
    keuzejaar <- as.character(max(input$keuzejaren))
    map_data_gdp <- map_data %>%
      mutate(cat = cut_to_classes(x = gdp, n = klassen, style = "quantile"))
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("")
    dumbell.broeikas.subtitel <- glue("")
    dumbell.broeikas.caption <- glue("Eurostat (2019) Real GDP per capita
                                     Eurostat (2019) Greenhouse gas emissions per capita")
    dumbell.broeikas.y.as <- glue("")
    dumbell.broeikas.x.as <- glue("")
    
    ggplot(data = map_data_gdp) +
      geom_sf(data = geodata_N0,
              fill = "gray85",
              alpha = 1) +
      geom_sf(data = map_data_gdp[map_data_gdp$jaar == keuzejaar,],
              aes(fill = cat),
              color = "gray40",
              size = 0.2) +
      geom_sf(
        data = geodata_N0,
        color = "gray30",
        size = 0.703,
        alpha = 0
      ) +
      coord_sf(xlim = c(-25, 45), ylim = c(35, 72)) +
      
      scale_fill_discrete_sequential(name = "<AANVULLEN>", "ag_GrnYl") +
      
      theme(legend.position = "left",
            legend.title.align = 0) +
      guides(fill = guide_legend(reverse = TRUE)) +
      
      # labs aanroepen
      labs(title = dumbell.broeikas.titel,
           subtitle = dumbell.broeikas.subtitel,
           caption = dumbell.broeikas.caption) +
      xlab(dumbell.broeikas.x.as) + 
      ylab(dumbell.broeikas.y.as)
  })
  
  # lijngrafiek BBP
  output$lijn_gdp <- renderPlot({
    klassen <- as.character(input$klassen)
    keuzejaar.min <- as.character(min(input$keuzejaren))
    keuzejaar.max <- as.character(max(input$keuzejaren))
    
    lijn.data <- gasGdpCapita %>%
      filter(jaar %in% seq(keuzejaar.min, keuzejaar.max)) %>%
      mutate(range_gdp = cut_to_classes(
        gdp,
        n = klassen,
        style = "quantile",
        decimals = 0
      )) %>%
      dplyr::select(jaar, gdp = range_gdp, gas) %>%
      group_by(gdp, jaar) %>%
      summarize(gas = mean(gas)) %>%
      ungroup()
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("")
    dumbell.broeikas.subtitel <- glue("")
    dumbell.broeikas.caption <- glue("Eurostat (2019) Real GDP per capita
                                     Eurostat (2019) Greenhouse gas emissions per capita")
    dumbell.broeikas.y.as <- glue("")
    dumbell.broeikas.x.as <- glue("") 
    
    ggplot(lijn.data, aes(x = jaar, y = gas, colour = gdp)) +
      geom_line(size = 1) +
      
      # labs aanroepen
      labs(title = dumbell.broeikas.titel,
           subtitle = dumbell.broeikas.subtitel,
           caption = dumbell.broeikas.caption) +
      xlab(dumbell.broeikas.x.as) + 
      ylab(dumbell.broeikas.y.as)
  })
  
  # lijngrafiek uitstoot
  output$lijn_gas <- renderPlot({
    klassen <- as.character(input$klassen)
    keuzejaar.min <- as.character(min(input$keuzejaren))
    keuzejaar.max <- as.character(max(input$keuzejaren))
    
    lijn.data <- gasGdpCapita %>%
      filter(jaar %in% seq(keuzejaar.min, keuzejaar.max)) %>%
      mutate(range_gas = cut_to_classes(
        gas,
        n = klassen,
        style = "quantile",
        decimals = 0
      )) %>%
      dplyr::select(jaar, gas = range_gas, gdp) %>%
      group_by(gas, jaar) %>%
      summarize(gdp = mean(gdp)) %>%
      ungroup()
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("")
    dumbell.broeikas.subtitel <- glue("")
    dumbell.broeikas.caption <- glue("Eurostat (2019) Real GDP per capita
                                     Eurostat (2019) Greenhouse gas emissions per capita")
    dumbell.broeikas.y.as <- glue("")
    dumbell.broeikas.x.as <- glue("")
    
    ggplot(lijn.data, aes(x = jaar, y = gdp, colour = gas)) +
      geom_line(size = 1) +
      
      # labs aanroepen
      labs(title = dumbell.broeikas.titel,
           subtitle = dumbell.broeikas.subtitel,
           caption = dumbell.broeikas.caption) +
      xlab(dumbell.broeikas.x.as) + 
      ylab(dumbell.broeikas.y.as)
  })
  
  # dumbbell uitstoot
  output$dumbell <- renderPlot({
    keuzejaar.min <- as.character(min(input$keuzejaren))
    keuzejaar.max <- as.character(max(input$keuzejaren))
    dumbell.data <- dumbell %>% 
      mutate(gap = dumbell[, keuzejaar.max] - dumbell[, keuzejaar.min]) %>%
      arrange(desc(gap))
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("Ontwikkeling uitstoot broeikasgassen per hoofd bevolking")
    dumbell.broeikas.subtitel <- glue("{keuzejaar.min} - {keuzejaar.max}")
    dumbell.broeikas.caption <- glue("Eurostat (2019) Real GDP per capita")
    dumbell.broeikas.y.as <- glue("Landsnamen")
    dumbell.broeikas.x.as <- glue("Uitstoot broeikasgassen (gemeten in tonnen CO2 equivalent per hoofd)")
    
    ggplot(dumbell.data,
           aes(
             x = dumbell.data[, keuzejaar.max],
             xend = dumbell.data[, keuzejaar.min],
             y = reorder(geo, gap),
             group = geo
           )) +
      geom_dumbbell(
        colour = "#dddddd",
        size = 3,
        colour_x = "#FAAB18",
        colour_xend = "#1380A1"
      ) +
      
      scale_color_manual(name = "", values = c("orange", "blue") ) +
      
      # labs aanroepen
      labs(title = dumbell.broeikas.titel,
           subtitle = dumbell.broeikas.subtitel,
           caption = dumbell.broeikas.caption) +
      xlab(dumbell.broeikas.x.as) + 
      ylab(dumbell.broeikas.y.as)
  })
  
  # correlatie per land
  output$correlatie <- renderPlot({
    keuzelanden <- input$keuzelanden
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("")
    dumbell.broeikas.subtitel <- glue("")
    dumbell.broeikas.caption <- glue("Eurostat (2019) Real GDP per capita
                                      Eurostat (2019) Greenhouse gas emissions per capita")
    dumbell.broeikas.y.as <- glue("")
    dumbell.broeikas.x.as <- glue("")
    
    gasGdpCapita %>%
      filter(jaar %in% seq(as.character(min( 
        input$keuzejaren
      )), as.character(max(
        input$keuzejaren
      ))),
      geo %in% keuzelanden) %>%
      ggplot(aes(x = gdp,
                 y = gas,
                 fill = geo)) +
      
      # Scatterplot met vorm, kleur en grootte
      geom_point(shape = 21,
                 color = "black",
                 size = 1.5) +
      
      stat_ellipse(
        aes(color = geo),
        color = "black",
        geom = "polygon",
        level = 0.95,
        alpha = 0.5
      ) +
      
      geom_label_repel(
        aes(label = jaar),
        size = 4,
        box.padding = 0.5,
        segment.color = "blue"
      ) +
      bbc_style()
  })
  
  # correlatie per inkomensklasse
  output$correlatie_gdp <- renderPlot({
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("")
    dumbell.broeikas.subtitel <- glue("")
    dumbell.broeikas.caption <- glue("Eurostat (2019) Real GDP per capita
                                     Eurostat (2019) Greenhouse gas emissions per capita")
    dumbell.broeikas.y.as <- glue("")
    dumbell.broeikas.x.as <- glue("")
    
    gasGdpCapita %>%
      filter(jaar %in% seq(as.character(min(
        input$keuzejaren
      )), as.character(max(
        input$keuzejaren
      )))) %>%
      mutate(bbp = cut_to_classes(gdp, as.character(input$klassen))) %>%
      ggplot(aes(x = gdp,
                 y = gas,
                 fill = bbp)) +
      
      # Scatterplot met vorm, kleur en grootte
      geom_point(shape = 21,
                 color = "black",
                 size = 1.5) +
      
      stat_ellipse(
        aes(color = bbp),
        color = "black",
        geom = "polygon",
        level = 0.95,
        alpha = 0.5
      ) +
      bbc_style()
  })
  
  # histgram uitstoot beginjaar
  output$hist_gas_min <- renderPlot({
    keuzejaar.min <- as.character(min(input$keuzejaren))
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("")
    dumbell.broeikas.subtitel <- glue("")
    dumbell.broeikas.caption <- glue("Eurostat (2019) Real GDP per capita
                                     Eurostat (2019) Greenhouse gas emissions per capita")
    dumbell.broeikas.y.as <- glue("")
    dumbell.broeikas.x.as <- glue("")
    
    gasGdpCapita %>%
      mutate(cat_gas = cut_to_classes(gas, input$klassen)) %>%
      filter(jaar == keuzejaar.min) %>%
      ggplot(aes(x = cat_gas)) +
      geom_bar(colour = "white",
               fill = "#1380A1",
               stat = "count") +
      geom_hline(yintercept = 0,
                 size = 1,
                 colour = "#333333") +
      labs(title = glue("Verdeling uitstoot broeikasgassen {keuzejaar.min}")) +
      scale_y_continuous(breaks = pretty_breaks())
  })
  
  # histogram uitstoot eindjaar
  output$hist_gas_max <- renderPlot({
    keuzejaar.max <- as.character(max(input$keuzejaren))
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("")
    dumbell.broeikas.subtitel <- glue("")
    dumbell.broeikas.caption <- glue("Eurostat (2019) Real GDP per capita
                                     Eurostat (2019) Greenhouse gas emissions per capita")
    dumbell.broeikas.y.as <- glue("")
    dumbell.broeikas.x.as <- glue("")
    
    gasGdpCapita %>%
      mutate(cat_gas = cut_to_classes(gas, input$klassen)) %>%
      filter(jaar == keuzejaar.max) %>%
      ggplot(aes(x = cat_gas)) +
      geom_bar(colour = "white",
               fill = "#1380A1",
               stat = "count") +
      geom_hline(yintercept = 0,
                 size = 1,
                 colour = "#333333") +
      labs(title = glue("Verdeling uitstoot broeikasgassen {keuzejaar.max}")) +
      scale_y_continuous(breaks = pretty_breaks())
  })
  
  # histogram BBP beginjaar
  output$hist_gdp_min <- renderPlot({
    keuzejaar.min <- as.character(min(input$keuzejaren))
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("")
    dumbell.broeikas.subtitel <- glue("")
    dumbell.broeikas.caption <- glue("Eurostat (2019) Real GDP per capita
                                     Eurostat (2019) Greenhouse gas emissions per capita")
    dumbell.broeikas.y.as <- glue("")
    dumbell.broeikas.x.as <- glue("")
    
    gasGdpCapita %>%
      mutate(cat_gdp = cut_to_classes(gdp, input$klassen)) %>%
      filter(jaar == keuzejaar.min) %>%
      ggplot(aes(x = cat_gdp)) +
      geom_bar(colour = "white",
               fill = "#1380A1",
               stat = "count") +
      geom_hline(yintercept = 0,
                 size = 1,
                 colour = "#333333") +
      labs(title = glue("Verdeling uitstoot broeikasgassen {keuzejaar.min}")) +
      scale_y_continuous(breaks = pretty_breaks())
  })
  
  # histogram BBP eindjaar
  output$hist_gdp_max <- renderPlot({
    keuzejaar.max <- as.character(max(input$keuzejaren))
    
    # DUMBELL LABS
    dumbell.broeikas.titel <- glue("")
    dumbell.broeikas.subtitel <- glue("")
    dumbell.broeikas.caption <- glue("Eurostat (2019) Real GDP per capita
                                     Eurostat (2019) Greenhouse gas emissions per capita")
    dumbell.broeikas.y.as <- glue("")
    dumbell.broeikas.x.as <- glue("")
    
    gasGdpCapita %>%
      mutate(cat_gdp = cut_to_classes(gdp, input$klassen)) %>%
      filter(jaar == keuzejaar.max) %>%
      ggplot(aes(x = cat_gdp)) +
      geom_bar(colour = "white",
               fill = "#1380A1",
               stat = "count") +
      geom_hline(yintercept = 0,
                 size = 1,
                 colour = "#333333") +
      labs(title = glue("Verdeling uitstoot broeikasgassen {keuzejaar.max}")) +
      scale_y_continuous(breaks = pretty_breaks())
  })
}

shinyApp(ui, server)
