
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
                menuItem("Lijngrafieken", tabName = "Lijngrafieken", icon = icon("th")),
                menuItem("Proportion table", tabName = "Proportion table", icon = icon("th")),
                menuItem("Dumbell plots", tabName = "Dumbell plots", icon = icon("th")),
                menuItem("Geospatiale plots", tabName = "Geospatiale plots", icon = icon("th")))
  ),
  dashboardBody(
    fluidRow(box(plotOutput("hist.gdp"))),
    fluidRow(box(title = "Controls",
                 sliderInput("hist.gdp.slider", "Number of observations:", 2000, 2016, 2001),
                 radioButtons("hist.gdp.verdeling", "de verdeling", 
                              choices = list(1,2,3,4,5), 
                              selected=list(5)))),
    tabItem(tabName = "widgets",
            h2("Widgets tab content"))
  )
)


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