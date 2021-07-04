### load packages  
library(shiny)
library(shinyWidgets)
library(shinythemes)

library(tidyverse) ## data manipulation, cleaning, aggregation
library(leaflet) ## create interactive html maps
library(kableExtra) ## html table formatting

#### Data ####
### load data  
bw.data <- read_csv("bw_data_clean_1992-2020.csv")

### convert status to ordered factor
bw.data <- bw.data %>% 
    mutate(status = factor(status, 
                           levels = c("excellent", "good", "sufficient", "poor", "not classified"), 
                           ordered = TRUE))

### define custom colours based on EU BWD colour scheme
custom.cols <- bw.data %>% 
    select(status) %>% 
    distinct() %>% 
    arrange(status) %>% 
    mutate(colour_name = case_when(status == "excellent" ~ "royalblue4",
                                   status == "good" ~ "steelblue2",
                                   status == "sufficient" ~ "forestgreen",
                                   status == "poor" ~ "red3",
                                   status == "not classified" ~ "gray60"), 
           colour_hex = gplots::col2hex(colour_name))

### define custom colour palette to map factor levels to colours (using my manual colours)   
status.custom.pal <- leaflet::colorFactor(custom.cols$colour_name, domain = custom.cols$status)

### construct additional data attribution label for the map  
data.attribution <- '<div>Data: DG ENV, <a href="https://www.eea.europa.eu/data-and-maps/data/bathing-water-directive-status-of-bathing-water-13" title="EEA">EEA</a></div>'




#### Shiny app ####  
### Define UI 
ui <- fluidPage(
    
    ## App theme 
    theme = shinytheme("flatly"),

    ## Application title
    titlePanel("Bathing water status in Europe"),

    ## Sidebar with user input controls 
    sidebarLayout(
        sidebarPanel(
            ## drop-down menu - select country 
            selectInput(
                inputId = "country", 
                label = "Country", 
                choices = bw.data %>% select(country_name) %>% arrange(country_name) %>% unique(), 
                multiple = FALSE
            ), 
            
            ## slider: select date (year)
            sliderInput(inputId = "year",
                        label = "Year",
                        min = 1990,
                        max = 2020,
                        value = 1990, step = 1,
                        sep = "")
        ),

        ## Main panel layout
        mainPanel(
            ## main panel tabs 
            tabsetPanel(type = "tabs",
                        ## description tab - static text (background & app documentation)
                        tabPanel("Description", 
                                 h3("Background"),
                                 p("Under the Bathing Waters Directive (BWD), EU Member States 
                                 identify popular bathing places in fresh and coastal waters, 
                                 and monitor them for microbiological pollution (and other substances) 
                                 throughout the bathing season, from May to September."),
                                 p('The status of bathing waters is classified as poor, sufficient, 
                                 good or excellent based on numerical quality standards for 
                                 bacteriological quality. "Sufficient" is the minimum quality 
                                 threshold that all Member States should attain by the end of the 2015 
                                 season at the latest. Where water is classified as "poor", Member States 
                                 should ban bathing or advise against it, and take suitable corrective measures.'),
                                 h4("Data description and source"),
                                 p("The data come from Member State national submissions to the BWD, and include 
                                 monitoring locations, quality assessments of all bathing waters for the current year, and historical data 
                                 back to 1990 (where available), as well as data on the monitoring programme implementation."),
                                 p("The data are available from the ", 
                                   a("European Environment Agency (EEA). ",
                                     href = "https://www.eea.europa.eu/data-and-maps/data/bathing-water-directive-status-of-bathing-water-13"),
                                   "Re-use for commercial or non-commercial purposes is permitted free of charge, provided that the source 
                                   is acknowledged. Copyright holder: Directorate-General for Environment (DG ENV), 
                                   EEA."),
                                 br(),
                                 h3("App features and usage"),
                                 p('This app plots the locations of bathing waters in a user-selected 
                                 European country on an interactive map, and colours them according to their status 
                                 for a selected year. The name of the bathing water is displayed when you click on 
                                 a point (tab "Map"). A short table summarizing the number and percentage 
                                   of bathing waters with each status is displayed in the tab "Summary table".'),
                                 h4("Usage"), 
                                 p("- select a country from the drop-down menu"),
                                 p("- select a year on the slider. Note: some countries have started monitoring later than others, 
                                 so not all years in the range are available; you will be prompted to select an appropriate value"),
                                 p("- explore the map and the summary table.")),
                        
                        ## tab: interactive map of bathing waters in Europe
                        tabPanel("Map", 
                                 br(),
                                 textOutput("selected_year"), 
                                 br(),
                                 leafletOutput("map")),
                        ## tab: table of summary statistics
                        tabPanel("Summary table", 
                                 br(),
                                 textOutput("selected_country_smry"),
                                 textOutput("selected_year_smry"),
                                 br(),
                                 br(),
                                 tableOutput("table")))
            
        )
    )
)

### Define server logic
server <- function(input, output) {
    
    ### Filter data based on input
    ## get minimum available year for the selected country to check if there is data available
    min.year <- reactive({
        bw.data %>% 
            filter(country_name == input$country) %>% 
            pull(year) %>% 
            min()
    })
    
    ## filter data for selected country and year 
    data <- reactive({
        ## check if input year is valid
        validate( 
            need(input$year >= min.year(), 
                 paste("Please select year >=", min.year()))
        )
        
        bw.data %>% 
            ## filter by selected country 
            filter(country_name == input$country) %>%
            ## filter by selected year
            filter(year == input$year)
    })
    
    
    #### Tab: map
    ### Text: selected year
    output$selected_year <- renderText({
        paste("Year:", input$year)
        })

    ### Map
    output$map <- renderLeaflet({
        data() %>%
            leaflet() %>% 
            ## add attribution to both Open Street Map (for the base map) and the EEA and DG ENV for the data 
            addTiles(attribution = paste(
                '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors', 
                data.attribution, 
                sep = " |")) %>%
            
            ## markers = circles
            addCircleMarkers(lat = ~lat, lng = ~lon, 
                             ## colour markers according to status
                             color = ~status.custom.pal(status), 
                             stroke = FALSE, fillOpacity = 1, radius = 8, 
                             ## cluster closely located markers (there are way too many)
                             clusterOptions = markerClusterOptions(), 
                             ## popup label with name displayed on click
                             popup = ~bw_name) %>% 
            
            ## legend - status
            addLegend("bottomleft", 
                      ## colours hex codes for the status
                      colors = custom.cols$colour_hex,
                      labels = custom.cols$status,
                      title = "Status",
                      opacity = 1)
        
        })
    
    #### Tab: summary 
    ### Text: selected country
    output$selected_country_smry <- renderText({
        paste("Country:", input$country)
    })

    ### Text: selected year
    output$selected_year_smry <- renderText({
        paste("Year:", input$year)
    })

    ### Summary table 
    output$table <- function(){ 
        data() %>% 
            ## summarize - number and % of bathing waters with a given status each year
            group_by(status) %>% 
            summarize(bw_nb = n()) %>% 
            mutate(bw_percent = (bw_nb/sum(bw_nb))*100) %>% 

            ## make table prettier
            ## round the percentages to 2 digits and fix column names
            kbl(digits = 2, align = "lcc", 
                col.names = c("Status", "Number bathing waters", "%")) %>%
            kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center") %>% 
            
            ## add a footnote explaining the "not classified" status
            footnote(symbol = c("<small>not classified: quality classification not possible, e.g. not enough samples / new bathing waters / bathing waters with changes.</small>"), 
                     escape = FALSE)
        }    
    

    
    
}
    



### Run the application 
shinyApp(ui = ui, server = server)
