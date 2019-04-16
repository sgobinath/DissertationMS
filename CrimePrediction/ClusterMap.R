# Install the required packages Load the libraries
install.packages("shiny")
if (!require('devtools')) install.packages('devtools')

install.packages('leaflet')
devtools::install_github('rstudio/leaflet')

library(shiny)
library("dplyr")
library(leaflet)

# Load the crime data which is cleaned and amalgamated
dfCrimeFinal <- read.csv("data/final_crime_data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
str(dfCrimeFinal)

# Get the top 25 Crime types based on number of crimes
dfCrimeCluster <- dfCrimeFinal %>% group_by(Crime.Type) %>% summarize(count = n())
dfCrimeCluster <- dfCrimeCluster[order(-dfCrimeCluster$count),]
dfTopCrimes <- dfCrimeCluster[1:25,]

# Filter the crime data based on the top crime types
dfTopCrimeCluster <- dfCrimeFinal[dfCrimeFinal$Crime.Type %in% dfTopCrimes$Crime.Type,]

# Remove the columns that are not required for clustering
dfTopCrimeCluster$Time.Occurred <- NULL
dfTopCrimeCluster$Temperature <- NULL
dfTopCrimeCluster$Month <- NULL
str(dfTopCrimeCluster)

# Add Shiny Interactive App UI
uiCluster <- fluidPage(
    mainPanel(
        
        leafletOutput(outputId = "cmap", width = "1460", height = "760"),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
            top = 60, left = "auto", right = 20, bottom = "auto", width = 345, height = "auto",

            h2("Filter Crime"),

            selectInput("type", "Crime Type", sort(unique(as.character(dfTopCrimeCluster$Crime.Type)))),
            selectInput("year", "Year", 2010:2018, selected = "2018")
        )
    )
)

# Add Shiny Interactive App Server for performing dynamic operations and plots
serverCluster <- function(input, output, session) {

    data <- reactive({
        dfTopCrimeCluster %>%
            filter(Crime.Type == input$type & Year == input$year)
    })

    output$cmap <- renderLeaflet({
        df <- data()

        leaflet() %>% addTiles() %>% setView(-118.243683, 34.052235, zoom = 12) %>%
            addCircles(data = df, weight = 3, lat = ~Latitude, lng = ~Longitude,
                popup = ~paste(df$Crime.Type, ' at ', df$Area.Name, ' on ', Date.Occurred),
                color = '#7B1600', fillOpacity = 0.9)

    })

}

# Start the shiny app
shinyApp(ui = uiCluster, server = serverCluster)

