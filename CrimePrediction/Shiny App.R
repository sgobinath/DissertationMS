# Install the required packages Load the libraries
install.packages("shinydashboard")
install.packages("shiny")
if (!require('devtools')) install.packages('devtools')

library(shiny)
library(shinydashboard)
library("dplyr")
library(ggplot2)
library(gridExtra)
library(leaflet)
library(RColorBrewer)


# Load the crime data which is cleaned and amalgamated
dfCrimeFinal <- read.csv("data/final_crime_data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
str(dfCrimeFinal)

dfCrimeFinal$Temperature <- NULL
dfCrimeFinal$Time.Occurred <- NULL

# Prepare data frame for Regression plots
dfReg <- dfCrimeFinal[!is.na(dfCrimeFinal$Celsius),]
dfReg$Date.Occurred <- NULL
dfReg$Area.Name <- NULL
dfReg$Longitude <- NULL
dfReg$Latitude <- NULL
str(dfReg)

# Prepare data frame for Cluster plots
dfMap <- dfCrimeFinal[dfCrimeFinal$Longitude != 0 & dfCrimeFinal$Latitude != 0,]
dfMap$Celsius <- NULL
str(dfMap)

# Get the colour palette Set1 and add more colors upto 20 for cluster colours
clr = brewer.pal(4, "Set1")
clr = colorRampPalette(clr)(30)



# Add Shiny Interactive App UI
uiRC <- dashboardPage(
    # App tile on the dashboard header
    dashboardHeader(title = "Los Angeles Crime Analysis", titleWidth = 280),

    # Set the dashboard sidebar with the tabs and input controls
    dashboardSidebar(
        width = 280,
        sidebarMenu(
            menuItem("Regression", tabName = "Regression", icon = icon("line-chart")),
            menuItem("Clustering Point", tabName = "ClusterCircle", icon = icon("globe")),
            menuItem("Clustering Marker", tabName = "ClusterMarker", icon = icon("map-marker")),
            h4("Input controls"),
            # Input: Slider for the Year range
            sliderInput(inputId = "year", label = "Year", min = 2010, max = 2018, value = c(2017, 2018)),
            # Input: Dropdown for the Crime Type
            selectInput(inputId = "type", label = "Crime Type",
                choices = c("ALL", sort(unique(dfCrimeFinal$Crime.Type))), selected = "THEFT"),
            # Input: Check boxes for the Month
            checkboxGroupInput(inputId = "month", label = "Month", choices = unique(dfCrimeFinal$Month), selected = c("Jan", "Jun"))
        )
    ),

    # Set the dashboard body to display the output
    dashboardBody(
        tabItems(
            tabItem(tabName = "Regression",
                fluidRow(
                    # Area for regression output
                    box(width = NULL,
                        plotOutput(outputId = "regressionplot", height = 600)
                    )
                )
            ),
            tabItem(tabName = "ClusterCircle",
                fluidRow(
                    # Area for cluster points output
                    box(width = NULL,
                        leafletOutput(outputId = "clustercircle", width = "1230", height = "645")
                    ),
                    absolutePanel(top = 90, right = 10,
                        sliderInput(inputId = "cluster", label = "Clusters", min = 1, max = 30, value = 10),
                        sliderInput(inputId = "threshold", label = "Threshold %", min = 1, max = 100, value = 10)
                    )
                )
            ),
            tabItem(tabName = "ClusterMarker",
                fluidRow(
                    # Area for cluster area output
                    box(width = NULL,
                        leafletOutput(outputId = "clustermarker", width = "1230", height = "645")
                    )
                )
            )
        )
    )
)

# Add Shiny Interactive App Server for performing dynamic operations and plots
serverRC <- function(input, output, session) {

    output$regressionplot <- renderPlot({

        # Filter the data frame based on the input selected
        dfCrimeTmp <- dfReg[dfReg$Year %in% input$year[1]:input$year[2],]
        if (input$type != "ALL") {
            dfCrimeTmp <- dfCrimeTmp[dfCrimeTmp$Crime.Type == input$type,]
        }
        dfCrimeTmp <- dfCrimeTmp[dfCrimeTmp$Month %in% input$month,]

        dfCrimeTmp <- dfCrimeTmp %>% group_by(Celsius, Month) %>% summarize(count = n())
        
        # Frame the dynamic title for the plots
        st <- paste0(input$type, ' crimes that happened between ', input$year[1], ' and ', input$year[2], ' during the months ', paste(input$month, sep = '', collapse = ','))

        # Render the plot with Temperature in Celsius in the X axis and Number of Crimes in the y axis
        p2 <- ggplot(dfCrimeTmp) + geom_line(aes(x = Celsius, y = count, group = Month, colour = Month)) +
            labs(title = "Crimes by month (Temperature vs Crime)", subtitle = st, x = "Temperature in Celsius",
            y = "Number of Crimes", colour = "Month") + theme_bw()

        # Add the inverse of above map
        p1 <- p2 + coord_flip()

        grid.arrange(p1, p2, ncol = 1)
        
    }, height = 600)

    # Prepare the data for clustering plot
    data <- reactive({
        dfMap %>% filter(Crime.Type == input$type & Year %in% input$year[1]:input$year[2] & Month %in% input$month)
    })

    # Display the location of each crime event with the clustering applied by K-Means algorithm
    output$clustercircle <- renderLeaflet({
        df <- data()
        # Apply K-Means clustering for the Longitude Latitude data points (areas)
        clusterCrime <- kmeans(df[, 4:5], input$cluster)
        # Add the cluster value in a separate column of data frame
        df$cluster <- as.factor(clusterCrime$cluster)

        # Add the color palette
        pal <- colorFactor(palette = clr, domain = df$cluster)

        # Initialize the leaflet map
        crimeMap <- leaflet() %>% addTiles() %>% setView(-118.243683, 34.052235, zoom = 10)

        # Caculate the threshold value
        csize <- numeric()
        for (group in levels(df$cluster)) {
            dfOutline <- df[df$cluster == group,]
            csize <- c(csize, nrow(dfOutline))
        }
        clevel <- as.integer(100 / input$cluster)
        cout <- as.integer(input$threshold / clevel)
        if (cout == 0) {
            cobs = 0
        } else {
            cobs <- sort(csize)[cout]
        }

        # Add the circle points and overlay polygon for each cluster above threshold
        for (group in levels(df$cluster)) {
            dfCluster <- df[df$cluster == group,]
            if (nrow(dfCluster) > cobs) {
                dfOutline <- dfCluster[chull(dfCluster$Longitude, dfCluster$Latitude),]

                crimeMap = addPolygons(crimeMap, data = dfOutline, lng = ~Longitude, lat = ~Latitude, fill = F,
                        weight = 2, color = ~pal(dfOutline$cluster), options = pathOptions(clickable = FALSE)) %>%
                        addCircles(data = dfCluster, weight = 3, lat = ~Latitude, lng = ~Longitude,
                        popup = ~paste(df$Crime.Type, ' at ', df$Area.Name, ' on ', Date.Occurred),
                        color = ~pal(cluster), fillOpacity = 0.9)
            }
        }

        crimeMap
    })

    # Display the area of crime events in cluster with number of events in each area
    output$clustermarker <- renderLeaflet({
        df <- data()

        leaflet() %>% addTiles() %>% setView(-118.243683, 34.052235, zoom = 10) %>%
            addMarkers(data = df, lat = ~Latitude, lng = ~Longitude, clusterOptions = markerClusterOptions())

    })
    
}

# Start the shiny app
shinyApp(ui = uiRC, server = serverRC)

        