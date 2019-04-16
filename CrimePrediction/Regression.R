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

# Load the crime data which is cleaned and amalgamated
dfCrimeFinal <- read.csv("data/final_crime_data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
str(dfCrimeFinal)


# Create a dataframe with crime temperature and crime count
dfCrimeCount <- as.data.frame(table(dfCrimeFinal$Temperature))
colnames(dfCrimeCount) <- c("Temperature", "CrimeCount")

dfCrimeCount$Temperature <- as.numeric(as.character(dfCrimeCount$Temperature))
str(dfCrimeCount)

# Apply scatter plot
scatter.smooth(x = dfCrimeCount$Temperature, y = dfCrimeCount$CrimeCount, main = "Crime count ~ Temperature")
dfCrimeCount

# Get the correlation for linear dependence between the variables
# The p-value of the test is 0.05291 Temperature and Crime events are correlated with a correlation coefficient of -0.22
cor.test(dfCrimeCount$Temperature, dfCrimeCount$CrimeCount, method = "pearson")


# Fit the polynomial regression model
polyMod <- lm(dfCrimeCount$CrimeCount ~ dfCrimeCount$Temperature + I(dfCrimeCount$Temperature ^ 2) + I(dfCrimeCount$Temperature ^ 3))
summary(polyMod)


# Add a separate column for Celcius value by converting the temperate in fahrenheit
dfCrimeFinal$Celcius <- (dfCrimeFinal$Temperature - 32) * 5/9


# Add Shiny Interactive App UI
uiReg <- dashboardPage(
    #  App title
    dashboardHeader(title = "Los Angeles Crime Analysis", titleWidth = 280),

    dashboardSidebar(
        width = 280,
        sidebarMenu(
            menuItem("Regression", tabName = "Regression", icon = icon("line-chart")),
            menuItem("Clustering Point", tabName = "ClusterCircle", icon = icon("globe")),
            menuItem("Clustering Marker", tabName = "ClusterMarker", icon = icon("map-marker")),
            h4("Input controls"),
            #box(title = "Input controls", status = "warning"),
            # Input: Slider for the Year range
            sliderInput(inputId = "year", label = "Year", min = 2010, max = 2018, value = c(2017, 2018)),
            # Input: Dropdown for the Crime Type
            selectInput(inputId = "type", label = "Crime Type",
                choices = c("ALL", sort(unique(dfCrimeFinal$Crime.Type))), selected = "THEFT"),
            # Input: Check boxes for the Month
            checkboxGroupInput(inputId = "month", label = "Month", choices = unique(dfCrimeFinal$Month), selected = c("Jan", "Jun"))
        )
    ),

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
                    # Area for clustering output
                    box(width = NULL,
                        leafletOutput(outputId = "clustercircle", width = "1230", height = "645")
                    )
                )
            ),
            tabItem(tabName = "ClusterMarker",
                fluidRow(
                    # Area for clustering output
                    box(width = NULL,
                        leafletOutput(outputId = "clustermarker", width = "1230", height = "645")
                    )
                )
            )
        )
    )
)

# Add Shiny Interactive App Server for performing dynamic operations and plots
serverReg <- function(input, output, session) {

    output$regressionplot <- renderPlot({

        # Filter the data frame based on the input selected
        dfCrimeTmp <- dfCrimeFinal[dfCrimeFinal$Year %in% input$year[1]:input$year[2],]
        if (input$type != "ALL") {
            dfCrimeTmp <- dfCrimeTmp[dfCrimeTmp$Crime.Type == input$type,]
        }
        dfCrimeTmp <- dfCrimeTmp[dfCrimeTmp$Month %in% input$month,]

        dfCrimePlot <- dfCrimeTmp %>% group_by(Celcius, Month) %>% summarize(count = n())
        dfCrimePlot$Celcius <- as.numeric(as.character(dfCrimePlot$Celcius))

        # Frame the dynamic title for the plots
        st <- paste0(input$type, ' crimes that happened between ', input$year[1], ' and ', input$year[2], ' during the months ', paste(input$month, sep = '', collapse = ','))

        # Render the plot with Number of Crimes in the X axis and Temperature in Celcius in the y axis
        p1 <- ggplot(dfCrimePlot) + geom_line(aes(x = count, y = Celcius, group = Month, colour = Month)) +
            labs(title = "Crimes by month (Crime vs Temperature)", subtitle = st, x = "Number of Crimes",
            y = "Temperature in Celcius", colour = "Month") + theme_bw()

        # Render the plot with Temperature in Celcius in the X axis and Number of Crimes in the y axis
        p2 <- ggplot(dfCrimePlot) + geom_line(aes(x = Celcius, y = count, group = Month, colour = Month)) +
            labs(title = "Crimes by month (Temperature vs Crime)", subtitle = st, x = "Temperature in Celcius",
            y = "Number of Crimes", colour = "Month") + theme_bw()


        grid.arrange(p1, p2, ncol = 1)
        
    }, height = 600)

    data <- reactive({
        dfCrimeFinal %>%
            filter(Crime.Type == input$type & Year %in% input$year[1]:input$year[2] & Month %in% input$month)
    })

    output$clustercircle <- renderLeaflet({
        df <- data()

        leaflet() %>% addTiles() %>% setView(-118.243683, 34.052235, zoom = 11) %>%
            addCircles(data = df, weight = 3, lat = ~Latitude, lng = ~Longitude,
                popup = ~paste(df$Crime.Type, ' at ', df$Area.Name, ' on ', Date.Occurred),
                color = '#7B1600', fillOpacity = 0.9)

    })

    output$clustermarker <- renderLeaflet({
        df <- data()

        leaflet() %>% addTiles() %>% setView(-118.243683, 34.052235, zoom = 11) %>%
            addMarkers(data = df, lat = ~Latitude, lng = ~Longitude, clusterOptions = markerClusterOptions())

    })
    
}

# Start the shiny app
shinyApp(ui = uiReg, server = serverReg)
