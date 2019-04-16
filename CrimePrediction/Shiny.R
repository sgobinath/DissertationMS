# Install the required packages Load the libraries
install.packages("shiny")
if (!require('devtools')) install.packages('devtools')

install.packages('leaflet')
devtools::install_github('rstudio/leaflet')

library(shiny)
library(ggplot2)
library("dplyr")
library(gridExtra)

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
# The value -0.2 shows negative weak relationship
cor(dfCrimeCount$Temperature, dfCrimeCount$CrimeCount)

# Fit the polynomial regression model
polyMod <- lm(dfCrimeCount$CrimeCount ~ dfCrimeCount$Temperature + I(dfCrimeCount$Temperature ^ 2) + I(dfCrimeCount$Temperature ^ 3))
summary(polyMod)

# Add a separate column for Celcius value by converting the temperate in fahrenheit
dfCrimeFinal$Celcius <- (dfCrimeFinal$Temperature - 32) * 5/9

# Add Shiny Interactive App UI
uiReg <- fluidPage(
    #  App title
    titlePanel("Los Angeles city crime versus temperature"),

    # Sidebar layout with input and output
    sidebarLayout(
        sidebarPanel(
            # Input: Slider for the Year range
            sliderInput(
                inputId = "year",
                label = "Year",
                min = 2010,
                max = 2018,
                value = c(2017, 2018)),
            # Input: Dropdown for the Crime Type
            selectInput(
                inputId = "type",
                label = h4("Crime Type"),
                choices = c("ALL", sort(unique(dfCrimeFinal$Crime.Type))),
                selected = "THEFT"),
            # Input: Check boxes for the Month
            checkboxGroupInput(
                inputId = "month",
                label = h4("Month"),
                choices = unique(dfCrimeFinal$Month),
                selected = "Jan")
        ),

        # Main panel for displaying output
        mainPanel(
            plotOutput(outputId = "crimeplot")
        )
    )
)

# Add Shiny Interactive App Server for performing dynamic operations and plots
serverReg <- function(input, output, session) {

    output$crimeplot <- renderPlot({

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
    
}

# Start the shiny app
shinyApp(ui = uiReg, server = serverReg)
