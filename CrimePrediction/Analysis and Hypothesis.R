library(ggplot2)
library(gridExtra)

# Load the crime data which is cleaned and amalgamated
dfCrimeFinal <- read.csv("data/final_crime_data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)

# Get top 10 crimes and plot them in bar chart
dfCrimeType <- as.data.frame(table(dfCrimeFinal$Crime.Type))
colnames(dfCrimeType) <- c("CrimeType", "CrimeCount")
dfCrimeType <- dfCrimeType[order(-dfCrimeType$CrimeCount),]
dfCrimeType <- dfCrimeType[dfCrimeType$CrimeCount > 50000,]
row.names(dfCrimeType) <- NULL
dfCrimeType

ggplot(data = dfCrimeType, aes(CrimeType, CrimeCount)) + geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = CrimeCount), vjust = -0.3, size = 3.5) + theme_minimal()


# Get the relationshio between robbery and temperature and plot them
dfCrimeRob <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "ROBBERY",]
dfCrimeRob <- merge(aggregate(Crime.Type ~ Date.Occurred, dfCrimeRob, length), aggregate(Temperature ~ Date.Occurred, dfCrimeRob, mean))
colnames(dfCrimeRob) <- c("Date", "CrimeCount", "Temperature")
dfCrimeRob$Temperature <- round(dfCrimeRob$Temperature)
dfCrimeRob$Date <- as.Date(dfCrimeRob$Date, "%m/%d/%Y")

ggplot(dfCrimeRob, aes(x = Date)) + geom_line(aes(y = Temperature, color = "Temperature")) +
    geom_line(aes(y = CrimeCount, color = "Total Crimes"))


# Create a data frame with temperature and number of crimes
dfCrimeCount <- as.data.frame(table(dfCrimeFinal$Celsius))
colnames(dfCrimeCount) <- c("Temperature", "CrimeCount")
dfCrimeCount$Temperature <- as.numeric(as.character(dfCrimeCount$Temperature))
str(dfCrimeCount)

# Get scatter plot to show the relation between Crime count and Temperature
scatter.smooth(x = dfCrimeCount$Temperature, y = dfCrimeCount$CrimeCount, xlab = "Temperature in celcius", ylab = "Number of crimes",
               main = " Temperature versus number of crimes", lpars = list(col = "blue", lwd = 3, lty = 3))


# Create a data frame with hour of crime occured and number of crimes to plot them
dfCrimeTime <- as.data.frame(table(dfCrimeFinal$Time.Occurred))
colnames(dfCrimeTime) <- c("Time", "CrimeCount")
dfCrimeTime$Time <- as.numeric(as.character(dfCrimeTime$Time))

getTimeHour <- function(strHour) {

    lenHour <- nchar(strHour)

    switch(nchar(strHour), {
        return(0)
    }, {
        return(0)
    }, {
        return(as.numeric(substr(as.character(strHour), 1, 1)))
    }, {
        return(as.numeric(substr(as.character(strHour), 1, 2)))
    })

    return(NA)
}
dfCrimeTime$Hour <- sapply(dfCrimeTime$Time, getTimeHour)

ggplot(dfCrimeTime) + geom_line(aes(x = Hour, y = CrimeCount)) + labs(title = "Crime vs Time", x = "Number of Crimes", y = "Hour")


# Create data frames for each of the top 10 crimes and plot them in an single page
dfTheft <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "THEFT",]
dfTheft <- as.data.frame(table(dfTheft$Celsius))
colnames(dfTheft) <- c("Temperature", "Crime")
dfTheft$Temperature <- round(as.numeric(as.character(dfTheft$Temperature)), 1)
pTheft <- ggplot(dfTheft, aes(Temperature, Crime)) + geom_point(color = "#333333") + ggtitle("Theft")

dfBurglary <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "BURGLARY",]
dfBurglary <- as.data.frame(table(dfBurglary$Celsius))
colnames(dfBurglary) <- c("Temperature", "Crime")
dfBurglary$Temperature <- round(as.numeric(as.character(dfBurglary$Temperature)), 1)
pBurglary <- ggplot(dfBurglary, aes(Temperature, Crime)) + geom_point(color = "#E69F00") + ggtitle("Burglary")

dfBattery <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "BATTERY",]
dfBattery <- as.data.frame(table(dfBattery$Celsius))
colnames(dfBattery) <- c("Temperature", "Crime")
dfBattery$Temperature <- round(as.numeric(as.character(dfBattery$Temperature)), 1)
pBattery <- ggplot(dfBattery, aes(Temperature, Crime)) + geom_point(color = "#56B4E9") + ggtitle("Battery")

dfVandalism <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "VANDALISM",]
dfVandalism <- as.data.frame(table(dfVandalism$Celsius))
colnames(dfVandalism) <- c("Temperature", "Crime")
dfVandalism$Temperature <- round(as.numeric(as.character(dfVandalism$Temperature)), 1)
pVandalism <- ggplot(dfVandalism, aes(Temperature, Crime)) + geom_point(color = "#009E73") + ggtitle("Vandalism")

dfVehicle <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "VEHICLE - STOLEN",]
dfVehicle <- as.data.frame(table(dfVehicle$Celsius))
colnames(dfVehicle) <- c("Temperature", "Crime")
dfVehicle$Temperature <- round(as.numeric(as.character(dfVehicle$Temperature)), 1)
pVehicle <- ggplot(dfVehicle, aes(Temperature, Crime)) + geom_point(color = "#F0E442") + ggtitle("Vehicle Stolen")

dfIdentity <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "IDENTITY THEFT",]
dfIdentity <- as.data.frame(table(dfIdentity$Celsius))
colnames(dfIdentity) <- c("Temperature", "Crime")
dfIdentity$Temperature <- round(as.numeric(as.character(dfIdentity$Temperature)), 1)
pIdentity <- ggplot(dfIdentity, aes(Temperature, Crime)) + geom_point(color = "#0072B2") + ggtitle("Identity Theft")

dfIntimate <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "INTIMATE PARTNER",]
dfIntimate <- as.data.frame(table(dfIntimate$Celsius))
colnames(dfIntimate) <- c("Temperature", "Crime")
dfIntimate$Temperature <- round(as.numeric(as.character(dfIntimate$Temperature)), 1)
pIntimate <- ggplot(dfIntimate, aes(Temperature, Crime)) + geom_point(color = "#D55E00") + ggtitle("Intimate Partner")

dfRobbery <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "ROBBERY",]
dfRobbery <- as.data.frame(table(dfRobbery$Celsius))
colnames(dfRobbery) <- c("Temperature", "Crime")
dfRobbery$Temperature <- round(as.numeric(as.character(dfRobbery$Temperature)), 1)
pRobbery <- ggplot(dfRobbery, aes(Temperature, Crime)) + geom_point(color = "#CC79A7") + ggtitle("Robbery")

dfAssault <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "ASSAULT WITH DEADLY WEAPON",]
dfAssault <- as.data.frame(table(dfAssault$Celsius))
colnames(dfAssault) <- c("Temperature", "Crime")
dfAssault$Temperature <- round(as.numeric(as.character(dfAssault$Temperature)), 1)
pAssault <- ggplot(dfAssault, aes(Temperature, Crime)) + geom_point(color = "#ff0000") + ggtitle("Assault with deadly weapon")

dfThreat <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "CRIMINAL THREATS",]
dfThreat <- as.data.frame(table(dfThreat$Celsius))
colnames(dfThreat) <- c("Temperature", "Crime")
dfThreat$Temperature <- round(as.numeric(as.character(dfThreat$Temperature)), 1)
pThreat <- ggplot(dfThreat, aes(Temperature, Crime)) + geom_point(color = "#9DE0D0") + ggtitle("Criminal Threats")

grid.arrange(pTheft, pBurglary, pBattery, pVandalism, pVehicle, pIdentity, pIntimate, pRobbery, pAssault, pThreat, ncol = 4)


# Perform the correlation test for identifying linear relationship between temperature and Number of crimes
cor.test(dfCrimeCount$Temperature, dfCrimeCount$CrimeCount, method = "pearson")

# Apply polynomial regression
polyMod <- lm(dfCrimeCount$CrimeCount ~ dfCrimeCount$Temperature + I(dfCrimeCount$Temperature ^ 2))
summary(polyMod)


# Create a data frame for crime type burglary
dfBurglary <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "BURGLARY",]

# Remove the columns that are not required for this analysis
dfBurglary$Date.Occurred <- NULL
dfBurglary$Time.Occurred <- NULL
dfBurglary$Crime.Type <- NULL
dfBurglary$Longitude <- NULL
dfBurglary$Latitude <- NULL
dfBurglary$Temperature <- NULL
dfBurglary$Celsius <- NULL

# Create Month Year column
dfBurglary$MonthYear <- paste0(dfBurglary$Month, dfBurglary$Year)
dfBurglary$Month <- NULL
dfBurglary$Year <- NULL

# Create data frame for the required areas
dfCentral <- dfBurglary[dfBurglary$Area.Name == "Central",]
dfPacific <- dfBurglary[dfBurglary$Area.Name == "Pacific",]

# Consolidate the data frames to get the count for each month and year
dfCentral <- as.data.frame(table(dfCentral$MonthYear))
colnames(dfCentral) <- c("MonthYear", "Central")

dfPacific <- as.data.frame(table(dfPacific$MonthYear))
colnames(dfPacific) <- c("MonthYear2", "Pacific")

# Concatenate the data frames
dfBurglary <- cbind(dfCentral, dfPacific)
dfBurglary$MonthYear2 <- NULL

str(dfBurglary)
head(dfBurglary)

# Perform the t test to compare the mean of two paired crime count values
t.test(dfBurglary$Pacific, dfBurglary$Central, alternative = "two.sided", var.equal = FALSE, paired = TRUE)

