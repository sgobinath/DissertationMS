library(stringr)
library(ggplot2)

# Get the current working directory of the R process
getwd()

# Load the crime data
dfCrime <- read.csv("data/Crime_Data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)

# Get the structure of the data frame postcode_data
str(dfCrime)
# Show the first 10 rows of the dataframe containing all of the NIPostcode data.
head(dfCrime)
nrow(dfCrime)

# Remove the unwanted columns
dfCrime$DR.Number <- NULL
dfCrime$Date.Reported <- NULL
dfCrime$Area.ID <- NULL
dfCrime$Reporting.District <- NULL
dfCrime$Crime.Code <- NULL
dfCrime$MO.Codes <- NULL
dfCrime$Victim.Descent <- NULL
dfCrime$Premise.Code <- NULL
dfCrime$Weapon.Used.Code <- NULL
dfCrime$Weapon.Description <- NULL
dfCrime$Status.Code <- NULL
dfCrime$Status.Description <- NULL
dfCrime$Crime.Code.1 <- NULL
dfCrime$Crime.Code.2 <- NULL
dfCrime$Crime.Code.3 <- NULL
dfCrime$Crime.Code.4 <- NULL
dfCrime$Address <- NULL
dfCrime$Cross.Street <- NULL
dfCrime$Premise.Description <- NULL
dfCrime$Victim.Age <- NULL
dfCrime$Victim.Sex <- NULL

# Fix the Latitude and Longitude column format
lat <- str_split_fixed(dfCrime$Location, ", ", 2)[, 1]
dfCrime$Latitude <- gsub("\\(", "", lat)

lon <- str_split_fixed(dfCrime$Location, ", ", 2)[, 2]
dfCrime$Longitude <- gsub("\\)", "", lon)

dfCrime$Location <- NULL

tail(dfCrime)
unique(dfCrime$Crime.Code.Description)

colnames(dfCrime)[4] <- "Crime.Type"

# Get the count of missing values
sapply(dfCrime, function(x) sum(is.na(x)))

# Write the clean crime data to a separate file in the working directory
write.csv(dfCrime, file = "data/clean_crime_data.csv", quote = FALSE, na = "", row.names = FALSE)

# Load the weather data
dfWeather <- read.csv("data/Weather_Data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)

# Get the structure of the data frame for weather data
str(dfWeather)

# Remove the unwanted columns
dfWeather$Site <- NULL
dfWeather$Dewpoint <- NULL
dfWeather$RH <- NULL
dfWeather$WindDir <- NULL
dfWeather$CldFrac <- NULL
dfWeather$MSLP <- NULL
dfWeather$Weather <- NULL
dfWeather$Source <- NULL

# Verify the missing values
sapply(dfWeather, function(x) sum(is.na(x)))

head(dfWeather, 20)
dfWeather$Date <- str_split_fixed(dfWeather$Date, " ", 2)[, 1]

colnames(dfWeather)[5] <- "Precipitation"

dfWeather$Precipitation[is.na(dfWeather$Precipitation)] <- 0

# Remove the data for the years from 1998 to 2009
for (year in 1998:2009) {
    dfWeather <- dfWeather[!grepl( paste0("/", year), dfWeather$Date),]
}

# Write the clean crime data to a separate file in the working directory
write.csv(dfWeather, file = "data/clean_weather_data.csv", quote = FALSE, na = "", row.names = FALSE)

# Fix the format of date values in the weather data for amalgamate the datasets
dfWeather$DateHour <- paste0(as.character(as.Date(dfWeather$Date, "%m/%d/%Y"), "%m/%d/%Y"), dfWeather$Hour)

head(dfCrime, 20)

# Fix the format of hour values in the weather data for amalgamate the datasets
getHour <- function(strHour) {

    lenHour <- nchar(strHour)

    switch(nchar(strHour),
        {
            return("0")
        },
        {
            return(ifelse(strHour < 30, "0", "1"))
        },
        {
            strMin <- substr(strHour, 2, 3)
            strHr <- substr(strHour, 1, 1)
            return(ifelse(strMin < 30, strHr, as.character(as.numeric(strHr)+1)))
        },
        {
            strMin <- substr(strHour, 3, 4)
            strHr <- substr(strHour, 1, 2)
            return(ifelse(strMin < 30, strHr, as.character(as.numeric(strHr) + 1)))
        }
    )

    return(nchar(strHour))
}
dfCrime$Hour <- sapply(dfCrime$Time.Occurred, getHour)

sapply(head(dfCrime$Time.Occurred), getHour)

incDate <- function(x) {
    pval <- unlist(strsplit(x, "~"))
    strHr <- trimws(pval[1])
    strDt <- trimws(pval[2])

    if (strHr != "24") { return(strDt) }

    return(as.character(as.Date(strDt, "%m/%d/%Y") + 1, "%m/%d/%Y"))
}
dfCrime$Date.Occurred <- sapply(paste0(dfCrime$Hour, "~", dfCrime$DateOccur), incDate)

dfCrime$Hour[dfCrime$Hour == "24"] <- "0"

head(dfCrime)

dfCrime$DateHour <- paste0(dfCrime$Date.Occurred, dfCrime$Hour)

write.csv(dfCrime, file = "data/crime_data2.csv", quote = FALSE, na = "", row.names = FALSE)

write.csv(dfWeather, file = "data/weather_data2.csv", quote = FALSE, na = "", row.names = FALSE)

# Amalgamate the datasets to create a new data frame
dfCrimeFinal <- merge(x = dfCrime, y = dfWeather, by = "DateHour", all.x = TRUE)

head(dfCrimeFinal)

dfCrimeFinal$Crime.Type <- gsub(",", " ", dfCrimeFinal$Crime.Type)
dfCrimeFinal$Area.Name <- gsub(",", " ", dfCrimeFinal$Area.Name)

dfCrimeFinal$DateHour <- NULL
dfCrimeFinal$Hour.x <- NULL
dfCrimeFinal$Hour.y <- NULL
dfCrimeFinal$Date <- NULL
dfCrimeFinal$Windspeed <- NULL
dfCrimeFinal$Precipitation <- NULL

# Write the amalgamated final crime dataset to a separate file in the working directory
write.csv(dfCrimeFinal, file = "data/final_crime_data.csv", quote = FALSE, na = "", row.names = FALSE)


dfCrimeFinal <- read.csv("data/final_crime_data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
head(dfCrimeFinal)

# Create a dataframe with crime temperature and crime count
dfCrimeCount <- as.data.frame(table(dfCrimeFinal$Temperature))
colnames(dfCrimeCount) <- c("Temperature", "CrimeCount")

dfCrimeCount$Temperature <- as.numeric(as.character(dfCrimeCount$Temperature))
str(dfCrimeCount)
dfCrimeCount

# Consolidate the crime types
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "ASSAULT WITH DEADLY WEAPON  AGGRAVATED ASSAULT"] <- "ASSAULT WITH DEADLY WEAPON"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER"] <- "ASSAULT WITH DEADLY WEAPON"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "ATTEMPTED ROBBERY"] <- "ROBBERY"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BATTERY - SIMPLE ASSAULT"] <- "BATTERY"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BATTERY ON A FIREFIGHTER"] <- "BATTERY"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BATTERY POLICE(SIMPLE)"] <- "BATTERY"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BATTERY WITH SEXUAL CONTACT"] <- "BATTERY"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BIKE - ATTEMPTED STOLEN"] <- "BIKE - STOLEN"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BUNCO  ATTEMPT"] <- "BUNCO"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BUNCO  GRAND THEFT"] <- "BUNCO"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BUNCO  PETTY THEFT"] <- "BUNCO"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BURGLARY  ATTEMPTED"] <- "BURGLARY"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BURGLARY FROM VEHICLE"] <- "BURGLARY"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BURGLARY FROM VEHICLE  ATTEMPTED"] <- "BURGLARY"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "CHILD ABANDONMENT"] <- "CHILD ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "CHILD ABUSE (PHYSICAL) - AGGRAVATED ASSAULT"] <- "CHILD ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT"] <- "CHILD ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "CHILD ANNOYING (17YRS & UNDER)"] <- "CHILD ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "CHILD NEGLECT (SEE 300 W.I.C.)"] <- "CHILD ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "CHILD PORNOGRAPHY"] <- "CHILD ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "CHILD STEALING"] <- "CHILD ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)"] <- "CHILD ABUSE"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "CRIMINAL THREATS - NO WEAPON DISPLAYED"] <- "CRIMINAL THREATS"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "CREDIT CARDS  FRAUD USE ($950 & UNDER"] <- "CREDIT CARDS FRAUD"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "CREDIT CARDS  FRAUD USE ($950.01 & OVER)"] <- "CREDIT CARDS FRAUD"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "DEFRAUDING INNKEEPER/THEFT OF SERVICES  $400 & UNDER"] <- "DEFRAUDING INNKEEPER / THEFT OF SERVICES"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "DEFRAUDING INNKEEPER/THEFT OF SERVICES  OVER $400"] <- "DEFRAUDING INNKEEPER / THEFT OF SERVICES"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "DISHONEST EMPLOYEE ATTEMPTED THEFT"] <- "DISHONEST EMPLOYEE THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "DISHONEST EMPLOYEE - GRAND THEFT"] <- "DISHONEST EMPLOYEE THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "DISHONEST EMPLOYEE - PETTY THEFT"] <- "DISHONEST EMPLOYEE THEFT"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "DOCUMENT WORTHLESS ($200 & UNDER)"] <- "DOCUMENT WORTHLESS"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "DOCUMENT WORTHLESS ($200.01 & OVER)"] <- "DOCUMENT WORTHLESS"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "DRUNK ROLL - ATTEMPT"] <- "DRUNK ROLL"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "EMBEZZLEMENT GRAND THEFT($950.01 & OVER)"] <- "EMBEZZLEMENT THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "EMBEZZLEMENT PETTY THEFT($950 & UNDER)"] <- "EMBEZZLEMENT THEFT"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "GRAND THEFT / AUTO REPAIR"] <- "GRAND THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "GRAND THEFT / INSURANCE FRAUD"] <- "GRAND THEFT"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "HUMAN TRAFFICKING - COMMERCIAL SEX ACTS"] <- "HUMAN TRAFFICKING"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE"] <- "HUMAN TRAFFICKING"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "INTIMATE PARTNER - AGGRAVATED ASSAULT"] <- "INTIMATE PARTNER"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "INTIMATE PARTNER - SIMPLE ASSAULT"] <- "INTIMATE PARTNER"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "KIDNAPPING - GRAND ATTEMPT"] <- "KIDNAPPING"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "LYNCHING - ATTEMPTED"] <- "LYNCHING"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "PICKPOCKET  ATTEMPT"] <- "PICKPOCKET"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "PURSE SNATCHING - ATTEMPT"] <- "PURSE SNATCHING"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "SHOPLIFTING - ATTEMPT"] <- "SHOPLIFTING"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "SHOPLIFTING - PETTY THEFT ($950 & UNDER)"] <- "SHOPLIFTING"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "SHOPLIFTING-GRAND THEFT ($950.01 & OVER)"] <- "SHOPLIFTING"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "SHOTS FIRED AT INHABITED DWELLING"] <- "SHOTS FIRED"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "SHOTS FIRED AT MOVING VEHICLE  TRAIN OR AIRCRAFT"] <- "SHOTS FIRED"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "BEASTIALITY  CRIME AGAINST NATURE SEXUAL ASSLT WITH ANIM"] <- "SEXUAL ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "INCEST (SEXUAL ACTS BETWEEN BLOOD RELATIVES)"] <- "SEXUAL ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "RAPE  ATTEMPTED"] <- "SEXUAL ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "RAPE  FORCIBLE"] <- "SEXUAL ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "SEX UNLAWFUL(INC MUTUAL CONSENT  PENETRATION W/ FRGN OBJ"] <- "SEXUAL ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "SEXUAL PENETRATION W/FOREIGN OBJECT"] <- "SEXUAL ABUSE"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "SODOMY/SEXUAL CONTACT B/W PENIS OF ONE PERS TO ANUS OTH"] <- "SEXUAL ABUSE"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT  COIN MACHINE - ATTEMPT"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT  COIN MACHINE - GRAND ($950.01 & OVER)"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT  COIN MACHINE - PETTY ($950 & UNDER)"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT  PERSON"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT FROM MOTOR VEHICLE - ATTEMPT"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT FROM MOTOR VEHICLE - GRAND ($400 AND OVER)"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT FROM PERSON - ATTEMPT"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT PLAIN - ATTEMPT"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT PLAIN - PETTY ($950 & UNDER)"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT-GRAND ($950.01 & OVER)EXCPT GUNS FOWL LIVESTK PROD"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "PETTY THEFT - AUTO REPAIR"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "GRAND THEFT"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "GRAND THEFT"] <- "THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "GRAND THEFT"] <- "THEFT"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "EMBEZZLEMENT  GRAND THEFT ($950.01 & OVER)"] <- "EMBEZZLEMENT THEFT"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "EMBEZZLEMENT  PETTY THEFT ($950 & UNDER)"] <- "EMBEZZLEMENT THEFT"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "TILL TAP - ATTEMPT"] <- "TILL TAP"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "TILL TAP - GRAND THEFT ($950.01 & OVER)"] <- "TILL TAP"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "TILL TAP - PETTY ($950 & UNDER)"] <- "TILL TAP"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "VANDALISM - FELONY ($400 & OVER  ALL CHURCH VANDALISMS)"] <- "VANDALISM"
dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "VANDALISM - MISDEAMEANOR ($399 OR UNDER)"] <- "VANDALISM"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "VEHICLE - ATTEMPT STOLEN"] <- "VEHICLE - STOLEN"

dfCrimeFinal$Crime.Type[dfCrimeFinal$Crime.Type == "THEFT OF IDENTITY"] <- "IDENTITY THEFT"

unique(dfCrimeFinal$Crime.Type)
str(dfCrimeFinal)

dfCrimeType <- as.data.frame(table(dfCrimeFinal$Crime.Type))
colnames(dfCrimeType) <- c("CrimeType", "CrimeCount")
dfCrimeType <- dfCrimeType[order(-dfCrimeType$CrimeCount),]
dfCrimeType <- dfCrimeType[dfCrimeType$CrimeCount > 50000,]
row.names(dfCrimeType) <- NULL
dfCrimeType

ggplot(data = dfCrimeType, aes(CrimeType, CrimeCount)) + geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = CrimeCount), vjust = -0.3, size = 3.5) + theme_minimal()


str(dfCrimeFinal)

#Get abbreviated month name in separate month column
dfCrimeFinal$Month <- month.abb[as.numeric(str_split_fixed(dfCrimeFinal$Date.Occurred, "/", 2)[, 1])]
tail(dfCrimeFinal)


tail(dfCrimeFinal)
str_split_fixed(head(dfCrimeFinal$Date.Occurred), "/", 3)[,3]

#Get Year value in separate month column
dfCrimeFinal$Year <- as.numeric(str_split_fixed(dfCrimeFinal$Date.Occurred, "/", 3)[, 3])


remove.packages("ggmap")
install.packages("ggmap")
library(ggmap)
install_github("dkahle/ggmap")

ggmap(get_googlemap())
register_google(key = "")
geocode("waco texas")
map <- get_map(location = c(lon = mean(dfCrimeTheft$Longitude), lat = mean(dfCrimeTheft$Latitude)), zoom = 4)
mapPoints <- ggmap(map) + geom_point(aes(x = Longitude, y = Latitude), data = dfCrimeFinal, alpha = .5)

sessionInfo()

dfCrimeTheft <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "THEFT",]
head(dfCrimeTheft)
qmplot(Longitude, Latitude, data = dfCrimeTheft, colour = I('red'), size = I(3), darken = .3)

install.packages("plotGoogleMaps")
library("plotGoogleMaps")

coords <- as.data.frame(cbind(lon = dfCrimeTheft$Longitude, lat = dfCrimeTheft$Latitude))

# make it a spatial object by defining its coordinates in a reference system
coordinates(coords) <- ~lon + lat

# you also need a reference system, the following should be a fine default
proj4string(coords) <- CRS("+init=epsg:4326")

# then just plot
mp <- plotGoogleMaps(coords, mapTypeId = 'ROADMAP')

# Apply scatter plot
scatter.smooth(x = dfCrimeCount$Temperature, y = dfCrimeCount$CrimeCount, col = 'navy', main = "Crime count ~ Temperature")
dfCrimeCount

# Get the correlation for linear dependence between the variables
# The value -0.2 shows negative weak relationship
cor(dfCrimeCount$Temperature, dfCrimeCount$CrimeCount)

# Fit the polynomial regression model
polyMod <- lm(dfCrimeCount$CrimeCount ~ dfCrimeCount$Temperature + I(dfCrimeCount$Temperature ^ 2) + I(dfCrimeCount$Temperature ^ 3))
summary(polyMod)


dt = data.frame(date = 1:10, value = c(10, 11, 15, 13, 16, 17, 18, 19, 16, 22))

# plot everything in one graph
ggplot(dfCrimeCount, aes(Temperature, CrimeCount)) + geom_point() +
    stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw = T), se = F, level = 0.95, col = "blue")



dfCrimeJan <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "Jan",]$Temperature))
colnames(dfCrimeJan) <- c("Temperature", "CrimeCount")
dfCrimeJan$Temperature <- as.numeric(as.character(dfCrimeJan$Temperature))
dfCrimeJan$Month <- "Jan"
str(dfCrimeJan)

dfCrimeFeb <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "Feb",]$Temperature))
colnames(dfCrimeFeb) <- c("Temperature", "CrimeCount")
dfCrimeFeb$Temperature <- as.numeric(as.character(dfCrimeFeb$Temperature))
dfCrimeFeb$Month <- "Feb"

dfCrimeMar <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "Mar",]$Temperature))
colnames(dfCrimeMar) <- c("Temperature", "CrimeCount")
dfCrimeMar$Temperature <- as.numeric(as.character(dfCrimeMar$Temperature))
dfCrimeMar$Month <- "Mar"

dfCrimeApr <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "Apr",]$Temperature))
colnames(dfCrimeApr) <- c("Temperature", "CrimeCount")
dfCrimeApr$Temperature <- as.numeric(as.character(dfCrimeApr$Temperature))
dfCrimeApr$Month <- "Apr"

dfCrimeMay <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "May",]$Temperature))
colnames(dfCrimeMay) <- c("Temperature", "CrimeCount")
dfCrimeMay$Temperature <- as.numeric(as.character(dfCrimeMay$Temperature))
dfCrimeMay$Month <- "May"

dfCrimeJun <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "Jun",]$Temperature))
colnames(dfCrimeJun) <- c("Temperature", "CrimeCount")
dfCrimeJun$Temperature <- as.numeric(as.character(dfCrimeJun$Temperature))
dfCrimeJun$Month <- "Jun"

dfCrimeJul <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "Jul",]$Temperature))
colnames(dfCrimeJul) <- c("Temperature", "CrimeCount")
dfCrimeJul$Temperature <- as.numeric(as.character(dfCrimeJul$Temperature))
dfCrimeJul$Month <- "Jul"

dfCrimeAug <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "Aug",]$Temperature))
colnames(dfCrimeAug) <- c("Temperature", "CrimeCount")
dfCrimeAug$Temperature <- as.numeric(as.character(dfCrimeAug$Temperature))
dfCrimeAug$Month <- "Aug"

dfCrimeSep <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "Sep",]$Temperature))
colnames(dfCrimeSep) <- c("Temperature", "CrimeCount")
dfCrimeSep$Temperature <- as.numeric(as.character(dfCrimeSep$Temperature))
dfCrimeSep$Month <- "Sep"

dfCrimeOct <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "Oct",]$Temperature))
colnames(dfCrimeOct) <- c("Temperature", "CrimeCount")
dfCrimeOct$Temperature <- as.numeric(as.character(dfCrimeOct$Temperature))
dfCrimeOct$Month <- "Oct"

dfCrimeNov <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "Nov",]$Temperature))
colnames(dfCrimeNov) <- c("Temperature", "CrimeCount")
dfCrimeNov$Temperature <- as.numeric(as.character(dfCrimeNov$Temperature))
dfCrimeNov$Month <- "Nov"

dfCrimeDec <- as.data.frame(table(dfCrimeFinal[dfCrimeFinal$Month == "Dec",]$Temperature))
colnames(dfCrimeDec) <- c("Temperature", "CrimeCount")
dfCrimeDec$Temperature <- as.numeric(as.character(dfCrimeDec$Temperature))
dfCrimeDec$Month <- "Dec"

dfCrimeMonth <- rbind(dfCrimeJan, dfCrimeFeb, dfCrimeMar, dfCrimeApr, dfCrimeMay, dfCrimeJun, dfCrimeJul, dfCrimeAug, dfCrimeSep, dfCrimeOct, dfCrimeNov, dfCrimeDec)

head(dfCrimeMonth, 100)

ggplot(dfCrimeMonth) + geom_line(aes(x = Temperature, y = CrimeCount, group = Month, colour = Month)) +
    labs(title = "Number of crimes per month", subtitle = "(2010-2018)", x = "Temperature in Fahrenheit",
    y = "Total Crimes", colour = "Gears") + theme_bw()

dfCrimeMonth

ggplot(dfCrimeMonth) + geom_line(aes(x = Month, y = Temperature))
    scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + theme_bw()

ggplot(dfCrimeMonth) + geom_line(aes(x = Month, y = CrimeCount))
scale_x_continuous(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + theme_bw()

ggplot(dfCrimeMonth, aes(x = CrimeCount, y = Month)) +
    geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
    scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), name = "Crime #") +
    labs(title = 'Number of crimes by Month')

install.packages("ggridges")
library("ggridges")

+ geom_line(aes(x = Month, y = CrimeCount)) +
    labs(title = "Number of crimes per month", subtitle = "(2010-2018)", x = "Temperature in Fahrenheit",
    y = "Total Crimes", colour = "Gears") + theme_bw()


round(mean(dfCrimeDec$Temperature))
round(mean(dfCrimeDec$CrimeCount))

max(dfCrimeDec$Temperature)
max(dfCrimeDec$CrimeCount)
dfCrimeDec[dfCrimeDec$CrimeCount == max(dfCrimeDec$CrimeCount),]

dfCrimeMax <- rbind(dfCrimeJan[dfCrimeJan$CrimeCount == max(dfCrimeJan$CrimeCount),],
    dfCrimeFeb[dfCrimeFeb$CrimeCount == max(dfCrimeFeb$CrimeCount),],
    dfCrimeMar[dfCrimeMar$CrimeCount == max(dfCrimeMar$CrimeCount),],
    dfCrimeApr[dfCrimeApr$CrimeCount == max(dfCrimeApr$CrimeCount),],
    dfCrimeMay[dfCrimeMay$CrimeCount == max(dfCrimeMay$CrimeCount),],
    dfCrimeJun[dfCrimeJun$CrimeCount == max(dfCrimeJun$CrimeCount),],
    dfCrimeJul[dfCrimeJul$CrimeCount == max(dfCrimeJul$CrimeCount),],
    dfCrimeAug[dfCrimeAug$CrimeCount == max(dfCrimeAug$CrimeCount),],
    dfCrimeSep[dfCrimeSep$CrimeCount == max(dfCrimeSep$CrimeCount),],
    dfCrimeOct[dfCrimeOct$CrimeCount == max(dfCrimeOct$CrimeCount),],
    dfCrimeNov[dfCrimeNov$CrimeCount == max(dfCrimeNov$CrimeCount),],
    dfCrimeDec[dfCrimeDec$CrimeCount == max(dfCrimeDec$CrimeCount),])

dfCrimeMin <- rbind(dfCrimeJan[dfCrimeJan$CrimeCount == min(dfCrimeJan$CrimeCount),],
    dfCrimeFeb[dfCrimeFeb$CrimeCount == min(dfCrimeFeb$CrimeCount),],
    dfCrimeMar[dfCrimeMar$CrimeCount == min(dfCrimeMar$CrimeCount),],
    dfCrimeApr[dfCrimeApr$CrimeCount == min(dfCrimeApr$CrimeCount),],
    dfCrimeMay[dfCrimeMay$CrimeCount == min(dfCrimeMay$CrimeCount),],
    dfCrimeJun[dfCrimeJun$CrimeCount == min(dfCrimeJun$CrimeCount),],
    dfCrimeJul[dfCrimeJul$CrimeCount == min(dfCrimeJul$CrimeCount),],
    dfCrimeAug[dfCrimeAug$CrimeCount == min(dfCrimeAug$CrimeCount),],
    dfCrimeSep[dfCrimeSep$CrimeCount == min(dfCrimeSep$CrimeCount),],
    dfCrimeOct[dfCrimeOct$CrimeCount == min(dfCrimeOct$CrimeCount),],
    dfCrimeNov[dfCrimeNov$CrimeCount == min(dfCrimeNov$CrimeCount),],
    dfCrimeDec[dfCrimeDec$CrimeCount == min(dfCrimeDec$CrimeCount),])

dfCrimeAvg <- rbind(dfCrimeMax, dfCrimeMin)

ggplot(dfCrimeAvg, aes(x = Month)) +
    geom_line(aes(y = Temperature, color = "Temperature")) + geom_line(aes(y = CrimeCount, color = "Total Crimes")) +
    scale_y_continuous(sec.axis = sec_axis(~. * 0.02, name = "Temperature")) +
    scale_colour_manual(values = c("blue", "red")) +
    labs(y = "Total Crimes", x = "Month", colour = "Parameter") +
    scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    theme(legend.position = c(0.8, 0.9))


dfCrimeRob <- dfCrimeFinal[dfCrimeFinal$Crime.Type == "ROBBERY",]
nrow(dfCrimeRob)
dfCrimeRob <- merge(aggregate(Crime.Type ~ Date.Occurred, dfCrimeRob, length), aggregate(Temperature ~ Date.Occurred, dfCrimeRob, mean))
colnames(dfCrimeRob) <- c("Date", "CrimeCount", "Temperature")
dfCrimeRob$Temperature <- round(dfCrimeRob$Temperature)
dfCrimeRob$Date <- as.Date(dfCrimeRob$Date, "%m/%d/%Y")
str(dfCrimeRob)

ggplot(dfCrimeRob, aes(x = Date)) + geom_line(aes(y = Temperature)) + geom_line(aes(y = CrimeCount))

dfCrimeRob$Month <- month.abb[as.numeric(as.character(dfCrimeRob$Date, "%m"))]

ggplot(dfCrimeRob, aes(x = Date)) +
    geom_line(aes(y = Temperature, color = "Temperature")) + geom_line(aes(y = CrimeCount, color = "Total Crimes"))

    +
    scale_colour_manual(values = c("blue", "red")) +
    labs(y = "Total Crimes", x = "Date", colour = "Parameter") +
    theme(legend.position = c(0.8, 0.9))

tail(dfCrimeRob)

