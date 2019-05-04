# Load the required libraries
library(stringr)

# Get the current working directory of the R process
getwd()

# Load the raw data of crime from csv file to a data frame
dfCrime <- read.csv("data/Crime_Data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)

# Get the structure of the crime data frame
str(dfCrime)

# Display the first 5 rows of the dataframe
head(dfCrime)

# Remove the columns that are not required for this data analysis
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

# Separate the Latitude and Longitude values in different columns
lat <- str_split_fixed(dfCrime$Location, ", ", 2)[, 1]
dfCrime$Latitude <- gsub("\\(", "", lat)

lon <- str_split_fixed(dfCrime$Location, ", ", 2)[, 2]
dfCrime$Longitude <- gsub("\\)", "", lon)

dfCrime$Location <- NULL

tail(dfCrime)

# Set the Crime Type column heading
unique(dfCrime$Crime.Code.Description)
colnames(dfCrime)[4] <- "Crime.Type"

# Get the count of missing values in the data frame
sapply(dfCrime, function(x) sum(is.na(x)))

# Write the clean crime data to a separate file inside the working directory
write.csv(dfCrime, file = "data/clean_crime_data.csv", quote = FALSE, na = "", row.names = FALSE)

# Load the raw data of weather
dfWeather <- read.csv("data/Weather_Data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)

# Get the structure of the data frame for weather data
str(dfWeather)

# Remove the columns that are not required
dfWeather$Site <- NULL
dfWeather$Dewpoint <- NULL
dfWeather$RH <- NULL
dfWeather$WindDir <- NULL
dfWeather$CldFrac <- NULL
dfWeather$MSLP <- NULL
dfWeather$Weather <- NULL
dfWeather$Source <- NULL
dfWeather$Windspeed <- NULL
dfWeather$Precip <- NULL

# Verify whether there are any missing values in the data frame
sapply(dfWeather, function(x) sum(is.na(x)))

# Display first 20 rows of weather data frame
head(dfWeather, 20)

# Fix the Date column format to have only date
dfWeather$Date <- str_split_fixed(dfWeather$Date, " ", 2)[, 1]

# Remove the data for the years from 1998 to 2009
for (year in 1998:2009) {
    dfWeather <- dfWeather[!grepl( paste0("/", year), dfWeather$Date),]
}

# Write the clean crime data to a separate file in the working directory
write.csv(dfWeather, file = "data/clean_weather_data.csv", quote = FALSE, na = "", row.names = FALSE)

# Create a new column DateHour with concatenation of date and hour values for amalgamating the data frames
dfWeather$DateHour <- paste0(as.character(as.Date(dfWeather$Date, "%m/%d/%Y"), "%m/%d/%Y"), dfWeather$Hour)

head(dfWeather, 20)

# Now similar DateHour value is required in the crime dataframe
# Get hour value in separate column from the Time Occured value which has hour and minute values in HHMM format
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

# Attach the date of crime occured after 11:30 PM to 00 hours of next day
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

# Concatenate the date and hour values similar to weather dataframe for merging
dfCrime$DateHour <- paste0(dfCrime$Date.Occurred, dfCrime$Hour)

# Write the prepared crime and weather dataframe to separate files in the working directory
write.csv(dfCrime, file = "data/crime_data2.csv", quote = FALSE, na = "", row.names = FALSE)
write.csv(dfWeather, file = "data/weather_data2.csv", quote = FALSE, na = "", row.names = FALSE)

# Amalgamate the datasets to create a new data frame
dfCrimeFinal <- merge(x = dfCrime, y = dfWeather, by = "DateHour", all.x = TRUE)

head(dfCrimeFinal)

# Fix the format of Crime Type and Area values
dfCrimeFinal$Crime.Type <- gsub(",", " ", dfCrimeFinal$Crime.Type)
dfCrimeFinal$Area.Name <- gsub(",", " ", dfCrimeFinal$Area.Name)

# Remove the columns that are not necessary after amalgamation
dfCrimeFinal$DateHour <- NULL
dfCrimeFinal$Hour.x <- NULL
dfCrimeFinal$Hour.y <- NULL
dfCrimeFinal$Date <- NULL

# Consolidate the crime type values
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

# Get abbreviated month name in separate month column
dfCrimeFinal$Month <- month.abb[as.numeric(str_split_fixed(dfCrimeFinal$Date.Occurred, "/", 2)[, 1])]
tail(dfCrimeFinal)

# Get Year value in separate month column
dfCrimeFinal$Year <- as.numeric(str_split_fixed(dfCrimeFinal$Date.Occurred, "/", 3)[, 3])

# Add a separate column for Celsius value by converting the temperate in fahrenheit
dfCrimeFinal$Celsius <- (dfCrimeFinal$Temperature - 32) * 5 / 9

# Write the amalgamated final crime dataset to a separate file in the working directory
# This prepared dataset will be used for further analysis
write.csv(dfCrimeFinal, file = "data/final_crime_data.csv", quote = FALSE, na = "", row.names = FALSE)
