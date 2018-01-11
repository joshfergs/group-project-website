library(readxl)
library(zoo)
library(stringr)
library(dplyr)

setwd("C:/Users/Jevon/Desktop/School/Code4Policy")

#Read FBI Crime statistics file
Hate <- read_xls("HateCrimes.xls", skip = 4)

#Read data files for comparison - All data is 5-year Estimates from 2012-2016
MedianIncome <- read.csv("MedianIncome_byCounty.csv", header = T, sep = ",", skip = 1, check.names = F, stringsAsFactors = F)
WhiteIncome <- read.csv("WhiteIncome_byCounty.csv", header = T, sep = ",", skip = 1,check.names = F, stringsAsFactors = F)
Education <- read.csv("Education_byCounty.csv", header = T, sep = ",", skip = 1, check.names = F, stringsAsFactors = F)
Gini <- read.csv("Gini_byCounty.csv", header = T, sep = ",", skip = 1, check.names = F, stringsAsFactors = F)
Employment <- read.csv("Employment_byCounty.csv",  header = T, sep = ",", skip = 1, check.names = F, stringsAsFactors = F)

#Read data to convert from cities to counties (from https://simplemaps.com/data/us-cities)
CityCounty <- read.csv("uscities.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
CityCounty <- CityCounty[,c(13,5,3,6)]

#Remove quarterly incident information
Hate <- Hate[-1,c(1:9)]

#Fix column names
colnames(Hate) <- c("State", "Agency_Type", "Agency_Name", "Race", "Religion", "Sexual_Orientation", "Disability", "Gender", "Gender_Identity")

#Pull down category names to replace NAs
Hate$State <- na.locf(Hate$State)
Hate$Agency_Type <- na.locf(Hate$Agency_Type)

#Remove aggregate/Total entries
Hate <- subset(Hate, !is.na(Hate$Agency_Name))

#Remove numbers from city names (Phoenix has a note, so it's Phoenix2)
Hate$Agency_Name <- gsub("[[:digit:]]+", "", Hate$Agency_Name)

#Remove anything that is not a city or county
Hate <- subset(Hate, Agency_Type != "State Police Agencies")
Hate <- subset(Hate, Agency_Type != "Universities and Colleges")
Hate <- subset(Hate, Agency_Type != "Other Agencies")
Hate <- subset(Hate, Agency_Type != "Tribal Agencies")

#Add state abbreviations to Hate
Hate$State_Abbr <- state.abb[match(Hate$State, state.name)]

#Replace all "St." with "Saint" for merge below
Hate$Agency_Name <- gsub("St\\.", "Saint", Hate$Agency_Name)

#Create City_State column
Hate$city_state <- ifelse(Hate$Agency_Type=="Cities", paste(Hate$Agency_Name, Hate$State_Abbr, sep = ", "), NA)


#Merge with CityCounty for County Column
Hate <- Hate %>% left_join(CityCounty[,1:2], by="city_state")
Hate$county_name <- ifelse(Hate$Agency_Type!= "Cities", Hate$Agency_Name, Hate$county_name)

#Merge with FIPS code (Use unique because there are multiple rows per FIPS code)
Hate <- Hate %>% left_join(unique(CityCounty[,2:4]), by=c("county_name"="county_name", "State_Abbr"="state_id"))

#Add a FIPS for Washington DC
Hate <- within(Hate, county_fips[State == "District of Columbia"] <- "11001")

#Aggregate by County
Hate <- Hate[,c(1,12,13,4:9)]
Hate[,4:9] <- as.integer(unlist(Hate[,4:9]))
Hate <- Hate %>% group_by(State, county_name, county_fips) %>% summarise_all(funs(sum(., na.rm = TRUE)))

#Convert to prep for merges below
Hate$county_fips <- as.integer(Hate$county_fips)

#Merge with Median Income
Hate <- Hate %>% left_join(MedianIncome[,c(2,4)], by=c("county_fips"="Id2"))
colnames(Hate)[length(Hate)] <- "Median_Income"
Hate$Median_Income <- as.numeric(Hate$Median_Income)

#Merge with Median White Income
Hate <- Hate %>% left_join(WhiteIncome[,c(2,4)], by=c("county_fips"="Id2"))
colnames(Hate)[length(Hate)] <- "White_Income"
Hate$White_Income <- as.numeric(Hate$White_Income)

#Create Column for difference between Median Income and Median White Income
Hate$White_Diff <- Hate$White_Income - Hate$Median_Income

#Merge with Gini
Hate <- Hate %>% left_join(Gini[,c(2:4)], by=c("county_fips"="Id2")) #I keep the county name here, because I prefer that format
Hate <- Hate[!is.na(Hate$Geography),] #Noticed extra junk, and this filters it out

#Create values for Unemployment and Prime Age LFPR
Employment[,4:283] <- as.numeric(unlist(Employment[,4:283]))
Employment$LFPR <- (Employment[,28]*Employment[,30] + Employment[,36]*Employment[,38] + Employment[,44]*Employment[,46] + Employment[,52]*Employment[,54] + Employment[,60]*Employment[,62] + Employment[,68]*Employment[,70]) / (Employment[,28] + Employment[,36] + Employment[,44] + Employment[,52] + Employment[,60] + Employment[,68])
Employment <- Employment[,c(2,10,284)]
colnames(Employment)[2] <- "Unemployment"

#Merge with Unemployment and LFPR
Hate <- Hate %>% left_join(Employment, by=c("county_fips"="Id2"))

#Calculate Avg Number of Years of Education
years <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,14,15,15,16,17,18,20,20,23)
Education$AvgYears <- (as.matrix(Education[,c(seq(6,52,2))]) %*% years) / Education[,4]
Hate <- Hate %>% left_join(Education[,c(2,54)], by=c("county_fips"="Id2"))

#Add Total# of Hate Crimes
Hate$Total <- Hate$Race + Hate$Religion + Hate$Sexual_Orientation + Hate$Sexual_Orientation + Hate$Disability + Hate$Gender

#Tidy up
Test <- Hate[,c(1,13,3:9,18,10:12,14:17)]

#Run Regressions
