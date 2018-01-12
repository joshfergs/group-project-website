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
Density <- read.csv("Density_byCounty.csv",  header = T, sep = ",", skip = 1, check.names = F, stringsAsFactors = F)

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
Hate <- Hate[,c(1,13,3:9,18,10:12,14:17)]
Hate$Gender_both <- Hate$Gender + Hate$Gender_Identity
Hate <- Hate[,c(1:9,18,10:17)]
colnames(Hate)[15] <- "Gini"

#Merge with Density
colnames(Density)[c(5,13)] <- c("county_fips", "Density")
Hate <- Hate %>% left_join(Density[,c(5,13)], by="county_fips")




# Create dataframe to save regression output
Regressions <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(Regressions) <- c("Label", "Median_Income", "White Income", "White_Diff", "Gini", "LFPR", "Unemployment", "Education")

#Create function to add significance to coefficients
signif <- function(x) {
  as.character(
    symnum(coef(summary(x))[2,4], corr = FALSE, na = FALSE,
         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
         symbols = c("***", "**", "*", ".", " ")))
}

getCoef <- function(x) {
  paste(substr(as.character(coef(x)[2]),1,6), signif(x), sep = "")
}

#### Run Regressions #################################################################

#Total Hate Crimes
Econ_TotalbyIncome <- lm(Total ~ scale(Median_Income) + scale(Density), data = Hate)
Econ_TotalbyWhite <- lm(Total ~ scale(White_Income) + scale(Density), data = Hate)
Econ_TotalbyWhiteDiff <- lm(Total ~ scale(White_Diff) + scale(Density), data = Hate)
Econ_TotalbyGini <- lm(Total ~ scale(Gini) + scale(Density), data = Hate)
Econ_TotalbyLFPR <- lm(Total ~ scale(LFPR) + scale(Density), data = Hate)
Econ_TotalbyUnemployed <- lm(Total ~ scale(Unemployment) + scale(Density), data = Hate)
Econ_TotalbySchooling <- lm(Total ~ scale(AvgYears) + scale(Density), data = Hate)

#Add to Table
Regressions[1,] <- c("Total", getCoef(Econ_TotalbyIncome), getCoef(Econ_TotalbyWhite), 
                     getCoef(Econ_TotalbyWhiteDiff), getCoef(Econ_TotalbyGini), 
                     getCoef(Econ_TotalbyLFPR), getCoef(Econ_TotalbyUnemployed), 
                     getCoef(Econ_TotalbySchooling))

#Race Hate Crimes
Econ_RacebyIncome <- lm(Race ~ scale(Median_Income) + scale(Density), data = Hate)
Econ_RacebyWhite <- lm(Race ~ scale(White_Income) + scale(Density), data = Hate)
Econ_RacebyWhiteDiff <- lm(Race ~ scale(White_Diff) + scale(Density), data = Hate)
Econ_RacebyGini <- lm(Race ~ scale(Gini) + scale(Density), data = Hate)
Econ_RacebyLFPR <- lm(Race ~ scale(LFPR) + scale(Density), data = Hate)
Econ_RacebyUnemployed <- lm(Race ~ scale(Unemployment) + scale(Density), data = Hate)
Econ_RacebySchooling <- lm(Race ~ scale(AvgYears) + scale(Density), data = Hate)

#Add to Table
Regressions[2,] <- c("Race", getCoef(Econ_RacebyIncome), getCoef(Econ_RacebyWhite), 
                     getCoef(Econ_RacebyWhiteDiff), getCoef(Econ_RacebyGini), 
                     getCoef(Econ_RacebyLFPR), getCoef(Econ_RacebyUnemployed), 
                     getCoef(Econ_RacebySchooling))

#Religion Hate Crimes
Econ_ReligionbyIncome <- lm(Religion ~ scale(Median_Income) + scale(Density), data = Hate)
Econ_ReligionbyWhite <- lm(Religion ~ scale(White_Income) + scale(Density), data = Hate)
Econ_ReligionbyWhiteDiff <- lm(Religion ~ scale(White_Diff) + scale(Density), data = Hate)
Econ_ReligionbyGini <- lm(Religion ~ scale(Gini) + scale(Density), data = Hate)
Econ_ReligionbyLFPR <- lm(Religion ~ scale(LFPR) + scale(Density), data = Hate)
Econ_ReligionbyUnemployed <- lm(Religion ~ scale(Unemployment) + scale(Density), data = Hate)
Econ_ReligionbySchooling <- lm(Religion ~ scale(AvgYears) + scale(Density), data = Hate)

#Add to Table
Regressions[3,] <- c("Religion", getCoef(Econ_ReligionbyIncome), getCoef(Econ_ReligionbyWhite), 
                     getCoef(Econ_ReligionbyWhiteDiff), getCoef(Econ_ReligionbyGini), 
                     getCoef(Econ_ReligionbyLFPR), getCoef(Econ_ReligionbyUnemployed), 
                     getCoef(Econ_ReligionbySchooling))

#Sexual_Orientation Hate Crimes
Econ_Sexual_OrientationbyIncome <- lm(Sexual_Orientation ~ scale(Median_Income) + scale(Density), data = Hate)
Econ_Sexual_OrientationbyWhite <- lm(Sexual_Orientation ~ scale(White_Income) + scale(Density), data = Hate)
Econ_Sexual_OrientationbyWhiteDiff <- lm(Sexual_Orientation ~ scale(White_Diff) + scale(Density), data = Hate)
Econ_Sexual_OrientationbyGini <- lm(Sexual_Orientation ~ scale(Gini) + scale(Density), data = Hate)
Econ_Sexual_OrientationbyLFPR <- lm(Sexual_Orientation ~ scale(LFPR) + scale(Density), data = Hate)
Econ_Sexual_OrientationbyUnemployed <- lm(Sexual_Orientation ~ scale(Unemployment) + scale(Density), data = Hate)
Econ_Sexual_OrientationbySchooling <- lm(Sexual_Orientation ~ scale(AvgYears) + scale(Density), data = Hate)

#Add to Table
Regressions[4,] <- c("Sexual_Orientation", getCoef(Econ_Sexual_OrientationbyIncome), getCoef(Econ_Sexual_OrientationbyWhite), 
                     getCoef(Econ_Sexual_OrientationbyWhiteDiff), getCoef(Econ_Sexual_OrientationbyGini), 
                     getCoef(Econ_Sexual_OrientationbyLFPR), getCoef(Econ_Sexual_OrientationbyUnemployed), 
                     getCoef(Econ_Sexual_OrientationbySchooling))

#Disability Hate Crimes
Econ_DisabilitybyIncome <- lm(Disability ~ scale(Median_Income) + scale(Density), data = Hate)
Econ_DisabilitybyWhite <- lm(Disability ~ scale(White_Income) + scale(Density), data = Hate)
Econ_DisabilitybyWhiteDiff <- lm(Disability ~ scale(White_Diff) + scale(Density), data = Hate)
Econ_DisabilitybyGini <- lm(Disability ~ scale(Gini) + scale(Density), data = Hate)
Econ_DisabilitybyLFPR <- lm(Disability ~ scale(LFPR) + scale(Density), data = Hate)
Econ_DisabilitybyUnemployed <- lm(Disability ~ scale(Unemployment) + scale(Density), data = Hate)
Econ_DisabilitybySchooling <- lm(Disability ~ scale(AvgYears) + scale(Density), data = Hate)

#Add to Table
Regressions[5,] <- c("Disability", getCoef(Econ_DisabilitybyIncome), getCoef(Econ_DisabilitybyWhite), 
                     getCoef(Econ_DisabilitybyWhiteDiff), getCoef(Econ_DisabilitybyGini), 
                     getCoef(Econ_DisabilitybyLFPR), getCoef(Econ_DisabilitybyUnemployed), 
                     getCoef(Econ_DisabilitybySchooling))

#Gender_both Hate Crimes
Econ_Gender_bothbyIncome <- lm(Gender_both ~ scale(Median_Income) + scale(Density), data = Hate)
Econ_Gender_bothbyWhite <- lm(Gender_both ~ scale(White_Income) + scale(Density), data = Hate)
Econ_Gender_bothbyWhiteDiff <- lm(Gender_both ~ scale(White_Diff) + scale(Density), data = Hate)
Econ_Gender_bothbyGini <- lm(Gender_both ~ scale(Gini) + scale(Density), data = Hate)
Econ_Gender_bothbyLFPR <- lm(Gender_both ~ scale(LFPR) + scale(Density), data = Hate)
Econ_Gender_bothbyUnemployed <- lm(Gender_both ~ scale(Unemployment) + scale(Density), data = Hate)
Econ_Gender_bothbySchooling <- lm(Gender_both ~ scale(AvgYears) + scale(Density), data = Hate)

#Add to Table
Regressions[6,] <- c("Gender", getCoef(Econ_Gender_bothbyIncome), getCoef(Econ_Gender_bothbyWhite), 
                     getCoef(Econ_Gender_bothbyWhiteDiff), getCoef(Econ_Gender_bothbyGini), 
                     getCoef(Econ_Gender_bothbyLFPR), getCoef(Econ_Gender_bothbyUnemployed), 
                     getCoef(Econ_Gender_bothbySchooling))



### Export Results ################################################################
write.csv(Hate, "Hate.csv", row.names = F)
write.csv(Regressions, "Hate_Regressions.csv", row.names = F)