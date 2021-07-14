#### Getting Started ####
## Set the aproriate working directory
setwd("~/Downloads/Submission")

## Load all required packages
# Packages from the system library (do not have to be installed first)
pkg_sys <- c("datasets", "foreign", "MASS", "stats","stats4")

# Additional packages (need to be installed prior to loading)
pkg <- c("dplyr","tidyr","ggplot2","stargazer","reshape2","readr","haven","dummies",
         "Hmisc","lmtest","sandwich","doBy","readxl","multiwayvcov","car",
         "purrr","knitr","wesanderson","ggvis","shiny","lubridate","reporttools", 
         "stringr", "data.table","matlib",
         "ggiraphExtra","estimatr","ivpack","Jmisc","lfe","plm","mFilter",
         "gmm","metRology","gridExtra","meta","kableExtra","vars","aTSA")

# Install packages into user library (if required)
lapply(pkg, install.packages, character.only = FALSE)

# Load packages
invisible(lapply(c(pkg, pkg_sys), library, character.only = TRUE))

# Clear the Global Environment
rm(list=ls())



#### Data Preprocessing (load, reshape, merge and clean the data)
### GDP data ####
# Load the "Countries_GDP_OilRent" dataset containing the yearly GDP per capita levels of each country as well as the Oil Rents as % of GDP
df <- read_excel("Data/Countries_GDP_OilRent.xlsx")

# Transform the data into a dataframe
df <- as.data.frame(df)

## Handle missing values
# Check for missing values
sum(is.na(df))
# Check how many rows contain missing values
sum(!complete.cases(df))
# Remove the 5 rows with missing values, as nothing important is contained there
df <- df[complete.cases(df),]

## As cells with missing entries for a certain year and country are filled with ".." we replace these entries by NA such that we can change the
## type of the entries from characters to numeric later on
# Replace ".." entries by NA
df[df == ".."] <- NA

# Realizing that the data for 2020 is entirely missing, we drop this column
df$`2020 [YR2020]` <- NULL

## Check how many values are missing in each country's GDP per capita series from 1960-2019
# Get the part of the data containing the GDP per capita in US$
GDP <- df[df$`Series Name` == "GDP per capita (current US$)",]
# Create a variable to monitor which countries we want to remove later on
idx_rm_gdp <- rep(F,nrow(GDP))
# Display how many values are missing for each country
for(country in c(1:nrow(GDP))){
  # Get the number of NAs per country
  n_missing <- sum(is.na(GDP[country, !colnames(GDP) %in% c("Series Name","Series Code", "Country Name", "Country Code")]))
  # Print out how many missing values are obtained
  print(paste("Country", GDP$`Country Code`[country], "has", n_missing, "missing values", sep = " "))
  # Store the row index of each country with more than 15 missing values
  if(n_missing > 15){
    idx_rm_gdp[country] <- T
  }
  # Print out which countries we have to remove due to too many missing values
  if(country == nrow(GDP)){
    print(paste("The following", sum(idx_rm_gdp),"countries are removed due to too many missing values in the GDP per capita data:"))
    print(GDP$`Country Name`[idx_rm_gdp])
  }
}

# Remove countries with more than 15 missing values in the GDP per capita data
GDP <- GDP[!idx_rm_gdp,]


### Oil Rent data ####
# Get the part of the data containing the Oil Rent per country in percentage points of the GDP
Oil <- df[df$`Series Name` == "Oil rents (% of GDP)",]

# Only keep the countries for which enough GDP per capita data is available
Oil <- Oil[Oil$`Country Name` %in% unique(GDP$`Country Name`),]

## Check how many values are missing in each country's Oil rent series from 1960-2019 and remove the ones with many NAs
# Create a variable to monitor which countries we want to remove later on
idx_rm_oil <- rep(F,nrow(Oil))
# Display how many values are missing for each country
for(country in c(1:nrow(Oil))){
  # Get the number of NAs per country
  n_missing <- sum(is.na(Oil[country, !colnames(Oil) %in% c("Series Name","Series Code", "Country Name", "Country Code")]))
  # Print out how many missing values are obtained
  print(paste("Country", Oil$`Country Code`[country], "has", n_missing, "missing values", sep = " "))
  # Store the row index of each country with more than 20 missing values
  if(n_missing > 20){
    idx_rm_oil[country] <- T
  }
  # Print out which countries we have to remove due to too many missing values
  if(country == nrow(Oil)){
    print(paste("The following", sum(idx_rm_oil),"countries are removed due to too many missing values in the oil rent data:"))
    print(Oil$`Country Name`[idx_rm_oil])
  }
}

# Remove countries with more than 15 missing values in the oil rent data
Oil <- Oil[!idx_rm_oil,]
GDP <- GDP[!idx_rm_oil,]

# Finally, we are left with data on the following 89 countries
unique(GDP$`Country Name`)


### Oil Price data ####
## Source: https://inflationdata.com/articles/inflation-adjusted-prices/historical-crude-oil-prices-table/ 
## Note: The oil prices are based on historical free market (stripper) oil prices of Illinois Crude as presented 
##       by Illinois Oil and Gas Association and Plains All American Oil.
# Load the "Oil_Prices_per_Barrel" dataset containing the annual average domestic crude oil prices per barrel in USD
Oil_prices <- read_excel("Data/Oil_Prices_per_Barrel.xlsx")

# Remove all years for which no GDP data is available
Oil_prices <- Oil_prices[Oil_prices$Year %in% c(1960:2019),]


### Distance data ####
## Load the "countries_distances" dataset containing the shortest distance between the borders of two countries in kilometers
# Get it directly from the url
#c_dist <- read.csv(url("https://gist.githubusercontent.com/mtriff/185e15be85b44547ed110e412a1771bf/raw/1bb4d287f79ca07f63d4c56110099c26e7c6ee7d/countries_distances.csv"))
# Load the file manually
c_dist <- read.csv("Data/countries_distances.csv")

# Set the first column to index
rownames(c_dist) <- c_dist$X
c_dist$X <- NULL

# Set the column names as desired
colnames(c_dist) <- c("Country_1", "Country_2", "Distance_km")

# Check for missing values
sum(is.na(c_dist))

# Get an overview of the data and some summary statistics
str(c_dist)
summary(c_dist)

# Check which country names of the GDP dataframe are not compatible with the ones of the c_dist dataframe
unique(GDP$`Country Name`)[!unique(GDP$`Country Name`) %in% unique(c_dist$Country_1)]

# Rename these countries in the c_dist dataframe, such that the names coincide
c_dist$Country_1[c_dist$Country_1 == "Brunei"] <- "Brunei Darussalam"
c_dist$Country_2[c_dist$Country_2 == "Brunei"] <- "Brunei Darussalam"
c_dist$Country_1[c_dist$Country_1 == "Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
c_dist$Country_2[c_dist$Country_2 == "Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
c_dist$Country_1[c_dist$Country_1 == "Republic of Congo"] <- "Congo, Rep."
c_dist$Country_2[c_dist$Country_2 == "Republic of Congo"] <- "Congo, Rep."
c_dist$Country_1[c_dist$Country_1 == "Ivory Coast"] <- "Cote d'Ivoire"
c_dist$Country_2[c_dist$Country_2 == "Ivory Coast"] <- "Cote d'Ivoire"
c_dist$Country_1[c_dist$Country_1 == "Egypt"] <- "Egypt, Arab Rep."
c_dist$Country_2[c_dist$Country_2 == "Egypt"] <- "Egypt, Arab Rep."
c_dist$Country_1[c_dist$Country_1 == "Iran"] <- "Iran, Islamic Rep."
c_dist$Country_2[c_dist$Country_2 == "Iran"] <- "Iran, Islamic Rep."
c_dist$Country_1[c_dist$Country_1 == "South Korea"] <- "Korea, Rep."
c_dist$Country_2[c_dist$Country_2 == "South Korea"] <- "Korea, Rep."
c_dist$Country_1[c_dist$Country_1 == "Tobago"] <- "Trinidad and Tobago"
c_dist$Country_2[c_dist$Country_2 == "Tobago"] <- "Trinidad and Tobago"
c_dist$Country_1[c_dist$Country_1 == "Trinidad"] <- "Trinidad and Tobago"
c_dist$Country_2[c_dist$Country_2 == "Trinidad"] <- "Trinidad and Tobago"
c_dist$Country_1[c_dist$Country_1 == "UK"] <- "United Kingdom"
c_dist$Country_2[c_dist$Country_2 == "UK"] <- "United Kingdom"
c_dist$Country_1[c_dist$Country_1 == "USA"] <- "United States"
c_dist$Country_2[c_dist$Country_2 == "USA"] <- "United States"
c_dist$Country_1[c_dist$Country_1 == "Venezuela"] <- "Venezuela, RB"
c_dist$Country_2[c_dist$Country_2 == "Venezuela"] <- "Venezuela, RB"

# Drop HongKong from the datasets, as it is not contained in the c_dist dataframe
GDP <- GDP[!GDP$`Country Name` == "Hong Kong SAR, China",]
Oil <- Oil[!Oil$`Country Name` == "Hong Kong SAR, China",]

## Get a dataframe containing the distances between all pairs of the remaining countries in the GDP dataframe
# Create a dataframe to store the values
c_dist_final <- matrix(NA, nrow = nrow(GDP), ncol = nrow(GDP))
c_dist_final <- as.data.frame(c_dist_final)
rownames(c_dist_final) <- unique(GDP$`Country Name`)
colnames(c_dist_final) <- unique(GDP$`Country Name`)
# Set up a loop to extract the according values
for(c1 in unique(GDP$`Country Name`)){
  for(c2 in unique(GDP$`Country Name`)){
    if(c1 != c2){
      # Note, we take the mean as Trinidad and Tobago are merged together
      c_dist_final[c1,c2] <- mean(c_dist[c_dist$Country_1 == c1 & c_dist$Country_2 == c2, 3])
    }
    else{
      c_dist_final[c1,c2] <- 0
    }
  }
}

# Set 'Distance to Country Name' as colnames
colnames(c_dist_final) <- paste("Distance to", colnames(c_dist_final))


### Reshape and Merge the data ####
## GDP
# Set 'Country Name GDP/capita USD' as index (i.e. the column names later on)
rownames(GDP) <- paste(GDP[,"Country Name"], "GDP/capita USD")

# Remove redundant columns
GDP[,c(1,2,3,4)] <- NULL

# Transpose the dataframe
GDP <- as.data.frame(t(GDP))

# Transform the entries into numerical values
for(col in colnames(GDP)){
  GDP[,col] <- as.numeric(GDP[,col])
}

# Add a collumn containing the year and make it the first one
GDP$Year <- c(1960:2019)
GDP <- GDP[,c(89,1:88)]

# Interpolate the missing values for Kuwait
GDP[c(33:35),49] <- seq(GDP[32,49], GDP[36,49], (GDP[36,49]-GDP[32,49])/4)[c(2:4)]

# Set Year as index
rownames(GDP) <- GDP$Year

# Take a look at the GDP data
str(GDP)
stargazer(GDP, type = "text")


## Oil Rent
# Set 'Country Name Oil Rent share of GDP' as index (i.e. the column names later on)
rownames(Oil) <- paste(Oil[,"Country Name"], "Oil Rent share of GDP")

# Remove redundant columns
Oil[,c(1,2,3,4)] <- NULL

# Transpose the dataframe
Oil <- as.data.frame(t(Oil))

# Transform the entries into numerical values
for(col in colnames(Oil)){
  Oil[,col] <- as.numeric(Oil[,col])
}

# Set Year as index
rownames(Oil) <- c(1960:2019)

## Interpolate the missing values
# Kuwait
Oil[20,48] <- (Oil[21,48] + Oil[19,48])/2
# Qatar
Oil[20,67] <- (Oil[21,67] + Oil[19,67])/2

# Take a look at the data
str(Oil)
stargazer(Oil, type = "text")


### Merge Everything together and export the cleaned data ####
# Export the distance matrix as csv
write.csv(c_dist_final, "Data/Countries_distances_preprocessed.csv", row.names = T)

# Merge the GDP, Oil Rent and Oil Price dataframes
GDP_preprocessed <- cbind(GDP,Oil,Oil_prices[,c(2,3)])

# Export the data as csv
write.csv(GDP_preprocessed, "Data/GDP_data_preprocessed.csv", row.names = F)

### Load the data as follows ####
# Country distance data
c_dist <- read.csv("Data/Countries_distances_preprocessed.csv", row.names = 1)
# GDP data
GDP_data <- read.csv("Data/GDP_data_preprocessed.csv")
for(col in colnames(GDP_data)){
  GDP_data[,col] <- as.numeric(GDP_data[,col])
}