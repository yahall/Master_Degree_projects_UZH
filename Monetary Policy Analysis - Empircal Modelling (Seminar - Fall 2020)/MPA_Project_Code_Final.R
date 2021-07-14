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



#### Data Preprocessing (load, reshape, merge and clean the data) ####

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

## Check how many values are missing in each country's Oil rent series from 1960-2019
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

# Drop HongKong from the datasets, as it is not contained in the c_dist dataframe
GDP <- GDP[!GDP$`Country Name` == "Hong Kong SAR, China",]
Oil <- Oil[!Oil$`Country Name` == "Hong Kong SAR, China",]

# Finally, we are left with data on the following 88 countries
unique(GDP$`Country Name`)

# Remove unnecessary variables
rm(df, idx_rm_gdp, idx_rm_oil, n_missing)



### Reshape the data ####
## GDP
# Set 'Country Name GDP/capita USD' as index (i.e. the column names later on)
rownames(GDP) <- GDP[,"Country Name"]

# Remove redundant columns
GDP[,c(1,2,3,4)] <- NULL

# Transpose the dataframe
GDP <- as.data.frame(t(GDP))

# Transform the entries into numerical values
for(col in colnames(GDP)){
  GDP[,col] <- as.numeric(as.character(GDP[,col]))
}

# Add a collumn containing the year and make it the first one
GDP$Year <- c(1960:2019)
GDP <- GDP[,c(89,1:88)]

# Interpolate the missing values for Kuwait
GDP[c(33:35),49] <- seq(GDP[32,49], GDP[36,49], (GDP[36,49]-GDP[32,49])/4)[c(2:4)]

# Set Year as index
rownames(GDP) <- GDP$Year


## Oil Rent
# Set 'Country Name Oil Rent share of GDP' as index (i.e. the column names later on)
#rownames(Oil) <- paste(Oil[,"Country Name"], "Oil Rent share of GDP")
rownames(Oil) <- Oil[,"Country Name"]

# Remove redundant columns
Oil[,c(1,2,3,4)] <- NULL

# Transpose the dataframe
Oil <- as.data.frame(t(Oil))

# Transform the entries into numerical values
for(col in colnames(Oil)){
  Oil[,col] <- as.numeric(as.character(Oil[,col]))
}

# Set Year as index
rownames(Oil) <- c(1960:2019)

## Interpolate the missing values
# Kuwait
Oil[20,48] <- (Oil[21,48] + Oil[19,48])/2
# Qatar
Oil[20,67] <- (Oil[21,67] + Oil[19,67])/2



### Country Selection ####
# Select countries
country_index <- c(3,4,7,10,13,15,16,23,25,26,28,29,31,33,37,38,41,42,44,48,52,53,55,56,58,59,60,63,64,66,67,68,71,72,75,76,80,81,82,83,84)

# Take a look at the selection
colnames(GDP[,-1])[country_index]

# Apply country selection to pure GDP data
country_index_GDP <- country_index + 1
GDP[,-c(1,country_index_GDP)] <- NULL

# Apply country selection to Oil rent data
Oil[,-c(country_index)] <- NULL

## Apply country selection to cleaned dataset
# GDP preprocessed
GDP_pp <- read.csv("Data/GDP_data_preprocessed.csv")
GDP_pp[,-c(1,country_index_GDP,(country_index+89),178,179)] <- NULL

# Country distance preprocessed
c_dist <- read.csv("Data/Countries_distances_preprocessed.csv", row.names = 1)
c_dist <- c_dist[country_index,country_index]



### GDP graphs and Grouping ####
# Transform everything to logs
Year <- GDP$Year
GDP_log <- cbind(Year, log(GDP[,-1]))
rm(Year)

# Make a DataFrame for plotting
n_years     <- nrow(GDP_log)
n_countries <- ncol(GDP_log)-1 # Omit the year column
GDP_log_plot <- matrix(NA, nrow = (n_years*n_countries), ncol = 3)
GDP_log_plot <- as.data.frame(GDP_log_plot)
colnames(GDP_log_plot) <- c("Year", "Log_GDP_per_capita", "Country")
GDP_log_plot$Year <- rep(GDP_log$Year, n_countries)
i <- 0
for(country in colnames(GDP_log)[-1]){
  # Assign countries
  GDP_log_plot$Country[c((i*n_years+1):(i*n_years+n_years))] <- rep(country, n_years)
  # Assign logGDPs
  GDP_log_plot$Log_GDP_per_capita[c((i*n_years+1):(i*n_years+n_years))] <- GDP_log[,country]
  # Adjust running variable
  i <- i + 1
}

## Group the counties before plotting
# EU
EU_idx <- c(2,3,8,11,12,13,14,17,18,23,30,34,35)
unique(GDP_log_plot$Country)[EU_idx]

# Europe non EU
nonEU_idx <- c(15,26,36,40)
unique(GDP_log_plot$Country)[nonEU_idx]

# Europe (entire)
Europe_idx <- c(2,3,8,11,12,13,14,15,17,18,23,26,30,34,35,36,40)
unique(GDP_log_plot$Country)[Europe_idx]

# Africa
Africa_idx <- c(10,22,25,33,37)
unique(GDP_log_plot$Country)[Africa_idx]

# North America
NAmerica_idx <- c(5,21,41)
unique(GDP_log_plot$Country)[NAmerica_idx]

# South America
SAmerica_idx <- c(4,7,9,28,29)
unique(GDP_log_plot$Country)[SAmerica_idx]

# Asia
Asia_idx <- c(6,16,19,20,27,31,32,38,39)
unique(GDP_log_plot$Country)[Asia_idx]

# Australia
Australia_idx <- c(1,24)
unique(GDP_log_plot$Country)[Australia_idx]

# G7
G7_idx <- c(5,12,13,18,19,40,41)
unique(GDP_log_plot$Country)[G7_idx]


## Make plots
plot_lGDP <- function(idx, title = "title"){
  ggplot(GDP_log_plot[GDP_log_plot$Country %in% unique(GDP_log_plot$Country)[idx],], 
         mapping = aes(x = Year, y = Log_GDP_per_capita, color = Country)) +
    geom_line(size = 1) +
    theme_bw() +
    xlab("Year") +
    ylab("Log GDP per Capita") +
    ggtitle(title)
}

# Continents and G7
grid.arrange(
# EU
plot_lGDP(EU_idx, "Log GDP per Capita 1960-2019 - EU Countries"),

# Europe non EU
plot_lGDP(nonEU_idx, "Log GDP per Capita 1960-2019 - Europ. Non-EU Countries"),

# Africa
plot_lGDP(Africa_idx, "Log GDP per Capita 1960-2019 - African Countries"),

# North America
plot_lGDP(NAmerica_idx, "Log GDP per Capita 1960-2019 - North American Countries"),

# South America
plot_lGDP(SAmerica_idx, "Log GDP per Capita 1960-2019 - South American Countries"),

# Asia
plot_lGDP(Asia_idx, "Log GDP per Capita 1960-2019 - Asian Countries"),

# Australia
plot_lGDP(Australia_idx, "Log GDP per Capita 1960-2019 - Australian Countries"),

# G7
plot_lGDP(G7_idx, "Log GDP per Capita 1960-2019 - G7 Countries"),

ncol = 2)


# Entire Europe and entire America
grid.arrange(
  
# Europe (entire)
plot_lGDP(Europe_idx, "Log GDP per Capita 1960-2019 - European Countries"),
  
# America (entire)
  plot_lGDP(c(NAmerica_idx,SAmerica_idx), "Log GDP per Capita 1960-2019 - American Countries"),

ncol = 2)


# Remove unnecessary variables
rm(col, country, country_index, country_index_GDP, n_countries, n_years, i, G7_idx, GDP_log_plot)



### Order the datasets according to the continent ####
# Get the ordered index
ordered_idx <- c(EU_idx, nonEU_idx, NAmerica_idx, SAmerica_idx, Africa_idx, Asia_idx, Australia_idx)

# Order the Oil rent dataset
Oil <- Oil[, ordered_idx]

# Order the GDP_log dataset
GDP_log <- GDP_log[, c(1,(ordered_idx+1))]

# Order the GDP_pp dataset
GDP_pp  <- GDP_pp[, c(1,(1+ordered_idx), (42+ordered_idx),84,85)]

# Order the c_dist dataset
c_dist <- c_dist[ordered_idx, ordered_idx]

## Redefine the idices
# Europe EU
idx_first <- 1
idx_last <- length(EU_idx)
EU_idx <- c(idx_first:idx_last)
# Europe non-EU 
idx_first <- idx_last + 1
idx_last <- idx_first + length(nonEU_idx) - 1
nonEU_idx <- c(idx_first:idx_last)
# North America
idx_first <- idx_last + 1
idx_last <- idx_first + length(NAmerica_idx) - 1
NAmerica_idx <- c(idx_first:idx_last)
# South America
idx_first <- idx_last + 1
idx_last <- idx_first + length(SAmerica_idx) - 1
SAmerica_idx <- c(idx_first:idx_last)
# Africa
idx_first <- idx_last + 1
idx_last <- idx_first + length(Africa_idx) - 1
Africa_idx <- c(idx_first:idx_last)
# Asia
idx_first <- idx_last + 1
idx_last <- idx_first + length(Asia_idx) - 1
Asia_idx <- c(idx_first:idx_last)
# Australia
idx_first <- idx_last + 1
idx_last <- idx_first + length(Australia_idx) - 1
Australia_idx <- c(idx_first:idx_last)

# Remove unnecessary variables
rm(idx_first, idx_last, ordered_idx, GDP)



### Perform Augmented Dickey Fuller tests to dlog_GDP of all selected countries ####
## Aim: Reveal whether the log_GDP level series are I(1) (unit Root of order 1). I.e. reveal if the first difference of log_GDP is stationary (without trend)
## Write a function to automatically choose the optimal lag order (up to a max of 4) and perform an ADF test subsequently
full_adf <- function(data){
  # Identify the non-missing values of the series
  vals <- !is.na(data)
  
  ## Get the optimal lag length according to the AIC
  # With drift but no trend (as we need stationarity in the n-th (n=1 in our case) difference for the EG test, not trend-stationarity)
  lags <- as.numeric(VARselect(data[vals], type = "const", lag.max = 4)$selection[1])
  
  ## Perform the adf test
  adf_result <- adf.test(data[vals], nlag = 5, output = F)
  
  ## Get the p-values from the model with the optimally chosen lag length
  # With drift but no trend
  p_val <- round(as.numeric(adf_result$type2[(lags+1),3]),3)
  
  ## Get an indicator variable to capture whether stationarity is revealed on a 95% level
  # With drift but no trend
  st <- 0
  if(p_val <= 0.05){st <- 1}
  
  # Return the results
  return(c(lags, p_val, st))
}

## Perform the ADF test for each country's dlog_GDP
# Create a dataframe containing the first differences of the log_GDP for each country
GDP_dlog <- GDP_log
for(country in c(2:ncol(GDP_log))){
  GDP_dlog[,country] <- (GDP_log[,country] - shift(GDP_log[,country], -1))
}
# Create a dataframe to store the ADF results
ADF_GDP_dlog <- matrix(NA, nrow = (ncol(GDP_dlog) - 1), ncol = 3)
ADF_GDP_dlog <- as.data.frame(ADF_GDP_dlog)
rownames(ADF_GDP_dlog) <- colnames(GDP_dlog)[-1]
colnames(ADF_GDP_dlog) <- c("Optimal number of Lags", "P-Value", "Stationarity")
# Perform the actual ADF tests for all countries
for(country in c(1:nrow(ADF_GDP_dlog))){
  ADF_GDP_dlog[country,] <- full_adf(GDP_dlog[,(country + 1)])
}

# Create a Latex table our of the ADF_GDP_dlog dataframe using kableExtra
ADF_GDP_dlog_k <- kable(ADF_GDP_dlog, format = 'latex', digits = 3, booktabs = T,
                        linesep = "", align = c("c","c","c"))
ADF_GDP_dlog_k <- kable_styling(ADF_GDP_dlog_k, latex_options = c("striped"))
ADF_GDP_dlog_k

## NOTE: In the given time frame, the USA appears to have no unit root of order 1 (i.e. I(1)) in log_GDP as the only country. 
##       Hence, we drop it for further analyses as EG test are not applicable to the US data (note: US index = 20)
# First save the summary statistics of USA
US_stats <- c("North America", "G7", round(min(GDP_log[,21], na.rm = T),3), round(max(GDP_log[,21], na.rm = T),3),
              sum(is.na(GDP_log[,21])), round(mean(Oil[,20], na.rm = T),3), sum(is.na(Oil[,20])))
# Then remove US from all dataframes
ADF_GDP_dlog  <- ADF_GDP_dlog[-20,]
GDP_log       <- GDP_log[,-21]       # 20 + 1 since the first columns contains the years
GDP_dlog      <- GDP_dlog[,-21]
GDP_pp        <- GDP_pp[,-c(21, (21+41))]
Oil           <- Oil[, -20]
c_dist        <- c_dist[-20,-20]
NAmerica_idx  <- NAmerica_idx[-3]
SAmerica_idx  <- SAmerica_idx - 1
Africa_idx    <- Africa_idx - 1
Asia_idx      <- Asia_idx - 1
Australia_idx <- Australia_idx - 1



### Perform pairwise Engle-Granger cointegration tests to log_GDP (levels) of all remaining countries ####
## Get a dataframe containing indicator variables which equal to 1 if two countries are cointegrated on a 95% level
# Create a dataframe to store the results
EG_GDP_log <- matrix(0, nrow = nrow(ADF_GDP_dlog), ncol = nrow(ADF_GDP_dlog))
EG_GDP_log <- as.data.frame(EG_GDP_log)
rownames(EG_GDP_log) <- rownames(ADF_GDP_dlog)
colnames(EG_GDP_log) <- rownames(ADF_GDP_dlog)

# Set up a loop to perform the Engle-Granger test to all possible pairs
for(c1 in c(1:nrow(ADF_GDP_dlog))){
  for(c2 in c(1:nrow(ADF_GDP_dlog))){
    if(c1 == c2){
      EG_GDP_log[c1,c2] <- NA
    }else{
      # Perform the cointegration test with d = 0 (as we are interested in cointegration in levels) and 0 lags for the residuals (corresponding to nlag = 1)
      EG <- coint.test(GDP_log[,(c1 + 1)], GDP_log[,(c2 + 1)], d = 0, nlag = 1, output = F)
      # Get the P-value of the test of type1 (as the ADF test on the residuals should only be performed with a constant, but no trend)
      # Note: 1. all values >= 0.1 are set to 0.1 and all values <= 0.01 are set to 0.01
      #       2. The P-value from the cointegration test where the adf-test on the residuals is applied with a constant but no trend is stored in EG[7]
      pval <- EG[7]
      # Set the corresponding cell to 1 if the two countries are found to be cointegrated on the 95% level
      if(pval <= 0.05){
        EG_GDP_log[c1,c2] <- 1
      }
    }
  }
}

## Get a dataframe containing the p-values of all performed EG tests
# Create a dataframe to store the results
EG_GDP_log_pval <- matrix(0, nrow = nrow(ADF_GDP_dlog), ncol = nrow(ADF_GDP_dlog))
EG_GDP_log_pval <- as.data.frame(EG_GDP_log_pval)
rownames(EG_GDP_log_pval) <- rownames(ADF_GDP_dlog)
colnames(EG_GDP_log_pval) <- rownames(ADF_GDP_dlog)

# Set up a loop to perform the Engle-Granger test to all possible pairs
for(c1 in c(1:nrow(ADF_GDP_dlog))){
  for(c2 in c(1:nrow(ADF_GDP_dlog))){
    if(c1 == c2){
      EG_GDP_log_pval[c1,c2] <- NA
    }else{
      # Perform the cointegration test with d = 0 (as we are interested in cointegration in levels) and 0 lags for the residuals (corresponding to nlag = 1)
      EG <- coint.test(GDP_log[,(c1 + 1)], GDP_log[,(c2 + 1)], d = 0, nlag = 1, output = F)
      # Get the P-value of the test of type1 (as the ADF test on the residuals should only be performed with a constant, but no trend)
      # Note: all values >= 0.1 are set to 0.1 and all values <= 0.01 are set to 0.01
      pval <- EG[7]
      EG_GDP_log_pval[c1,c2] <- round(pval,2)
    }
  }
}

# Remove unnecessary variables
rm(EG, pval)

# Create a Latex table our of the EG_GDP_log dataframe using kableExtra
#EG_GDP_log[is.na(EG_GDP_log)] <- "-"
## Europe
Europe_idx <- c(EU_idx, nonEU_idx)
EG_GDP_log_k <- kable(EG_GDP_log[,Europe_idx], format = 'latex', digits = 1, booktabs = T,
                        linesep = "", align = rep("c", length(Europe_idx)))
EG_GDP_log_k <- kable_styling(EG_GDP_log_k, latex_options = c("striped"))
EG_GDP_log_k

## America
America_idx <- c(NAmerica_idx, SAmerica_idx)
EG_GDP_log_k <- kable(EG_GDP_log[,America_idx], format = 'latex', digits = 1, booktabs = T,
                      linesep = "", align = rep("c", length(America_idx)))
EG_GDP_log_k <- kable_styling(EG_GDP_log_k, latex_options = c("striped"))
EG_GDP_log_k

## Africa, Asia, Australia
EG_GDP_log_k <- kable(EG_GDP_log[,c(Africa_idx,Asia_idx,Australia_idx)], format = 'latex', digits = 1, booktabs = T,
                      linesep = "", align = rep("c", length(c(Africa_idx,Asia_idx,Australia_idx))))
EG_GDP_log_k <- kable_styling(EG_GDP_log_k, latex_options = c("striped"))
EG_GDP_log_k



### Some statistics/analyses ####
# Get the share of cointegrated pairs
mean(EG_GDP_log[!is.na(EG_GDP_log)])

# Display cointegration status for european EU countries
EG_GDP_log[,EU_idx]

# Display cointegration status for european non-EU countries
EG_GDP_log[,nonEU_idx]

# Display cointegration status for north-american countries
EG_GDP_log[,NAmerica_idx]

# Display cointegration status for south-american EU countries
EG_GDP_log[,SAmerica_idx]

# Display cointegration status for asian EU countries
EG_GDP_log[,Asia_idx]

# Display cointegration status for australian EU countries
EG_GDP_log[,Australia_idx]



### Prepare the data for Logistic Regression ####
### Model: regression of distance and indicators capturing whether 0, 1, or 2 countries with high oil rent shares are involved on the cointegration dummy
## Get the average oil rent share of GDP for each country and an indicator whether this average is above 4%
# Create a dataframe to store the average oil rent share
Oil_rents_avg <- matrix(NA, nrow = nrow(ADF_GDP_dlog), ncol = 2)
Oil_rents_avg <- as.data.frame(Oil_rents_avg)
rownames(Oil_rents_avg) <- rownames(ADF_GDP_dlog)
colnames(Oil_rents_avg) <- c("Average Oil Rent Share of GDP in Percentage Points", "High Oil Rent Share (>4%)")
Oil_rents_avg[,2] <- 0

# Set up a loop to extract all average oil rent shares
for(country in c(1:nrow(ADF_GDP_dlog))){
  Oil_rents_avg[country,1] <- mean(Oil[,country], na.rm = T)
  if(Oil_rents_avg[country,1] >= 4){
    Oil_rents_avg[country,2] <- 1
  }
}

# Get the index of all countries that are "Oil intense"
Oil_intense_idx <- Oil_rents_avg[,2] == 1

# Print out the Oil intenses countries
rownames(Oil_rents_avg)[Oil_intense_idx]

## Since we performed the EG test for each pair twice (either country's log_GDP was once treated as the dependent variable in the 1. step of the EG test),
## we quickly check for pairs, where this order led to a different outcome (i.e. cointegration status)
# Print out for which variables we have different coint. status depending on which country's logGDP was the dependent variable in the EG test,
# and keep track of the number of cases (country-pairings) for which we observe this.
n_cases <- 0
for(c1 in c(1:nrow(ADF_GDP_dlog))){
  for(c2 in c(1:nrow(ADF_GDP_dlog))){
    if(c1 != c2 & c1 < c2 & EG_GDP_log[c1,c2] != EG_GDP_log[c2,c1]){
      print(paste("Difference for", rownames(EG_GDP_log)[c1], "and", rownames(EG_GDP_log)[c2], sep = " "))
      n_cases <- n_cases + 1
    }
  }
}
n_cases

## We observe that we have 18 pairings, for which it makes a difference which country is treated as the dependent variable. To account for this,
## we will include 2 observations for such pairings (both possible cases) in the dataframe we produce subsequently for the logistic regression,
## while introducing a weighting vector, such that both observations of such pairings are weighted half as much as all regular pairings.

# Get the number of datapoints to produce for the logistic regression (i.e. number of possible pairings out of the remaining 40 countries + 18)
n_obs <- 780 + 18

## Get the data needed to perform the logistic regression
# Create a dataframe for the logistic regression (with one observations for each country pair, except for those revealed above, for which 2 are included)
LR_data <- matrix(NA, nrow = n_obs, ncol = 8)
LR_data <- as.data.frame(LR_data)
colnames(LR_data) <- c("Country 1 (dependent variable)", "Country 2 (explanatory variable)",
                       "Coint", "Distance", "N_Oil_intense_countries", "Oil_rent_share_diff",
                       "weights_baseline", "weights_rebalanced")

# Set the baseline weights initially to 1
LR_data$weights_baseline <- 1

# Get the variables for the logistic regression of each pair
i <- 1
for(c1 in c(1:nrow(ADF_GDP_dlog))){
  for(c2 in c(1:nrow(ADF_GDP_dlog))){
    # Normal cases
    if(c1 != c2 & c1 < c2 & EG_GDP_log[c1,c2] == EG_GDP_log[c2,c1]){
      # Get country names
      LR_data[i,1] <- rownames(EG_GDP_log)[c1]
      LR_data[i,2] <- rownames(EG_GDP_log)[c2]
      # Get the cointegration indicator
      LR_data[i,3] <- EG_GDP_log[c1,c2]
      # Get the distances
      LR_data[i,4] <- c_dist[c1,c2]
      # Get the number of oil intense countries in each pairing
      LR_data[i,5] <- Oil_rents_avg[c1,2] + Oil_rents_avg[c2,2]
      # Get the difference between the average oil rent shares of the two countries (country dependent variable - country explanatory variable)
      LR_data[i,6] <- Oil_rents_avg[c1,1] - Oil_rents_avg[c2,1]
      i <- i + 1
    }
    # Special cases
    if(c1 != c2 & c1 < c2 & EG_GDP_log[c1,c2] != EG_GDP_log[c2,c1]){
      ## 1. pairing possibility
      # Get country names
      LR_data[i,1] <- rownames(EG_GDP_log)[c1]
      LR_data[i,2] <- rownames(EG_GDP_log)[c2]
      # Get the cointegration indicator (combination 1)
      LR_data[i,3] <- EG_GDP_log[c1,c2]
      # Get the distances (combination 1)
      LR_data[i,4] <- c_dist[c1,c2]
      # Get the number of oil intense countries in each pairing
      LR_data[i,5] <- Oil_rents_avg[c1,2] + Oil_rents_avg[c2,2]
      # Get the difference between the average oil rent shares of the two countries (country dependent variable - country explanatory variable)
      LR_data[i,6] <- Oil_rents_avg[c1,1] - Oil_rents_avg[c2,1]
      # Adjust the baseline weights of this observation to 0.5
      LR_data[i,7] <- 0.5
      i <- i + 1
      
      ## 2. pairing possibility
      # Get country names
      LR_data[i,1] <- rownames(EG_GDP_log)[c2]
      LR_data[i,2] <- rownames(EG_GDP_log)[c1]
      # Get the cointegration indicator (combination 2)
      LR_data[i,3] <- EG_GDP_log[c2,c1]
      # Get the distances (combination 2)
      LR_data[i,4] <- c_dist[c2,c1]
      # Get the number of oil intense countries in each pairing
      LR_data[i,5] <- Oil_rents_avg[c2,2] + Oil_rents_avg[c1,2]
      # Get the difference between the average oil rent shares of the two countries (country dependent variable - country explanatory variable)
      LR_data[i,6] <- Oil_rents_avg[c2,1] - Oil_rents_avg[c1,1]
      # Adjust the baseline weights of this observation to 0.5
      LR_data[i,7] <- 0.5
      i <- i + 1
    }
  }
}

## Since we get a pretty unbalanaced dataset (only ~10% of observations are cointegrated) we also want to calculated rebalanced weights
## to account for this issue in the logistic regression later on. To do so, we simply multiply the baseline weight of each observation with the inverse
## of the (weighted by the base weights) share of its cointegration status. However, we will perform the logsitic regression once for the baseline weights, 
## and once for the rebalanced weights

# Transform the columns Coint into integers
LR_data$Coint       <- as.integer(LR_data$Coint)
# Calculate the weighted share of cointegrated observation
share_coint <- sum(LR_data[,3]*LR_data$weights_baseline)/sum(LR_data$weights_baseline)
## Calculate the rebalanced weights
# Observations with cointegration
LR_data$weights_rebalanced[LR_data[,3] == 1] <- LR_data$weights_baseline[LR_data[,3] == 1] * (1/share_coint)
# Observations without cointegration
LR_data$weights_rebalanced[LR_data[,3] == 0] <- LR_data$weights_baseline[LR_data[,3] == 0] * (1/(1-share_coint))

## Crosscheck if everything seems fine with the produced dataframe
# Check for the datatypes of the columns
str(LR_data)

# Check for remaining NAs
sum(is.na(LR_data))
# Oil intense pairings
LR_data[LR_data[,5] == 2,]

# Remove unnecessary variables
rm(c1, c2, country, i, n_cases, n_obs)



### Perform the Logistic Regression ####
## Regressions using the absolute difference between the average oil rent shares
# Create a column containing the absolute value of the difference between the average oil rent shares
LR_data$Oil_rent_share_diff_abs <- abs(LR_data$Oil_rent_share_diff)

# LR with baseline weights
LR_base <- glm(Coint ~ 1 + Distance + Oil_rent_share_diff_abs, family = "quasibinomial", data = LR_data,
               weights = LR_data$weights_baseline)
summary(LR_base)

# LR with balanced weights
LR_balanced <- glm(Coint ~ 1 + Distance + Oil_rent_share_diff_abs, family = "quasibinomial", data = LR_data,
                   weights = LR_data$weights_balanced)
summary(LR_balanced)

# Take a look at the resulting coefficients
stargazer(LR_base, LR_balanced, type = "text", title = "Logistic Regression") # with standard errors
stargazer(LR_base, LR_balanced, type = "text", title = "Logistic Regression", report=c("vc*p")) # with P-values



### Plot the Oil price's development ####
# Inflation adjusted
grid.arrange(
ggplot(GDP_pp, 
       mapping = aes(x = Year, y = Inflation_Adjusted_Oil_Price_USD)) +
  geom_line(size = 1) +
  theme_bw() +
  xlab("Year") +
  ylab("Inflation Adjusted Oil Price in USD") +
  ggtitle("Inflation Adjusted Oil Price in USD 1960-2019"),

# Nominal
ggplot(GDP_pp, 
       mapping = aes(x = Year, y = Nominal_Oil_Price_USD)) +
  geom_line(size = 1) +
  theme_bw() +
  xlab("Year") +
  ylab("Nominal Oil Price in USD") +
  ggtitle("Nominal Oil Price in USD 1960-2019"),
ncol = 2)



### Perform Augmented Dickey Fuller test to first differences of the nominal Oil price ####
# Get the first differences of the nominal Oil price
GDP_dlog$dOil_Price <- (GDP_pp$Nominal_Oil_Price_USD - shift(GDP_pp$Nominal_Oil_Price_USD, -1))

# Perform the ADF test on the first differences of the nominal Oil price
ADF_dOil_Price <- matrix(NA, nrow = 1, ncol = 3)
ADF_dOil_Price <- as.data.frame(ADF_dOil_Price)
rownames(ADF_dOil_Price) <- "First Differences of Nominal Oil Price"
colnames(ADF_dOil_Price) <- c("Optimal number of Lags", "P-Value", "Stationarity")
ADF_dOil_Price[1,] <- full_adf(GDP_dlog$dOil_Price)

## Note: As we observe that the nominal oil price is also a unit root process of order 1 (i.e. I(1)), the EG test with the
##       log_GDP's of our remaining countries is indeed applicable

# Create a Latex table our of the ADF_dOil_Price dataframe using kableExtra
ADF_dOil_Price_k <- kable(ADF_dOil_Price, format = 'latex', digits = 3, booktabs = T,
                          linesep = "", align = c("c","c","c"))
ADF_dOil_Price_k <- kable_styling(ADF_dOil_Price_k, latex_options = c("striped"))
ADF_dOil_Price_k



### Perform EG test between all Oil intense countries of our data set and the nominal Oil Price ####
# Have a quick look at the oil intense countries
rownames(EG_GDP_log)[Oil_intense_idx]

# Create a dataframe to store the EG results
EG_Oil   <- matrix(NA, nrow = sum(Oil_intense_idx), ncol = 3)
EG_Oil   <- as.data.frame(EG_Oil)
rownames(EG_Oil) <- rownames(EG_GDP_log)[Oil_intense_idx]
colnames(EG_Oil) <- c("Cointegrated with Nominal Oil Price", "P-Value", "Avg Oil rent share in %")

# Set the initial Cointegration status to 0
EG_Oil[,1] <- 0

# Perform the actual EG tests (with nominal oil prices being the explanatory variable)
i <- 1
for(country in c(1:nrow(ADF_GDP_dlog))[Oil_intense_idx]){
  # Perform the cointegration test with d = 0 (as we are interested in cointegration in levels) and 0 lags for the residuals (corresponding to nlag = 1)
  EG <- coint.test(GDP_log[, (country + 1)], GDP_pp$Nominal_Oil_Price_USD, d = 0, nlag = 1, output = F)
  # Get the P-value of the test of type1 (as the ADF test on the residuals should only be performed with a constant, but no trend)
  # Note: 1. all values >= 0.1 are set to 0.1 and all values <= 0.01 are set to 0.01
  #       2. The P-value from the cointegration test where the adf-test on the residuals is applied with a constant but no trend is stored in EG[7]
  pval <- EG[7]
  EG_Oil[i,2] <- pval
  # Set the corresponding cell to 1 if the two variables are found to be cointegrated on the 95% level
  if(pval <= 0.05){
    EG_Oil[i,1] <- 1
  }
  # Get average oil rent shares
  EG_Oil[i,3] <- Oil_rents_avg[c(1:nrow(ADF_GDP_dlog))[Oil_intense_idx][i],1]
  i <- i + 1
}

# Remove unnecessary variables
rm(i, pval, EG)

# Create a Latex table our of the EG_Oil dataframe using kableExtra
EG_Oil_k <- kable(EG_Oil, format = 'latex', digits = 3, booktabs = T,
                  linesep = "", align = c("c","c","c"))
EG_Oil_k <- kable_styling(EG_Oil_k, latex_options = c("striped"))
EG_Oil_k



### Create a table of summary statistics ####
# Create a dataframe to store the EG results
sum_stats   <- matrix(NA, nrow = nrow(EG_GDP_log), ncol = 7)
sum_stats   <- as.data.frame(sum_stats)
rownames(sum_stats) <- rownames(EG_GDP_log)
colnames(sum_stats) <- c("Continent","Subgroup","Min Log-GDP pc","Max Log-GDP pc","# NAs in GDP pc",
                         "Avg ORS in %", "# NAs in ORS")

# Fill in the continent
for(i in Europe_idx){
  sum_stats[i,1] <- "Europe"
}
for(i in NAmerica_idx){
  sum_stats[i,1] <- "North America"
}
for(i in SAmerica_idx){
  sum_stats[i,1] <- "South America"
}
for(i in Africa_idx){
  sum_stats[i,1] <- "Africa"
}
for(i in Asia_idx){
  sum_stats[i,1] <- "Asia"
}
for(i in Australia_idx){
  sum_stats[i,1] <- "Australia"
}

## Fill in the Subgroups
# EU
for(i in EU_idx){
  sum_stats[i,2] <- "EU"
}
# G7
G7_idx <- c(5,6,9,17,18,32)
for(i in G7_idx){
  if(is.na(sum_stats[i,2])){
    sum_stats[i,2] <- "G7"
  }else{
    sum_stats[i,2] <-"G7, EU"
  }
}
# Fill "-" for countries which don not belong to any group
sum_stats[is.na(sum_stats[,2]),2] <- "-"

## Fill the remaining statistics
for(i in c(1:nrow(sum_stats))){
  # Min Log-GDP pc
  sum_stats[i,3] <- round(min(GDP_log[,(i+1)], na.rm = T),3)
  # Max Log-GDP pc
  sum_stats[i,4] <- round(max(GDP_log[,(i+1)], na.rm = T),3)
  # Number of NAs in GDP pc
  sum_stats[i,5] <- sum(is.na(GDP_log[,(i+1)]))
  # Avg ORS
  sum_stats[i,6] <- round(Oil_rents_avg[i,1],3)
  # Number of NAs in ORS
  sum_stats[i,7] <- sum(is.na(Oil[,i]))
}

# Add USA
sum_stats <- rbind(sum_stats, US_stats)
rownames(sum_stats) <- c(rownames(EG_GDP_log),"United States")
sum_stats <- sum_stats[c(1:19,41,20:40),]


# Create a Latex table our of the sum_stats dataframe using kableExtra
sum_stats_k <- kable(sum_stats, format = 'latex', digits = 3, booktabs = T,
                  linesep = "", align = c("l",rep("c",6)))
sum_stats_k <- kable_styling(sum_stats_k, latex_options = c("striped"))
sum_stats_k



### ECM models on cointegrated country pairs to test for weak exogeneity ####
# Make a function to execute a VECM with one lag
custom_ECM <- function(v1, lag1 = 1, v2, lag2 = 1){
  # Regress the latter on the former variable
  reg <- lm(v1 ~ 1 + v2)
  # Safe the residuals
  res <- residuals(reg)
  # Create lags of the residuals
  res_lag <- shift(res, -1)[-1]
  # Create the first differences of the variables
  dv1 <- shift(diff(v1), -1)[c((length(v1)+1-length(res_lag)):length(v1))]
  dv2 <- shift(diff(v2), -1)[c((length(v2)+1-length(res_lag)):length(v2))]
  # Create the first lag of the first differences of the variables
  dv1_lag1 <- shift(dv1, -1)
  dv2_lag1 <- shift(dv2, -1)
  # Create the second lag of the first differences of the variables
  dv1_lag2 <- shift(dv1, -2)
  dv2_lag2 <- shift(dv2, -2)
  # Create the third lag of the first differences of the variables
  dv1_lag3 <- shift(dv1, -3)
  dv2_lag3 <- shift(dv2, -3)
  # Create the fourth lag of the first differences of the variables
  dv1_lag4 <- shift(dv1, -4)
  dv2_lag4 <- shift(dv2, -4)
  
  # Estimate the error correction models according to the chosen lag structure
  ## 1 lag in v1
  if(lag1 <= 1){
    ## 1 lag in v2
    if(lag2 <= 1){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv2_lag1)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv2_lag1)
    }
    ## 2 lags in v2
    if(lag2 == 2){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv2_lag1 + dv2_lag2)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv2_lag1 + dv2_lag2)
    }
    ## 3 lags in v2
    if(lag2 == 3){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv2_lag1 + dv2_lag2 + dv2_lag3)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv2_lag1 + dv2_lag2 + dv2_lag3)
    }
    ## 4 lags in v2
    if(lag2 >= 4){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv2_lag1 + dv2_lag2 + dv2_lag3 + dv2_lag4)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv2_lag1 + dv2_lag2 + dv2_lag3 + dv2_lag4)
    }
  }
  ## 2 lags in v1
  if(lag1 == 2){
    ## 1 lag in v2
    if(lag2 <= 1){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv2_lag1)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv2_lag1)
    }
    ## 2 lags in v2
    if(lag2 == 2){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv2_lag1 + dv2_lag2)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv2_lag1 + dv2_lag2)
    }
    ## 3 lags in v2
    if(lag2 == 3){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv2_lag1 + dv2_lag2 + dv2_lag3)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv2_lag1 + dv2_lag2 + dv2_lag3)
    }
    ## 4 lags in v2
    if(lag2 >= 4){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv2_lag1 + dv2_lag2 + dv2_lag3 + dv2_lag4)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv2_lag1 + dv2_lag2 + dv2_lag3 + dv2_lag4)
    }
  }
  ## 3 lags in v1
  if(lag1 == 3){
    ## 1 lag in v2
    if(lag2 <= 1){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv2_lag1)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv2_lag1)
    }
    ## 2 lags in v2
    if(lag2 == 2){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv2_lag1 + dv2_lag2)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv2_lag1 + dv2_lag2)
    }
    ## 3 lags in v2
    if(lag2 == 3){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv2_lag1 + dv2_lag2 + dv2_lag3)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv2_lag1 + dv2_lag2 + dv2_lag3)
    }
    ## 4 lags in v2
    if(lag2 >= 4){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv2_lag1 + dv2_lag2 + dv2_lag3 + dv2_lag4)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv2_lag1 + dv2_lag2 + dv2_lag3 + dv2_lag4)
    }
  }
  ## 4 lags in v1
  if(lag1 >= 4){
    ## 1 lag in v2
    if(lag2 <= 1){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv1_lag4 + dv2_lag1)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv1_lag4 + dv2_lag1)
    }
    ## 2 lags in v2
    if(lag2 == 2){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv1_lag4 + dv2_lag1 + dv2_lag2)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv1_lag4 + dv2_lag1 + dv2_lag2)
    }
    ## 3 lags in v2
    if(lag2 == 3){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv1_lag4 + dv2_lag1 + dv2_lag2 + dv2_lag3)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv1_lag4 + dv2_lag1 + dv2_lag2 + dv2_lag3)
    }
    ## 4 lags in v2
    if(lag2 >= 4){
      ECM1 <- lm(dv1 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv1_lag4 + dv2_lag1 + dv2_lag2 + dv2_lag3 + dv2_lag4)
      ECM2 <- lm(dv2 ~ 1 + res_lag + dv1_lag1 + dv1_lag2 + dv1_lag3 + dv1_lag4 + dv2_lag1 + dv2_lag2 + dv2_lag3 + dv2_lag4)
    }
  }
  
  # Extract the desired values
  alpha_1     <- round(as.numeric(summary(ECM1)$coefficients[2,1]),3)
  p_1         <- round(as.numeric(summary(ECM1)$coefficients[2,4]),3)
  weak_exog_1 <- if(p_1 > 0.05){1}else{0}
  alpha_2     <- round(as.numeric(summary(ECM2)$coefficients[2,1]),3)
  p_2         <- round(as.numeric(summary(ECM2)$coefficients[2,4]),3)
  weak_exog_2 <- if(p_2 > 0.05){1}else{0}
  
  # Return the estimated coefficients
  return(c(alpha_1, p_1, weak_exog_1, alpha_2, p_2, weak_exog_2))
}

# Create a dataframe to store the results
ECM_GDP_log <- matrix(NA, nrow = sum(EG_GDP_log[!is.na(EG_GDP_log)]), ncol = 7)
ECM_GDP_log   <- as.data.frame(ECM_GDP_log)
colnames(ECM_GDP_log) <- c("Country D (dep. variable)","Country E (exp. variable)","Coefficient on D", "Coefficient on E", 
                            "P-Value D", "P-Value E", "Weak Exogeneity Code")

# Estimate the ECM models for all country pairs which were found to be cointegrated
i <- 1
for(c1 in c(1:nrow(EG_GDP_log))){
  for(c2 in c(1:nrow(EG_GDP_log))){
    if(c1 != c2){
      if(EG_GDP_log[c1,c2] == 1){
        # Get the country labels
        ECM_GDP_log[i,1] <- rownames(EG_GDP_log)[c1]
        ECM_GDP_log[i,2] <- rownames(EG_GDP_log)[c2]
        # Estimate the ECM model
        out <- custom_ECM(GDP_log[,(c1+1)], lag1 = ADF_GDP_dlog[c1,1], GDP_log[,(c2+1)], lag2 = ADF_GDP_dlog[c2,1])
        # Get the estimated coefficients
        ECM_GDP_log[i,3] <- out[1]
        ECM_GDP_log[i,4] <- out[4]
        # Get the corresponding p-values
        ECM_GDP_log[i,5] <- out[2] 
        ECM_GDP_log[i,6] <- out[5]
        # Return scheme: 
        ## Return N if neither variable is weakly exogenous
        ## Return D if the dependent variable (c1) is weakly exogenous
        ## Return E if the explanatory variable (c2) is weakly exogenous
        ## Return DE if both variables are weakly exogenous
        if(out[3] == 0 & out[6] == 0){
          ECM_GDP_log[i,7] <- "N"
        }
        if(out[3] == 1 & out[6] == 0){
          ECM_GDP_log[i,7] <- "D"
        }
        if(out[3] == 0 & out[6] == 1){
          ECM_GDP_log[i,7] <- "E"
        }
        if(out[3] == 1 & out[6] == 1){
          ECM_GDP_log[i,7] <- "DE"
        }
        i <- i + 1
      }
    }
  }
}

# Create a Latex table our of the ECM_GDP_log dataframe using kableExtra
# Observations 1-37
ECM_GDP_log_k1 <- kable(ECM_GDP_log[c(1:37),], format = 'latex', digits = 3, booktabs = T,
                        linesep = "", align = c("l","l","c","c","c","c","c"), row.names = F)
ECM_GDP_log_k1 <- kable_styling(ECM_GDP_log_k1, latex_options = c("striped"))
ECM_GDP_log_k1
# Observations 38-75
ECM_GDP_log_k2 <- kable(ECM_GDP_log[c(38:75),], format = 'latex', digits = 3, booktabs = T,
                        linesep = "", align = c("l","l","c","c","c","c","c"), row.names = F)
ECM_GDP_log_k2 <- kable_styling(ECM_GDP_log_k2, latex_options = c("striped"))
ECM_GDP_log_k2
# Observations 76-112
ECM_GDP_log_k3 <- kable(ECM_GDP_log[c(76:112),], format = 'latex', digits = 3, booktabs = T,
                        linesep = "", align = c("l","l","c","c","c","c","c"), row.names = F)
ECM_GDP_log_k3 <- kable_styling(ECM_GDP_log_k3, latex_options = c("striped"))
ECM_GDP_log_k3
# Observations 113-150
ECM_GDP_log_k4 <- kable(ECM_GDP_log[c(113:150),], format = 'latex', digits = 3, booktabs = T,
                        linesep = "", align = c("l","l","c","c","c","c","c"), row.names = F)
ECM_GDP_log_k4 <- kable_styling(ECM_GDP_log_k4, latex_options = c("striped"))
ECM_GDP_log_k4



### ECM models on oil intensive counties that are cointegrated with the nominal oil price ####
# Create a dataframe to store the results
ECM_Oil <- matrix(NA, nrow = sum(EG_Oil[,1]), ncol = 8)
ECM_Oil <- as.data.frame(ECM_Oil)
colnames(ECM_Oil) <- c("County D (dep. variable)","Nominal Oil Price (exp. variable)", "Coef. D", "Coef. E", "P-Value D", "P-Value E",
                        "Weak Exogeneity Code", "Avg Oil rent share in %")
ECM_Oil[,2] <- "Nominal Oil Price"

# Estimate the ECM models for all oil intensive counties that are found to be cointegrated with the nominal oil price
i <- 1
for(country in c(1:nrow(EG_Oil))){
  if(EG_Oil[country,1] == 1){
    # Get the country label
    ECM_Oil[i,1] <- rownames(EG_Oil)[country]
    # Estimate the ECM model
    out <- custom_ECM(GDP_log[,rownames(EG_Oil)[country]], lag1 = ADF_GDP_dlog[ECM_Oil[i,1],1], 
                       GDP_pp$Nominal_Oil_Price_USD, lag2 = ADF_dOil_Price[1,1])
    # Get the estimated coefficients
    ECM_Oil[i,3] <- out[1]
    ECM_Oil[i,4] <- out[4]
    # Get the corresponding p-values
    ECM_Oil[i,5] <- out[2] 
    ECM_Oil[i,6] <- out[5]
    # Return scheme: 
    ## Return N if neither variable is weakly exogenous
    ## Return D if the dependent variable (c1) is weakly exogenous
    ## Return E if the explanatory variable (c2) is weakly exogenous
    ## Return DE if both variables are weakly exogenous
    if(out[3] == 0 & out[6] == 0){
      ECM_Oil[i,7] <- "N"
    }
    if(out[3] == 1 & out[6] == 0){
      ECM_Oil[i,7] <- "D"
    }
    if(out[3] == 0 & out[6] == 1){
      ECM_Oil[i,7] <- "E"
    }
    if(out[3] == 1 & out[6] == 1){
      ECM_Oil[i,7] <- "DE"
    }
    # Get the avg. ORS
    ECM_Oil[i,8] <- EG_Oil[country,3]
    i <- i + 1
  }
}

# Create a Latex table our of the ECM_GDP_log dataframe using kableExtra
ECM_Oil_k <- kable(ECM_Oil, format = 'latex', digits = 3, booktabs = T,
                        linesep = "", align = c("l","l","c","c","c","c","c","c"), row.names = F)
ECM_Oil_k <- kable_styling(ECM_Oil_k, latex_options = c("striped"))
ECM_Oil_k



