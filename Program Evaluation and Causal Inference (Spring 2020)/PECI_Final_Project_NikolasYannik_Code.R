########### Read in and Reshape the Data
#### Getting started [Set the working directory to the folder "NikolasYannik_finalproject2020" where 
#### this R script and the SCM data folder are located] ####
# Set working directory
setwd("~/NikolasYannik_finalproject2020")

# Install & load required packages
pkg <- c("data.table","stargazer","lubridate","zoo","ggplot2","gridExtra",
         "dplyr","maps","tmap","reshape2","ggthemes","Synth","knitr","kableExtra")
pkg_2 <- c("tmaptools")
lapply(pkg, install.packages, character.only = FALSE, repos = "http://cran.us.r-project.org")
invisible(lapply(c(pkg,pkg_2), library, character.only = TRUE))
rm(list = ls())

# Create the proper round function
round2 = function(x, n = 0) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Read in the data
acc_raw <- fread("US_accidents_Dec19.csv", sep = ",", header = T)



#### Reshape the data ####
# Remove useless variables
acc_raw <- acc_raw[,c("Severity","Start_Time","State","Source")]

# Time variable
acc_raw$Start_Time <- ymd_hms(acc_raw$Start_Time)

# Get rid of outliers
acc_raw <- acc_raw[acc_raw$Start_Time >= ymd_hms("2016-02-08 00:00:00") & 
                     acc_raw$Start_Time < ymd_hms("2020-01-01 00:00:00")]

# Get rid of states with accidents on less than 500 dates in our time frame
acc_raw <- acc_raw[!acc_raw$State %in% c("MT","ND","SD","VT","WY"),]


# Generate the date variable
acc_raw$Date <- date(acc_raw$Start_Time)

# create a count variable to be able to sum up the number of accidents later on
acc_raw$Count <- 1

# Final clean (quality data cleaning)
acc_raw <- acc_raw[((acc_raw$Source == "Bing" & acc_raw$Date < ymd("2017-07-01"))|
                      (acc_raw$Source %in% c("Bing","MapQuest") & 
                         acc_raw$Date >= ymd("2017-07-01") &
                         acc_raw$Date < ymd("2019-06-01"))|
                      (acc_raw$Source == "MapQuest" & acc_raw$Date >= ymd("2019-06-01")))&
                     !acc_raw$State %in% c("AR","ID","NV","OR","PA","UT")]



#### Aggregate to daily data by state ####
# All accidents (regardless of severity)
str(acc_raw)
acc_tot <- acc_raw[, list(Accidents    = sum(Count), 
                          Avg_Severity = mean(Severity, na.rm = T)
),
by = list(Date, State)]



#### Add days with 0 accidents for each state for acc_tot ####
# Create a grid of dates that span the dataset
date_range <- seq(from = ymd(min(acc_tot$Date)), to = ymd(max(acc_tot$Date)), by = "day")

# Create a data frame which contains each date for every state
date_state_range <- matrix(NA, nrow = length(date_range)*length(unique(acc_tot$State)), ncol = 2)
date_state_range <- as.data.frame(date_state_range)
colnames(date_state_range) <- c("Date","State")
date_state_range[, "Date"]  <- rep(date_range, times = length(unique(acc_tot$State)))
date_state_range[, "State"] <- rep(unique(acc_tot$State), each = length(date_range))
date_state_range <- as.data.table(date_state_range)

# Check if we really could get rid of all NAs
sum(is.na(date_state_range))

# Merge this data frame with the aggregated accident data
#str(date_state_range)
#str(acc_tot)
acc_tot <- merge(date_state_range, acc_tot, by = c("State","Date"), all = T)
rm(date_range,date_state_range)

# Generate a 0 in Accidents whenever the number of accidents is NA
acc_tot[is.na(acc_tot$Accidents),"Accidents"] <- 0

# Generate a 0 in Severity whenever the severity is NA
acc_tot[is.na(acc_tot$Avg_Severity),"Avg_Severity"] <- 0

# Check for residual NAs
sum(is.na(acc_tot))



#### Generate usefull variables after merging ####
# month
acc_tot$Month <- month(acc_tot$Date)

# year
acc_tot$Year <- year(acc_tot$Date)

# year-month combination
acc_tot$Year_Month <- as.yearmon(paste(acc_tot$Year, acc_tot$Month), "%Y %m")

# day of week
acc_tot$Day_of_Week <- weekdays(acc_tot$Date, T)



## Aggregate to daily data by state, Add days with 0 accidents for each state and generate post-meging variables for acc_sev2-4 ####
# Aggregate accidents with severity = 1
acc_sev1 <- acc_raw[acc_raw$Severity == 1, 
                    list(Accidents   = sum(Count)
                    ),
                    by = list(Date, State)]

# Aggregate accidents with severity = 2
acc_sev2 <- acc_raw[acc_raw$Severity == 2, 
                    list(Accidents   = sum(Count)
                    ),
                    by = list(Date, State)]

# Aggregate accidents with severity = 3
acc_sev3 <- acc_raw[acc_raw$Severity == 3, 
                    list(Accidents   = sum(Count)
                    ),
                    by = list(Date, State)]

# Aggregate accidents with severity = 4
acc_sev4 <- acc_raw[acc_raw$Severity == 4, 
                    list(Accidents   = sum(Count)
                    ),
                    by = list(Date, State)]

# Merge this data frame with the aggregated accident data
acc_sev2 <- merge(date_state_range, acc_sev2, by = c("State","Date"), all = T)
acc_sev3 <- merge(date_state_range, acc_sev3, by = c("State","Date"), all = T)
acc_sev4 <- merge(date_state_range, acc_sev4, by = c("State","Date"), all = T)

# Generate a 0 in Accidents whenever the number of accidents is NA
acc_sev2[is.na(acc_sev2$Accidents),"Accidents"] <- 0
acc_sev3[is.na(acc_sev3$Accidents),"Accidents"] <- 0
acc_sev4[is.na(acc_sev4$Accidents),"Accidents"] <- 0


## Generate usefull variables after merging
# month
acc_sev2$Month <- month(acc_sev2$Date)
acc_sev3$Month <- month(acc_sev3$Date)
acc_sev4$Month <- month(acc_sev4$Date)

# year
acc_sev2$Year <- year(acc_sev2$Date)
acc_sev3$Year <- year(acc_sev3$Date)
acc_sev4$Year <- year(acc_sev4$Date)

# year-month combination
acc_sev2$Year_Month <- as.yearmon(paste(acc_sev2$Year, acc_sev2$Month), "%Y %m")
acc_sev3$Year_Month <- as.yearmon(paste(acc_sev3$Year, acc_sev3$Month), "%Y %m")
acc_sev4$Year_Month <- as.yearmon(paste(acc_sev4$Year, acc_sev4$Month), "%Y %m")

# day of week
acc_sev2$Day_of_Week <- weekdays(acc_sev2$Date, T)
acc_sev3$Day_of_Week <- weekdays(acc_sev3$Date, T)
acc_sev4$Day_of_Week <- weekdays(acc_sev4$Date, T)

# Check for residual NAs
sum(is.na(acc_sev2))
sum(is.na(acc_sev3))
sum(is.na(acc_sev4))



###### SCM (Variable Adding) [CODE] ####
## Important: either run this block (line 180-537) or the SCM (Variable Adding) [QUICK] block (lines 538-759). But not both in a row! 
## continue than with row 760
## Note: This is the code we used to extract and interpolate all predictor variables from their yearly or monthly format to our daily 
## dataset. As this code might take some time to run, we also provide a quick version for loading all these interpolated variables from 
## datasets we extracted after applying this code (see SCM (Variable Adding) [QUICK]).
#### Adding daily vehicle miles travelled (VMT) per state (in million miles) [CODE] ####
# Read in the data
VMT <- fread("SCM data/Daily_VMT.csv", sep = ";", header = T)

# Create the column
acc_tot$VMT <- NA

# Insert the corresponding values
VMT <- as.data.frame(VMT)
for(s in c(1:nrow(VMT))){
  for(p in c(1:(length(VMT)-1))){
    acc_tot$VMT[acc_tot$State == as.character(VMT[s,1]) &
                  acc_tot$Month == month(dmy(colnames(VMT)[-1][p])) &
                  acc_tot$Year == year(dmy(colnames(VMT)[-1][p]))] <- VMT[s,(p+1)]
  }
}
rm(p,s,VMT)

# Check for completeness within the constructed column
sum(is.na(acc_tot$VMT))

# Extract a file to conveniently add the VMT later on (containing EVERY state)
#write.csv(acc_tot[,c("State","Date","VMT")], "D:\\Dokumente\\Yannik\\UZH\\20FS\\Program Evaluation and Causal Inference\\Final Project\\US traffic accident\\SCM data\\Daily_VMT_cleaned.csv", row.names = F)



#### Adding rural and urban road mileage per state [CODE] ####
# Read in the data
rural <- fread("SCM data/rural_road_mileage.csv", sep = ";", header = T)
urban <- fread("SCM data/urban_road_mileage.csv", sep = ";", header = T)

# Create the columns
acc_tot$Rural_Mileage <- NA
acc_tot$Urban_Mileage <- NA

# Insert the corresponding values for rural mileage
rural <- as.data.frame(rural)

for(s in c(1:nrow(rural))){
  for(p in c(2:(length(rural)-1))){
    period_range <- seq(from = dmy(colnames(rural)[p]), to = dmy(colnames(rural)[(p+1)]), by = "day")
    acc_tot$Rural_Mileage[acc_tot$State == rural[s,1] &
                            acc_tot$Date > dmy(colnames(rural)[p]) &
                            acc_tot$Date <= dmy(colnames(rural)[(p+1)])] <-
      rural[s,p]+(((rural[s,(p+1)]-rural[s,p])/(length(period_range)-1))*
                    as.numeric(acc_tot$Date[acc_tot$State == rural[s,1] &
                                              acc_tot$Date > dmy(colnames(rural)[p]) &
                                              acc_tot$Date <= dmy(colnames(rural)[(p+1)])] - dmy(colnames(rural)[p])))
  }
}

for(s in c(1:nrow(rural))){
  period_range <- seq(from = dmy(colnames(rural)[5]), to = dmy(colnames(rural)[(6)]), by = "day")
  acc_tot$Rural_Mileage[acc_tot$State == rural[s,1] &
                          acc_tot$Date > dmy(colnames(rural)[(6)])] <-
    rural[s,5]+(((rural[s,6]-rural[s,5])/(length(period_range)-1))*
                  as.numeric(acc_tot$Date[acc_tot$State == rural[s,1] &
                                            acc_tot$Date > dmy(colnames(rural)[(6)])] - dmy(colnames(rural)[5])))
}

# Insert the corresponding values for urban mileage
urban <- as.data.frame(urban)

for(s in c(1:nrow(urban))){
  for(p in c(2:(length(urban)-1))){
    period_range <- seq(from = dmy(colnames(urban)[p]), to = dmy(colnames(urban)[(p+1)]), by = "day")
    acc_tot$Urban_Mileage[acc_tot$State == urban[s,1] &
                            acc_tot$Date > dmy(colnames(urban)[p]) &
                            acc_tot$Date <= dmy(colnames(urban)[(p+1)])] <-
      urban[s,p]+(((urban[s,(p+1)]-urban[s,p])/(length(period_range)-1))*
                    as.numeric(acc_tot$Date[acc_tot$State == urban[s,1] &
                                              acc_tot$Date > dmy(colnames(urban)[p]) &
                                              acc_tot$Date <= dmy(colnames(urban)[(p+1)])] - dmy(colnames(urban)[p])))
  }
}

for(s in c(1:nrow(urban))){
  period_range <- seq(from = dmy(colnames(urban)[5]), to = dmy(colnames(urban)[(6)]), by = "day")
  acc_tot$Urban_Mileage[acc_tot$State == urban[s,1] &
                          acc_tot$Date > dmy(colnames(urban)[(6)])] <-
    urban[s,5]+(((urban[s,6]-urban[s,5])/(length(period_range)-1))*
                  as.numeric(acc_tot$Date[acc_tot$State == urban[s,1] &
                                            acc_tot$Date > dmy(colnames(urban)[(6)])] - dmy(colnames(urban)[5])))
}
rm(p,s,period_range,rural,urban)

# Check for completeness within the constructed column
sum(is.na(acc_tot$Rural_Mileage))
sum(is.na(acc_tot$Urban_Mileage))

# Extract a files to conveniently add the road lane mileage later on (containing EVERY state)
#write.csv(acc_tot[,c("State","Date","Rural_Mileage")], "D:\\Dokumente\\Yannik\\UZH\\20FS\\Program Evaluation and Causal Inference\\Final Project\\US traffic accident\\SCM data\\rural_road_mileage_cleaned.csv", row.names = F)
#write.csv(acc_tot[,c("State","Date","Urban_Mileage")], "D:\\Dokumente\\Yannik\\UZH\\20FS\\Program Evaluation and Causal Inference\\Final Project\\US traffic accident\\SCM data\\urban_road_mileage_cleaned.csv", row.names = F)



#### Adding number of registered motor-vehicles (RMV) per state [CODE] ####
# Read in the data
RMV <- fread("SCM data/rmv.csv", sep = ";", header = T)

# Create the columns
acc_tot$RMV <- NA

# Insert the corresponding values for RMV
RMV <- as.data.frame(RMV)
RMV[,2] <- as.numeric(RMV[,2])
RMV[,3] <- as.numeric(RMV[,3])
RMV[,4] <- as.numeric(RMV[,4])
RMV[,5] <- as.numeric(RMV[,5])
RMV[,6] <- as.numeric(RMV[,6])

for(s in c(1:nrow(RMV))){
  for(p in c(2:(length(RMV)-1))){
    period_range <- seq(from = dmy(colnames(RMV)[p]), to = dmy(colnames(RMV)[(p+1)]), by = "day")
    acc_tot$RMV[acc_tot$State == RMV[s,1] &
                  acc_tot$Date > dmy(colnames(RMV)[p]) &
                  acc_tot$Date <= dmy(colnames(RMV)[(p+1)])] <-
      RMV[s,p]+(((RMV[s,(p+1)]-RMV[s,p])/(length(period_range)-1))*
                  as.numeric(acc_tot$Date[acc_tot$State == RMV[s,1] &
                                            acc_tot$Date > dmy(colnames(RMV)[p]) &
                                            acc_tot$Date <= dmy(colnames(RMV)[(p+1)])] - dmy(colnames(RMV)[p])))
  }
}
rm(p,s,period_range,RMV)

# Check for completeness within the constructed column
sum(is.na(acc_tot$RMV))

# Extract a files to conveniently add the road lane mileage later on (containing EVERY state)
#write.csv(acc_tot[,c("State","Date","RMV")], "D:\\Dokumente\\Yannik\\UZH\\20FS\\Program Evaluation and Causal Inference\\Final Project\\US traffic accident\\SCM data\\rmv_cleaned.csv", row.names = F)


#### Adding population per state [CODE] ####
# Read in the data
POP <- fread("SCM data/population.csv", sep = ";", header = T)

# Create the columns
acc_tot$Population <- NA

# Insert the corresponding values for the population
POP <- as.data.frame(POP)
POP[,2] <- as.numeric(POP[,2])
POP[,3] <- as.numeric(POP[,3])
POP[,4] <- as.numeric(POP[,4])
POP[,5] <- as.numeric(POP[,5])
POP[,6] <- as.numeric(POP[,6])
POP[,7] <- as.numeric(POP[,7])

for(s in c(1:nrow(POP))){
  for(p in c(2:(length(POP)-1))){
    period_range <- seq(from = dmy(colnames(POP)[p]), to = dmy(colnames(POP)[(p+1)]), by = "day")
    acc_tot$Population[acc_tot$State == POP[s,1] &
                         acc_tot$Date > dmy(colnames(POP)[p]) &
                         acc_tot$Date <= dmy(colnames(POP)[(p+1)])] <-
      POP[s,p]+(((POP[s,(p+1)]-POP[s,p])/(length(period_range)-1))*
                  as.numeric(acc_tot$Date[acc_tot$State == POP[s,1] &
                                            acc_tot$Date > dmy(colnames(POP)[p]) &
                                            acc_tot$Date <= dmy(colnames(POP)[(p+1)])] - dmy(colnames(POP)[p])))
  }
}
rm(p,s,period_range,POP)

# Check for completeness within the constructed column
sum(is.na(acc_tot$Population))

# Extract a files to conveniently add the population later on (containing EVERY state)
#write.csv(acc_tot[,c("State","Date","Population")], "D:\\Dokumente\\Yannik\\UZH\\20FS\\Program Evaluation and Causal Inference\\Final Project\\US traffic accident\\SCM data\\population_cleaned.csv", row.names = F)



#### Adding land area (size) of every state (in square miles) [CODE/QUICK] ####
# Read in the data
size <- fread("SCM data/land_area.csv", sep = ";", header = T)

# Create the columns
acc_tot$Land_Area <- NA

# Insert the corresponding values for the state size
size <- as.data.frame(size)
size[,2] <- as.numeric(size[,2])
for(i in c(1:nrow(size))){
  acc_tot$Land_Area[acc_tot$State == size$State[i]] <- size$Size[i]
}
rm(i,size)

# Check for completeness within the constructed column
sum(is.na(acc_tot$Land_Area))



#### Creating population density variable (in persons per square mile) ####
acc_tot$Pop_Density <- acc_tot$Population/acc_tot$Land_Area



#### Adding daily alcohol sales per state (in gallons of pure ethanol (alcohol) per state) [CODE/QUICK] ####
# Load the data
alc_cons <- fread("SCM data/alcohol/alcohol_consumption.csv", sep = ",", header = T)

# Reshape the data
alc_cons$Year[alc_cons$Year == 2015] <- 2019
alc_cons <- alc_cons[alc_cons$Type_of_beverage == 4, c("Year","State","Gallons_of_ethanol")]
alc_cons$Gallons_of_ethanol[alc_cons$Year == 2019] <-
  alc_cons$Gallons_of_ethanol[alc_cons$Year == 2018] +
  (alc_cons$Gallons_of_ethanol[alc_cons$Year == 2018] - alc_cons$Gallons_of_ethanol[alc_cons$Year == 2017])
alc_cons$Alc_Cons <- NA
alc_cons$Alc_Cons[alc_cons$Year == 2016] <- alc_cons$Gallons_of_ethanol[alc_cons$Year == 2016] / 366
alc_cons$Alc_Cons[!alc_cons$Year == 2016] <- alc_cons$Gallons_of_ethanol[!alc_cons$Year == 2016] / 365
alc_cons <- alc_cons[,-"Gallons_of_ethanol"]

# Merge the daily alcohol consumption per state with the accident data
acc_tot <- merge(acc_tot, alc_cons, by = c("State","Year"), all.x = T)
rm(alc_cons)



#### Creating the required dummy variables ####
# Creating a dummy which indicates whether recreational marijuana is legal in a state
acc_tot$Rec_Mar_Legal <- 0

acc_tot$Rec_Mar_Legal[acc_tot$State %in% c("CO","DC","OR","VT","WA")] <- 1

acc_tot$Rec_Mar_Legal[acc_tot$State == "CA" &
                        acc_tot$Date >= ymd("2016-11-09")] <- 1

acc_tot$Rec_Mar_Legal[acc_tot$State == "ME" &
                        acc_tot$Date >= ymd("2017-01-30")] <- 1

acc_tot$Rec_Mar_Legal[acc_tot$State == "MA" &
                        acc_tot$Date >= ymd("2016-12-15")] <- 1

acc_tot$Rec_Mar_Legal[acc_tot$State == "MI" &
                        acc_tot$Date >= ymd("2018-12-06")] <- 1

acc_tot$Rec_Mar_Legal[acc_tot$State == "NV" &
                        acc_tot$Date >= ymd("2017-01-01")] <- 1


# Creating a dummy which indicates whether the commercial sale of rereational marijuana is legal in a state
acc_tot$Sale_Mar_Legal <- 0

acc_tot$Sale_Mar_Legal[acc_tot$State %in% c("CO","OR","WA")] <- 1

acc_tot$Sale_Mar_Legal[acc_tot$State == "CA" &
                         acc_tot$Date >= ymd("2018-01-01")] <- 1

acc_tot$Sale_Mar_Legal[acc_tot$State == "ME" &
                         acc_tot$Date >= ymd("2018-02-01")] <- 1

acc_tot$Sale_Mar_Legal[acc_tot$State == "MA" &
                         acc_tot$Date >= ymd("2018-07-01")] <- 1

acc_tot$Sale_Mar_Legal[acc_tot$State == "MI" &
                         acc_tot$Date >= ymd("2018-12-06")] <- 1

acc_tot$Sale_Mar_Legal[acc_tot$State == "NV" &
                         acc_tot$Date >= ymd("2017-07-01")] <- 1


# Creating a dummy which indicates whether medical marijuana is legal in a state
acc_tot$Med_Mar_Legal <- 0

acc_tot$Med_Mar_Legal[acc_tot$State %in% c("AZ","CA","CO","CT","DE",
                                           "DC","FL","IL","LA","ME",
                                           "MD","MA","MI","MN","MT",
                                           "NV","NH","NJ","NM","NY",
                                           "OR","RI","VT","WA")]      <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "AR" &
                        acc_tot$Date >= ymd("2016-11-09")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "MO" &
                        acc_tot$Date >= ymd("2018-12-06")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "ND" &
                        acc_tot$Date >= ymd("2016-12-08")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "OH" &
                        acc_tot$Date >= ymd("2016-06-08")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "OK" &
                        acc_tot$Date >= ymd("2018-07-12")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "PA" &
                        acc_tot$Date >= ymd("2016-04-17")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "UT" &
                        acc_tot$Date >= ymd("2018-12-03")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "WV" &
                        acc_tot$Date >= ymd("2019-07-01")] <- 1


# Creating a dummy which indicates whether a state shares a boarder with a state or country where
## recreational marijuana can be bought
acc_tot$Sale_Mar_Legal_Neighbour <- 0

acc_tot$Sale_Mar_Legal_Neighbour[acc_tot$State %in% c("AZ","CA","CO","ID","KS",
                                                      "ME","MA","MI","MN","MT",
                                                      "NE","NV","NH","NM","NY",
                                                      "ND","OH","OK","OR","PA",
                                                      "UT","VT","WA","WI","WY")]      <- 1

acc_tot$Sale_Mar_Legal_Neighbour[acc_tot$State == "CT" &
                                   acc_tot$Date >= ymd("2018-07-01")] <- 1

acc_tot$Sale_Mar_Legal_Neighbour[acc_tot$State == "IN" &
                                   acc_tot$Date >= ymd("2018-12-06")] <- 1

acc_tot$Sale_Mar_Legal_Neighbour[acc_tot$State == "RI" &
                                   acc_tot$Date >= ymd("2018-07-01")] <- 1


# Creating a dummy which indicates whether the data contains data from Bing at a date
acc_tot$SRC_Bing <- 0
acc_tot$SRC_Bing[acc_tot$Date < ymd("2019-06-01")] <- 1

# Creating a dummy which indicates whether the data contains data from MapQuest at a date
acc_tot$SRC_MQ <- 0
acc_tot$SRC_MQ[acc_tot$Date >= ymd("2017-07-01")] <- 1



#### Creating a second data table with all variables per capita (PC) ####
# Get the unchanged variables
acc_tot_PC <- acc_tot[,c("State","Year","Date","Avg_Severity","Month","Year_Month","Day_of_Week",
                         "Rec_Mar_Legal","Sale_Mar_Legal","Med_Mar_Legal","Sale_Mar_Legal_Neighbour",
                         "SRC_Bing","SRC_MQ")]

# Creating accidents per capita and accidents per RMV (accident per million units)
acc_tot_PC$Acc_PC      <- acc_tot$Accidents*1e6/acc_tot$Population
acc_tot_PC$Acc_per_RMV <- acc_tot$Accidents*1e6/acc_tot$RMV

# VMT per capita
acc_tot_PC$VMT_PC <- acc_tot$VMT*1e6/acc_tot$Population

# Rural and Urban road mileage per capita (miles per million population)
acc_tot_PC$Rural_Mileage_PC <- acc_tot$Rural_Mileage*1e6/acc_tot$Population
acc_tot_PC$Urban_Mileage_PC <- acc_tot$Urban_Mileage*1e6/acc_tot$Population

# RMV per capita
acc_tot_PC$RMV_PC <- acc_tot$RMV/acc_tot$Population

# Get population and population density
acc_tot_PC$Population  <- acc_tot$Population
acc_tot_PC$Pop_Density <- acc_tot$Pop_Density

# Alcohol consumption per capita
acc_tot_PC$Alc_Cons_PC <- acc_tot$Alc_Cons*1e6/acc_tot$Population



###### SCM (Variable Adding) [QUICK] ####
#### Adding daily vehicle miles travelled (VMT) per state (in million miles) [QUICK] ####
# Use the extracted file to merge it with the accidents dataset
VMT <- fread("SCM data/Daily_VMT_cleaned.csv", sep = ",", header = T)
VMT$Date <- ymd(VMT$Date)
acc_tot <- merge(acc_tot, VMT, by = c("State","Date"), all.x = T)
rm(VMT)



#### Adding rural and urban road mileage per state [QUICK] ####
# Use the extracted file to merge it with the accidents dataset
rural <- fread("SCM data/rural_road_mileage_cleaned.csv", sep = ",", header = T)
urban <- fread("SCM data/urban_road_mileage_cleaned.csv", sep = ",", header = T)
rural$Date <- ymd(rural$Date)
urban$Date <- ymd(urban$Date)
acc_tot <- merge(acc_tot, rural, by = c("State","Date"), all.x = T)
acc_tot <- merge(acc_tot, urban, by = c("State","Date"), all.x = T)
rm(rural,urban)



#### Adding number of registered motor-vehicles (RMV) per state [QUICK] ####
# Use the extracted file to merge it with the accidents dataset
RMV <- fread("SCM data/rmv_cleaned.csv", sep = ",", header = T)
RMV$Date <- ymd(RMV$Date)
acc_tot <- merge(acc_tot, RMV, by = c("State","Date"), all.x = T)
rm(RMV)



#### Adding population per state [QUICK] ####
# Use the extracted file to merge it with the accidents dataset
POP <- fread("SCM data/population_cleaned.csv", sep = ",", header = T)
POP$Date <- ymd(POP$Date)
acc_tot <- merge(acc_tot, POP, by = c("State","Date"), all.x = T)
rm(POP)



#### Adding land area (size) of every state (in square miles) [CODE/QUICK] ####
# Read in the data
size <- fread("SCM data/land_area.csv", sep = ";", header = T)

# Create the columns
acc_tot$Land_Area <- NA

# Insert the corresponding values for the state size
size <- as.data.frame(size)
size[,2] <- as.numeric(size[,2])
for(i in c(1:nrow(size))){
  acc_tot$Land_Area[acc_tot$State == size$State[i]] <- size$Size[i]
}
rm(i,size)

# Check for completeness within the constructed column
sum(is.na(acc_tot$Land_Area))



#### Creating population density variable (in persons per square mile) ####
acc_tot$Pop_Density <- acc_tot$Population/acc_tot$Land_Area



#### Adding daily alcohol sales per state (in gallons of pure ethanol (alcohol) per state) [CODE/QUICK] ####
# Load the data
alc_cons <- fread("SCM data/alcohol/alcohol_consumption.csv", sep = ",", header = T)

# Reshape the data
alc_cons$Year[alc_cons$Year == 2015] <- 2019
alc_cons <- alc_cons[alc_cons$Type_of_beverage == 4, c("Year","State","Gallons_of_ethanol")]
alc_cons$Gallons_of_ethanol[alc_cons$Year == 2019] <-
  alc_cons$Gallons_of_ethanol[alc_cons$Year == 2018] +
  (alc_cons$Gallons_of_ethanol[alc_cons$Year == 2018] - alc_cons$Gallons_of_ethanol[alc_cons$Year == 2017])
alc_cons$Alc_Cons <- NA
alc_cons$Alc_Cons[alc_cons$Year == 2016] <- alc_cons$Gallons_of_ethanol[alc_cons$Year == 2016] / 366
alc_cons$Alc_Cons[!alc_cons$Year == 2016] <- alc_cons$Gallons_of_ethanol[!alc_cons$Year == 2016] / 365
alc_cons <- alc_cons[,-"Gallons_of_ethanol"]

# Merge the daily alcohol consumption per state with the accident data
acc_tot <- merge(acc_tot, alc_cons, by = c("State","Year"), all.x = T)
rm(alc_cons)



#### Creating the required dummy variables ####
# Creating a dummy which indicates whether recreational marijuana is legal in a state
acc_tot$Rec_Mar_Legal <- 0

acc_tot$Rec_Mar_Legal[acc_tot$State %in% c("CO","DC","OR","VT","WA")] <- 1

acc_tot$Rec_Mar_Legal[acc_tot$State == "CA" &
                        acc_tot$Date >= ymd("2016-11-09")] <- 1

acc_tot$Rec_Mar_Legal[acc_tot$State == "ME" &
                        acc_tot$Date >= ymd("2017-01-30")] <- 1

acc_tot$Rec_Mar_Legal[acc_tot$State == "MA" &
                        acc_tot$Date >= ymd("2016-12-15")] <- 1

acc_tot$Rec_Mar_Legal[acc_tot$State == "MI" &
                        acc_tot$Date >= ymd("2018-12-06")] <- 1

acc_tot$Rec_Mar_Legal[acc_tot$State == "NV" &
                        acc_tot$Date >= ymd("2017-01-01")] <- 1


# Creating a dummy which indicates whether the commercial sale of rereational marijuana is legal in a state
acc_tot$Sale_Mar_Legal <- 0

acc_tot$Sale_Mar_Legal[acc_tot$State %in% c("CO","OR","WA")] <- 1

acc_tot$Sale_Mar_Legal[acc_tot$State == "CA" &
                         acc_tot$Date >= ymd("2018-01-01")] <- 1

acc_tot$Sale_Mar_Legal[acc_tot$State == "ME" &
                         acc_tot$Date >= ymd("2018-02-01")] <- 1

acc_tot$Sale_Mar_Legal[acc_tot$State == "MA" &
                         acc_tot$Date >= ymd("2018-07-01")] <- 1

acc_tot$Sale_Mar_Legal[acc_tot$State == "MI" &
                         acc_tot$Date >= ymd("2018-12-06")] <- 1

acc_tot$Sale_Mar_Legal[acc_tot$State == "NV" &
                         acc_tot$Date >= ymd("2017-07-01")] <- 1


# Creating a dummy which indicates whether medical marijuana is legal in a state
acc_tot$Med_Mar_Legal <- 0

acc_tot$Med_Mar_Legal[acc_tot$State %in% c("AZ","CA","CO","CT","DE",
                                           "DC","FL","IL","LA","ME",
                                           "MD","MA","MI","MN","MT",
                                           "NV","NH","NJ","NM","NY",
                                           "OR","RI","VT","WA")]      <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "AR" &
                        acc_tot$Date >= ymd("2016-11-09")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "MO" &
                        acc_tot$Date >= ymd("2018-12-06")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "ND" &
                        acc_tot$Date >= ymd("2016-12-08")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "OH" &
                        acc_tot$Date >= ymd("2016-06-08")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "OK" &
                        acc_tot$Date >= ymd("2018-07-12")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "PA" &
                        acc_tot$Date >= ymd("2016-04-17")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "UT" &
                        acc_tot$Date >= ymd("2018-12-03")] <- 1

acc_tot$Med_Mar_Legal[acc_tot$State == "WV" &
                        acc_tot$Date >= ymd("2019-07-01")] <- 1


# Creating a dummy which indicates whether a state shares a boarder with a state or country where
## recreational marijuana can be bought
acc_tot$Sale_Mar_Legal_Neighbour <- 0

acc_tot$Sale_Mar_Legal_Neighbour[acc_tot$State %in% c("AZ","CA","CO","ID","KS",
                                                      "ME","MA","MI","MN","MT",
                                                      "NE","NV","NH","NM","NY",
                                                      "ND","OH","OK","OR","PA",
                                                      "UT","VT","WA","WI","WY")]      <- 1

acc_tot$Sale_Mar_Legal_Neighbour[acc_tot$State == "CT" &
                                   acc_tot$Date >= ymd("2018-07-01")] <- 1

acc_tot$Sale_Mar_Legal_Neighbour[acc_tot$State == "IN" &
                                   acc_tot$Date >= ymd("2018-12-06")] <- 1

acc_tot$Sale_Mar_Legal_Neighbour[acc_tot$State == "RI" &
                                   acc_tot$Date >= ymd("2018-07-01")] <- 1


# Creating a dummy which indicates whether the data contains data from Bing at a date
acc_tot$SRC_Bing <- 0
acc_tot$SRC_Bing[acc_tot$Date < ymd("2019-06-01")] <- 1

# Creating a dummy which indicates whether the data contains data from MapQuest at a date
acc_tot$SRC_MQ <- 0
acc_tot$SRC_MQ[acc_tot$Date >= ymd("2017-07-01")] <- 1



#### Creating a second data table with all variables per capita (PC) ####
# Get the unchanged variables
acc_tot_PC <- acc_tot[,c("State","Year","Date","Avg_Severity","Month","Year_Month","Day_of_Week",
                         "Rec_Mar_Legal","Sale_Mar_Legal","Med_Mar_Legal","Sale_Mar_Legal_Neighbour",
                         "SRC_Bing","SRC_MQ")]

# Creating accidents per capita and accidents per RMV (accident per million units)
acc_tot_PC$Acc_PC      <- acc_tot$Accidents*1e6/acc_tot$Population
acc_tot_PC$Acc_per_RMV <- acc_tot$Accidents*1e6/acc_tot$RMV

# VMT per capita
acc_tot_PC$VMT_PC <- acc_tot$VMT*1e6/acc_tot$Population

# Rural and Urban road mileage per capita (miles per million population)
acc_tot_PC$Rural_Mileage_PC <- acc_tot$Rural_Mileage*1e6/acc_tot$Population
acc_tot_PC$Urban_Mileage_PC <- acc_tot$Urban_Mileage*1e6/acc_tot$Population

# RMV per capita
acc_tot_PC$RMV_PC <- acc_tot$RMV/acc_tot$Population

# Get population and population density
acc_tot_PC$Population  <- acc_tot$Population
acc_tot_PC$Pop_Density <- acc_tot$Pop_Density

# Alcohol consumption per capita
acc_tot_PC$Alc_Cons_PC <- acc_tot$Alc_Cons*1e6/acc_tot$Population



###### Read in the cleaned file [Accidents] ####
acc_tot <- fread("US_accidents_cleaned.csv", sep = ",", header = T)
acc_tot$Date <- ymd(acc_tot$Date)
acc_tot$Year_Month <- as.yearmon(paste(acc_tot$Year, acc_tot$Month), "%Y %m")



###### Read in the cleaned per capita file [Accidents PC] ####
acc_tot_PC <- fread("US_accidents_PC_cleaned.csv", sep = ",", header = T)
acc_tot_PC$Date <- ymd(acc_tot_PC$Date)
acc_tot_PC$Year_Month <- as.yearmon(paste(acc_tot_PC$Year, acc_tot_PC$Month), "%Y %m")



#### Create a monthly dataset [Monthly Accidents PC] ####
acc_tot_mon_PC <- acc_tot_PC[, list(Acc_PC           = sum(Acc_PC),
                                    Acc_per_RMV      = sum(Acc_per_RMV),
                                    VMT_PC           = sum(VMT_PC),
                                    Rural_Mileage_PC = mean(Rural_Mileage_PC),
                                    Urban_Mileage_PC = mean(Urban_Mileage_PC),
                                    RMV_PC           = mean(RMV_PC),
                                    Population       = mean(Population),
                                    Pop_Density      = mean(Pop_Density),
                                    Alc_Cons_PC      = sum(Alc_Cons_PC),
                                    Year             = head(Year,1),
                                    Avg_Severity     = mean(Avg_Severity),
                                    Month            = head(Month,1),
                                    Rec_Mar_Legal    = round2(mean(Rec_Mar_Legal)),
                                    Sale_Mar_Legal   = round2(mean(Sale_Mar_Legal)),
                                    Med_Mar_Legal    = round2(mean(Med_Mar_Legal)),
                                    Sale_Mar_Legal_Neighbour = round2(mean(Sale_Mar_Legal_Neighbour)),
                                    SRC_Bing         = head(SRC_Bing,1),
                                    SRC_MQ           = head(SRC_MQ,1)),
                             by = list(State, Year_Month)]



###### Read in the cleaned monthly per capita file [Accidents PC] ####
acc_tot_mon_PC <- fread("US_accidents_monthly_PC_cleaned.csv", sep = ",", header = T)
acc_tot_mon_PC$Year_Month <- as.yearmon(paste(acc_tot_mon_PC$Year, acc_tot_mon_PC$Month), "%Y %m")

########### Create Plots
#### Quality data cleaning plots (Outlier & Measurement Error Detection by State and Source) ####
## Read in the data
acc_raw <- fread("US_accidents_Dec19.csv", sep = ",", header = T)

## Global data reshaping [Prior to quality data cleaning]
# Remove useless variables
acc_raw <- acc_raw[,c("Severity","Start_Time","State","Source")]
# Time variable
acc_raw$Start_Time <- ymd_hms(acc_raw$Start_Time)
# Get rid of outliers
acc_raw <- acc_raw[acc_raw$Start_Time >= ymd_hms("2016-02-08 00:00:00") & 
                     acc_raw$Start_Time < ymd_hms("2020-01-01 00:00:00")]
# Get rid of states with accidents on less than 500 dates in our time frame
acc_raw <- acc_raw[!acc_raw$State %in% c("MT","ND","SD","VT","WY"),]
# Generate the date variable
acc_raw$Date <- date(acc_raw$Start_Time)


## Plot 1: Bing-MapQuest density by state comparison
grid.arrange(
  ggplot(acc_raw[acc_raw$Source == "Bing"], 
         aes(Date, col = State)) +
    geom_density(size = 1) +
    ggtitle("Density by State: Bing") +
    theme_bw(),
  ggplot(acc_raw[acc_raw$Source == "MapQuest"], 
         aes(Date, col = State)) +
    geom_density(size = 1) +
    ggtitle("Density by State: MapQuest") +
    theme_bw(), nrow = 2)

## Plot 2: Total density by state (incl. period restriction by source)
acc_raw_clean <- acc_raw[(acc_raw$Source == "Bing" & acc_raw$Date < ymd("2017-07-01"))|
                           (acc_raw$Source %in% c("Bing","MapQuest") & 
                              acc_raw$Date >= ymd("2017-07-01") &
                              acc_raw$Date < ymd("2019-06-01"))|
                           (acc_raw$Source == "MapQuest" & acc_raw$Date >= ymd("2019-06-01"))]

# Cleaned Plot 2
ggplot(acc_raw_clean, aes(Date, col = State)) +
  geom_density(size = 1) +
  ggtitle("Total Density by State (incl. period restriction by source)") +
  theme_bw()


## Plot 3: Total density by state (incl. period restriction by source & outlier restriction by state)
acc_raw_clean <- acc_raw[((acc_raw$Source == "Bing" & acc_raw$Date < ymd("2017-07-01"))|
                            (acc_raw$Source %in% c("Bing","MapQuest") & 
                               acc_raw$Date >= ymd("2017-07-01") &
                               acc_raw$Date < ymd("2019-06-01"))|
                            (acc_raw$Source == "MapQuest" & acc_raw$Date >= ymd("2019-06-01")))&
                           !acc_raw$State %in% c("AR","ID","NV","OR","PA","UT")]

# Cleaned Plot 3
ggplot(acc_raw_clean, aes(Date, col = State)) +
  geom_density(size = 1) +
  ggtitle("Total Density by State (incl. period restriction by source & outlier restriction by state)") +
  theme_bw()


#### Nik's Plots ####
# Read in the data
USAccidents <- fread("US_accidents_PC_cleaned.csv", sep = ",", header = T)
USAccidents <- as.data.frame(USAccidents)
USAccidents$Date <- ymd(USAccidents$Date)
USAccidents$ID <- as.numeric(as.factor(USAccidents$State))
USAccidents$Timeline <- as.numeric(as.factor(USAccidents$Date))


### Analysis of parallel trends for accidents Per Capita and predictor variables #### 

## Analysis of the relationship between California and the US average

# This code corresponds to the part "Data and Methodology" from our project. It summarizes the relationships over time
# for our variable of interest as well as the predictor variables. We plot both the monthly as well as daily relationship

# We start by plotting the relationship for daily trends for accidents

# Take the daily averages of accidents per capita for the US average, except California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Control_Mean_Acc = mean(Acc_PC[State != "CA"]))
# Take the daily average of accidents per capita for California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Treat_Mean_Acc = Acc_PC[State == "CA"])

# Take the daily averages of accidents per Registered Vehicle - RMV for the US average, except California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Control_Mean_Acc_RMV = mean(Acc_per_RMV[State != "CA"]))
# Take the daily average of accidents per Registered Vehicle - RMV for California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Treat_Mean_Acc_RMV = Acc_per_RMV[State == "CA"])

# Take the daily average of Vehicle Miles Travelled - VMT for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Control_Mean_VMT_PC = mean(VMT_PC[State != "CA"]))
# Take the daily average of Vehicle Miles Travelled - VMT for California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Treat_Mean_VMT_PC = mean(VMT_PC[State == "CA"]))

# Take the daily average of Rural Mileage per Capita for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Control_Mean_Rural_Mileage_PC = mean(Rural_Mileage_PC[State != "CA"]))
# Take the daily average of Rural Mileage per Capita for California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Treat_Mean_Rural_Mileage_PC = mean(Rural_Mileage_PC[State == "CA"]))

# Take the daily average of Urban Mileage per Capita for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Control_Mean_Urban_Mileage_PC = mean(Urban_Mileage_PC[State != "CA"]))
# Take the daily average of Urban Mileage per Capita for California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Treat_Mean_Urban_Mileage_PC = mean(Urban_Mileage_PC[State == "CA"]))

# Take the daily average of Registered Vehihcles per Capita for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Control_Mean_RMV = mean(RMV_PC[State != "CA"]))
# Take the daily average of Registered Vehihcles per Capita for California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Treat_Mean_RMV = mean(RMV_PC[State == "CA"]))

# Take the daily average of Population Density for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Control_Mean_Pop_Dens = mean(Pop_Density[State != "CA"]))
# Take the daily average of Population Density for California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Treat_Mean_Pop_Dens = mean(Pop_Density[State == "CA"]))

# Take the daily average of Alcohol Consumption for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Control_Mean_Alc = mean(Alc_Cons_PC[State != "CA"]))
# Take the daily average of Alcohol Consumption for California
USAccidents <- USAccidents %>%
  group_by(Date) %>%
  mutate(Treat_Mean_Alc = mean(Alc_Cons_PC[State == "CA"]))

# Then we take the monthly averages of the relationship between California and the US Average

# Change the data into a date structure 
USAccidents$Year_Month <-  as.yearmon(paste(USAccidents$Year, USAccidents$Month), "%Y %m")
str(USAccidents)

# Monthly accidents per capita for California
USAccidents <- USAccidents %>% 
  group_by(Month, Year) %>%
  mutate(Monthly_Accidents_PC_CA = mean(Acc_PC[State == "CA"]))
# Monthly accidents per capita for all states, except California
USAccidents <- USAccidents %>% 
  group_by(Month, Year) %>%
  mutate(Monthly_Accidents_PC = mean(Acc_PC[State != "CA"]))

# Monthly accidents per registered vehicle for US Average
USAccidents <- USAccidents %>% 
  group_by(Month, Year) %>%
  mutate(Monthly_Accidents_per_RMV_CA = mean(Acc_per_RMV[State == "CA"]))
# Monthly accidents per registered vehicle for US Average
USAccidents <- USAccidents %>% 
  group_by(Month, Year) %>%
  mutate(Monthly_Accidents_per_RMV = mean(Acc_per_RMV[State != "CA"]))

# Take the monthly averages of accidents per Registered Vehicle - RMV for the US average, except California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Control_Mean_Acc_RMV_Monthly = mean(Acc_per_RMV[State != "CA"]))
# Take the monthly average of accidents per Registered Vehicle - RMV for California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Treat_Mean_Acc_RMV_Monthly = mean(Acc_per_RMV[State == "CA"]))

# Take the monthly average of Vehicle Miles Travelled - VMT for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Control_Mean_VMT_PC_Monthly = mean(VMT_PC[State != "CA"]))
# Take the monthly average of Vehicle Miles Travelled - VMT for California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Treat_Mean_VMT_PC_Monthly = mean(VMT_PC[State == "CA"]))

# Take the monthly average of Rural Mileage per Capita for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Control_Mean_Rural_Mileage_PC_Monthly = mean(Rural_Mileage_PC[State != "CA"]))
# Take the monthly average of Rural Mileage per Capita for California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Treat_Mean_Rural_Mileage_PC_Monthly = mean(Rural_Mileage_PC[State == "CA"]))

# Take the monthly average of Urban Mileage per Capita for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Control_Mean_Urban_Mileage_PC_Monthly = mean(Urban_Mileage_PC[State != "CA"]))
# Take the monthly average of Urban Mileage per Capita for California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Treat_Mean_Urban_Mileage_PC_Monthly = mean(Urban_Mileage_PC[State == "CA"]))

# Take the monthly average of Registered Vehihcles per Capita for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Control_Mean_RMV_Monthly = mean(RMV_PC[State != "CA"]))
# Take the monthly average of Registered Vehihcles per Capita for California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Treat_Mean_RMV_Monthly = mean(RMV_PC[State == "CA"]))

# Take the monthly average of Population Density for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Control_Mean_Pop_Dens_Monthly = mean(Pop_Density[State != "CA"]))
# Take the monthly average of Population Density for California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Treat_Mean_Pop_Dens_Monthly = mean(Pop_Density[State == "CA"]))

# Take the monthly average of Alcohol Consumption for the US avereage, except California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Control_Mean_Alc_Monthly = mean(Alc_Cons_PC[State != "CA"]))
# Take the monthly average of Alcohol Consumption for California
USAccidents <- USAccidents %>%
  group_by(Year_Month) %>%
  mutate(Treat_Mean_Alc_Monthly = mean(Alc_Cons_PC[State == "CA"]))
# Get the timeline trend of the US average and California for daily data through the melt function + ggplot

# For daily trends

USAccidents_timeline_daily <- melt(subset(USAccidents, State %in% c("CA", "NY")), id = c(1, 3, 25:40))
levels(USAccidents_timeline_daily$State) <- c(levels(USAccidents_timeline_daily$State), "US Average")
USAccidents_timeline_daily$State[USAccidents_timeline_daily$State == "NY"] <- "US Average"
USAccidents_timeline_daily <- USAccidents_timeline_daily[!duplicated(USAccidents_timeline_daily[c(1,2)]), ]


# Plots for Accidents PC & Accidents per RMV
ggplot(USAccidents_timeline_daily) + 
  geom_line(aes(Date, Control_Mean_Acc, color = "US Average")) + 
  geom_line(aes(Date, Treat_Mean_Acc, color = "California")) +
  theme_bw()
ggplot(USAccidents_timeline_daily) + 
  geom_line(aes(Date, Control_Mean_Acc_RMV, color = "US Average")) + 
  geom_line(aes(Date, Treat_Mean_Acc_RMV, color = "California")) +
  theme_bw()

# Plots for VMT PC 
ggplot(USAccidents_timeline_daily) + 
  geom_line(aes(Date, Control_Mean_VMT_PC, color = "US Average")) + 
  geom_line(aes(Date, Treat_Mean_VMT_PC, color = "California")) +
  theme_bw()

# Plots for Rural Mileage PC 
ggplot(USAccidents_timeline_daily) + 
  geom_line(aes(Date, Control_Mean_Rural_Mileage_PC, color = "US Average")) + 
  geom_line(aes(Date, Treat_Mean_Rural_Mileage_PC, color = "California")) +
  theme_bw()

# Plots for Urban Mileage PC
ggplot(USAccidents_timeline_daily) + 
  geom_line(aes(Date, Control_Mean_Urban_Mileage_PC, color = "US Average")) + 
  geom_line(aes(Date, Treat_Mean_Urban_Mileage_PC, color = "California")) +
  theme_bw()

# Plots for RMV
ggplot(USAccidents_timeline_daily) + 
  geom_line(aes(Date, Control_Mean_RMV, color = "US Average")) + 
  geom_line(aes(Date, Treat_Mean_RMV, color = "California")) +
  theme_bw()

# Plots for Population Density
ggplot(USAccidents_timeline_daily) + 
  geom_line(aes(Date, Control_Mean_Pop_Dens, color = "US Average")) + 
  geom_line(aes(Date, Treat_Mean_Pop_Dens, color = "California")) +
  theme_bw()

# Plots for Alcohol Consumption
ggplot(USAccidents_timeline_daily) + 
  geom_line(aes(Date, Control_Mean_Alc, color = "US Average")) + 
  geom_line(aes(Date, Treat_Mean_Alc, color = "California")) +
  theme_bw()


# For monthly trends
USAccidents_Monthly <- melt(subset(USAccidents, State %in% c("CA", "NY")), id = c(1, 6, 41:58))
levels(USAccidents_Monthly$State) <- c(levels(USAccidents_Monthly$State), "US Average")
USAccidents_Monthly$State[USAccidents_Monthly$State == "NY"] <- "US Average"
USAccidents_Monthly <- USAccidents_Monthly[!duplicated(USAccidents_Monthly[c(1,3)]), ]

# Plots for Accidents PC & Accidents per RMV
ggplot(USAccidents_Monthly) + 
  geom_line(aes(Year_Month, Monthly_Accidents_PC, color = "US Average")) + 
  geom_line(aes(Year_Month, Monthly_Accidents_PC_CA, color = "California")) +
  theme_bw()
ggplot(USAccidents_Monthly) + 
  geom_line(aes(Year_Month, Control_Mean_Acc_RMV_Monthly, color = "US Average")) + 
  geom_line(aes(Year_Month, Monthly_Accidents_per_RMV_CA, color = "California")) +
  theme_bw()

# Plots for VMT PC 
ggplot(USAccidents_Monthly) + 
  geom_line(aes(Year_Month, Control_Mean_VMT_PC_Monthly, color = "US Average")) + 
  geom_line(aes(Year_Month, Treat_Mean_VMT_PC_Monthly, color = "California")) +
  theme_bw()

# Plots for Rural Mileage PC 
ggplot(USAccidents_Monthly) + 
  geom_line(aes(Year_Month, Control_Mean_Rural_Mileage_PC_Monthly, color = "US Average")) + 
  geom_line(aes(Year_Month, Treat_Mean_Rural_Mileage_PC_Monthly, color = "California")) +
  theme_bw()

# Plots for Urban Mileage PC
ggplot(USAccidents_Monthly) + 
  geom_line(aes(Year_Month, Control_Mean_Urban_Mileage_PC_Monthly, color = "US Average")) + 
  geom_line(aes(Year_Month, Treat_Mean_Urban_Mileage_PC_Monthly, color = "California")) +
  theme_bw()

# Plots for RMV
ggplot(USAccidents_Monthly) + 
  geom_line(aes(Year_Month, Control_Mean_RMV_Monthly, color = "US Average")) + 
  geom_line(aes(Year_Month, Treat_Mean_RMV_Monthly, color = "California")) +
  theme_bw()

# Plots for Population Density
ggplot(USAccidents_Monthly) + 
  geom_line(aes(Year_Month, Control_Mean_Pop_Dens_Monthly, color = "US Average")) + 
  geom_line(aes(Year_Month, Treat_Mean_Pop_Dens_Monthly, color = "California")) +
  theme_bw()

# Plots for Alcohol Consumption
ggplot(USAccidents_Monthly) + 
  geom_line(aes(Year_Month, Control_Mean_Alc_Monthly, color = "US Average")) + 
  geom_line(aes(Year_Month, Treat_Mean_Alc_Monthly, color = "California")) +
  theme_bw()



#### Some data visualization plots ####
### Read in the cleaned files
acc_tot <- fread("US_accidents_cleaned.csv", sep = ",", header = T)
acc_tot$Date <- ymd(acc_tot$Date)
acc_tot$Year_Month <- as.yearmon(paste(acc_tot$Year, acc_tot$Month), "%Y %m")

acc_tot_PC <- fread("US_accidents_PC_cleaned.csv", sep = ",", header = T)
acc_tot_PC$Date <- ymd(acc_tot_PC$Date)
acc_tot_PC$Year_Month <- as.yearmon(paste(acc_tot_PC$Year, acc_tot_PC$Month), "%Y %m")

grid.arrange(
  # Monthly accidents for all state
  ggplot(acc_tot[, list(Accidents = sum(Accidents)), 
                 by = list(Year_Month, State)], 
         aes(Year_Month, Accidents, col = State)) +
    geom_line() +
    theme_bw(),
  
  # Monthly accidents PC for all states
  ggplot(acc_tot_PC[, list(Acc_PC = sum(Acc_PC)), 
                    by = list(Year_Month, State)]
         , aes(Year_Month, Acc_PC, col = State)) +
    geom_line() +
    theme_bw(), nrow = 2)


########### SCM
#### Synthetic Control Method Part 1 - Preparation of the dataset #### 

# Calculation of the SCM method for the analysis of marijuana consumption on traffic accidents

# This part corresponds to the "Synthetic Control Method" part of our "Data and Methodology" section. 

# We introdcute the SCM and perform the necessary steps in order to receive a synthetic control of our treatment state for
# the observational period. 

# We have a balanced dataset according to our pre-defined characteristics which we already cleaned
USAccidents <- fread("US_accidents_PC_cleaned.csv", sep = ",", header = T)
USAccidents$Date <- ymd(USAccidents$Date)

# Now we set the filter for the relevant time period

USAccidents <- USAccidents[USAccidents$State %in% 
                             c(unique(USAccidents$State[USAccidents$Date == ymd("2019-12-31") & 
                                                          USAccidents$Rec_Mar_Legal != 1]),"CA"),]
USAccidents$ID <- as.numeric(as.factor(USAccidents$State))
USAccidents$Timeline <- as.numeric(as.factor(USAccidents$Date))

# Now we set the filter for the relevant time period

Enactment_date <- 276
Observational_Period  <- seq(Enactment_date - 275, Enactment_date + 1147)

# We then set the reference variable to California and the treatment day to the Enactment date of legalisation

Cali_ref <- USAccidents$Acc_PC[USAccidents$State == "CA" & USAccidents$Timeline == Enactment_date]


# As we mentioned, we need to clear out several states since we assume them to suffer from large measurement error, which is in-
# consistent with the rest of the data set. Consequently, we set a simulation set including only the members we select with regard
# to the Measurement Error 

Cali_synth <- filter(USAccidents) %>%
  group_by(ID) %>%
  summarise(complete.panel = ifelse(length(Date) == length(Observational_Period), 1, 0),
            missing.Acc = sum(is.na(Acc_PC)), 
            missing.VMT = sum(is.na(VMT_PC)),
            missing.rural = sum(is.na(Rural_Mileage_PC)), 
            missing.urban = sum(is.na(Urban_Mileage_PC)),
            missing.RMV = sum(is.na(RMV_PC)),
            missing.PopDens = sum(is.na(Pop_Density)),
            missing.Alc = sum(is.na(Alc_Cons_PC))) 

Cali_control = Cali_synth %>%
  filter(complete.panel == 1 & missing.Acc <= 1 & missing.VMT <= 1 &
           missing.rural <= 1 & missing.urban <= 1 & missing.RMV <= 1 & 
           missing.PopDens <= 1 & missing.Alc <= 1 & ID != 3) %>%
  select(ID) %>%
  unlist()
names(Cali_control) = NULL
Cali_control

USAccidents_real <- USAccidents[which(USAccidents$ID %in% c(Cali_control, unique(USAccidents$ID[USAccidents$State == "CA"]))), ]


match.criteria <- c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC")


USAccidents_real$ID<- as.numeric(as.factor(USAccidents_real$ID))
USAccidents_real$State <- as.character(USAccidents_real$State)
USAccidents_real$Date <- ymd(USAccidents_real$Date)
Observational_Period <- as.numeric(Observational_Period)
USAccidents_real <- as.data.frame(USAccidents_real)


### Synthetic Control Method Part 2 - Calculating the Synthetic version of California ####

SCM_synth <- dataprep(
  foo = USAccidents_real,
  predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
  time.predictors.prior = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  dependent = "Acc_PC",
  unit.variable = "ID",
  unit.names.variable = "State",
  time.variable = "Timeline",
  treatment.identifier = 3,
  controls.identifier = c(1:2, 4:32), # If for all states: adjust to 4:38
  time.optimize.ssr = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  time.plot = Observational_Period
)

# Apply synth() to those data and inspect matching results via tables
SCM_synth_new <- synth(SCM_synth, optimxmethod="All")
SCM_synth_tables <- synth.tab(dataprep.res = SCM_synth, synth.res = SCM_synth_new)
predictor_means <- SCM_synth_tables$tab.pred  # inspect balance
weights <- as.data.frame(SCM_synth_tables$tab.w) %>% arrange(desc(w.weights))

# Analyze the pre trend features for the Treated vs Synthetic vs the Sample Mean of the variables 
SCM_synth_tables$tab.pred

# Analyze the weight structure 
names(weights)[1] <- "Weights"
names(weights)[2] <- "State"
names(weights)[3] <- "Unit Numbers"
weights





# Plot path

path.case <- SCM_synth$Y1plot
path.synth <- SCM_synth$Y0plot %*% SCM_synth_new$solution.w 

trends_acc <- path.plot(synth.res = SCM_synth_new,
                        dataprep.res = SCM_synth,
                        Xlab = "Days",
                        Ylab = "Accidents per million",
                        Legend = c("Cali", "Synthetic Cali"),
                        tr.intake = 276)

# Plot differences
gap <- SCM_synth$Y1plot - (SCM_synth$Y0plot %*% SCM_synth_new$solution.w)

gap_acc <- plot(x = Observational_Period, y = gap, 
                xlab = "Days", ylab = "Gap in Accidents per million population", lwd = 2) +
  segments(x0 = min(Observational_Period), x1 = max(Observational_Period), y0=0, y1=0, lwd=2, lty=2, col="gray25") +
  abline(v=Enactment_date, col="red", lty=2) + 
  text(x = 550, y = -18, "reform day - 11th November 2016", pos = 2, cex = 0.8) +
  axis(1, tick=FALSE) +
  # Plot the series
  lines(x=Observational_Period, y=gap, type="l", lwd=2)




#### Synthetic Control Method Part 3 - Calculate the Synthetic Controls for other States #### 

# Now, we can create the synthetic trends for our different states

# Function to apply; same specs as above for treated case
f.placebo.data <- function(ID) {
  
  placebo.dat <- dataprep(
    # drop the UK from data set for this
    foo = filter(USAccidents_real, State !="CA"),
    predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
    time.predictors.prior = seq(from = min(Observational_Period), to = Enactment_date - 1),
    dependent = "Acc_PC",
    unit.variable = "ID",
    unit.names.variable = "State",
    time.variable = "Timeline",
    treatment.identifier = ID,
    controls.identifier = Cali_control[which(Cali_control != ID)],
    time.optimize.ssr = seq(from = min(Observational_Period), to = Enactment_date - 1),
    time.plot = Observational_Period)
  
  return(placebo.dat)
  
}

placebo_data_list <- lapply(Cali_control, f.placebo.data)

# All states now are the equivalent to the "SCM_synth_new" variable for California

placebo_results_1 <- synth(placebo_data_list[[1]]) 
placebo_results_2 <- synth(placebo_data_list[[2]]) 
placebo_results_3 <- synth(placebo_data_list[[3]]) 
placebo_results_4 <- synth(placebo_data_list[[4]]) 
placebo_results_5 <- synth(placebo_data_list[[5]]) 
placebo_results_6 <- synth(placebo_data_list[[6]]) 
placebo_results_7 <- synth(placebo_data_list[[7]]) 
placebo_results_8 <- synth(placebo_data_list[[8]]) 
placebo_results_9 <- synth(placebo_data_list[[9]]) 
placebo_results_10 <- synth(placebo_data_list[[10]]) 
placebo_results_11 <- synth(placebo_data_list[[11]]) 
placebo_results_12 <- synth(placebo_data_list[[12]])
placebo_results_13 <- synth(placebo_data_list[[13]]) 
placebo_results_14 <- synth(placebo_data_list[[14]]) 
placebo_results_15 <- synth(placebo_data_list[[15]]) 
placebo_results_16 <- synth(placebo_data_list[[16]])
placebo_results_17 <- synth(placebo_data_list[[17]])
placebo_results_18 <- synth(placebo_data_list[[18]])
placebo_results_19 <- synth(placebo_data_list[[19]])
placebo_results_20 <- synth(placebo_data_list[[20]])
placebo_results_21 <- synth(placebo_data_list[[21]])
placebo_results_22 <- synth(placebo_data_list[[22]])
placebo_results_23 <- synth(placebo_data_list[[23]])
placebo_results_24 <- synth(placebo_data_list[[24]])
placebo_results_25 <- synth(placebo_data_list[[25]])
placebo_results_26 <- synth(placebo_data_list[[26]])
placebo_results_27 <- synth(placebo_data_list[[27]])
placebo_results_28 <- synth(placebo_data_list[[28]])
placebo_results_29 <- synth(placebo_data_list[[29]])
placebo_results_30 <- synth(placebo_data_list[[30]])
placebo_results_31 <- synth(placebo_data_list[[31]])


# Function to plot lines from placebo results by number
placebo.line.plot <- function(i) {
  solution.i <- eval(parse(text=paste0("placebo_results_", i, "$solution.w")))
  gap <- placebo_data_list[[i]]$Y1plot - 
    (placebo_data_list[[i]]$Y0plot %*% solution.i)
  lines(x = Observational_Period, y = gap, col = "grey70")
}

## Make plot

# Find gap
gap <- SCM_synth$Y1plot - (SCM_synth$Y0plot %*% SCM_synth_new$solution.w)

# Make vector of placebo tests to plot
placebos <- c(1:37)

# Set up plot using the main time series
plot(x=Observational_Period, y=gap, type="n", xaxs="i", yaxs="i", ylim=c(-20,60),
     xlab="year", ylab="Differences per capita rel. to synthetic control", lwd=2)

# Plot the placebo time series
for (k in placebos) {
  placebo.line.plot(k)
}

# Horizontal Line at 0
abline(h=0, lwd=2, lty=2)

# Vertical line at the point of treatment
abline(v=Enactment_date, col="red", lty=2)

# Replot the main time series
lines(x=Observational_Period, y=gap, type="l", lwd=2) 

# Legend
legend(x="bottomleft", bty="n", legend=c("California", "placebo tests"), 
       col=c("black","grey70"), lwd=c(2,1), cex=0.8)

#### Synthetic Control Method Part 4 - Quantify the RMSE ratio for California ####

# Now, we can quantify the selected state with the pre and post treatment RSME ratios

# Define function for calculating the RMSE
rmse <- function(x,y){
  sqrt(mean((x - y)^2))
}

# Define vector for pre/post-intervention subsetting

pre_intervention <- Observational_Period < Enactment_date

## California

# Extract the weights for synthetic Cali
Cali_weights <- SCM_synth_new$solution.w

# Calculate the outcome for synthetic Cali using matrix multiplication
synthetic_Cali <- as.numeric(SCM_synth$Y0plot %*% Cali_weights)

# Extract the true outcome for Cali
true_Cali <- USAccidents_real[USAccidents_real$ID == 3,]$Acc_PC

# Calculate the RMSE for the pre-intervention period for Cali

pre_rmse_CA <- rmse(x = true_Cali[pre_intervention], y = synthetic_Cali[pre_intervention])

# Calculate the RMSE for the post-intervention period for Cali

post_rmse_CA <- rmse(x = true_Cali[!pre_intervention], y = synthetic_Cali[!pre_intervention])

CA_RMSE <- post_rmse_CA/pre_rmse_CA

#### Synthetic Control Method Part 5 - Quantify RMSE ratios for other states #### 

# Now we can quantify the RMSE ratios for all other states 

## Exclude Cali from data so that it never enters the donor pool

emu_donors <- USAccidents_real[USAccidents_real$ID != 3,]
emu_donors$ID2 <- as.numeric(as.factor(emu_donors$State))


## Alabama

pre_intervention <- Observational_Period < 276
AL_weights <- placebo_results_1$solution.w
synthetic_AL <- as.numeric(placebo_data_list[[1]]$Y0plot %*% AL_weights)
true_AL <- emu_donors[emu_donors$ID2 == 1,]$Acc_PC
pre_rmse_AL <- rmse(x = true_AL[pre_intervention], y = synthetic_AL[pre_intervention])
post_rmse_AL <- rmse(x = true_AL[!pre_intervention], y = synthetic_AL[!pre_intervention])
AL_RMSE <- post_rmse_AL/pre_rmse_AL

## Arizona

pre_intervention <- Observational_Period < 276
AZ_weights <- placebo_results_2$solution.w
synthetic_AZ <- as.numeric(placebo_data_list[[2]]$Y0plot %*% AZ_weights)
true_AZ <- emu_donors[emu_donors$ID2 == 2,]$Acc_PC
pre_rmse_AZ <- rmse(x = true_AZ[pre_intervention], y = synthetic_AZ[pre_intervention])
post_rmse_AZ <- rmse(x = true_AZ[!pre_intervention], y = synthetic_AZ[!pre_intervention])
AZ_RMSE <- post_rmse_AZ/pre_rmse_AZ

## Connecticut

pre_intervention <- Observational_Period < 276
CT_weights <- placebo_results_3$solution.w
synthetic_CT <- as.numeric(placebo_data_list[[3]]$Y0plot %*% CT_weights)
true_CT <- emu_donors[emu_donors$ID2 == 3,]$Acc_PC
pre_rmse_CT <- rmse(x = true_CT[pre_intervention], y = synthetic_CT[pre_intervention])
post_rmse_CT <- rmse(x = true_CT[!pre_intervention], y = synthetic_CT[!pre_intervention])
CT_RMSE <- post_rmse_CT/pre_rmse_CT

## Delaware

pre_intervention <- Observational_Period < 276
DE_weights <- placebo_results_4$solution.w
synthetic_DE <- as.numeric(placebo_data_list[[4]]$Y0plot %*% DE_weights)
true_DE <- emu_donors[emu_donors$ID2 == 4,]$Acc_PC
pre_rmse_DE <- rmse(x = true_DE[pre_intervention], y = synthetic_DE[pre_intervention])
post_rmse_DE <- rmse(x = true_DE[!pre_intervention], y = synthetic_DE[!pre_intervention])
DE_RMSE <- post_rmse_DE/pre_rmse_DE

## Florida

pre_intervention <- Observational_Period < 276
FL_weights <- placebo_results_5$solution.w
synthetic_FL <- as.numeric(placebo_data_list[[5]]$Y0plot %*% FL_weights)
true_FL <- emu_donors[emu_donors$ID2 == 5,]$Acc_PC
pre_rmse_FL <- rmse(x = true_FL[pre_intervention], y = synthetic_FL[pre_intervention])
post_rmse_FL <- rmse(x = true_FL[!pre_intervention], y = synthetic_FL[!pre_intervention])
FL_RMSE <- post_rmse_FL/pre_rmse_FL

## Georgia

pre_intervention <- Observational_Period < 276
GA_weights <- placebo_results_6$solution.w
synthetic_GA <- as.numeric(placebo_data_list[[6]]$Y0plot %*% GA_weights)
true_GA <- emu_donors[emu_donors$ID2 == 6,]$Acc_PC
pre_rmse_GA <- rmse(x = true_GA[pre_intervention], y = synthetic_GA[pre_intervention])
post_rmse_GA <- rmse(x = true_GA[!pre_intervention], y = synthetic_GA[!pre_intervention])
GA_RMSE <- post_rmse_GA/pre_rmse_GA

## Iowa

pre_intervention <- Observational_Period < 276
IA_weights <- placebo_results_7$solution.w
synthetic_IA <- as.numeric(placebo_data_list[[7]]$Y0plot %*% IA_weights)
true_IA <- emu_donors[emu_donors$ID2 == 7,]$Acc_PC
pre_rmse_IA <- rmse(x = true_IA[pre_intervention], y = synthetic_IA[pre_intervention])
post_rmse_IA <- rmse(x = true_IA[!pre_intervention], y = synthetic_IA[!pre_intervention])
IA_RMSE <- post_rmse_IA/pre_rmse_IA

## Illionis

pre_intervention <- Observational_Period < 276
IL_weights <- placebo_results_8$solution.w
synthetic_IL <- as.numeric(placebo_data_list[[8]]$Y0plot %*% IL_weights)
true_IL <- emu_donors[emu_donors$ID2 == 8,]$Acc_PC
pre_rmse_IL <- rmse(x = true_IL[pre_intervention], y = synthetic_IL[pre_intervention])
post_rmse_IL <- rmse(x = true_IL[!pre_intervention], y = synthetic_IL[!pre_intervention])
IL_RMSE <- post_rmse_IL/pre_rmse_IL


## Indiana

pre_intervention <- Observational_Period < 276
IN_weights <- placebo_results_9$solution.w
synthetic_IN <- as.numeric(placebo_data_list[[9]]$Y0plot %*% IN_weights)
true_IN <- emu_donors[emu_donors$ID2 == 9,]$Acc_PC
pre_rmse_IN <- rmse(x = true_IN[pre_intervention], y = synthetic_IN[pre_intervention])
post_rmse_IN <- rmse(x = true_IN[!pre_intervention], y = synthetic_IN[!pre_intervention])
IN_RMSE <- post_rmse_IN/pre_rmse_IN

## Kansas

pre_intervention <- Observational_Period < 276
KS_weights <- placebo_results_10$solution.w
synthetic_KS <- as.numeric(placebo_data_list[[10]]$Y0plot %*% KS_weights)
true_KS <- emu_donors[emu_donors$ID2 == 10,]$Acc_PC
pre_rmse_KS <- rmse(x = true_KS[pre_intervention], y = synthetic_KS[pre_intervention])
post_rmse_KS <- rmse(x = true_KS[!pre_intervention], y = synthetic_KS[!pre_intervention])
KS_RMSE <- post_rmse_KS/pre_rmse_KS

## Kentucky

pre_intervention <- Observational_Period < 276
KY_weights <- placebo_results_11$solution.w
synthetic_KY <- as.numeric(placebo_data_list[[11]]$Y0plot %*% KY_weights)
true_KY <- emu_donors[emu_donors$ID2 == 11,]$Acc_PC
pre_rmse_KY <- rmse(x = true_KY[pre_intervention], y = synthetic_KY[pre_intervention])
post_rmse_KY <- rmse(x = true_KY[!pre_intervention], y = synthetic_KY[!pre_intervention])
KY_RMSE <- post_rmse_KY/pre_rmse_KY

## Louisiana

pre_intervention <- Observational_Period < 276
LA_weights <- placebo_results_12$solution.w
synthetic_LA <- as.numeric(placebo_data_list[[12]]$Y0plot %*% LA_weights)
true_LA <- emu_donors[emu_donors$ID2 == 12,]$Acc_PC
pre_rmse_LA <- rmse(x = true_LA[pre_intervention], y = synthetic_LA[pre_intervention])
post_rmse_LA <- rmse(x = true_LA[!pre_intervention], y = synthetic_LA[!pre_intervention])
LA_RMSE <- post_rmse_LA/pre_rmse_LA

## Maryland

pre_intervention <- Observational_Period < 276
MD_weights <- placebo_results_13$solution.w
synthetic_MD <- as.numeric(placebo_data_list[[13]]$Y0plot %*% MD_weights)
true_MD <- emu_donors[emu_donors$ID2 == 13,]$Acc_PC
pre_rmse_MD <- rmse(x = true_MD[pre_intervention], y = synthetic_MD[pre_intervention])
post_rmse_MD <- rmse(x = true_MD[!pre_intervention], y = synthetic_MD[!pre_intervention])
MD_RMSE <- post_rmse_MD/pre_rmse_MD


## Minnesota 

pre_intervention <- Observational_Period < 276
MN_weights <- placebo_results_14$solution.w
synthetic_MN <- as.numeric(placebo_data_list[[14]]$Y0plot %*% MN_weights)
true_MN <- emu_donors[emu_donors$ID2 == 14,]$Acc_PC
pre_rmse_MN <- rmse(x = true_MN[pre_intervention], y = synthetic_MN[pre_intervention])
post_rmse_MN <- rmse(x = true_MN[!pre_intervention], y = synthetic_MN[!pre_intervention])
MN_RMSE <- post_rmse_MN/pre_rmse_MN

## Montana

pre_intervention <- Observational_Period < 276
MO_weights <- placebo_results_15$solution.w
synthetic_MO <- as.numeric(placebo_data_list[[15]]$Y0plot %*% MO_weights)
true_MO <- emu_donors[emu_donors$ID2 == 15,]$Acc_PC
pre_rmse_MO <- rmse(x = true_MO[pre_intervention], y = synthetic_MO[pre_intervention])
post_rmse_MO <- rmse(x = true_MO[!pre_intervention], y = synthetic_MO[!pre_intervention])
MO_RMSE <- post_rmse_MO/pre_rmse_MO

## Mississippi

pre_intervention <- Observational_Period < 276
MS_weights <- placebo_results_16$solution.w
synthetic_MS <- as.numeric(placebo_data_list[[16]]$Y0plot %*% MS_weights)
true_MS <- emu_donors[emu_donors$ID2 == 16,]$Acc_PC
pre_rmse_MS <- rmse(x = true_MS[pre_intervention], y = synthetic_MS[pre_intervention])
post_rmse_MS <- rmse(x = true_MS[!pre_intervention], y = synthetic_MS[!pre_intervention])
MS_RMSE <- post_rmse_MS/pre_rmse_MS

## North Carolina

pre_intervention <- Observational_Period < 276
NC_weights <- placebo_results_17$solution.w
synthetic_NC <- as.numeric(placebo_data_list[[17]]$Y0plot %*% NC_weights)
true_NC <- emu_donors[emu_donors$ID2 == 17,]$Acc_PC
pre_rmse_NC <- rmse(x = true_NC[pre_intervention], y = synthetic_NC[pre_intervention])
post_rmse_NC <- rmse(x = true_NC[!pre_intervention], y = synthetic_NC[!pre_intervention])
NC_RMSE <- post_rmse_NC/pre_rmse_NC

## Nebraska

pre_intervention <- Observational_Period < 276
NE_weights <- placebo_results_18$solution.w
synthetic_NE <- as.numeric(placebo_data_list[[18]]$Y0plot %*% NE_weights)
true_NE <- emu_donors[emu_donors$ID2 == 18,]$Acc_PC
pre_rmse_NE <- rmse(x = true_NE[pre_intervention], y = synthetic_NE[pre_intervention])
post_rmse_NE <- rmse(x = true_NE[!pre_intervention], y = synthetic_NE[!pre_intervention])
NE_RMSE <- post_rmse_NE/pre_rmse_NE

## New Hampshire

pre_intervention <- Observational_Period < 276
NH_weights <- placebo_results_19$solution.w
synthetic_NH <- as.numeric(placebo_data_list[[19]]$Y0plot %*% NH_weights)
true_NH <- emu_donors[emu_donors$ID2 == 19,]$Acc_PC
pre_rmse_NH <- rmse(x = true_NH[pre_intervention], y = synthetic_NH[pre_intervention])
post_rmse_NH <- rmse(x = true_NH[!pre_intervention], y = synthetic_NH[!pre_intervention])
NH_RMSE <- post_rmse_NH/pre_rmse_NH


## New Jersey

pre_intervention <- Observational_Period < 276
NJ_weights <- placebo_results_20$solution.w
synthetic_NJ <- as.numeric(placebo_data_list[[20]]$Y0plot %*% NJ_weights)
true_NJ <- emu_donors[emu_donors$ID2 == 20,]$Acc_PC
pre_rmse_NJ <- rmse(x = true_NJ[pre_intervention], y = synthetic_NJ[pre_intervention])
post_rmse_NJ <- rmse(x = true_NJ[!pre_intervention], y = synthetic_NJ[!pre_intervention])
NJ_RMSE <- post_rmse_NJ/pre_rmse_NJ

## New Mexico

pre_intervention <- Observational_Period < 276
NM_weights <- placebo_results_21$solution.w
synthetic_NM <- as.numeric(placebo_data_list[[21]]$Y0plot %*% NM_weights)
true_NM <- emu_donors[emu_donors$ID2 == 21,]$Acc_PC
pre_rmse_NM <- rmse(x = true_NM[pre_intervention], y = synthetic_NM[pre_intervention])
post_rmse_NM <- rmse(x = true_NM[!pre_intervention], y = synthetic_NM[!pre_intervention])
NM_RMSE <- post_rmse_NM/pre_rmse_NM

## New York

pre_intervention <- Observational_Period < 276
NY_weights <- placebo_results_22$solution.w
synthetic_NY <- as.numeric(placebo_data_list[[22]]$Y0plot %*% NY_weights)
true_NY <- emu_donors[emu_donors$ID2 == 22,]$Acc_PC
pre_rmse_NY <- rmse(x = true_NY[pre_intervention], y = synthetic_NY[pre_intervention])
post_rmse_NY <- rmse(x = true_NY[!pre_intervention], y = synthetic_NY[!pre_intervention])
NY_RMSE <- post_rmse_NY/pre_rmse_NY

## Ohio 

pre_intervention <- Observational_Period < 276
OH_weights <- placebo_results_23$solution.w
synthetic_OH <- as.numeric(placebo_data_list[[23]]$Y0plot %*% OH_weights)
true_OH <- emu_donors[emu_donors$ID2 == 23,]$Acc_PC
pre_rmse_OH <- rmse(x = true_OH[pre_intervention], y = synthetic_OH[pre_intervention])
post_rmse_OH <- rmse(x = true_OH[!pre_intervention], y = synthetic_OH[!pre_intervention])
OH_RMSE <- post_rmse_OH/pre_rmse_OH

## Oklahoma

pre_intervention <- Observational_Period < 276
OK_weights <- placebo_results_24$solution.w
synthetic_OK <- as.numeric(placebo_data_list[[24]]$Y0plot %*% OK_weights)
true_OK <- emu_donors[emu_donors$ID2 == 24,]$Acc_PC
pre_rmse_OK <- rmse(x = true_OK[pre_intervention], y = synthetic_OK[pre_intervention])
post_rmse_OK <- rmse(x = true_OK[!pre_intervention], y = synthetic_OK[!pre_intervention])
OK_RMSE <- post_rmse_OK/pre_rmse_OK

## Rhode Island

pre_intervention <- Observational_Period < 276
RI_weights <- placebo_results_25$solution.w
synthetic_RI <- as.numeric(placebo_data_list[[25]]$Y0plot %*% RI_weights)
true_RI <- emu_donors[emu_donors$ID2 == 25,]$Acc_PC
pre_rmse_RI <- rmse(x = true_RI[pre_intervention], y = synthetic_RI[pre_intervention])
post_rmse_RI <- rmse(x = true_RI[!pre_intervention], y = synthetic_RI[!pre_intervention])
RI_RMSE <- post_rmse_RI/pre_rmse_RI

## South Carolina
pre_intervention <- Observational_Period < 276
SC_weights <- placebo_results_26$solution.w
synthetic_SC <- as.numeric(placebo_data_list[[26]]$Y0plot %*% SC_weights)
true_SC <- emu_donors[emu_donors$ID2 == 26,]$Acc_PC
pre_rmse_SC <- rmse(x = true_SC[pre_intervention], y = synthetic_SC[pre_intervention])
post_rmse_SC <- rmse(x = true_SC[!pre_intervention], y = synthetic_SC[!pre_intervention])
SC_RMSE <- post_rmse_SC/pre_rmse_SC

## Tenessee
pre_intervention <- Observational_Period < 276
TN_weights <- placebo_results_27$solution.w
synthetic_TN <- as.numeric(placebo_data_list[[27]]$Y0plot %*% TN_weights)
true_TN <- emu_donors[emu_donors$ID2 == 27,]$Acc_PC
pre_rmse_TN <- rmse(x = true_TN[pre_intervention], y = synthetic_TN[pre_intervention])
post_rmse_TN <- rmse(x = true_TN[!pre_intervention], y = synthetic_TN[!pre_intervention])
TN_RMSE <- post_rmse_TN/pre_rmse_TN

## Texas
pre_intervention <- Observational_Period < 276
TX_weights <- placebo_results_28$solution.w
synthetic_TX <- as.numeric(placebo_data_list[[28]]$Y0plot %*% TX_weights)
true_TX <- emu_donors[emu_donors$ID2 == 28,]$Acc_PC
pre_rmse_TX <- rmse(x = true_TX[pre_intervention], y = synthetic_TX[pre_intervention])
post_rmse_TX <- rmse(x = true_TX[!pre_intervention], y = synthetic_TX[!pre_intervention])
TX_RMSE <- post_rmse_TX/pre_rmse_TX

## Virginia
pre_intervention <- Observational_Period < 276
VA_weights <- placebo_results_29$solution.w
synthetic_VA <- as.numeric(placebo_data_list[[29]]$Y0plot %*% VA_weights)
true_VA <- emu_donors[emu_donors$ID2 == 29,]$Acc_PC
pre_rmse_VA <- rmse(x = true_VA[pre_intervention], y = synthetic_VA[pre_intervention])
post_rmse_VA <- rmse(x = true_VA[!pre_intervention], y = synthetic_VA[!pre_intervention])
VA_RMSE <- post_rmse_VA/pre_rmse_VA

## Wyoming
pre_intervention <- Observational_Period < 276
WI_weights <- placebo_results_30$solution.w
synthetic_WI <- as.numeric(placebo_data_list[[30]]$Y0plot %*% WI_weights)
true_WI <- emu_donors[emu_donors$ID2 == 30,]$Acc_PC
pre_rmse_WI <- rmse(x = true_WI[pre_intervention], y = synthetic_WI[pre_intervention])
post_rmse_WI <- rmse(x = true_WI[!pre_intervention], y = synthetic_WI[!pre_intervention])
WI_RMSE <- post_rmse_WI/pre_rmse_WI

## West Virginia
pre_intervention <- Observational_Period < 276
WV_weights <- placebo_results_31$solution.w
synthetic_WV <- as.numeric(placebo_data_list[[31]]$Y0plot %*% WV_weights)
true_WV <- emu_donors[emu_donors$ID2 == 31,]$Acc_PC
pre_rmse_WV <- rmse(x = true_WV[pre_intervention], y = synthetic_WV[pre_intervention])
post_rmse_WV <- rmse(x = true_WV[!pre_intervention], y = synthetic_WV[!pre_intervention])
WV_RMSE <- post_rmse_WV/pre_rmse_WV

# Now we plot all ratios 

rmse_ratios <- data.frame(value = c(CA_RMSE, AL_RMSE, AZ_RMSE, CT_RMSE, IL_RMSE,
                                    DE_RMSE, FL_RMSE, GA_RMSE, IA_RMSE, IN_RMSE, 
                                    KS_RMSE, KY_RMSE, LA_RMSE, MD_RMSE, MN_RMSE, 
                                    MO_RMSE, MS_RMSE, NC_RMSE, NE_RMSE, NH_RMSE, 
                                    NJ_RMSE, NM_RMSE,
                                    NY_RMSE, OH_RMSE, OK_RMSE, RI_RMSE, SC_RMSE, 
                                    TN_RMSE, TX_RMSE, VA_RMSE, WI_RMSE, WV_RMSE), 
                          name = c("CA_RMSE", "AL_RMSE", "AZ_RMSE", "IL_RMSE",
                                   "CT_RMSE", "DE_RMSE", "FL_RMSE", 
                                   "GA_RMSE", "IA_RMSE","IN_RMSE", "KS_RMSE", 
                                   "KY_RMSE", "LA_RMSE",
                                   "MD_RMSE", "MN_RMSE", "MO_RMSE", "MS_RMSE", 
                                   "NC_RMSE", "NJ_RMSE", "NM_RMSE", "NE_RMSE", "NH_RMSE",
                                   "NY_RMSE", "OH_RMSE", "OK_RMSE",
                                   "RI_RMSE", "SC_RMSE", "TN_RMSE",
                                   "TX_RMSE", "VA_RMSE",
                                   "WI_RMSE", "WV_RMSE"))

highlight_df <- rmse_ratios %>% 
  filter(name == "CA_RMSE")


rmse_r <- ggplot(rmse_ratios, aes(x = value, y = name)) +
  geom_point(color = "gray", fill = "white") + 
  geom_point(data = highlight_df, color = "red", aes(x = value, y = name)) + 
  xlab("RMSE Ratios Pre / Post Treatment") + ylab("US States") + 
  theme(plot.title= element_text(size=14, 
                                 color="grey26",
                                 hjust=0.5,
                                 lineheight=1.2),
        panel.background = element_rect(fill="#f7f7f7"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill="#f7f7f7", color = "#f7f7f7"),
        axis.title.x = element_text(color="grey26", size=12),
        axis.title.y = element_text(color="grey26", size=12),
        axis.line = element_line(color = "black")) + 
  geom_vline(xintercept = mean(rmse_ratios$value), linetype = "dashed") + 
  geom_text(aes(x= 11, y = 4), label="Average RMSE ratio") +
  geom_point(data = highlight_df, color = "red", aes(x = value, y = name))


#### Monthly try out SCM Part 1  ####

# We have a balanced dataset according to our pre-defined characteristics which we already cleaned

USAccidents <- fread("US_accidents_monthly_PC_cleaned.csv")
USAccidents$Y_M_Date <- as.yearmon(paste(USAccidents$Year, USAccidents$Month), "%Y%m")

# Now we set the filter for the relevant time period

USAccidents <- USAccidents[USAccidents$State %in% 
                             c(unique(USAccidents$State[USAccidents$Year_Month == "Dez 2019" & 
                                                          USAccidents$Rec_Mar_Legal != 1]),"CA"),]
USAccidents$ID <- as.numeric(as.factor(USAccidents$State))
USAccidents$Timeline <- as.numeric(as.factor(USAccidents$Y_M_Date))


# We then set the reference variable to California and the treatment day to the Enactment date of legalisation

Enactment_date <- 10
Observational_Period  <- seq(Enactment_date - 9, Enactment_date + 37)

Cali_ref <- USAccidents$Acc_PC[USAccidents$State == "CA" & USAccidents$Timeline == Enactment_date]


# As we mentioned, we need to clear out several states since we assume them to suffer from large measurement error, which is in-
# consistent with the rest of the data set. Consequently, we set a simulation set including only the members we select with regard
# to the Measurement Error 

Cali_synth <- filter(USAccidents) %>%
  group_by(ID) %>%
  summarise(complete.panel = ifelse(length(Y_M_Date) == length(Observational_Period), 1, 0),
            missing.Acc = sum(is.na(Acc_PC)), 
            missing.VMT = sum(is.na(VMT_PC)),
            missing.rural = sum(is.na(Rural_Mileage_PC)), 
            missing.urban = sum(is.na(Urban_Mileage_PC)),
            missing.RMV = sum(is.na(RMV_PC)),
            missing.PopDens = sum(is.na(Pop_Density)),
            missing.Alc = sum(is.na(Alc_Cons_PC))) 

Cali_control = Cali_synth %>%
  filter(complete.panel == 1 & missing.Acc <= 1 & missing.VMT <= 1 &
           missing.rural <= 1 & missing.urban <= 1 & missing.RMV <= 1 & 
           missing.PopDens <= 1 & missing.Alc <= 1 & ID != 3) %>%
  select(ID) %>%
  unlist()
names(Cali_control) = NULL
Cali_control

USAccidents_real <- USAccidents[which(USAccidents$ID %in% c(Cali_control, unique(USAccidents$ID[USAccidents$State == "CA"]))), ]


match.criteria <- c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC")


USAccidents_real$ID<- as.numeric(as.factor(USAccidents_real$ID))
USAccidents_real$State <- as.character(USAccidents_real$State)
Observational_Period <- as.numeric(Observational_Period)
USAccidents_real <- as.data.frame(USAccidents_real)


### Monthly try out SCM Part 2 ####

SCM_synth <- dataprep(
  foo = USAccidents_real,
  predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
  time.predictors.prior = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  dependent = "Acc_PC",
  unit.variable = "ID",
  unit.names.variable = "State",
  time.variable = "Timeline",
  treatment.identifier = 3,
  controls.identifier = c(1:2, 4:32), # If for all states: adjust to 4:38
  time.optimize.ssr = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  time.plot = Observational_Period
)

# Apply synth() to those data and inspect matching results via tables
SCM_synth_new <- synth(SCM_synth, optimxmethod="All")
SCM_synth_tables <- synth.tab(dataprep.res = SCM_synth, synth.res = SCM_synth_new)
predictor_means <- SCM_synth_tables$tab.pred  # inspect balance
weights <- as.data.frame(SCM_synth_tables$tab.w) %>% arrange(desc(w.weights))

# Analyze the pre trend features for the Treated vs Synthetic vs the Sample Mean of the variables 
SCM_synth_tables$tab.pred

# Analyze the weight structure 
names(weights)[1] <- "Weights"
names(weights)[2] <- "State"
names(weights)[3] <- "Unit Numbers"
weights

# Plot path

path.case <- SCM_synth$Y1plot
path.synth <- SCM_synth$Y0plot %*% SCM_synth_new$solution.w 

trends_acc <- path.plot(synth.res = SCM_synth_new,
                        dataprep.res = SCM_synth,
                        Xlab = "Months",
                        Ylab = "Accidents per million",
                        Legend = c("Cali", "Synthetic Cali"),
                        tr.intake = 10)



# Plot differences
gap <- SCM_synth$Y1plot - (SCM_synth$Y0plot %*% SCM_synth_new$solution.w)

gap_acc <- plot(x = Observational_Period, y = gap, 
                xlab = "Days", ylab = "Gap in Accidents per million population", lwd = 2) +
  segments(x0 = min(Observational_Period), x1 = max(Observational_Period), y0=0, y1=0, lwd=2, lty=2, col="gray25") +
  abline(v=Enactment_date, col="red", lty=2) + 
  text(x = 960, y = -18, "reform day - 11th November 2016", pos = 2, cex = 0.8) +
  axis(1, tick=FALSE) +
  # Plot the series
  lines(x=Observational_Period, y=gap, type="l", lwd=2)


#### Monthly try out SCM Part 3 #### 

# Now, we can create the synthetic trends for our different states

# Function to apply; same specs as above for treated case
f.placebo.data <- function(ID) {
  
  placebo.dat <- dataprep(
    # drop the UK from data set for this
    foo = filter(USAccidents_real, State !="CA"),
    predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
    time.predictors.prior = seq(from = min(Observational_Period), to = Enactment_date - 1),
    dependent = "Acc_PC",
    unit.variable = "ID",
    unit.names.variable = "State",
    time.variable = "Timeline",
    treatment.identifier = ID,
    controls.identifier = Cali_control[which(Cali_control != ID)],
    time.optimize.ssr = seq(from = min(Observational_Period), to = Enactment_date - 1),
    time.plot = Observational_Period)
  
  return(placebo.dat)
  
}

placebo_data_list <- lapply(Cali_control, f.placebo.data)

# All states now are the equivalent to the "SCM_synth_new" variable for California

placebo_results_1 <- synth(placebo_data_list[[1]]) 
placebo_results_2 <- synth(placebo_data_list[[2]]) 
placebo_results_3 <- synth(placebo_data_list[[3]]) 
placebo_results_4 <- synth(placebo_data_list[[4]]) 
placebo_results_5 <- synth(placebo_data_list[[5]]) 
placebo_results_6 <- synth(placebo_data_list[[6]]) 
placebo_results_7 <- synth(placebo_data_list[[7]]) 
placebo_results_8 <- synth(placebo_data_list[[8]]) 
placebo_results_9 <- synth(placebo_data_list[[9]]) 
placebo_results_10 <- synth(placebo_data_list[[10]]) 
placebo_results_11 <- synth(placebo_data_list[[11]]) 
placebo_results_12 <- synth(placebo_data_list[[12]])
placebo_results_13 <- synth(placebo_data_list[[13]]) 
placebo_results_14 <- synth(placebo_data_list[[14]]) 
placebo_results_15 <- synth(placebo_data_list[[15]]) 
placebo_results_16 <- synth(placebo_data_list[[16]])
placebo_results_17 <- synth(placebo_data_list[[17]])
placebo_results_18 <- synth(placebo_data_list[[18]])
placebo_results_19 <- synth(placebo_data_list[[19]])
placebo_results_20 <- synth(placebo_data_list[[20]])
placebo_results_21 <- synth(placebo_data_list[[21]])
placebo_results_22 <- synth(placebo_data_list[[22]])
placebo_results_23 <- synth(placebo_data_list[[23]])
placebo_results_24 <- synth(placebo_data_list[[24]])
placebo_results_25 <- synth(placebo_data_list[[25]])
placebo_results_26 <- synth(placebo_data_list[[26]])
placebo_results_27 <- synth(placebo_data_list[[27]])
placebo_results_28 <- synth(placebo_data_list[[28]])
placebo_results_29 <- synth(placebo_data_list[[29]])
placebo_results_30 <- synth(placebo_data_list[[30]])
placebo_results_31 <- synth(placebo_data_list[[31]])


# Function to plot lines from placebo results by number
placebo.line.plot <- function(i) {
  solution.i <- eval(parse(text=paste0("placebo_results_", i, "$solution.w")))
  gap <- placebo_data_list[[i]]$Y1plot - 
    (placebo_data_list[[i]]$Y0plot %*% solution.i)
  lines(x = Observational_Period, y = gap, col = "grey70")
}

## Make plot

# Find gap
gap <- SCM_synth$Y1plot - (SCM_synth$Y0plot %*% SCM_synth_new$solution.w)

# Make vector of placebo tests to plot
placebos <- c(1:37)

# Set up plot using the main time series
plot(x=Observational_Period, y=gap, type="n", xaxs="i", yaxs="i", ylim=c(-400,250),
     xlab="Month", ylab="Differences per capita rel. to synthetic control", lwd=2)

# Plot the placebo time series
for (k in placebos) {
  placebo.line.plot(k)
}

# Horizontal Line at 0
abline(h=0, lwd=2, lty=2)

# Vertical line at the point of treatment
abline(v=Enactment_date, col="red", lty=2)

# Replot the main time series
lines(x=Observational_Period, y=gap, type="l", lwd=2) 

# Legend
legend(x="bottomleft", bty="n", legend=c("California", "placebo tests"), 
       col=c("black","grey70"), lwd=c(2,1), cex=0.8)

#### Monthly try out SCM Part 4  ####

# Now, we can quantify the selected state with the pre and post treatment RSME ratios

# Define function for calculating the RMSE
rmse <- function(x,y){
  sqrt(mean((x - y)^2))
}

# Define vector for pre/post-intervention subsetting

pre_intervention <- Observational_Period < Enactment_date

## California

# Extract the weights for synthetic Cali
Cali_weights <- SCM_synth_new$solution.w

# Calculate the outcome for synthetic Cali using matrix multiplication
synthetic_Cali <- as.numeric(SCM_synth$Y0plot %*% Cali_weights)

# Extract the true outcome for Cali
true_Cali <- USAccidents_real[USAccidents_real$ID == 3,]$Acc_PC

# Calculate the RMSE for the pre-intervention period for Cali

pre_rmse_CA <- rmse(x = true_Cali[pre_intervention], y = synthetic_Cali[pre_intervention])

# Calculate the RMSE for the post-intervention period for Cali

post_rmse_CA <- rmse(x = true_Cali[!pre_intervention], y = synthetic_Cali[!pre_intervention])

CA_RMSE <- post_rmse_CA/pre_rmse_CA

#### Monthly try out SCM Part 5 #### 

# Now we can quantify the RMSE ratios for all other states 

## Exclude Cali from data so that it never enters the donor pool

emu_donors <- USAccidents_real[USAccidents_real$ID != 3,]
emu_donors$ID2 <- as.numeric(as.factor(emu_donors$State))


## Alabama

pre_intervention <- Observational_Period < 10
AL_weights <- placebo_results_1$solution.w
synthetic_AL <- as.numeric(placebo_data_list[[1]]$Y0plot %*% AL_weights)
true_AL <- emu_donors[emu_donors$ID2 == 1,]$Acc_PC
pre_rmse_AL <- rmse(x = true_AL[pre_intervention], y = synthetic_AL[pre_intervention])
post_rmse_AL <- rmse(x = true_AL[!pre_intervention], y = synthetic_AL[!pre_intervention])
AL_RMSE <- post_rmse_AL/pre_rmse_AL

## Arizona

pre_intervention <- Observational_Period < 10
AZ_weights <- placebo_results_2$solution.w
synthetic_AZ <- as.numeric(placebo_data_list[[2]]$Y0plot %*% AZ_weights)
true_AZ <- emu_donors[emu_donors$ID2 == 2,]$Acc_PC
pre_rmse_AZ <- rmse(x = true_AZ[pre_intervention], y = synthetic_AZ[pre_intervention])
post_rmse_AZ <- rmse(x = true_AZ[!pre_intervention], y = synthetic_AZ[!pre_intervention])
AZ_RMSE <- post_rmse_AZ/pre_rmse_AZ

## Connecticut

pre_intervention <- Observational_Period < 10
CT_weights <- placebo_results_3$solution.w
synthetic_CT <- as.numeric(placebo_data_list[[3]]$Y0plot %*% CT_weights)
true_CT <- emu_donors[emu_donors$ID2 == 3,]$Acc_PC
pre_rmse_CT <- rmse(x = true_CT[pre_intervention], y = synthetic_CT[pre_intervention])
post_rmse_CT <- rmse(x = true_CT[!pre_intervention], y = synthetic_CT[!pre_intervention])
CT_RMSE <- post_rmse_CT/pre_rmse_CT

## Delaware

pre_intervention <- Observational_Period < 10
DE_weights <- placebo_results_4$solution.w
synthetic_DE <- as.numeric(placebo_data_list[[4]]$Y0plot %*% DE_weights)
true_DE <- emu_donors[emu_donors$ID2 == 4,]$Acc_PC
pre_rmse_DE <- rmse(x = true_DE[pre_intervention], y = synthetic_DE[pre_intervention])
post_rmse_DE <- rmse(x = true_DE[!pre_intervention], y = synthetic_DE[!pre_intervention])
DE_RMSE <- post_rmse_DE/pre_rmse_DE

## Florida

pre_intervention <- Observational_Period < 10
FL_weights <- placebo_results_5$solution.w
synthetic_FL <- as.numeric(placebo_data_list[[5]]$Y0plot %*% FL_weights)
true_FL <- emu_donors[emu_donors$ID2 == 5,]$Acc_PC
pre_rmse_FL <- rmse(x = true_FL[pre_intervention], y = synthetic_FL[pre_intervention])
post_rmse_FL <- rmse(x = true_FL[!pre_intervention], y = synthetic_FL[!pre_intervention])
FL_RMSE <- post_rmse_FL/pre_rmse_FL

## Georgia

pre_intervention <- Observational_Period < 10
GA_weights <- placebo_results_6$solution.w
synthetic_GA <- as.numeric(placebo_data_list[[6]]$Y0plot %*% GA_weights)
true_GA <- emu_donors[emu_donors$ID2 == 6,]$Acc_PC
pre_rmse_GA <- rmse(x = true_GA[pre_intervention], y = synthetic_GA[pre_intervention])
post_rmse_GA <- rmse(x = true_GA[!pre_intervention], y = synthetic_GA[!pre_intervention])
GA_RMSE <- post_rmse_GA/pre_rmse_GA

## Iowa

pre_intervention <- Observational_Period < 10
IA_weights <- placebo_results_7$solution.w
synthetic_IA <- as.numeric(placebo_data_list[[7]]$Y0plot %*% IA_weights)
true_IA <- emu_donors[emu_donors$ID2 == 7,]$Acc_PC
pre_rmse_IA <- rmse(x = true_IA[pre_intervention], y = synthetic_IA[pre_intervention])
post_rmse_IA <- rmse(x = true_IA[!pre_intervention], y = synthetic_IA[!pre_intervention])
IA_RMSE <- post_rmse_IA/pre_rmse_IA

## Illionis

pre_intervention <- Observational_Period < 10
IL_weights <- placebo_results_8$solution.w
synthetic_IL <- as.numeric(placebo_data_list[[8]]$Y0plot %*% IL_weights)
true_IL <- emu_donors[emu_donors$ID2 == 8,]$Acc_PC
pre_rmse_IL <- rmse(x = true_IL[pre_intervention], y = synthetic_IL[pre_intervention])
post_rmse_IL <- rmse(x = true_IL[!pre_intervention], y = synthetic_IL[!pre_intervention])
IL_RMSE <- post_rmse_IL/pre_rmse_IL


## Indiana

pre_intervention <- Observational_Period < 10
IN_weights <- placebo_results_9$solution.w
synthetic_IN <- as.numeric(placebo_data_list[[9]]$Y0plot %*% IN_weights)
true_IN <- emu_donors[emu_donors$ID2 == 9,]$Acc_PC
pre_rmse_IN <- rmse(x = true_IN[pre_intervention], y = synthetic_IN[pre_intervention])
post_rmse_IN <- rmse(x = true_IN[!pre_intervention], y = synthetic_IN[!pre_intervention])
IN_RMSE <- post_rmse_IN/pre_rmse_IN

## Kansas

pre_intervention <- Observational_Period < 10
KS_weights <- placebo_results_10$solution.w
synthetic_KS <- as.numeric(placebo_data_list[[10]]$Y0plot %*% KS_weights)
true_KS <- emu_donors[emu_donors$ID2 == 10,]$Acc_PC
pre_rmse_KS <- rmse(x = true_KS[pre_intervention], y = synthetic_KS[pre_intervention])
post_rmse_KS <- rmse(x = true_KS[!pre_intervention], y = synthetic_KS[!pre_intervention])
KS_RMSE <- post_rmse_KS/pre_rmse_KS

## Kentucky

pre_intervention <- Observational_Period < 10
KY_weights <- placebo_results_11$solution.w
synthetic_KY <- as.numeric(placebo_data_list[[11]]$Y0plot %*% KY_weights)
true_KY <- emu_donors[emu_donors$ID2 == 11,]$Acc_PC
pre_rmse_KY <- rmse(x = true_KY[pre_intervention], y = synthetic_KY[pre_intervention])
post_rmse_KY <- rmse(x = true_KY[!pre_intervention], y = synthetic_KY[!pre_intervention])
KY_RMSE <- post_rmse_KY/pre_rmse_KY

## Louisiana

pre_intervention <- Observational_Period < 10
LA_weights <- placebo_results_12$solution.w
synthetic_LA <- as.numeric(placebo_data_list[[12]]$Y0plot %*% LA_weights)
true_LA <- emu_donors[emu_donors$ID2 == 12,]$Acc_PC
pre_rmse_LA <- rmse(x = true_LA[pre_intervention], y = synthetic_LA[pre_intervention])
post_rmse_LA <- rmse(x = true_LA[!pre_intervention], y = synthetic_LA[!pre_intervention])
LA_RMSE <- post_rmse_LA/pre_rmse_LA

## Maryland

pre_intervention <- Observational_Period < 10
MD_weights <- placebo_results_13$solution.w
synthetic_MD <- as.numeric(placebo_data_list[[13]]$Y0plot %*% MD_weights)
true_MD <- emu_donors[emu_donors$ID2 == 13,]$Acc_PC
pre_rmse_MD <- rmse(x = true_MD[pre_intervention], y = synthetic_MD[pre_intervention])
post_rmse_MD <- rmse(x = true_MD[!pre_intervention], y = synthetic_MD[!pre_intervention])
MD_RMSE <- post_rmse_MD/pre_rmse_MD


## Minnesota 

pre_intervention <- Observational_Period < 10
MN_weights <- placebo_results_14$solution.w
synthetic_MN <- as.numeric(placebo_data_list[[14]]$Y0plot %*% MN_weights)
true_MN <- emu_donors[emu_donors$ID2 == 14,]$Acc_PC
pre_rmse_MN <- rmse(x = true_MN[pre_intervention], y = synthetic_MN[pre_intervention])
post_rmse_MN <- rmse(x = true_MN[!pre_intervention], y = synthetic_MN[!pre_intervention])
MN_RMSE <- post_rmse_MN/pre_rmse_MN

## Montana

pre_intervention <- Observational_Period < 10
MO_weights <- placebo_results_15$solution.w
synthetic_MO <- as.numeric(placebo_data_list[[15]]$Y0plot %*% MO_weights)
true_MO <- emu_donors[emu_donors$ID2 == 15,]$Acc_PC
pre_rmse_MO <- rmse(x = true_MO[pre_intervention], y = synthetic_MO[pre_intervention])
post_rmse_MO <- rmse(x = true_MO[!pre_intervention], y = synthetic_MO[!pre_intervention])
MO_RMSE <- post_rmse_MO/pre_rmse_MO

## Mississippi

pre_intervention <- Observational_Period < 10
MS_weights <- placebo_results_16$solution.w
synthetic_MS <- as.numeric(placebo_data_list[[16]]$Y0plot %*% MS_weights)
true_MS <- emu_donors[emu_donors$ID2 == 16,]$Acc_PC
pre_rmse_MS <- rmse(x = true_MS[pre_intervention], y = synthetic_MS[pre_intervention])
post_rmse_MS <- rmse(x = true_MS[!pre_intervention], y = synthetic_MS[!pre_intervention])
MS_RMSE <- post_rmse_MS/pre_rmse_MS

## North Carolina

pre_intervention <- Observational_Period < 10
NC_weights <- placebo_results_17$solution.w
synthetic_NC <- as.numeric(placebo_data_list[[17]]$Y0plot %*% NC_weights)
true_NC <- emu_donors[emu_donors$ID2 == 17,]$Acc_PC
pre_rmse_NC <- rmse(x = true_NC[pre_intervention], y = synthetic_NC[pre_intervention])
post_rmse_NC <- rmse(x = true_NC[!pre_intervention], y = synthetic_NC[!pre_intervention])
NC_RMSE <- post_rmse_NC/pre_rmse_NC

## Nebraska

pre_intervention <- Observational_Period < 10
NE_weights <- placebo_results_18$solution.w
synthetic_NE <- as.numeric(placebo_data_list[[18]]$Y0plot %*% NE_weights)
true_NE <- emu_donors[emu_donors$ID2 == 18,]$Acc_PC
pre_rmse_NE <- rmse(x = true_NE[pre_intervention], y = synthetic_NE[pre_intervention])
post_rmse_NE <- rmse(x = true_NE[!pre_intervention], y = synthetic_NE[!pre_intervention])
NE_RMSE <- post_rmse_NE/pre_rmse_NE

## New Hampshire

pre_intervention <- Observational_Period < 10
NH_weights <- placebo_results_19$solution.w
synthetic_NH <- as.numeric(placebo_data_list[[19]]$Y0plot %*% NH_weights)
true_NH <- emu_donors[emu_donors$ID2 == 19,]$Acc_PC
pre_rmse_NH <- rmse(x = true_NH[pre_intervention], y = synthetic_NH[pre_intervention])
post_rmse_NH <- rmse(x = true_NH[!pre_intervention], y = synthetic_NH[!pre_intervention])
NH_RMSE <- post_rmse_NH/pre_rmse_NH


## New Jersey

pre_intervention <- Observational_Period < 10
NJ_weights <- placebo_results_20$solution.w
synthetic_NJ <- as.numeric(placebo_data_list[[20]]$Y0plot %*% NJ_weights)
true_NJ <- emu_donors[emu_donors$ID2 == 20,]$Acc_PC
pre_rmse_NJ <- rmse(x = true_NJ[pre_intervention], y = synthetic_NJ[pre_intervention])
post_rmse_NJ <- rmse(x = true_NJ[!pre_intervention], y = synthetic_NJ[!pre_intervention])
NJ_RMSE <- post_rmse_NJ/pre_rmse_NJ

## New Mexico

pre_intervention <- Observational_Period < 10
NM_weights <- placebo_results_21$solution.w
synthetic_NM <- as.numeric(placebo_data_list[[21]]$Y0plot %*% NM_weights)
true_NM <- emu_donors[emu_donors$ID2 == 21,]$Acc_PC
pre_rmse_NM <- rmse(x = true_NM[pre_intervention], y = synthetic_NM[pre_intervention])
post_rmse_NM <- rmse(x = true_NM[!pre_intervention], y = synthetic_NM[!pre_intervention])
NM_RMSE <- post_rmse_NM/pre_rmse_NM

## New York

pre_intervention <- Observational_Period < 10
NY_weights <- placebo_results_22$solution.w
synthetic_NY <- as.numeric(placebo_data_list[[22]]$Y0plot %*% NY_weights)
true_NY <- emu_donors[emu_donors$ID2 == 22,]$Acc_PC
pre_rmse_NY <- rmse(x = true_NY[pre_intervention], y = synthetic_NY[pre_intervention])
post_rmse_NY <- rmse(x = true_NY[!pre_intervention], y = synthetic_NY[!pre_intervention])
NY_RMSE <- post_rmse_NY/pre_rmse_NY

## Ohio 

pre_intervention <- Observational_Period < 10
OH_weights <- placebo_results_23$solution.w
synthetic_OH <- as.numeric(placebo_data_list[[23]]$Y0plot %*% OH_weights)
true_OH <- emu_donors[emu_donors$ID2 == 23,]$Acc_PC
pre_rmse_OH <- rmse(x = true_OH[pre_intervention], y = synthetic_OH[pre_intervention])
post_rmse_OH <- rmse(x = true_OH[!pre_intervention], y = synthetic_OH[!pre_intervention])
OH_RMSE <- post_rmse_OH/pre_rmse_OH

## Oklahoma

pre_intervention <- Observational_Period < 10
OK_weights <- placebo_results_24$solution.w
synthetic_OK <- as.numeric(placebo_data_list[[24]]$Y0plot %*% OK_weights)
true_OK <- emu_donors[emu_donors$ID2 == 24,]$Acc_PC
pre_rmse_OK <- rmse(x = true_OK[pre_intervention], y = synthetic_OK[pre_intervention])
post_rmse_OK <- rmse(x = true_OK[!pre_intervention], y = synthetic_OK[!pre_intervention])
OK_RMSE <- post_rmse_OK/pre_rmse_OK

## Rhode Island

pre_intervention <- Observational_Period < 10
RI_weights <- placebo_results_25$solution.w
synthetic_RI <- as.numeric(placebo_data_list[[25]]$Y0plot %*% RI_weights)
true_RI <- emu_donors[emu_donors$ID2 == 25,]$Acc_PC
pre_rmse_RI <- rmse(x = true_RI[pre_intervention], y = synthetic_RI[pre_intervention])
post_rmse_RI <- rmse(x = true_RI[!pre_intervention], y = synthetic_RI[!pre_intervention])
RI_RMSE <- post_rmse_RI/pre_rmse_RI

## South Carolina
pre_intervention <- Observational_Period < 10
SC_weights <- placebo_results_26$solution.w
synthetic_SC <- as.numeric(placebo_data_list[[26]]$Y0plot %*% SC_weights)
true_SC <- emu_donors[emu_donors$ID2 == 26,]$Acc_PC
pre_rmse_SC <- rmse(x = true_SC[pre_intervention], y = synthetic_SC[pre_intervention])
post_rmse_SC <- rmse(x = true_SC[!pre_intervention], y = synthetic_SC[!pre_intervention])
SC_RMSE <- post_rmse_SC/pre_rmse_SC

## Tenessee
pre_intervention <- Observational_Period < 10
TN_weights <- placebo_results_27$solution.w
synthetic_TN <- as.numeric(placebo_data_list[[27]]$Y0plot %*% TN_weights)
true_TN <- emu_donors[emu_donors$ID2 == 27,]$Acc_PC
pre_rmse_TN <- rmse(x = true_TN[pre_intervention], y = synthetic_TN[pre_intervention])
post_rmse_TN <- rmse(x = true_TN[!pre_intervention], y = synthetic_TN[!pre_intervention])
TN_RMSE <- post_rmse_TN/pre_rmse_TN

## Texas
pre_intervention <- Observational_Period < 10
TX_weights <- placebo_results_28$solution.w
synthetic_TX <- as.numeric(placebo_data_list[[28]]$Y0plot %*% TX_weights)
true_TX <- emu_donors[emu_donors$ID2 == 28,]$Acc_PC
pre_rmse_TX <- rmse(x = true_TX[pre_intervention], y = synthetic_TX[pre_intervention])
post_rmse_TX <- rmse(x = true_TX[!pre_intervention], y = synthetic_TX[!pre_intervention])
TX_RMSE <- post_rmse_TX/pre_rmse_TX

## Virginia
pre_intervention <- Observational_Period < 10
VA_weights <- placebo_results_29$solution.w
synthetic_VA <- as.numeric(placebo_data_list[[29]]$Y0plot %*% VA_weights)
true_VA <- emu_donors[emu_donors$ID2 == 29,]$Acc_PC
pre_rmse_VA <- rmse(x = true_VA[pre_intervention], y = synthetic_VA[pre_intervention])
post_rmse_VA <- rmse(x = true_VA[!pre_intervention], y = synthetic_VA[!pre_intervention])
VA_RMSE <- post_rmse_VA/pre_rmse_VA

## Wyoming
pre_intervention <- Observational_Period < 10
WI_weights <- placebo_results_30$solution.w
synthetic_WI <- as.numeric(placebo_data_list[[30]]$Y0plot %*% WI_weights)
true_WI <- emu_donors[emu_donors$ID2 == 30,]$Acc_PC
pre_rmse_WI <- rmse(x = true_WI[pre_intervention], y = synthetic_WI[pre_intervention])
post_rmse_WI <- rmse(x = true_WI[!pre_intervention], y = synthetic_WI[!pre_intervention])
WI_RMSE <- post_rmse_WI/pre_rmse_WI

## West Virginia
pre_intervention <- Observational_Period < 10
WV_weights <- placebo_results_31$solution.w
synthetic_WV <- as.numeric(placebo_data_list[[31]]$Y0plot %*% WV_weights)
true_WV <- emu_donors[emu_donors$ID2 == 31,]$Acc_PC
pre_rmse_WV <- rmse(x = true_WV[pre_intervention], y = synthetic_WV[pre_intervention])
post_rmse_WV <- rmse(x = true_WV[!pre_intervention], y = synthetic_WV[!pre_intervention])
WV_RMSE <- post_rmse_WV/pre_rmse_WV

# Now we plot all ratios 

rmse_ratios <- data.frame(value = c(CA_RMSE, AL_RMSE, AZ_RMSE, CT_RMSE, IL_RMSE,
                                    DE_RMSE, FL_RMSE, GA_RMSE, IA_RMSE, IN_RMSE, 
                                    KS_RMSE, KY_RMSE, LA_RMSE, MD_RMSE, MN_RMSE, 
                                    MO_RMSE, MS_RMSE, NC_RMSE, NE_RMSE, NH_RMSE, 
                                    NJ_RMSE, NM_RMSE,
                                    NY_RMSE, OH_RMSE, OK_RMSE, RI_RMSE, SC_RMSE, 
                                    TN_RMSE, TX_RMSE, VA_RMSE, WI_RMSE, WV_RMSE), 
                          name = c("CA_RMSE", "AL_RMSE", "AZ_RMSE", "IL_RMSE",
                                   "CT_RMSE", "DE_RMSE", "FL_RMSE", 
                                   "GA_RMSE", "IA_RMSE","IN_RMSE", "KS_RMSE", 
                                   "KY_RMSE", "LA_RMSE",
                                   "MD_RMSE", "MN_RMSE", "MO_RMSE", "MS_RMSE", 
                                   "NC_RMSE", "NJ_RMSE", "NM_RMSE", "NE_RMSE", "NH_RMSE",
                                   "NY_RMSE", "OH_RMSE", "OK_RMSE",
                                   "RI_RMSE", "SC_RMSE", "TN_RMSE",
                                   "TX_RMSE", "VA_RMSE",
                                   "WI_RMSE", "WV_RMSE"))

highlight_df <- rmse_ratios %>% 
  filter(name == "CA_RMSE")

rmse_r <- ggplot(rmse_ratios, aes(x = value, y = name)) +
  geom_point(color = "gray", fill = "white") + 
  geom_point(data = highlight_df, color = "red", aes(x = value, y = name)) + 
  xlab("RMSE Ratios Pre / Post Treatment") + ylab("US States") + 
  theme(plot.title= element_text(size=14, 
                                 color="grey26",
                                 hjust=0.5,
                                 lineheight=1.2),
        panel.background = element_rect(fill="#f7f7f7"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill="#f7f7f7", color = "#f7f7f7"),
        axis.title.x = element_text(color="grey26", size=12),
        axis.title.y = element_text(color="grey26", size=12),
        axis.line = element_line(color = "black")) + 
  geom_vline(xintercept = mean(rmse_ratios$value), linetype = "dashed") + 
  geom_text(aes(x= 60, y = 4), label="Average RMSE ratio") +
  geom_point(data = highlight_df, color = "red", aes(x = value, y = name))

rmse_r

#### SCM with the enactment period set at sales start Part 1 - Preparation of the dataset #### 

# Calculation of the SCM method for the analysis of marijuana consumption on traffic accidents

# This part corresponds to the "Synthetic Control Method" part of our "Data and Methodology" section. 

# We introdcute the SCM and perform the necessary steps in order to receive a synthetic control of our treatment state for
# the observational period. 

# We have a balanced dataset according to our pre-defined characteristics which we already cleaned
USAccidents <- fread("US_accidents_PC_cleaned.csv")
USAccidents$Date <- ymd(USAccidents$Date)

# Now we set the filter for the relevant time period

USAccidents <- USAccidents[USAccidents$State %in% 
                             c(unique(USAccidents$State[USAccidents$Date == ymd("2019-12-31") & 
                                                          USAccidents$Rec_Mar_Legal != 1]),"CA"),]
USAccidents$ID <- as.numeric(as.factor(USAccidents$State))
USAccidents$Timeline <- as.numeric(as.factor(USAccidents$Date))

# Now we set the filter for the relevant time period

Enactment_date <- 694
Observational_Period  <- seq(Enactment_date - 693, Enactment_date + 729)

# We then set the reference variable to California and the treatment day to the Enactment date of legalisation

Cali_ref <- USAccidents$Acc_PC[USAccidents$State == "CA" & USAccidents$Timeline == Enactment_date]


# As we mentioned, we need to clear out several states since we assume them to suffer from large measurement error, which is in-
# consistent with the rest of the data set. Consequently, we set a simulation set including only the members we select with regard
# to the Measurement Error 

Cali_synth <- filter(USAccidents) %>%
  group_by(ID) %>%
  summarise(complete.panel = ifelse(length(Date) == length(Observational_Period), 1, 0),
            missing.Acc = sum(is.na(Acc_PC)), 
            missing.VMT = sum(is.na(VMT_PC)),
            missing.rural = sum(is.na(Rural_Mileage_PC)), 
            missing.urban = sum(is.na(Urban_Mileage_PC)),
            missing.RMV = sum(is.na(RMV_PC)),
            missing.PopDens = sum(is.na(Pop_Density)),
            missing.Alc = sum(is.na(Alc_Cons_PC))) 

Cali_control = Cali_synth %>%
  filter(complete.panel == 1 & missing.Acc <= 1 & missing.VMT <= 1 &
           missing.rural <= 1 & missing.urban <= 1 & missing.RMV <= 1 & 
           missing.PopDens <= 1 & missing.Alc <= 1 & ID != 3) %>%
  select(ID) %>%
  unlist()
names(Cali_control) = NULL
Cali_control

USAccidents_real <- USAccidents[which(USAccidents$ID %in% c(Cali_control, unique(USAccidents$ID[USAccidents$State == "CA"]))), ]


match.criteria <- c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC")


USAccidents_real$ID<- as.numeric(as.factor(USAccidents_real$ID))
USAccidents_real$State <- as.character(USAccidents_real$State)
USAccidents_real$Date <- ymd(USAccidents_real$Date)
Observational_Period <- as.numeric(Observational_Period)
USAccidents_real <- as.data.frame(USAccidents_real)


### SCM with the enactment period set at sales startPart 2 - Calculating the Synthetic version of California ####

SCM_synth <- dataprep(
  foo = USAccidents_real,
  predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
  time.predictors.prior = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  dependent = "Acc_PC",
  unit.variable = "ID",
  unit.names.variable = "State",
  time.variable = "Timeline",
  treatment.identifier = 3,
  controls.identifier = c(1:2, 4:32), # If for all states: adjust to 4:38
  time.optimize.ssr = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  time.plot = Observational_Period
)

# Apply synth() to those data and inspect matching results via tables
SCM_synth_new <- synth(SCM_synth, optimxmethod="All")
SCM_synth_tables <- synth.tab(dataprep.res = SCM_synth, synth.res = SCM_synth_new)
predictor_means <- SCM_synth_tables$tab.pred  # inspect balance
weights <- as.data.frame(SCM_synth_tables$tab.w) %>% arrange(desc(w.weights))

# Analyze the pre trend features for the Treated vs Synthetic vs the Sample Mean of the variables 
SCM_synth_tables$tab.pred

# Analyze the weight structure 
names(weights)[1] <- "Weights"
names(weights)[2] <- "State"
names(weights)[3] <- "Unit Numbers"
weights


# Plot path

path.case <- SCM_synth$Y1plot
path.synth <- SCM_synth$Y0plot %*% SCM_synth_new$solution.w 

trends_acc <- path.plot(synth.res = SCM_synth_new,
                        dataprep.res = SCM_synth,
                        Xlab = "Days",
                        Ylab = "Accidents per million",
                        Legend = c("Cali", "Synthetic Cali"),
                        tr.intake = 694)

# Plot differences
gap <- SCM_synth$Y1plot - (SCM_synth$Y0plot %*% SCM_synth_new$solution.w)

gap_acc <- plot(x = Observational_Period, y = gap, 
                xlab = "Days", ylab = "Gap in Accidents per million population", lwd = 2) +
  segments(x0 = min(Observational_Period), x1 = max(Observational_Period), y0=0, y1=0, lwd=2, lty=2, col="gray25") +
  abline(v=Enactment_date, col="red", lty=2) + 
  text(x = 960, y = -18, "reform day - 11th November 2016", pos = 2, cex = 0.8) +
  axis(1, tick=FALSE) +
  # Plot the series
  lines(x=Observational_Period, y=gap, type="l", lwd=2)



#### SCM with the enactment period set at sales start Part 3 - Calculate the Synthetic Controls for other States #### 

# Now, we can create the synthetic trends for our different states

# Function to apply; same specs as above for treated case
f.placebo.data <- function(ID) {
  
  placebo.dat <- dataprep(
    # drop the UK from data set for this
    foo = filter(USAccidents_real, State !="CA"),
    predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
    time.predictors.prior = seq(from = min(Observational_Period), to = Enactment_date - 1),
    dependent = "Acc_PC",
    unit.variable = "ID",
    unit.names.variable = "State",
    time.variable = "Timeline",
    treatment.identifier = ID,
    controls.identifier = Cali_control[which(Cali_control != ID)],
    time.optimize.ssr = seq(from = min(Observational_Period), to = Enactment_date - 1),
    time.plot = Observational_Period)
  
  return(placebo.dat)
  
}

placebo_data_list <- lapply(Cali_control, f.placebo.data)

# All states now are the equivalent to the "SCM_synth_new" variable for California

placebo_results_1 <- synth(placebo_data_list[[1]]) 
placebo_results_2 <- synth(placebo_data_list[[2]]) 
placebo_results_3 <- synth(placebo_data_list[[3]]) 
placebo_results_4 <- synth(placebo_data_list[[4]]) 
placebo_results_5 <- synth(placebo_data_list[[5]]) 
placebo_results_6 <- synth(placebo_data_list[[6]]) 
placebo_results_7 <- synth(placebo_data_list[[7]]) 
placebo_results_8 <- synth(placebo_data_list[[8]]) 
placebo_results_9 <- synth(placebo_data_list[[9]]) 
placebo_results_10 <- synth(placebo_data_list[[10]]) 
placebo_results_11 <- synth(placebo_data_list[[11]]) 
placebo_results_12 <- synth(placebo_data_list[[12]])
placebo_results_13 <- synth(placebo_data_list[[13]]) 
placebo_results_14 <- synth(placebo_data_list[[14]]) 
placebo_results_15 <- synth(placebo_data_list[[15]]) 
placebo_results_16 <- synth(placebo_data_list[[16]])
placebo_results_17 <- synth(placebo_data_list[[17]])
placebo_results_18 <- synth(placebo_data_list[[18]])
placebo_results_19 <- synth(placebo_data_list[[19]])
placebo_results_20 <- synth(placebo_data_list[[20]])
placebo_results_21 <- synth(placebo_data_list[[21]])
placebo_results_22 <- synth(placebo_data_list[[22]])
placebo_results_23 <- synth(placebo_data_list[[23]])
placebo_results_24 <- synth(placebo_data_list[[24]])
placebo_results_25 <- synth(placebo_data_list[[25]])
placebo_results_26 <- synth(placebo_data_list[[26]])
placebo_results_27 <- synth(placebo_data_list[[27]])
placebo_results_28 <- synth(placebo_data_list[[28]])
placebo_results_29 <- synth(placebo_data_list[[29]])
placebo_results_30 <- synth(placebo_data_list[[30]])
placebo_results_31 <- synth(placebo_data_list[[31]])


# Function to plot lines from placebo results by number
placebo.line.plot <- function(i) {
  solution.i <- eval(parse(text=paste0("placebo_results_", i, "$solution.w")))
  gap <- placebo_data_list[[i]]$Y1plot - 
    (placebo_data_list[[i]]$Y0plot %*% solution.i)
  lines(x = Observational_Period, y = gap, col = "grey70")
}

## Make plot

# Find gap
gap <- SCM_synth$Y1plot - (SCM_synth$Y0plot %*% SCM_synth_new$solution.w)

# Make vector of placebo tests to plot
placebos <- c(1:37)

# Set up plot using the main time series
plot(x=Observational_Period, y=gap, type="n", xaxs="i", yaxs="i", ylim=c(-20,40),
     xlab="year", ylab="Differences per capita rel. to synthetic control", lwd=2)

# Plot the placebo time series
for (k in placebos) {
  placebo.line.plot(k)
}

# Horizontal Line at 0
abline(h=0, lwd=2, lty=2)

# Vertical line at the point of treatment
abline(v=Enactment_date, col="red", lty=2)

# Replot the main time series
lines(x=Observational_Period, y=gap, type="l", lwd=2) 

# Legend
legend(x="bottomleft", bty="n", legend=c("California", "placebo tests"), 
       col=c("black","grey70"), lwd=c(2,1), cex=0.8)

#### SCM with the enactment period set at sales start Part 4 - Quantify the RMSE ratio for California ####

# Now, we can quantify the selected state with the pre and post treatment RSME ratios

# Define function for calculating the RMSE
rmse <- function(x,y){
  sqrt(mean((x - y)^2))
}

# Define vector for pre/post-intervention subsetting

pre_intervention <- Observational_Period < Enactment_date

## California

# Extract the weights for synthetic Cali
Cali_weights <- SCM_synth_new$solution.w

# Calculate the outcome for synthetic Cali using matrix multiplication
synthetic_Cali <- as.numeric(SCM_synth$Y0plot %*% Cali_weights)

# Extract the true outcome for Cali
true_Cali <- USAccidents_real[USAccidents_real$ID == 3,]$Acc_PC

# Calculate the RMSE for the pre-intervention period for Cali

pre_rmse_CA <- rmse(x = true_Cali[pre_intervention], y = synthetic_Cali[pre_intervention])

# Calculate the RMSE for the post-intervention period for Cali

post_rmse_CA <- rmse(x = true_Cali[!pre_intervention], y = synthetic_Cali[!pre_intervention])

CA_RMSE <- post_rmse_CA/pre_rmse_CA


#### SCM with the enactment period set at sales start Part 5 - Quantify RMSE ratios for other states #### 

# Now we can quantify the RMSE ratios for all other states 

## Exclude Cali from data so that it never enters the donor pool

emu_donors <- USAccidents_real[USAccidents_real$ID != 3,]
emu_donors$ID2 <- as.numeric(as.factor(emu_donors$State))


## Alabama

pre_intervention <- Observational_Period < 694
AL_weights <- placebo_results_1$solution.w
synthetic_AL <- as.numeric(placebo_data_list[[1]]$Y0plot %*% AL_weights)
true_AL <- emu_donors[emu_donors$ID2 == 1,]$Acc_PC
pre_rmse_AL <- rmse(x = true_AL[pre_intervention], y = synthetic_AL[pre_intervention])
post_rmse_AL <- rmse(x = true_AL[!pre_intervention], y = synthetic_AL[!pre_intervention])
AL_RMSE <- post_rmse_AL/pre_rmse_AL

## Arizona

pre_intervention <- Observational_Period < 694
AZ_weights <- placebo_results_2$solution.w
synthetic_AZ <- as.numeric(placebo_data_list[[2]]$Y0plot %*% AZ_weights)
true_AZ <- emu_donors[emu_donors$ID2 == 2,]$Acc_PC
pre_rmse_AZ <- rmse(x = true_AZ[pre_intervention], y = synthetic_AZ[pre_intervention])
post_rmse_AZ <- rmse(x = true_AZ[!pre_intervention], y = synthetic_AZ[!pre_intervention])
AZ_RMSE <- post_rmse_AZ/pre_rmse_AZ

## Connecticut

pre_intervention <- Observational_Period < 694
CT_weights <- placebo_results_3$solution.w
synthetic_CT <- as.numeric(placebo_data_list[[3]]$Y0plot %*% CT_weights)
true_CT <- emu_donors[emu_donors$ID2 == 3,]$Acc_PC
pre_rmse_CT <- rmse(x = true_CT[pre_intervention], y = synthetic_CT[pre_intervention])
post_rmse_CT <- rmse(x = true_CT[!pre_intervention], y = synthetic_CT[!pre_intervention])
CT_RMSE <- post_rmse_CT/pre_rmse_CT

## Delaware

pre_intervention <- Observational_Period < 694
DE_weights <- placebo_results_4$solution.w
synthetic_DE <- as.numeric(placebo_data_list[[4]]$Y0plot %*% DE_weights)
true_DE <- emu_donors[emu_donors$ID2 == 4,]$Acc_PC
pre_rmse_DE <- rmse(x = true_DE[pre_intervention], y = synthetic_DE[pre_intervention])
post_rmse_DE <- rmse(x = true_DE[!pre_intervention], y = synthetic_DE[!pre_intervention])
DE_RMSE <- post_rmse_DE/pre_rmse_DE

## Florida

pre_intervention <- Observational_Period < 694
FL_weights <- placebo_results_5$solution.w
synthetic_FL <- as.numeric(placebo_data_list[[5]]$Y0plot %*% FL_weights)
true_FL <- emu_donors[emu_donors$ID2 == 5,]$Acc_PC
pre_rmse_FL <- rmse(x = true_FL[pre_intervention], y = synthetic_FL[pre_intervention])
post_rmse_FL <- rmse(x = true_FL[!pre_intervention], y = synthetic_FL[!pre_intervention])
FL_RMSE <- post_rmse_FL/pre_rmse_FL

## Georgia

pre_intervention <- Observational_Period < 694
GA_weights <- placebo_results_6$solution.w
synthetic_GA <- as.numeric(placebo_data_list[[6]]$Y0plot %*% GA_weights)
true_GA <- emu_donors[emu_donors$ID2 == 6,]$Acc_PC
pre_rmse_GA <- rmse(x = true_GA[pre_intervention], y = synthetic_GA[pre_intervention])
post_rmse_GA <- rmse(x = true_GA[!pre_intervention], y = synthetic_GA[!pre_intervention])
GA_RMSE <- post_rmse_GA/pre_rmse_GA

## Iowa

pre_intervention <- Observational_Period < 694
IA_weights <- placebo_results_7$solution.w
synthetic_IA <- as.numeric(placebo_data_list[[7]]$Y0plot %*% IA_weights)
true_IA <- emu_donors[emu_donors$ID2 == 7,]$Acc_PC
pre_rmse_IA <- rmse(x = true_IA[pre_intervention], y = synthetic_IA[pre_intervention])
post_rmse_IA <- rmse(x = true_IA[!pre_intervention], y = synthetic_IA[!pre_intervention])
IA_RMSE <- post_rmse_IA/pre_rmse_IA

## Illionis

pre_intervention <- Observational_Period < 694
IL_weights <- placebo_results_8$solution.w
synthetic_IL <- as.numeric(placebo_data_list[[8]]$Y0plot %*% IL_weights)
true_IL <- emu_donors[emu_donors$ID2 == 8,]$Acc_PC
pre_rmse_IL <- rmse(x = true_IL[pre_intervention], y = synthetic_IL[pre_intervention])
post_rmse_IL <- rmse(x = true_IL[!pre_intervention], y = synthetic_IL[!pre_intervention])
IL_RMSE <- post_rmse_IL/pre_rmse_IL


## Indiana

pre_intervention <- Observational_Period < 694
IN_weights <- placebo_results_9$solution.w
synthetic_IN <- as.numeric(placebo_data_list[[9]]$Y0plot %*% IN_weights)
true_IN <- emu_donors[emu_donors$ID2 == 9,]$Acc_PC
pre_rmse_IN <- rmse(x = true_IN[pre_intervention], y = synthetic_IN[pre_intervention])
post_rmse_IN <- rmse(x = true_IN[!pre_intervention], y = synthetic_IN[!pre_intervention])
IN_RMSE <- post_rmse_IN/pre_rmse_IN

## Kansas

pre_intervention <- Observational_Period < 694
KS_weights <- placebo_results_10$solution.w
synthetic_KS <- as.numeric(placebo_data_list[[10]]$Y0plot %*% KS_weights)
true_KS <- emu_donors[emu_donors$ID2 == 10,]$Acc_PC
pre_rmse_KS <- rmse(x = true_KS[pre_intervention], y = synthetic_KS[pre_intervention])
post_rmse_KS <- rmse(x = true_KS[!pre_intervention], y = synthetic_KS[!pre_intervention])
KS_RMSE <- post_rmse_KS/pre_rmse_KS

## Kentucky

pre_intervention <- Observational_Period < 694
KY_weights <- placebo_results_11$solution.w
synthetic_KY <- as.numeric(placebo_data_list[[11]]$Y0plot %*% KY_weights)
true_KY <- emu_donors[emu_donors$ID2 == 11,]$Acc_PC
pre_rmse_KY <- rmse(x = true_KY[pre_intervention], y = synthetic_KY[pre_intervention])
post_rmse_KY <- rmse(x = true_KY[!pre_intervention], y = synthetic_KY[!pre_intervention])
KY_RMSE <- post_rmse_KY/pre_rmse_KY

## Louisiana

pre_intervention <- Observational_Period < 694
LA_weights <- placebo_results_12$solution.w
synthetic_LA <- as.numeric(placebo_data_list[[12]]$Y0plot %*% LA_weights)
true_LA <- emu_donors[emu_donors$ID2 == 12,]$Acc_PC
pre_rmse_LA <- rmse(x = true_LA[pre_intervention], y = synthetic_LA[pre_intervention])
post_rmse_LA <- rmse(x = true_LA[!pre_intervention], y = synthetic_LA[!pre_intervention])
LA_RMSE <- post_rmse_LA/pre_rmse_LA

## Maryland

pre_intervention <- Observational_Period < 694
MD_weights <- placebo_results_13$solution.w
synthetic_MD <- as.numeric(placebo_data_list[[13]]$Y0plot %*% MD_weights)
true_MD <- emu_donors[emu_donors$ID2 == 13,]$Acc_PC
pre_rmse_MD <- rmse(x = true_MD[pre_intervention], y = synthetic_MD[pre_intervention])
post_rmse_MD <- rmse(x = true_MD[!pre_intervention], y = synthetic_MD[!pre_intervention])
MD_RMSE <- post_rmse_MD/pre_rmse_MD


## Minnesota 

pre_intervention <- Observational_Period < 694
MN_weights <- placebo_results_14$solution.w
synthetic_MN <- as.numeric(placebo_data_list[[14]]$Y0plot %*% MN_weights)
true_MN <- emu_donors[emu_donors$ID2 == 14,]$Acc_PC
pre_rmse_MN <- rmse(x = true_MN[pre_intervention], y = synthetic_MN[pre_intervention])
post_rmse_MN <- rmse(x = true_MN[!pre_intervention], y = synthetic_MN[!pre_intervention])
MN_RMSE <- post_rmse_MN/pre_rmse_MN

## Montana

pre_intervention <- Observational_Period < 694
MO_weights <- placebo_results_15$solution.w
synthetic_MO <- as.numeric(placebo_data_list[[15]]$Y0plot %*% MO_weights)
true_MO <- emu_donors[emu_donors$ID2 == 15,]$Acc_PC
pre_rmse_MO <- rmse(x = true_MO[pre_intervention], y = synthetic_MO[pre_intervention])
post_rmse_MO <- rmse(x = true_MO[!pre_intervention], y = synthetic_MO[!pre_intervention])
MO_RMSE <- post_rmse_MO/pre_rmse_MO

## Mississippi

pre_intervention <- Observational_Period < 694
MS_weights <- placebo_results_16$solution.w
synthetic_MS <- as.numeric(placebo_data_list[[16]]$Y0plot %*% MS_weights)
true_MS <- emu_donors[emu_donors$ID2 == 16,]$Acc_PC
pre_rmse_MS <- rmse(x = true_MS[pre_intervention], y = synthetic_MS[pre_intervention])
post_rmse_MS <- rmse(x = true_MS[!pre_intervention], y = synthetic_MS[!pre_intervention])
MS_RMSE <- post_rmse_MS/pre_rmse_MS

## North Carolina

pre_intervention <- Observational_Period < 694
NC_weights <- placebo_results_17$solution.w
synthetic_NC <- as.numeric(placebo_data_list[[17]]$Y0plot %*% NC_weights)
true_NC <- emu_donors[emu_donors$ID2 == 17,]$Acc_PC
pre_rmse_NC <- rmse(x = true_NC[pre_intervention], y = synthetic_NC[pre_intervention])
post_rmse_NC <- rmse(x = true_NC[!pre_intervention], y = synthetic_NC[!pre_intervention])
NC_RMSE <- post_rmse_NC/pre_rmse_NC

## Nebraska

pre_intervention <- Observational_Period < 694
NE_weights <- placebo_results_18$solution.w
synthetic_NE <- as.numeric(placebo_data_list[[18]]$Y0plot %*% NE_weights)
true_NE <- emu_donors[emu_donors$ID2 == 18,]$Acc_PC
pre_rmse_NE <- rmse(x = true_NE[pre_intervention], y = synthetic_NE[pre_intervention])
post_rmse_NE <- rmse(x = true_NE[!pre_intervention], y = synthetic_NE[!pre_intervention])
NE_RMSE <- post_rmse_NE/pre_rmse_NE

## New Hampshire

pre_intervention <- Observational_Period < 694
NH_weights <- placebo_results_19$solution.w
synthetic_NH <- as.numeric(placebo_data_list[[19]]$Y0plot %*% NH_weights)
true_NH <- emu_donors[emu_donors$ID2 == 19,]$Acc_PC
pre_rmse_NH <- rmse(x = true_NH[pre_intervention], y = synthetic_NH[pre_intervention])
post_rmse_NH <- rmse(x = true_NH[!pre_intervention], y = synthetic_NH[!pre_intervention])
NH_RMSE <- post_rmse_NH/pre_rmse_NH


## New Jersey

pre_intervention <- Observational_Period < 694
NJ_weights <- placebo_results_20$solution.w
synthetic_NJ <- as.numeric(placebo_data_list[[20]]$Y0plot %*% NJ_weights)
true_NJ <- emu_donors[emu_donors$ID2 == 20,]$Acc_PC
pre_rmse_NJ <- rmse(x = true_NJ[pre_intervention], y = synthetic_NJ[pre_intervention])
post_rmse_NJ <- rmse(x = true_NJ[!pre_intervention], y = synthetic_NJ[!pre_intervention])
NJ_RMSE <- post_rmse_NJ/pre_rmse_NJ

## New Mexico

pre_intervention <- Observational_Period < 694
NM_weights <- placebo_results_21$solution.w
synthetic_NM <- as.numeric(placebo_data_list[[21]]$Y0plot %*% NM_weights)
true_NM <- emu_donors[emu_donors$ID2 == 21,]$Acc_PC
pre_rmse_NM <- rmse(x = true_NM[pre_intervention], y = synthetic_NM[pre_intervention])
post_rmse_NM <- rmse(x = true_NM[!pre_intervention], y = synthetic_NM[!pre_intervention])
NM_RMSE <- post_rmse_NM/pre_rmse_NM

## New York

pre_intervention <- Observational_Period < 694
NY_weights <- placebo_results_22$solution.w
synthetic_NY <- as.numeric(placebo_data_list[[22]]$Y0plot %*% NY_weights)
true_NY <- emu_donors[emu_donors$ID2 == 22,]$Acc_PC
pre_rmse_NY <- rmse(x = true_NY[pre_intervention], y = synthetic_NY[pre_intervention])
post_rmse_NY <- rmse(x = true_NY[!pre_intervention], y = synthetic_NY[!pre_intervention])
NY_RMSE <- post_rmse_NY/pre_rmse_NY

## Ohio 

pre_intervention <- Observational_Period < 694
OH_weights <- placebo_results_23$solution.w
synthetic_OH <- as.numeric(placebo_data_list[[23]]$Y0plot %*% OH_weights)
true_OH <- emu_donors[emu_donors$ID2 == 23,]$Acc_PC
pre_rmse_OH <- rmse(x = true_OH[pre_intervention], y = synthetic_OH[pre_intervention])
post_rmse_OH <- rmse(x = true_OH[!pre_intervention], y = synthetic_OH[!pre_intervention])
OH_RMSE <- post_rmse_OH/pre_rmse_OH

## Oklahoma

pre_intervention <- Observational_Period < 694
OK_weights <- placebo_results_24$solution.w
synthetic_OK <- as.numeric(placebo_data_list[[24]]$Y0plot %*% OK_weights)
true_OK <- emu_donors[emu_donors$ID2 == 24,]$Acc_PC
pre_rmse_OK <- rmse(x = true_OK[pre_intervention], y = synthetic_OK[pre_intervention])
post_rmse_OK <- rmse(x = true_OK[!pre_intervention], y = synthetic_OK[!pre_intervention])
OK_RMSE <- post_rmse_OK/pre_rmse_OK

## Rhode Island

pre_intervention <- Observational_Period < 694
RI_weights <- placebo_results_25$solution.w
synthetic_RI <- as.numeric(placebo_data_list[[25]]$Y0plot %*% RI_weights)
true_RI <- emu_donors[emu_donors$ID2 == 25,]$Acc_PC
pre_rmse_RI <- rmse(x = true_RI[pre_intervention], y = synthetic_RI[pre_intervention])
post_rmse_RI <- rmse(x = true_RI[!pre_intervention], y = synthetic_RI[!pre_intervention])
RI_RMSE <- post_rmse_RI/pre_rmse_RI

## South Carolina
pre_intervention <- Observational_Period < 694
SC_weights <- placebo_results_26$solution.w
synthetic_SC <- as.numeric(placebo_data_list[[26]]$Y0plot %*% SC_weights)
true_SC <- emu_donors[emu_donors$ID2 == 26,]$Acc_PC
pre_rmse_SC <- rmse(x = true_SC[pre_intervention], y = synthetic_SC[pre_intervention])
post_rmse_SC <- rmse(x = true_SC[!pre_intervention], y = synthetic_SC[!pre_intervention])
SC_RMSE <- post_rmse_SC/pre_rmse_SC

## Tenessee
pre_intervention <- Observational_Period < 694
TN_weights <- placebo_results_27$solution.w
synthetic_TN <- as.numeric(placebo_data_list[[27]]$Y0plot %*% TN_weights)
true_TN <- emu_donors[emu_donors$ID2 == 27,]$Acc_PC
pre_rmse_TN <- rmse(x = true_TN[pre_intervention], y = synthetic_TN[pre_intervention])
post_rmse_TN <- rmse(x = true_TN[!pre_intervention], y = synthetic_TN[!pre_intervention])
TN_RMSE <- post_rmse_TN/pre_rmse_TN

## Texas
pre_intervention <- Observational_Period < 694
TX_weights <- placebo_results_28$solution.w
synthetic_TX <- as.numeric(placebo_data_list[[28]]$Y0plot %*% TX_weights)
true_TX <- emu_donors[emu_donors$ID2 == 28,]$Acc_PC
pre_rmse_TX <- rmse(x = true_TX[pre_intervention], y = synthetic_TX[pre_intervention])
post_rmse_TX <- rmse(x = true_TX[!pre_intervention], y = synthetic_TX[!pre_intervention])
TX_RMSE <- post_rmse_TX/pre_rmse_TX

## Virginia
pre_intervention <- Observational_Period < 694
VA_weights <- placebo_results_29$solution.w
synthetic_VA <- as.numeric(placebo_data_list[[29]]$Y0plot %*% VA_weights)
true_VA <- emu_donors[emu_donors$ID2 == 29,]$Acc_PC
pre_rmse_VA <- rmse(x = true_VA[pre_intervention], y = synthetic_VA[pre_intervention])
post_rmse_VA <- rmse(x = true_VA[!pre_intervention], y = synthetic_VA[!pre_intervention])
VA_RMSE <- post_rmse_VA/pre_rmse_VA

## Wyoming
pre_intervention <- Observational_Period < 694
WI_weights <- placebo_results_30$solution.w
synthetic_WI <- as.numeric(placebo_data_list[[30]]$Y0plot %*% WI_weights)
true_WI <- emu_donors[emu_donors$ID2 == 30,]$Acc_PC
pre_rmse_WI <- rmse(x = true_WI[pre_intervention], y = synthetic_WI[pre_intervention])
post_rmse_WI <- rmse(x = true_WI[!pre_intervention], y = synthetic_WI[!pre_intervention])
WI_RMSE <- post_rmse_WI/pre_rmse_WI

## West Virginia
pre_intervention <- Observational_Period < 694
WV_weights <- placebo_results_31$solution.w
synthetic_WV <- as.numeric(placebo_data_list[[31]]$Y0plot %*% WV_weights)
true_WV <- emu_donors[emu_donors$ID2 == 31,]$Acc_PC
pre_rmse_WV <- rmse(x = true_WV[pre_intervention], y = synthetic_WV[pre_intervention])
post_rmse_WV <- rmse(x = true_WV[!pre_intervention], y = synthetic_WV[!pre_intervention])
WV_RMSE <- post_rmse_WV/pre_rmse_WV

# Now we plot all ratios 

rmse_ratios <- data.frame(value = c(CA_RMSE, AL_RMSE, AZ_RMSE, CT_RMSE, IL_RMSE,
                                    DE_RMSE, FL_RMSE, GA_RMSE, IA_RMSE, IN_RMSE, 
                                    KS_RMSE, KY_RMSE, LA_RMSE, MD_RMSE, MN_RMSE, 
                                    MO_RMSE, MS_RMSE, NC_RMSE, NE_RMSE, NH_RMSE, 
                                    NJ_RMSE, NM_RMSE,
                                    OH_RMSE, OK_RMSE, RI_RMSE, SC_RMSE, 
                                    TN_RMSE, TX_RMSE, VA_RMSE, WI_RMSE, WV_RMSE), 
                          name = c("CA_RMSE", "AL_RMSE", "AZ_RMSE", "IL_RMSE",
                                   "CT_RMSE", "DE_RMSE", "FL_RMSE", 
                                   "GA_RMSE", "IA_RMSE","IN_RMSE", "KS_RMSE", 
                                   "KY_RMSE", "LA_RMSE",
                                   "MD_RMSE", "MN_RMSE", "MO_RMSE", "MS_RMSE", 
                                   "NC_RMSE", "NJ_RMSE", "NM_RMSE", "NE_RMSE", "NH_RMSE",
                                   "OH_RMSE", "OK_RMSE",
                                   "RI_RMSE", "SC_RMSE", "TN_RMSE",
                                   "TX_RMSE", "VA_RMSE",
                                   "WI_RMSE", "WV_RMSE"))

highlight_df <- rmse_ratios %>% 
  filter(name == "CA_RMSE")

rmse_r <- ggplot(rmse_ratios, aes(x = value, y = name)) +
  geom_point(color = "gray", fill = "white") + 
  geom_point(data = highlight_df, color = "red", aes(x = value, y = name)) + 
  xlab("RMSE Ratios Pre / Post Treatment") + ylab("US States") + 
  theme(plot.title= element_text(size=14, 
                                 color="grey26",
                                 hjust=0.5,
                                 lineheight=1.2),
        panel.background = element_rect(fill="#f7f7f7"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill="#f7f7f7", color = "#f7f7f7"),
        axis.title.x = element_text(color="grey26", size=12),
        axis.title.y = element_text(color="grey26", size=12),
        axis.line = element_line(color = "black")) + 
  geom_vline(xintercept = mean(rmse_ratios$value), linetype = "dashed") + 
  geom_text(aes(x= 3, y = 4), label="Average RMSE ratio") +
  geom_point(data = highlight_df, color = "red", aes(x = value, y = name))
rmse_r

#### Monthly try out sales start 01.01.18 SCM Part 1  ####

# We have a balanced dataset according to our pre-defined characteristics which we already cleaned

USAccidents <- fread("US_accidents_monthly_PC_cleaned.csv")
USAccidents$Y_M_Date <- as.yearmon(paste(USAccidents$Year, USAccidents$Month), "%Y%m")

# Now we set the filter for the relevant time period

USAccidents <- USAccidents[USAccidents$State %in% 
                             c(unique(USAccidents$State[USAccidents$Year_Month == "Dez 2019" & 
                                                          USAccidents$Rec_Mar_Legal != 1]),"CA"),]
USAccidents$ID <- as.numeric(as.factor(USAccidents$State))
USAccidents$Timeline <- as.numeric(as.factor(USAccidents$Y_M_Date))

# We then set the reference variable to California and the treatment day to the Enactment date of legalisation

Enactment_date <- 24
Observational_Period  <- seq(Enactment_date - 23, Enactment_date + 23)

Cali_ref <- USAccidents$Acc_PC[USAccidents$State == "CA" & USAccidents$Timeline == Enactment_date]


# As we mentioned, we need to clear out several states since we assume them to suffer from large measurement error, which is in-
# consistent with the rest of the data set. Consequently, we set a simulation set including only the members we select with regard
# to the Measurement Error 

Cali_synth <- filter(USAccidents) %>%
  group_by(ID) %>%
  summarise(complete.panel = ifelse(length(Y_M_Date) == length(Observational_Period), 1, 0),
            missing.Acc = sum(is.na(Acc_PC)), 
            missing.VMT = sum(is.na(VMT_PC)),
            missing.rural = sum(is.na(Rural_Mileage_PC)), 
            missing.urban = sum(is.na(Urban_Mileage_PC)),
            missing.RMV = sum(is.na(RMV_PC)),
            missing.PopDens = sum(is.na(Pop_Density)),
            missing.Alc = sum(is.na(Alc_Cons_PC))) 

Cali_control = Cali_synth %>%
  filter(complete.panel == 1 & missing.Acc <= 1 & missing.VMT <= 1 &
           missing.rural <= 1 & missing.urban <= 1 & missing.RMV <= 1 & 
           missing.PopDens <= 1 & missing.Alc <= 1 & ID != 3) %>%
  select(ID) %>%
  unlist()
names(Cali_control) = NULL
Cali_control

USAccidents_real <- USAccidents[which(USAccidents$ID %in% c(Cali_control, unique(USAccidents$ID[USAccidents$State == "CA"]))), ]


match.criteria <- c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC")


USAccidents_real$ID<- as.numeric(as.factor(USAccidents_real$ID))
USAccidents_real$State <- as.character(USAccidents_real$State)
Observational_Period <- as.numeric(Observational_Period)
USAccidents_real <- as.data.frame(USAccidents_real)



### Monthly try out sales start 01.01.18 SCM Part 2 ####

SCM_synth <- dataprep(
  foo = USAccidents_real,
  predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
  time.predictors.prior = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  dependent = "Acc_PC",
  unit.variable = "ID",
  unit.names.variable = "State",
  time.variable = "Timeline",
  treatment.identifier = 3,
  controls.identifier = c(1:2, 4:32), # If for all states: adjust to 4:38
  time.optimize.ssr = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  time.plot = Observational_Period
)

# Apply synth() to those data and inspect matching results via tables
SCM_synth_new <- synth(SCM_synth, optimxmethod="All")
SCM_synth_tables <- synth.tab(dataprep.res = SCM_synth, synth.res = SCM_synth_new)
predictor_means <- SCM_synth_tables$tab.pred  # inspect balance
weights <- as.data.frame(SCM_synth_tables$tab.w) %>% arrange(desc(w.weights))

# Analyze the pre trend features for the Treated vs Synthetic vs the Sample Mean of the variables 
SCM_synth_tables$tab.pred

# Analyze the weight structure 
names(weights)[1] <- "Weights"
names(weights)[2] <- "State"
names(weights)[3] <- "Unit Numbers"
weights

kable(weights, "latex", caption = "Weights for cannabis sales start on monthly basis", booktabs = T)%>%kable_styling(position = "center")


# Plot path

path.case <- SCM_synth$Y1plot
path.synth <- SCM_synth$Y0plot %*% SCM_synth_new$solution.w 

trends_acc <- path.plot(synth.res = SCM_synth_new,
                        dataprep.res = SCM_synth,
                        Xlab = "Months",
                        Ylab = "Accidents per million",
                        Legend = c("Cali", "Synthetic Cali"),
                        tr.intake = 24)



# Plot differences
gap <- SCM_synth$Y1plot - (SCM_synth$Y0plot %*% SCM_synth_new$solution.w)

gap_acc <- plot(x = Observational_Period, y = gap, 
                xlab = "Months", ylab = "Gap in Accidents per million population", lwd = 2) +
  segments(x0 = min(Observational_Period), x1 = max(Observational_Period), y0=0, y1=0, lwd=2, lty=2, col="gray25") +
  abline(v=Enactment_date, col="red", lty=2) + 
  text(x = 960, y = -18, "reform day - 11th November 2016", pos = 2, cex = 0.8) +
  axis(1, tick=FALSE) +
  # Plot the series
  lines(x=Observational_Period, y=gap, type="l", lwd=2)


#### Monthly try out sales start 01.01.18 SCM Part 3 #### 

# Now, we can create the synthetic trends for our different states

# Function to apply; same specs as above for treated case
f.placebo.data <- function(ID) {
  
  placebo.dat <- dataprep(
    # drop the UK from data set for this
    foo = filter(USAccidents_real, State !="CA"),
    predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
    time.predictors.prior = seq(from = min(Observational_Period), to = Enactment_date - 1),
    dependent = "Acc_PC",
    unit.variable = "ID",
    unit.names.variable = "State",
    time.variable = "Timeline",
    treatment.identifier = ID,
    controls.identifier = Cali_control[which(Cali_control != ID)],
    time.optimize.ssr = seq(from = min(Observational_Period), to = Enactment_date - 1),
    time.plot = Observational_Period)
  
  return(placebo.dat)
  
}

placebo_data_list <- lapply(Cali_control, f.placebo.data)

# All states now are the equivalent to the "SCM_synth_new" variable for California

placebo_results_1 <- synth(placebo_data_list[[1]]) 
placebo_results_2 <- synth(placebo_data_list[[2]]) 
placebo_results_3 <- synth(placebo_data_list[[3]]) 
placebo_results_4 <- synth(placebo_data_list[[4]]) 
placebo_results_5 <- synth(placebo_data_list[[5]]) 
placebo_results_6 <- synth(placebo_data_list[[6]]) 
placebo_results_7 <- synth(placebo_data_list[[7]]) 
placebo_results_8 <- synth(placebo_data_list[[8]]) 
placebo_results_9 <- synth(placebo_data_list[[9]]) 
placebo_results_10 <- synth(placebo_data_list[[10]]) 
placebo_results_11 <- synth(placebo_data_list[[11]]) 
placebo_results_12 <- synth(placebo_data_list[[12]])
placebo_results_13 <- synth(placebo_data_list[[13]]) 
placebo_results_14 <- synth(placebo_data_list[[14]]) 
placebo_results_15 <- synth(placebo_data_list[[15]]) 
placebo_results_16 <- synth(placebo_data_list[[16]])
placebo_results_17 <- synth(placebo_data_list[[17]])
placebo_results_18 <- synth(placebo_data_list[[18]])
placebo_results_19 <- synth(placebo_data_list[[19]])
placebo_results_20 <- synth(placebo_data_list[[20]])
placebo_results_21 <- synth(placebo_data_list[[21]])
placebo_results_22 <- synth(placebo_data_list[[22]])
placebo_results_23 <- synth(placebo_data_list[[23]])
placebo_results_24 <- synth(placebo_data_list[[24]])
placebo_results_25 <- synth(placebo_data_list[[25]])
placebo_results_26 <- synth(placebo_data_list[[26]])
placebo_results_27 <- synth(placebo_data_list[[27]])
placebo_results_28 <- synth(placebo_data_list[[28]])
placebo_results_29 <- synth(placebo_data_list[[29]])
placebo_results_30 <- synth(placebo_data_list[[30]])
placebo_results_31 <- synth(placebo_data_list[[31]])


# Function to plot lines from placebo results by number
placebo.line.plot <- function(i) {
  solution.i <- eval(parse(text=paste0("placebo_results_", i, "$solution.w")))
  gap <- placebo_data_list[[i]]$Y1plot - 
    (placebo_data_list[[i]]$Y0plot %*% solution.i)
  lines(x = Observational_Period, y = gap, col = "grey70")
}

## Make plot

# Find gap
gap <- SCM_synth$Y1plot - (SCM_synth$Y0plot %*% SCM_synth_new$solution.w)

# Make vector of placebo tests to plot
placebos <- c(1:37)

# Set up plot using the main time series
plot(x=Observational_Period, y=gap, type="n", xaxs="i", yaxs="i", ylim=c(-400,250),
     xlab="Month", ylab="Differences per capita rel. to synthetic control", lwd=2)

# Plot the placebo time series
for (k in placebos) {
  placebo.line.plot(k)
}

# Horizontal Line at 0
abline(h=0, lwd=2, lty=2)

# Vertical line at the point of treatment
abline(v=Enactment_date, col="red", lty=2)

# Replot the main time series
lines(x=Observational_Period, y=gap, type="l", lwd=2) 

# Legend
legend(x="bottomleft", bty="n", legend=c("California", "placebo tests"), 
       col=c("black","grey70"), lwd=c(2,1), cex=0.8)

#### Monthly try out sales start 01.01.18 SCM Part 4  ####

# Now, we can quantify the selected state with the pre and post treatment RSME ratios

# Define function for calculating the RMSE
rmse <- function(x,y){
  sqrt(mean((x - y)^2))
}

# Define vector for pre/post-intervention subsetting

pre_intervention <- Observational_Period < Enactment_date

## California

# Extract the weights for synthetic Cali
Cali_weights <- SCM_synth_new$solution.w

# Calculate the outcome for synthetic Cali using matrix multiplication
synthetic_Cali <- as.numeric(SCM_synth$Y0plot %*% Cali_weights)

# Extract the true outcome for Cali
true_Cali <- USAccidents_real[USAccidents_real$ID == 3,]$Acc_PC

# Calculate the RMSE for the pre-intervention period for Cali

pre_rmse_CA <- rmse(x = true_Cali[pre_intervention], y = synthetic_Cali[pre_intervention])

# Calculate the RMSE for the post-intervention period for Cali

post_rmse_CA <- rmse(x = true_Cali[!pre_intervention], y = synthetic_Cali[!pre_intervention])

CA_RMSE <- post_rmse_CA/pre_rmse_CA

#### Monthly try out sales start 01.01.18 SCM Part 5 #### 

# Now we can quantify the RMSE ratios for all other states 

## Exclude Cali from data so that it never enters the donor pool

emu_donors <- USAccidents_real[USAccidents_real$ID != 3,]
emu_donors$ID2 <- as.numeric(as.factor(emu_donors$State))


## Alabama

pre_intervention <- Observational_Period < 10
AL_weights <- placebo_results_1$solution.w
synthetic_AL <- as.numeric(placebo_data_list[[1]]$Y0plot %*% AL_weights)
true_AL <- emu_donors[emu_donors$ID2 == 1,]$Acc_PC
pre_rmse_AL <- rmse(x = true_AL[pre_intervention], y = synthetic_AL[pre_intervention])
post_rmse_AL <- rmse(x = true_AL[!pre_intervention], y = synthetic_AL[!pre_intervention])
AL_RMSE <- post_rmse_AL/pre_rmse_AL

## Arizona

pre_intervention <- Observational_Period < 10
AZ_weights <- placebo_results_2$solution.w
synthetic_AZ <- as.numeric(placebo_data_list[[2]]$Y0plot %*% AZ_weights)
true_AZ <- emu_donors[emu_donors$ID2 == 2,]$Acc_PC
pre_rmse_AZ <- rmse(x = true_AZ[pre_intervention], y = synthetic_AZ[pre_intervention])
post_rmse_AZ <- rmse(x = true_AZ[!pre_intervention], y = synthetic_AZ[!pre_intervention])
AZ_RMSE <- post_rmse_AZ/pre_rmse_AZ

## Connecticut

pre_intervention <- Observational_Period < 10
CT_weights <- placebo_results_3$solution.w
synthetic_CT <- as.numeric(placebo_data_list[[3]]$Y0plot %*% CT_weights)
true_CT <- emu_donors[emu_donors$ID2 == 3,]$Acc_PC
pre_rmse_CT <- rmse(x = true_CT[pre_intervention], y = synthetic_CT[pre_intervention])
post_rmse_CT <- rmse(x = true_CT[!pre_intervention], y = synthetic_CT[!pre_intervention])
CT_RMSE <- post_rmse_CT/pre_rmse_CT

## Delaware

pre_intervention <- Observational_Period < 10
DE_weights <- placebo_results_4$solution.w
synthetic_DE <- as.numeric(placebo_data_list[[4]]$Y0plot %*% DE_weights)
true_DE <- emu_donors[emu_donors$ID2 == 4,]$Acc_PC
pre_rmse_DE <- rmse(x = true_DE[pre_intervention], y = synthetic_DE[pre_intervention])
post_rmse_DE <- rmse(x = true_DE[!pre_intervention], y = synthetic_DE[!pre_intervention])
DE_RMSE <- post_rmse_DE/pre_rmse_DE

## Florida

pre_intervention <- Observational_Period < 10
FL_weights <- placebo_results_5$solution.w
synthetic_FL <- as.numeric(placebo_data_list[[5]]$Y0plot %*% FL_weights)
true_FL <- emu_donors[emu_donors$ID2 == 5,]$Acc_PC
pre_rmse_FL <- rmse(x = true_FL[pre_intervention], y = synthetic_FL[pre_intervention])
post_rmse_FL <- rmse(x = true_FL[!pre_intervention], y = synthetic_FL[!pre_intervention])
FL_RMSE <- post_rmse_FL/pre_rmse_FL

## Georgia

pre_intervention <- Observational_Period < 10
GA_weights <- placebo_results_6$solution.w
synthetic_GA <- as.numeric(placebo_data_list[[6]]$Y0plot %*% GA_weights)
true_GA <- emu_donors[emu_donors$ID2 == 6,]$Acc_PC
pre_rmse_GA <- rmse(x = true_GA[pre_intervention], y = synthetic_GA[pre_intervention])
post_rmse_GA <- rmse(x = true_GA[!pre_intervention], y = synthetic_GA[!pre_intervention])
GA_RMSE <- post_rmse_GA/pre_rmse_GA

## Iowa

pre_intervention <- Observational_Period < 10
IA_weights <- placebo_results_7$solution.w
synthetic_IA <- as.numeric(placebo_data_list[[7]]$Y0plot %*% IA_weights)
true_IA <- emu_donors[emu_donors$ID2 == 7,]$Acc_PC
pre_rmse_IA <- rmse(x = true_IA[pre_intervention], y = synthetic_IA[pre_intervention])
post_rmse_IA <- rmse(x = true_IA[!pre_intervention], y = synthetic_IA[!pre_intervention])
IA_RMSE <- post_rmse_IA/pre_rmse_IA

## Illionis

pre_intervention <- Observational_Period < 10
IL_weights <- placebo_results_8$solution.w
synthetic_IL <- as.numeric(placebo_data_list[[8]]$Y0plot %*% IL_weights)
true_IL <- emu_donors[emu_donors$ID2 == 8,]$Acc_PC
pre_rmse_IL <- rmse(x = true_IL[pre_intervention], y = synthetic_IL[pre_intervention])
post_rmse_IL <- rmse(x = true_IL[!pre_intervention], y = synthetic_IL[!pre_intervention])
IL_RMSE <- post_rmse_IL/pre_rmse_IL


## Indiana

pre_intervention <- Observational_Period < 10
IN_weights <- placebo_results_9$solution.w
synthetic_IN <- as.numeric(placebo_data_list[[9]]$Y0plot %*% IN_weights)
true_IN <- emu_donors[emu_donors$ID2 == 9,]$Acc_PC
pre_rmse_IN <- rmse(x = true_IN[pre_intervention], y = synthetic_IN[pre_intervention])
post_rmse_IN <- rmse(x = true_IN[!pre_intervention], y = synthetic_IN[!pre_intervention])
IN_RMSE <- post_rmse_IN/pre_rmse_IN

## Kansas

pre_intervention <- Observational_Period < 10
KS_weights <- placebo_results_10$solution.w
synthetic_KS <- as.numeric(placebo_data_list[[10]]$Y0plot %*% KS_weights)
true_KS <- emu_donors[emu_donors$ID2 == 10,]$Acc_PC
pre_rmse_KS <- rmse(x = true_KS[pre_intervention], y = synthetic_KS[pre_intervention])
post_rmse_KS <- rmse(x = true_KS[!pre_intervention], y = synthetic_KS[!pre_intervention])
KS_RMSE <- post_rmse_KS/pre_rmse_KS

## Kentucky

pre_intervention <- Observational_Period < 10
KY_weights <- placebo_results_11$solution.w
synthetic_KY <- as.numeric(placebo_data_list[[11]]$Y0plot %*% KY_weights)
true_KY <- emu_donors[emu_donors$ID2 == 11,]$Acc_PC
pre_rmse_KY <- rmse(x = true_KY[pre_intervention], y = synthetic_KY[pre_intervention])
post_rmse_KY <- rmse(x = true_KY[!pre_intervention], y = synthetic_KY[!pre_intervention])
KY_RMSE <- post_rmse_KY/pre_rmse_KY

## Louisiana

pre_intervention <- Observational_Period < 10
LA_weights <- placebo_results_12$solution.w
synthetic_LA <- as.numeric(placebo_data_list[[12]]$Y0plot %*% LA_weights)
true_LA <- emu_donors[emu_donors$ID2 == 12,]$Acc_PC
pre_rmse_LA <- rmse(x = true_LA[pre_intervention], y = synthetic_LA[pre_intervention])
post_rmse_LA <- rmse(x = true_LA[!pre_intervention], y = synthetic_LA[!pre_intervention])
LA_RMSE <- post_rmse_LA/pre_rmse_LA

## Maryland

pre_intervention <- Observational_Period < 10
MD_weights <- placebo_results_13$solution.w
synthetic_MD <- as.numeric(placebo_data_list[[13]]$Y0plot %*% MD_weights)
true_MD <- emu_donors[emu_donors$ID2 == 13,]$Acc_PC
pre_rmse_MD <- rmse(x = true_MD[pre_intervention], y = synthetic_MD[pre_intervention])
post_rmse_MD <- rmse(x = true_MD[!pre_intervention], y = synthetic_MD[!pre_intervention])
MD_RMSE <- post_rmse_MD/pre_rmse_MD


## Minnesota 

pre_intervention <- Observational_Period < 10
MN_weights <- placebo_results_14$solution.w
synthetic_MN <- as.numeric(placebo_data_list[[14]]$Y0plot %*% MN_weights)
true_MN <- emu_donors[emu_donors$ID2 == 14,]$Acc_PC
pre_rmse_MN <- rmse(x = true_MN[pre_intervention], y = synthetic_MN[pre_intervention])
post_rmse_MN <- rmse(x = true_MN[!pre_intervention], y = synthetic_MN[!pre_intervention])
MN_RMSE <- post_rmse_MN/pre_rmse_MN

## Montana

pre_intervention <- Observational_Period < 10
MO_weights <- placebo_results_15$solution.w
synthetic_MO <- as.numeric(placebo_data_list[[15]]$Y0plot %*% MO_weights)
true_MO <- emu_donors[emu_donors$ID2 == 15,]$Acc_PC
pre_rmse_MO <- rmse(x = true_MO[pre_intervention], y = synthetic_MO[pre_intervention])
post_rmse_MO <- rmse(x = true_MO[!pre_intervention], y = synthetic_MO[!pre_intervention])
MO_RMSE <- post_rmse_MO/pre_rmse_MO

## Mississippi

pre_intervention <- Observational_Period < 10
MS_weights <- placebo_results_16$solution.w
synthetic_MS <- as.numeric(placebo_data_list[[16]]$Y0plot %*% MS_weights)
true_MS <- emu_donors[emu_donors$ID2 == 16,]$Acc_PC
pre_rmse_MS <- rmse(x = true_MS[pre_intervention], y = synthetic_MS[pre_intervention])
post_rmse_MS <- rmse(x = true_MS[!pre_intervention], y = synthetic_MS[!pre_intervention])
MS_RMSE <- post_rmse_MS/pre_rmse_MS

## North Carolina

pre_intervention <- Observational_Period < 10
NC_weights <- placebo_results_17$solution.w
synthetic_NC <- as.numeric(placebo_data_list[[17]]$Y0plot %*% NC_weights)
true_NC <- emu_donors[emu_donors$ID2 == 17,]$Acc_PC
pre_rmse_NC <- rmse(x = true_NC[pre_intervention], y = synthetic_NC[pre_intervention])
post_rmse_NC <- rmse(x = true_NC[!pre_intervention], y = synthetic_NC[!pre_intervention])
NC_RMSE <- post_rmse_NC/pre_rmse_NC

## Nebraska

pre_intervention <- Observational_Period < 10
NE_weights <- placebo_results_18$solution.w
synthetic_NE <- as.numeric(placebo_data_list[[18]]$Y0plot %*% NE_weights)
true_NE <- emu_donors[emu_donors$ID2 == 18,]$Acc_PC
pre_rmse_NE <- rmse(x = true_NE[pre_intervention], y = synthetic_NE[pre_intervention])
post_rmse_NE <- rmse(x = true_NE[!pre_intervention], y = synthetic_NE[!pre_intervention])
NE_RMSE <- post_rmse_NE/pre_rmse_NE

## New Hampshire

pre_intervention <- Observational_Period < 10
NH_weights <- placebo_results_19$solution.w
synthetic_NH <- as.numeric(placebo_data_list[[19]]$Y0plot %*% NH_weights)
true_NH <- emu_donors[emu_donors$ID2 == 19,]$Acc_PC
pre_rmse_NH <- rmse(x = true_NH[pre_intervention], y = synthetic_NH[pre_intervention])
post_rmse_NH <- rmse(x = true_NH[!pre_intervention], y = synthetic_NH[!pre_intervention])
NH_RMSE <- post_rmse_NH/pre_rmse_NH


## New Jersey

pre_intervention <- Observational_Period < 10
NJ_weights <- placebo_results_20$solution.w
synthetic_NJ <- as.numeric(placebo_data_list[[20]]$Y0plot %*% NJ_weights)
true_NJ <- emu_donors[emu_donors$ID2 == 20,]$Acc_PC
pre_rmse_NJ <- rmse(x = true_NJ[pre_intervention], y = synthetic_NJ[pre_intervention])
post_rmse_NJ <- rmse(x = true_NJ[!pre_intervention], y = synthetic_NJ[!pre_intervention])
NJ_RMSE <- post_rmse_NJ/pre_rmse_NJ

## New Mexico

pre_intervention <- Observational_Period < 10
NM_weights <- placebo_results_21$solution.w
synthetic_NM <- as.numeric(placebo_data_list[[21]]$Y0plot %*% NM_weights)
true_NM <- emu_donors[emu_donors$ID2 == 21,]$Acc_PC
pre_rmse_NM <- rmse(x = true_NM[pre_intervention], y = synthetic_NM[pre_intervention])
post_rmse_NM <- rmse(x = true_NM[!pre_intervention], y = synthetic_NM[!pre_intervention])
NM_RMSE <- post_rmse_NM/pre_rmse_NM

## New York

pre_intervention <- Observational_Period < 10
NY_weights <- placebo_results_22$solution.w
synthetic_NY <- as.numeric(placebo_data_list[[22]]$Y0plot %*% NY_weights)
true_NY <- emu_donors[emu_donors$ID2 == 22,]$Acc_PC
pre_rmse_NY <- rmse(x = true_NY[pre_intervention], y = synthetic_NY[pre_intervention])
post_rmse_NY <- rmse(x = true_NY[!pre_intervention], y = synthetic_NY[!pre_intervention])
NY_RMSE <- post_rmse_NY/pre_rmse_NY

## Ohio 

pre_intervention <- Observational_Period < 10
OH_weights <- placebo_results_23$solution.w
synthetic_OH <- as.numeric(placebo_data_list[[23]]$Y0plot %*% OH_weights)
true_OH <- emu_donors[emu_donors$ID2 == 23,]$Acc_PC
pre_rmse_OH <- rmse(x = true_OH[pre_intervention], y = synthetic_OH[pre_intervention])
post_rmse_OH <- rmse(x = true_OH[!pre_intervention], y = synthetic_OH[!pre_intervention])
OH_RMSE <- post_rmse_OH/pre_rmse_OH

## Oklahoma

pre_intervention <- Observational_Period < 10
OK_weights <- placebo_results_24$solution.w
synthetic_OK <- as.numeric(placebo_data_list[[24]]$Y0plot %*% OK_weights)
true_OK <- emu_donors[emu_donors$ID2 == 24,]$Acc_PC
pre_rmse_OK <- rmse(x = true_OK[pre_intervention], y = synthetic_OK[pre_intervention])
post_rmse_OK <- rmse(x = true_OK[!pre_intervention], y = synthetic_OK[!pre_intervention])
OK_RMSE <- post_rmse_OK/pre_rmse_OK

## Rhode Island

pre_intervention <- Observational_Period < 10
RI_weights <- placebo_results_25$solution.w
synthetic_RI <- as.numeric(placebo_data_list[[25]]$Y0plot %*% RI_weights)
true_RI <- emu_donors[emu_donors$ID2 == 25,]$Acc_PC
pre_rmse_RI <- rmse(x = true_RI[pre_intervention], y = synthetic_RI[pre_intervention])
post_rmse_RI <- rmse(x = true_RI[!pre_intervention], y = synthetic_RI[!pre_intervention])
RI_RMSE <- post_rmse_RI/pre_rmse_RI

## South Carolina
pre_intervention <- Observational_Period < 10
SC_weights <- placebo_results_26$solution.w
synthetic_SC <- as.numeric(placebo_data_list[[26]]$Y0plot %*% SC_weights)
true_SC <- emu_donors[emu_donors$ID2 == 26,]$Acc_PC
pre_rmse_SC <- rmse(x = true_SC[pre_intervention], y = synthetic_SC[pre_intervention])
post_rmse_SC <- rmse(x = true_SC[!pre_intervention], y = synthetic_SC[!pre_intervention])
SC_RMSE <- post_rmse_SC/pre_rmse_SC

## Tenessee
pre_intervention <- Observational_Period < 10
TN_weights <- placebo_results_27$solution.w
synthetic_TN <- as.numeric(placebo_data_list[[27]]$Y0plot %*% TN_weights)
true_TN <- emu_donors[emu_donors$ID2 == 27,]$Acc_PC
pre_rmse_TN <- rmse(x = true_TN[pre_intervention], y = synthetic_TN[pre_intervention])
post_rmse_TN <- rmse(x = true_TN[!pre_intervention], y = synthetic_TN[!pre_intervention])
TN_RMSE <- post_rmse_TN/pre_rmse_TN

## Texas
pre_intervention <- Observational_Period < 10
TX_weights <- placebo_results_28$solution.w
synthetic_TX <- as.numeric(placebo_data_list[[28]]$Y0plot %*% TX_weights)
true_TX <- emu_donors[emu_donors$ID2 == 28,]$Acc_PC
pre_rmse_TX <- rmse(x = true_TX[pre_intervention], y = synthetic_TX[pre_intervention])
post_rmse_TX <- rmse(x = true_TX[!pre_intervention], y = synthetic_TX[!pre_intervention])
TX_RMSE <- post_rmse_TX/pre_rmse_TX

## Virginia
pre_intervention <- Observational_Period < 10
VA_weights <- placebo_results_29$solution.w
synthetic_VA <- as.numeric(placebo_data_list[[29]]$Y0plot %*% VA_weights)
true_VA <- emu_donors[emu_donors$ID2 == 29,]$Acc_PC
pre_rmse_VA <- rmse(x = true_VA[pre_intervention], y = synthetic_VA[pre_intervention])
post_rmse_VA <- rmse(x = true_VA[!pre_intervention], y = synthetic_VA[!pre_intervention])
VA_RMSE <- post_rmse_VA/pre_rmse_VA

## Wyoming
pre_intervention <- Observational_Period < 10
WI_weights <- placebo_results_30$solution.w
synthetic_WI <- as.numeric(placebo_data_list[[30]]$Y0plot %*% WI_weights)
true_WI <- emu_donors[emu_donors$ID2 == 30,]$Acc_PC
pre_rmse_WI <- rmse(x = true_WI[pre_intervention], y = synthetic_WI[pre_intervention])
post_rmse_WI <- rmse(x = true_WI[!pre_intervention], y = synthetic_WI[!pre_intervention])
WI_RMSE <- post_rmse_WI/pre_rmse_WI

## West Virginia
pre_intervention <- Observational_Period < 10
WV_weights <- placebo_results_31$solution.w
synthetic_WV <- as.numeric(placebo_data_list[[31]]$Y0plot %*% WV_weights)
true_WV <- emu_donors[emu_donors$ID2 == 31,]$Acc_PC
pre_rmse_WV <- rmse(x = true_WV[pre_intervention], y = synthetic_WV[pre_intervention])
post_rmse_WV <- rmse(x = true_WV[!pre_intervention], y = synthetic_WV[!pre_intervention])
WV_RMSE <- post_rmse_WV/pre_rmse_WV

# Now we plot all ratios 

rmse_ratios <- data.frame(value = c(CA_RMSE, AL_RMSE, AZ_RMSE, CT_RMSE, IL_RMSE,
                                    DE_RMSE, FL_RMSE, GA_RMSE, IA_RMSE, IN_RMSE, 
                                    KS_RMSE, KY_RMSE, LA_RMSE, MD_RMSE, MN_RMSE, 
                                    MO_RMSE, MS_RMSE, NC_RMSE, NE_RMSE, NH_RMSE, 
                                    NJ_RMSE, NM_RMSE,
                                    NY_RMSE, OH_RMSE, OK_RMSE, RI_RMSE, SC_RMSE, 
                                    TN_RMSE, TX_RMSE, VA_RMSE, WI_RMSE, WV_RMSE), 
                          name = c("CA_RMSE", "AL_RMSE", "AZ_RMSE", "IL_RMSE",
                                   "CT_RMSE", "DE_RMSE", "FL_RMSE", 
                                   "GA_RMSE", "IA_RMSE","IN_RMSE", "KS_RMSE", 
                                   "KY_RMSE", "LA_RMSE",
                                   "MD_RMSE", "MN_RMSE", "MO_RMSE", "MS_RMSE", 
                                   "NC_RMSE", "NJ_RMSE", "NM_RMSE", "NE_RMSE", "NH_RMSE",
                                   "NY_RMSE", "OH_RMSE", "OK_RMSE",
                                   "RI_RMSE", "SC_RMSE", "TN_RMSE",
                                   "TX_RMSE", "VA_RMSE",
                                   "WI_RMSE", "WV_RMSE"))

highlight_df <- rmse_ratios %>% 
  filter(name == "CA_RMSE")

rmse_r <- ggplot(rmse_ratios, aes(x = value, y = name)) +
  geom_point(color = "gray", fill = "white") + 
  geom_point(data = highlight_df, color = "red", aes(x = value, y = name)) + 
  xlab("RMSE Ratios Pre / Post Treatment") + ylab("US States") + 
  theme(plot.title= element_text(size=14, 
                                 color="grey26",
                                 hjust=0.5,
                                 lineheight=1.2),
        panel.background = element_rect(fill="#f7f7f7"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill="#f7f7f7", color = "#f7f7f7"),
        axis.title.x = element_text(color="grey26", size=12),
        axis.title.y = element_text(color="grey26", size=12),
        axis.line = element_line(color = "black")) + 
  geom_vline(xintercept = mean(rmse_ratios$value), linetype = "dashed") + 
  geom_text(aes(x= 9, y = 4), label="Average RMSE ratio") +
  geom_point(data = highlight_df, color = "red", aes(x = value, y = name))

rmse_r

########### Difference in Difference Estimation
###### Treatment 1 (Weed Legalization) - Daily SCM 1 [With controls, WITH response, only states where weed is illegal by 31.12.19] ####
#### Get the weights of the Synthtic Version of California / Synthetic Control Group (SCG) ####
### Synthetic Control Method Part 1 - Preparation of the dataset ### 

# Calculation of the SCM method for the analysis of marijuana consumption on traffic accidents

# This part corresponds to the "Synthetic Control Method" part of our "Data and Methodology" section. 

# We introdcute the SCM and perform the necessary steps in order to receive a synthetic control of our treatment state for
# the observational period. 

# We have a balanced dataset according to our pre-defined characteristics which we already cleaned

USAccidents <- fread("US_accidents_PC_cleaned.csv", sep = ",", header = T)
USAccidents <- as.data.frame(USAccidents)
USAccidents$Date <- ymd(USAccidents$Date)


# Now we can load the synth package and start preparing the final dataset
USAccidents <- USAccidents[USAccidents$State %in% 
                             c(unique(USAccidents$State[USAccidents$Date == ymd("2019-12-31") & 
                                                          USAccidents$Rec_Mar_Legal != 1]),"CA"),]
USAccidents$ID <- as.numeric(as.factor(USAccidents$State))
USAccidents$Timeline <- as.numeric(as.factor(USAccidents$Date))


# Now we set the filter for the relevant time period

Enactment_date <- 276
Observational_Period  <- seq(Enactment_date - 275, Enactment_date + 1147)

# We then set the reference variable to California and the treatment day to the Enactment date of legalisation

Cali_ref <- USAccidents$Acc_PC[USAccidents$State == "CA" & USAccidents$Timeline == Enactment_date]
Cali_ref

# As we mentioned, we need to clear out several states since we assume them to suffer from large measurement error, which is in-
# consistent with the rest of the data set. Consequently, we set a simulation set including only the members we select with regard
# to the Measurement Error 

Cali_synth <- filter(USAccidents) %>%
  group_by(ID) %>%
  summarise(complete.panel = ifelse(length(Date) == length(Observational_Period), 1, 0),
            missing.Acc = sum(is.na(Acc_PC)), 
            missing.VMT = sum(is.na(VMT_PC)),
            missing.rural = sum(is.na(Rural_Mileage_PC)), 
            missing.urban = sum(is.na(Urban_Mileage_PC)),
            missing.RMV = sum(is.na(RMV_PC)),
            missing.PopDens = sum(is.na(Pop_Density)),
            missing.Alc = sum(is.na(Alc_Cons_PC))) 

Cali_control = Cali_synth %>%
  filter(complete.panel == 1 & missing.Acc <= 1 & missing.VMT <= 1 &
           missing.rural <= 1 & missing.urban <= 1 & missing.RMV <= 1 & 
           missing.PopDens <= 1 & missing.Alc <= 1 & ID != 3) %>%
  select(ID) %>%
  unlist()
names(Cali_control) = NULL

USAccidents_real <- USAccidents[which(USAccidents$ID %in% c(Cali_control, unique(USAccidents$ID[USAccidents$State == "CA"]))), ]


match.criteria <- c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC")


USAccidents_real$ID<- as.numeric(as.factor(USAccidents_real$ID))
USAccidents_real$State <- as.character(USAccidents_real$State)
USAccidents_real$Date <- ymd(USAccidents_real$Date)
Observational_Period <- as.numeric(Observational_Period)
USAccidents_real <- as.data.frame(USAccidents_real)



### Synthetic Control Method Part 2 - Calculating the Synthetic version of California

SCM_synth <- dataprep(
  foo = USAccidents_real,
  predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
  time.predictors.prior = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  dependent = "Acc_PC",
  unit.variable = "ID",
  unit.names.variable = "State",
  time.variable = "Timeline",
  treatment.identifier = 3,
  controls.identifier = c(1:2, 4:32), # If for all states: adjust to 4:38
  time.optimize.ssr = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  time.plot = Observational_Period
)

# Apply synth() to those data and inspect matching results via tables
SCM_synth_new <- synth(SCM_synth, optimxmethod="All")
SCM_synth_tables <- synth.tab(dataprep.res = SCM_synth, synth.res = SCM_synth_new)
SCM_synth_tables$tab.pred  # inspect balance
weights <- as.data.frame(SCM_synth_tables$tab.w) %>% arrange(desc(w.weights))

# Analyze the pre trend features for the Treated vs Synthetic vs the Sample Mean of the variables 
SCM_synth_tables$tab.pred

# Analyze the weight structure 
names(weights)[1] <- "Weights"
names(weights)[2] <- "State"
names(weights)[3] <- "Unit Numbers"
weights

kable(weights, "latex", caption = "Weights of the Synthetic Control for cannabis legalisation analysis on daily basis", booktabs = T)%>%kable_styling(position = "center")


# Remove all unnecessary variables
rm(Cali_synth, SCM_synth, SCM_synth_new, SCM_synth_tables, USAccidents_real, Cali_control, 
   Cali_ref, match.criteria, Observational_Period)


#### Create the Dataset needed for estimating a DiD model ####

USAccidents_did <- USAccidents
USAccidents_did$Year_Month <- as.yearmon(paste(USAccidents_did$Year, USAccidents_did$Month), "%Y %m")

# Get the weights
weights_1 <- weights[,c("State","Weights")] # check if the State variable is changed from "Country" to "State"
USAccidents_did <- merge(USAccidents_did, weights_1, by = c("State"), all.x = T)
rm(weights,weights_1)
USAccidents_did$Weights[USAccidents_did$State == "CA"] <- 0
USAccidents_did <- as.data.table(USAccidents_did)
USAccidents_did <- USAccidents_did[order(State,Date)]

# Generate the synthetic control group
USAccidents_weighted <- USAccidents_did

USAccidents_weighted[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                        "Urban_Mileage_PC", "RMV_PC", "Population",
                        "Pop_Density", "Alc_Cons_PC")] <- 
  USAccidents_did$Weights*USAccidents_did[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                                             "Urban_Mileage_PC", "RMV_PC", "Population",
                                             "Pop_Density", "Alc_Cons_PC")]

USAccidents_weighted <- USAccidents_weighted[, list(Acc_PC           = sum(Acc_PC),
                                                    Acc_per_RMV      = sum(Acc_per_RMV),
                                                    VMT_PC           = sum(VMT_PC),
                                                    Rural_Mileage_PC = sum(Rural_Mileage_PC),
                                                    Urban_Mileage_PC = sum(Urban_Mileage_PC),
                                                    RMV_PC           = sum(RMV_PC),
                                                    Population       = sum(Population),
                                                    Pop_Density      = sum(Pop_Density),
                                                    Alc_Cons_PC      = sum(Alc_Cons_PC)),
                                             by = Date]

USAccidents_did$State[USAccidents_did$State == "NY"] <- "SCG"

USAccidents_did[USAccidents_did$State == "SCG", 
                c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                  "Urban_Mileage_PC", "RMV_PC", "Population",
                  "Pop_Density", "Alc_Cons_PC")] <-
  USAccidents_weighted[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                          "Urban_Mileage_PC", "RMV_PC", "Population",
                          "Pop_Density", "Alc_Cons_PC")]
rm(USAccidents_weighted)

USAccidents_did <- USAccidents_did[USAccidents_did$State %in% c("CA","SCG"),
                                   -c("Avg_Severity", "Med_Mar_Legal", "Sale_Mar_Legal_Neighbour",
                                      "ID","Timeline","Weights")]

USAccidents$Rec_Mar_Legal[USAccidents$State == "SCG"] <- 0

USAccidents$Sale_Mar_Legal[USAccidents$State == "SCG"] <- 0



#### Trend Plots ####
# Daily accidents
ggplot(USAccidents_did, aes(Date, Acc_PC, col = State)) +
  geom_line() +
  geom_vline(xintercept = USAccidents_did$Date[276], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw()

# Monthly accidents
ggplot(USAccidents_did[,list(Acc_PC = sum(Acc_PC)), 
                       by = list(Year_Month, State)], 
       aes(Year_Month, Acc_PC, col = State)) +
  geom_line(size = 1) +
  geom_vline(xintercept = USAccidents_did$Year_Month[276], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw() +
  xlab("Month") +
  ylab("Accidents per million")



#### Create additionally needed dummies ####
# Create a dummy which indicates the post treatment period
USAccidents_did$Post_Treat <- USAccidents_did$Rec_Mar_Legal
USAccidents_did$Post_Treat[USAccidents_did$State == "SCG"] <-
  USAccidents_did$Rec_Mar_Legal[USAccidents_did$State == "CA"]

# Create a dummy which indicates the treatment group
USAccidents_did$Treat_Grp <- as.numeric(USAccidents_did$State == "CA")

# Create the interaction dummy between treatment group and post treatment
USAccidents_did$Post_Treat_X_Treat_Grp <- USAccidents_did$Post_Treat * USAccidents_did$Treat_Grp

# Create the interaction dummy between the source dummies and the treatment group dummy
USAccidents_did$SRC_Bing_X_Treat_Grp <- USAccidents_did$SRC_Bing * USAccidents_did$Treat_Grp
USAccidents_did$SRC_MQ_X_Treat_Grp   <- USAccidents_did$SRC_MQ * USAccidents_did$Treat_Grp

# Transform the Day_of_Week variable into a factor to be able to control for weekday FE later on
USAccidents_did$Day_of_Week <- as.factor(USAccidents_did$Day_of_Week)

# Transform the Month variable into a factor to be able to control for month FE later on
USAccidents_did$Month <- as.factor(USAccidents_did$Month)

# Transform the Year variable into a factor to be able to control for year FE later on
USAccidents_did$Year <- as.factor(USAccidents_did$Year)



#### Estimate the Diff-in-Diff model ####
# Basic model
did_daily_PC.1 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp, 
                     USAccidents_did)

stargazer(did_daily_PC.1, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Basic model only for time period before MQ was added as a source
did_daily_PC.2 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp, 
                     USAccidents_did[USAccidents_did$Date < ymd("2017-07-01")])

stargazer(did_daily_PC.2, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Adding source variable to basic model (without interaction variable of source*treatment)
did_daily_PC.3 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp +
                       SRC_Bing + SRC_MQ, 
                     USAccidents_did)

stargazer(did_daily_PC.3, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Adding source variable to basic model (including interaction variable of source*treatment)
did_daily_PC.4 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp +
                       SRC_Bing + SRC_MQ + SRC_MQ_X_Treat_Grp + SRC_Bing_X_Treat_Grp, 
                     USAccidents_did)

stargazer(did_daily_PC.4, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Adding Time FE
did_daily_PC.5 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp +
                       SRC_Bing + SRC_MQ + SRC_MQ_X_Treat_Grp + SRC_Bing_X_Treat_Grp + 
                       Day_of_Week + Month + Year, 
                     USAccidents_did)

stargazer(did_daily_PC.5, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)



# Overview
# Whole time frame vs. only before July 2017 (before data from MQ is added)
stargazer(did_daily_PC.1, did_daily_PC.2, 
          type = "text", 
          title = "DiD estimation",
          column.labels = c("Model (1)","Model (2)"),
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)

# Source dummies added: with vs without source*treat interaction term
stargazer(did_daily_PC.3, did_daily_PC.4, 
          type = "text", 
          title = "DiD estimation",
          column.labels = c("Model (3)","Model (4)"),
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)

# Overview of short time frame vs. whole time frame, source & interaction terms added vs. time FE added
stargazer(did_daily_PC.2, did_daily_PC.4, did_daily_PC.5,
          type = "text", 
          title = "DiD estimation", 
          column.labels = c("Model (2)","Model (4)","Model (5)"),
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3, omit.stat = "F")



### Residuals plot ####
# Basic model
# Daily
USAccidents_did$Residuals <- did_daily_PC.1$residuals

ggplot(USAccidents_did, aes(Date, Residuals, col = State)) +
  geom_line(size = 1) +
  geom_vline(xintercept = USAccidents_did$Date[Enactment_date], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw()

# Monthly
ggplot(USAccidents_did[,list(Residuals = sum(Residuals)), 
                       by = list(Year_Month, State)], 
       aes(Year_Month, Residuals, col = State)) +
  geom_line(size = 1) +
  geom_vline(xintercept = USAccidents_did$Year_Month[Enactment_date], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw() +
  xlab("Month") +
  ggtitle("Residuals of Model (1)")

USAccidents_did$Residuals <- NULL


# Source FE
USAccidents_did$Residuals <- did_daily_PC.3$residuals

ggplot(USAccidents_did[,list(Residuals = sum(Residuals)), 
                       by = list(Year_Month, State)], 
       aes(Year_Month, Residuals, col = State)) +
  geom_line(size = 1) +
  geom_vline(xintercept = USAccidents_did$Year_Month[Enactment_date], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw() +
  xlab("Month") +
  ggtitle("Residuals of Model (3)")

USAccidents_did$Residuals <- NULL


# Source FE & Interaction
USAccidents_did$Residuals <- did_daily_PC.4$residuals

ggplot(USAccidents_did[,list(Residuals = sum(Residuals)), 
                       by = list(Year_Month, State)], 
       aes(Year_Month, Residuals, col = State)) +
  geom_line() +
  geom_vline(xintercept = USAccidents_did$Year_Month[Enactment_date], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw() +
  xlab("Month") +
  ggtitle("Residuals of Model (4)")

USAccidents_did$Residuals <- NULL


# Time FE
USAccidents_did$Residuals <- did_daily_PC.5$residuals

ggplot(USAccidents_did[,list(Residuals = sum(Residuals)), 
                       by = list(Year_Month, State)], 
       aes(Year_Month, Residuals, col = State)) +
  geom_line() +
  geom_vline(xintercept = USAccidents_did$Year_Month[Enactment_date], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw()

USAccidents_did$Residuals <- NULL


# Residual plots
USAccidents_did$Residuals0 <- did_daily_PC.1$residuals
USAccidents_did$Residuals1 <- did_daily_PC.3$residuals
USAccidents_did$Residuals2 <- did_daily_PC.4$residuals

grid.arrange(
  ggplot(USAccidents_did[,list(Residuals = sum(Residuals0)), 
                         by = list(Year_Month, State)], 
         aes(Year_Month, Residuals, col = State)) +
    geom_line(size = 1) +
    geom_vline(xintercept = USAccidents_did$Year_Month[Enactment_date], linetype="dashed", 
               color = "red", size = 1) +
    theme_bw() +
    xlab("Month") +
    ggtitle("Residuals of Model (1)"),
  ggplot(USAccidents_did[,list(Residuals = sum(Residuals1)), 
                         by = list(Year_Month, State)], 
         aes(Year_Month, Residuals, col = State)) +
    geom_line(size = 1) +
    geom_vline(xintercept = USAccidents_did$Year_Month[Enactment_date], linetype="dashed", 
               color = "red", size = 1) +
    theme_bw() +
    xlab("Month") +
    ggtitle("Residuals of Model (3)"),
  ggplot(USAccidents_did[,list(Residuals = sum(Residuals2)), 
                         by = list(Year_Month, State)], 
         aes(Year_Month, Residuals, col = State)) +
    geom_line(size = 1) +
    geom_vline(xintercept = USAccidents_did$Year_Month[Enactment_date], linetype="dashed", 
               color = "red", size = 1) +
    theme_bw() +
    xlab("Month") +
    ggtitle("Residuals of Model (4)"),
  nrow = 3)

USAccidents_did$Residuals0 <- NULL
USAccidents_did$Residuals1 <- NULL
USAccidents_did$Residuals2 <- NULL


###### Treatment 2 (Sales start) - Daily SCM 1 [Whole time frame, With Controls, WITH response & Only states where weed is illegal by 31.12.19] ####
#### Get the weights of the Synthtic Version of California / Synthetic Control Group (SCG) ####
### Synthetic Control Method Part 1 - Preparation of the dataset ### 

# Calculation of the SCM method for the analysis of marijuana consumption on traffic accidents

# This part corresponds to the "Synthetic Control Method" part of our "Data and Methodology" section. 

# We introdcute the SCM and perform the necessary steps in order to receive a synthetic control of our treatment state for
# the observational period. 

# We have a balanced dataset according to our pre-defined characteristics which we already cleaned

USAccidents <- fread("US_accidents_PC_cleaned.csv", sep = ",", header = T)
USAccidents <- as.data.frame(USAccidents)
USAccidents$Date <- ymd(USAccidents$Date)


# Now we can load the synth package and start preparing the final dataset
USAccidents <- USAccidents[USAccidents$State %in% 
                             c(unique(USAccidents$State[USAccidents$Date == ymd("2019-12-31") & 
                                                          USAccidents$Rec_Mar_Legal != 1]),"CA"),]
USAccidents$ID <- as.numeric(as.factor(USAccidents$State))
USAccidents$Timeline <- as.numeric(as.factor(USAccidents$Date))


# Now we set the filter for the relevant time period

Enactment_date <- 694
Observational_Period  <- seq(Enactment_date - 693, Enactment_date + 729)

# We then set the reference variable to California and the treatment day to the Enactment date of legalisation

Cali_ref <- USAccidents$Acc_PC[USAccidents$State == "CA" & USAccidents$Timeline == Enactment_date]
Cali_ref

# As we mentioned, we need to clear out several states since we assume them to suffer from large measurement error, which is in-
# consistent with the rest of the data set. Consequently, we set a simulation set including only the members we select with regard
# to the Measurement Error 

Cali_synth <- filter(USAccidents) %>%
  group_by(ID) %>%
  summarise(complete.panel = ifelse(length(Date) == length(Observational_Period), 1, 0),
            missing.Acc = sum(is.na(Acc_PC)), 
            missing.VMT = sum(is.na(VMT_PC)),
            missing.rural = sum(is.na(Rural_Mileage_PC)), 
            missing.urban = sum(is.na(Urban_Mileage_PC)),
            missing.RMV = sum(is.na(RMV_PC)),
            missing.PopDens = sum(is.na(Pop_Density)),
            missing.Alc = sum(is.na(Alc_Cons_PC))) 

Cali_control = Cali_synth %>%
  filter(complete.panel == 1 & missing.Acc <= 1 & missing.VMT <= 1 &
           missing.rural <= 1 & missing.urban <= 1 & missing.RMV <= 1 & 
           missing.PopDens <= 1 & missing.Alc <= 1 & ID != 3) %>%
  select(ID) %>%
  unlist()
names(Cali_control) = NULL

USAccidents_real <- USAccidents[which(USAccidents$ID %in% c(Cali_control, unique(USAccidents$ID[USAccidents$State == "CA"]))), ]


match.criteria <- c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC")


USAccidents_real$ID<- as.numeric(as.factor(USAccidents_real$ID))
USAccidents_real$State <- as.character(USAccidents_real$State)
USAccidents_real$Date <- ymd(USAccidents_real$Date)
Observational_Period <- as.numeric(Observational_Period)
USAccidents_real <- as.data.frame(USAccidents_real)



### Synthetic Control Method Part 2 - Calculating the Synthetic version of California

SCM_synth <- dataprep(
  foo = USAccidents_real,
  predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
  time.predictors.prior = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  dependent = "Acc_PC",
  unit.variable = "ID",
  unit.names.variable = "State",
  time.variable = "Timeline",
  treatment.identifier = 3,
  controls.identifier = c(1:2, 4:32), # If for all states: adjust to 4:38
  time.optimize.ssr = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  time.plot = Observational_Period
)

# Apply synth() to those data and inspect matching results via tables
SCM_synth_new <- synth(SCM_synth, optimxmethod="All")
SCM_synth_tables <- synth.tab(dataprep.res = SCM_synth, synth.res = SCM_synth_new)
SCM_synth_tables$tab.pred  # inspect balance
weights <- as.data.frame(SCM_synth_tables$tab.w) %>% arrange(desc(w.weights))

# Analyze the pre trend features for the Treated vs Synthetic vs the Sample Mean of the variables 
SCM_synth_tables$tab.pred

# Analyze the weight structure 
names(weights)[1] <- "Weights"
names(weights)[2] <- "State"
names(weights)[3] <- "Unit Numbers"
weights

kable(weights, "latex", caption = "Weights of the Synthetic Control for cannabis sales start analysis on daily basis (pre-treatment period: Feb. 16 - Dec. 17)", booktabs = T)%>%kable_styling(position = "center")

# Remove all unnecessary variables
rm(Cali_synth, SCM_synth, SCM_synth_new, SCM_synth_tables, USAccidents_real, Cali_control, 
   Cali_ref, Enactment_date, match.criteria, Observational_Period)


#### Create the Dataset needed for estimating a DiD model ####

USAccidents_did <- USAccidents
USAccidents_did$Year_Month <- as.yearmon(paste(USAccidents_did$Year, USAccidents_did$Month), "%Y %m")

# Get the weights
weights_1 <- weights[,c("State","Weights")] # check if the State variable is changed from "Country" to "State"
USAccidents_did <- merge(USAccidents_did, weights_1, by = c("State"), all.x = T)
rm(weights,weights_1)
USAccidents_did$Weights[USAccidents_did$State == "CA"] <- 0
USAccidents_did <- as.data.table(USAccidents_did)
USAccidents_did <- USAccidents_did[order(State,Date)]

# Generate the synthetic control group
USAccidents_weighted <- USAccidents_did

USAccidents_weighted[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                        "Urban_Mileage_PC", "RMV_PC", "Population",
                        "Pop_Density", "Alc_Cons_PC")] <- 
  USAccidents_did$Weights*USAccidents_did[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                                             "Urban_Mileage_PC", "RMV_PC", "Population",
                                             "Pop_Density", "Alc_Cons_PC")]

USAccidents_weighted <- USAccidents_weighted[, list(Acc_PC           = sum(Acc_PC),
                                                    Acc_per_RMV      = sum(Acc_per_RMV),
                                                    VMT_PC           = sum(VMT_PC),
                                                    Rural_Mileage_PC = sum(Rural_Mileage_PC),
                                                    Urban_Mileage_PC = sum(Urban_Mileage_PC),
                                                    RMV_PC           = sum(RMV_PC),
                                                    Population       = sum(Population),
                                                    Pop_Density      = sum(Pop_Density),
                                                    Alc_Cons_PC      = sum(Alc_Cons_PC)),
                                             by = Date]

USAccidents_did$State[USAccidents_did$State == "NY"] <- "SCG"

USAccidents_did[USAccidents_did$State == "SCG", 
                c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                  "Urban_Mileage_PC", "RMV_PC", "Population",
                  "Pop_Density", "Alc_Cons_PC")] <-
  USAccidents_weighted[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                          "Urban_Mileage_PC", "RMV_PC", "Population",
                          "Pop_Density", "Alc_Cons_PC")]
rm(USAccidents_weighted)

USAccidents_did <- USAccidents_did[USAccidents_did$State %in% c("CA","SCG"),
                                   -c("Avg_Severity", "Med_Mar_Legal", "Sale_Mar_Legal_Neighbour",
                                      "ID","Timeline","Weights")]

USAccidents$Rec_Mar_Legal[USAccidents$State == "SCG"] <- 0

USAccidents$Sale_Mar_Legal[USAccidents$State == "SCG"] <- 0



#### Trend Plots ####
# Daily accidents
ggplot(USAccidents_did, aes(Date, Acc_PC, col = State)) +
  geom_line() +
  geom_vline(xintercept = USAccidents_did$Date[694], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw()

# Monthly accidents
ggplot(USAccidents_did[,list(Acc_PC = sum(Acc_PC)), 
                       by = list(Year_Month, State)], 
       aes(Year_Month, Acc_PC, col = State)) +
  geom_line(size = 1) +
  geom_vline(xintercept = USAccidents_did$Year_Month[694], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw() +
  xlab("Month") +
  ylab("Accidents per million")



#### Create additionally needed dummies ####
# Create a dummy which indicates the post treatment period
USAccidents_did$Post_Treat <- USAccidents_did$Sale_Mar_Legal
USAccidents_did$Post_Treat[USAccidents_did$State == "SCG"] <-
  USAccidents_did$Sale_Mar_Legal[USAccidents_did$State == "CA"]

# Create a dummy which indicates the treatment group
USAccidents_did$Treat_Grp <- as.numeric(USAccidents_did$State == "CA")

# Create the interaction dummy between treatment group and post treatment
USAccidents_did$Post_Treat_X_Treat_Grp <- USAccidents_did$Post_Treat * USAccidents_did$Treat_Grp

# Create the interaction dummy between the source dummies and the treatment group dummy
USAccidents_did$SRC_Bing_X_Treat_Grp <- USAccidents_did$SRC_Bing * USAccidents_did$Treat_Grp
USAccidents_did$SRC_MQ_X_Treat_Grp   <- USAccidents_did$SRC_MQ * USAccidents_did$Treat_Grp

# Transform the Day_of_Week variable into a factor to be able to control for weekday FE later on
USAccidents_did$Day_of_Week <- as.factor(USAccidents_did$Day_of_Week)

# Transform the Month variable into a factor to be able to control for month FE later on
USAccidents_did$Month <- as.factor(USAccidents_did$Month)

# Transform the Year variable into a factor to be able to control for year FE later on
USAccidents_did$Year <- as.factor(USAccidents_did$Year)



#### Estimate the Diff-in-Diff model ####
# Basic model
did_daily_PC.1 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp, 
                     USAccidents_did)

stargazer(did_daily_PC.1, type = "text", 
          title = "Did estimation",
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Adding source variable to basic model (without interaction variable of source*treatment)
did_daily_PC.3 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp +
                       SRC_Bing + SRC_MQ, 
                     USAccidents_did)

stargazer(did_daily_PC.3, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Adding source variable to basic model (including interaction variable of source*treatment)
did_daily_PC.4 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp +
                       SRC_Bing + SRC_MQ + SRC_MQ_X_Treat_Grp + SRC_Bing_X_Treat_Grp, 
                     USAccidents_did)

stargazer(did_daily_PC.4, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Adding Time FE
did_daily_PC.5 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp +
                       SRC_Bing + SRC_MQ + SRC_MQ_X_Treat_Grp + SRC_Bing_X_Treat_Grp + 
                       Day_of_Week + Month + Year, 
                     USAccidents_did)

stargazer(did_daily_PC.5, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Overview
# Whole time frame vs. only before July 2017 (before data from MQ is added)
stargazer(did_daily_PC.1, 
          type = "text", 
          column.labels = "Model (6)",
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)

# Source dummies added: with vs without source*treat interaction term
stargazer(did_daily_PC.3, did_daily_PC.4, 
          type = "text", 
          column.labels = c("Model (7)","Model (8)"),
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)

# Overview of whole time frame vs. source & interaction terms added vs. time FE added
stargazer(did_daily_PC.4, did_daily_PC.5,
          type = "text", 
          column.labels = c("Model (8)","Model (9)"),
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)



###### Treatment 2 (Sales start) - Daily SCM 2 [Time frame = July 17 - May 19, With Controls, WITH response & Only states where weed is illegal by 31.12.19] ####
#### Get the weights of the Synthtic Version of California / Synthetic Control Group (SCG) ####
### Synthetic Control Method Part 1 - Preparation of the dataset ### 

# Calculation of the SCM method for the analysis of marijuana consumption on traffic accidents

# This part corresponds to the "Synthetic Control Method" part of our "Data and Methodology" section. 

# We introdcute the SCM and perform the necessary steps in order to receive a synthetic control of our treatment state for
# the observational period. 

# We have a balanced dataset according to our pre-defined characteristics which we already cleaned

USAccidents <- fread("US_accidents_PC_cleaned.csv", sep = ",", header = T)
USAccidents <- as.data.frame(USAccidents)
USAccidents$Date <- ymd(USAccidents$Date)


# Now we can load the synth package and start preparing the final dataset
USAccidents <- USAccidents[USAccidents$State %in% 
                             c(unique(USAccidents$State[USAccidents$Date == ymd("2019-12-31") & 
                                                          USAccidents$Rec_Mar_Legal != 1]),"CA"),]
USAccidents <- USAccidents[USAccidents$Date >= ymd("2017-07-01") &
                             USAccidents$Date < ymd("2019-06-01"),]
USAccidents$ID <- as.numeric(as.factor(USAccidents$State))
USAccidents$Timeline <- as.numeric(as.factor(USAccidents$Date))


# Now we set the filter for the relevant time period

Enactment_date <- 185
Observational_Period  <- seq(Enactment_date - 184, Enactment_date + 515)


# We then set the reference variable to California and the treatment day to the Enactment date of legalisation

Cali_ref <- USAccidents$Acc_PC[USAccidents$State == "CA" & USAccidents$Timeline == Enactment_date]
Cali_ref

# As we mentioned, we need to clear out several states since we assume them to suffer from large measurement error, which is in-
# consistent with the rest of the data set. Consequently, we set a simulation set including only the members we select with regard
# to the Measurement Error 

Cali_synth <- filter(USAccidents) %>%
  group_by(ID) %>%
  summarise(complete.panel = ifelse(length(Date) == length(Observational_Period), 1, 0),
            missing.Acc = sum(is.na(Acc_PC)), 
            missing.VMT = sum(is.na(VMT_PC)),
            missing.rural = sum(is.na(Rural_Mileage_PC)), 
            missing.urban = sum(is.na(Urban_Mileage_PC)),
            missing.RMV = sum(is.na(RMV_PC)),
            missing.PopDens = sum(is.na(Pop_Density)),
            missing.Alc = sum(is.na(Alc_Cons_PC))) 

Cali_control = Cali_synth %>%
  filter(complete.panel == 1 & missing.Acc <= 1 & missing.VMT <= 1 &
           missing.rural <= 1 & missing.urban <= 1 & missing.RMV <= 1 & 
           missing.PopDens <= 1 & missing.Alc <= 1 & ID != 3) %>%
  select(ID) %>%
  unlist()
names(Cali_control) = NULL

USAccidents_real <- USAccidents[which(USAccidents$ID %in% c(Cali_control, unique(USAccidents$ID[USAccidents$State == "CA"]))), ]


match.criteria <- c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC")


USAccidents_real$ID<- as.numeric(as.factor(USAccidents_real$ID))
USAccidents_real$State <- as.character(USAccidents_real$State)
USAccidents_real$Date <- ymd(USAccidents_real$Date)
Observational_Period <- as.numeric(Observational_Period)
USAccidents_real <- as.data.frame(USAccidents_real)



### Synthetic Control Method Part 2 - Calculating the Synthetic version of California

SCM_synth <- dataprep(
  foo = USAccidents_real,
  predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
  time.predictors.prior = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  dependent = "Acc_PC",
  unit.variable = "ID",
  unit.names.variable = "State",
  time.variable = "Timeline",
  treatment.identifier = 3,
  controls.identifier = c(1:2, 4:32), # If for all states: adjust to 4:38
  time.optimize.ssr = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  time.plot = Observational_Period
)

# Apply synth() to those data and inspect matching results via tables
SCM_synth_new <- synth(SCM_synth, optimxmethod="All")
SCM_synth_tables <- synth.tab(dataprep.res = SCM_synth, synth.res = SCM_synth_new)
SCM_synth_tables$tab.pred  # inspect balance
weights <- as.data.frame(SCM_synth_tables$tab.w) %>% arrange(desc(w.weights))

# Analyze the pre trend features for the Treated vs Synthetic vs the Sample Mean of the variables 
SCM_synth_tables$tab.pred

# Analyze the weight structure 
names(weights)[1] <- "Weights"
names(weights)[2] <- "State"
names(weights)[3] <- "Unit Numbers"
weights

kable(weights, "latex", caption = "Weights of the Synthetic Control for cannabis sales start analysis on daily basis (pre-treatment period: Jul. 17 - Dec. 17)", booktabs = T)%>%kable_styling(position = "center")

# Remove all unnecessary variables
rm(Cali_synth, SCM_synth, SCM_synth_new, SCM_synth_tables, USAccidents_real, Cali_control, 
   Cali_ref, Enactment_date, match.criteria, Observational_Period)


#### Create the Dataset needed for estimating a DiD model ####

USAccidents_did <- USAccidents
USAccidents_did$Year_Month <- as.yearmon(paste(USAccidents_did$Year, USAccidents_did$Month), "%Y %m")

# Get the weights
weights_1 <- weights[,c("State","Weights")] # check if the State variable is changed from "Country" to "State"
USAccidents_did <- merge(USAccidents_did, weights_1, by = c("State"), all.x = T)
rm(weights,weights_1)
USAccidents_did$Weights[USAccidents_did$State == "CA"] <- 0
USAccidents_did <- as.data.table(USAccidents_did)
USAccidents_did <- USAccidents_did[order(State,Date)]

# Generate the synthetic control group
USAccidents_weighted <- USAccidents_did

USAccidents_weighted[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                        "Urban_Mileage_PC", "RMV_PC", "Population",
                        "Pop_Density", "Alc_Cons_PC")] <- 
  USAccidents_did$Weights*USAccidents_did[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                                             "Urban_Mileage_PC", "RMV_PC", "Population",
                                             "Pop_Density", "Alc_Cons_PC")]

USAccidents_weighted <- USAccidents_weighted[, list(Acc_PC           = sum(Acc_PC),
                                                    Acc_per_RMV      = sum(Acc_per_RMV),
                                                    VMT_PC           = sum(VMT_PC),
                                                    Rural_Mileage_PC = sum(Rural_Mileage_PC),
                                                    Urban_Mileage_PC = sum(Urban_Mileage_PC),
                                                    RMV_PC           = sum(RMV_PC),
                                                    Population       = sum(Population),
                                                    Pop_Density      = sum(Pop_Density),
                                                    Alc_Cons_PC      = sum(Alc_Cons_PC)),
                                             by = Date]

USAccidents_did$State[USAccidents_did$State == "NY"] <- "SCG"

USAccidents_did[USAccidents_did$State == "SCG", 
                c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                  "Urban_Mileage_PC", "RMV_PC", "Population",
                  "Pop_Density", "Alc_Cons_PC")] <-
  USAccidents_weighted[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                          "Urban_Mileage_PC", "RMV_PC", "Population",
                          "Pop_Density", "Alc_Cons_PC")]
rm(USAccidents_weighted)

USAccidents_did <- USAccidents_did[USAccidents_did$State %in% c("CA","SCG"),
                                   -c("Avg_Severity", "Med_Mar_Legal", "Sale_Mar_Legal_Neighbour",
                                      "ID","Timeline","Weights")]

USAccidents$Rec_Mar_Legal[USAccidents$State == "SCG"] <- 0

USAccidents$Sale_Mar_Legal[USAccidents$State == "SCG"] <- 0



#### Trend Plots ####
# Daily accidents
ggplot(USAccidents_did, aes(Date, Acc_PC, col = State)) +
  geom_line() +
  geom_vline(xintercept = USAccidents_did$Date[185], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw()

# Monthly accidents
ggplot(USAccidents_did[,list(Acc_PC = sum(Acc_PC)), 
                       by = list(Year_Month, State)], 
       aes(Year_Month, Acc_PC, col = State)) +
  geom_line(size = 1) +
  geom_vline(xintercept = USAccidents_did$Year_Month[185], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw() +
  xlab("Month") +
  ylab("Accidents per million")



#### Create additionally needed dummies ####
# Create a dummy which indicates the post treatment period
USAccidents_did$Post_Treat <- USAccidents_did$Sale_Mar_Legal
USAccidents_did$Post_Treat[USAccidents_did$State == "SCG"] <-
  USAccidents_did$Sale_Mar_Legal[USAccidents_did$State == "CA"]

# Create a dummy which indicates the treatment group
USAccidents_did$Treat_Grp <- as.numeric(USAccidents_did$State == "CA")

# Create the interaction dummy between treatment group and post treatment
USAccidents_did$Post_Treat_X_Treat_Grp <- USAccidents_did$Post_Treat * USAccidents_did$Treat_Grp

# Create the interaction dummy between the source dummies and the treatment group dummy
USAccidents_did$SRC_Bing_X_Treat_Grp <- USAccidents_did$SRC_Bing * USAccidents_did$Treat_Grp
USAccidents_did$SRC_MQ_X_Treat_Grp   <- USAccidents_did$SRC_MQ * USAccidents_did$Treat_Grp

# Transform the Day_of_Week variable into a factor to be able to control for weekday FE later on
USAccidents_did$Day_of_Week <- as.factor(USAccidents_did$Day_of_Week)

# Transform the Month variable into a factor to be able to control for month FE later on
USAccidents_did$Month <- as.factor(USAccidents_did$Month)

# Transform the Year variable into a factor to be able to control for year FE later on
USAccidents_did$Year <- as.factor(USAccidents_did$Year)



#### Estimate the Diff-in-Diff model ####
# Basic model
did_daily_PC.1 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp, 
                     USAccidents_did)

stargazer(did_daily_PC.1, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Adding Time FE
did_daily_PC.2 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp + 
                       Day_of_Week + Month + Year, 
                     USAccidents_did)

stargazer(did_daily_PC.2, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Overview
# Overview of basic model for timeframe (Jul17 - May19) vs. time FE added
stargazer(did_daily_PC.1, did_daily_PC.2,
          type = "text", 
          title = "Did estimation", 
          column.labels = c("Model (10)","Model (11)"),
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)



###### Treatment 2 (Sales start) - Daily SCM 3 [Time frame = August 17 - May 19, With Controls, WITH response & Only states where weed is illegal by 31.12.19] ####
#### Get the weights of the Synthtic Version of California / Synthetic Control Group (SCG) ####
### Synthetic Control Method Part 1 - Preparation of the dataset ### 

# Calculation of the SCM method for the analysis of marijuana consumption on traffic accidents

# This part corresponds to the "Synthetic Control Method" part of our "Data and Methodology" section. 

# We introdcute the SCM and perform the necessary steps in order to receive a synthetic control of our treatment state for
# the observational period. 

# We have a balanced dataset according to our pre-defined characteristics which we already cleaned

USAccidents <- fread("US_accidents_PC_cleaned.csv", sep = ",", header = T)
USAccidents <- as.data.frame(USAccidents)
USAccidents$Date <- ymd(USAccidents$Date)


# Now we can load the synth package and start preparing the final dataset
USAccidents <- USAccidents[USAccidents$State %in% 
                             c(unique(USAccidents$State[USAccidents$Date == ymd("2019-12-31") & 
                                                          USAccidents$Rec_Mar_Legal != 1]),"CA"),]
USAccidents <- USAccidents[USAccidents$Date >= ymd("2017-08-01") &
                             USAccidents$Date < ymd("2019-06-01"),]
USAccidents$ID <- as.numeric(as.factor(USAccidents$State))
USAccidents$Timeline <- as.numeric(as.factor(USAccidents$Date))


# Now we set the filter for the relevant time period

Enactment_date <- 154
Observational_Period  <- seq(Enactment_date - 153, Enactment_date + 515)


# We then set the reference variable to California and the treatment day to the Enactment date of legalisation

Cali_ref <- USAccidents$Acc_PC[USAccidents$State == "CA" & USAccidents$Timeline == Enactment_date]
Cali_ref

# As we mentioned, we need to clear out several states since we assume them to suffer from large measurement error, which is in-
# consistent with the rest of the data set. Consequently, we set a simulation set including only the members we select with regard
# to the Measurement Error 

Cali_synth <- filter(USAccidents) %>%
  group_by(ID) %>%
  summarise(complete.panel = ifelse(length(Date) == length(Observational_Period), 1, 0),
            missing.Acc = sum(is.na(Acc_PC)), 
            missing.VMT = sum(is.na(VMT_PC)),
            missing.rural = sum(is.na(Rural_Mileage_PC)), 
            missing.urban = sum(is.na(Urban_Mileage_PC)),
            missing.RMV = sum(is.na(RMV_PC)),
            missing.PopDens = sum(is.na(Pop_Density)),
            missing.Alc = sum(is.na(Alc_Cons_PC))) 

Cali_control = Cali_synth %>%
  filter(complete.panel == 1 & missing.Acc <= 1 & missing.VMT <= 1 &
           missing.rural <= 1 & missing.urban <= 1 & missing.RMV <= 1 & 
           missing.PopDens <= 1 & missing.Alc <= 1 & ID != 3) %>%
  select(ID) %>%
  unlist()
names(Cali_control) = NULL

USAccidents_real <- USAccidents[which(USAccidents$ID %in% c(Cali_control, unique(USAccidents$ID[USAccidents$State == "CA"]))), ]


match.criteria <- c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC")


USAccidents_real$ID<- as.numeric(as.factor(USAccidents_real$ID))
USAccidents_real$State <- as.character(USAccidents_real$State)
USAccidents_real$Date <- ymd(USAccidents_real$Date)
Observational_Period <- as.numeric(Observational_Period)
USAccidents_real <- as.data.frame(USAccidents_real)



### Synthetic Control Method Part 2 - Calculating the Synthetic version of California

SCM_synth <- dataprep(
  foo = USAccidents_real,
  predictors =  c("Acc_PC", "VMT_PC", "Rural_Mileage_PC", "Urban_Mileage_PC", "RMV_PC", "Pop_Density", "Alc_Cons_PC"),
  time.predictors.prior = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  dependent = "Acc_PC",
  unit.variable = "ID",
  unit.names.variable = "State",
  time.variable = "Timeline",
  treatment.identifier = 3,
  controls.identifier = c(1:2, 4:32), # If for all states: adjust to 4:38
  time.optimize.ssr = seq(from = min(Observational_Period), to = Observational_Period[Enactment_date-1]),
  time.plot = Observational_Period
)

# Apply synth() to those data and inspect matching results via tables
SCM_synth_new <- synth(SCM_synth, optimxmethod="All")
SCM_synth_tables <- synth.tab(dataprep.res = SCM_synth, synth.res = SCM_synth_new)
SCM_synth_tables$tab.pred  # inspect balance
weights <- as.data.frame(SCM_synth_tables$tab.w) %>% arrange(desc(w.weights))

# Analyze the pre trend features for the Treated vs Synthetic vs the Sample Mean of the variables 
SCM_synth_tables$tab.pred

# Analyze the weight structure 
names(weights)[1] <- "Weights"
names(weights)[2] <- "State"
names(weights)[3] <- "Unit Numbers"
weights

kable(weights, "latex", caption = "Weights of the Synthetic Control for cannabis sales start analysis on daily basis (pre-treatment period: Aug. 17 - Dec. 17)", booktabs = T)%>%kable_styling(position = "center")

# Remove all unnecessary variables
rm(Cali_synth, SCM_synth, SCM_synth_new, SCM_synth_tables, USAccidents_real, Cali_control, 
   Cali_ref, Enactment_date, match.criteria, Observational_Period)


#### Create the Dataset needed for estimating a DiD model ####

USAccidents_did <- USAccidents
USAccidents_did$Year_Month <- as.yearmon(paste(USAccidents_did$Year, USAccidents_did$Month), "%Y %m")

# Get the weights
weights_1 <- weights[,c("State","Weights")] # check if the State variable is changed from "Country" to "State"
USAccidents_did <- merge(USAccidents_did, weights_1, by = c("State"), all.x = T)
rm(weights,weights_1)
USAccidents_did$Weights[USAccidents_did$State == "CA"] <- 0
USAccidents_did <- as.data.table(USAccidents_did)
USAccidents_did <- USAccidents_did[order(State,Date)]

# Generate the synthetic control group
USAccidents_weighted <- USAccidents_did

USAccidents_weighted[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                        "Urban_Mileage_PC", "RMV_PC", "Population",
                        "Pop_Density", "Alc_Cons_PC")] <- 
  USAccidents_did$Weights*USAccidents_did[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                                             "Urban_Mileage_PC", "RMV_PC", "Population",
                                             "Pop_Density", "Alc_Cons_PC")]

USAccidents_weighted <- USAccidents_weighted[, list(Acc_PC           = sum(Acc_PC),
                                                    Acc_per_RMV      = sum(Acc_per_RMV),
                                                    VMT_PC           = sum(VMT_PC),
                                                    Rural_Mileage_PC = sum(Rural_Mileage_PC),
                                                    Urban_Mileage_PC = sum(Urban_Mileage_PC),
                                                    RMV_PC           = sum(RMV_PC),
                                                    Population       = sum(Population),
                                                    Pop_Density      = sum(Pop_Density),
                                                    Alc_Cons_PC      = sum(Alc_Cons_PC)),
                                             by = Date]

USAccidents_did$State[USAccidents_did$State == "NY"] <- "SCG"

USAccidents_did[USAccidents_did$State == "SCG", 
                c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                  "Urban_Mileage_PC", "RMV_PC", "Population",
                  "Pop_Density", "Alc_Cons_PC")] <-
  USAccidents_weighted[,c("Acc_PC","Acc_per_RMV", "VMT_PC", "Rural_Mileage_PC", 
                          "Urban_Mileage_PC", "RMV_PC", "Population",
                          "Pop_Density", "Alc_Cons_PC")]
rm(USAccidents_weighted)

USAccidents_did <- USAccidents_did[USAccidents_did$State %in% c("CA","SCG"),
                                   -c("Avg_Severity", "Med_Mar_Legal", "Sale_Mar_Legal_Neighbour",
                                      "ID","Timeline","Weights")]

USAccidents$Rec_Mar_Legal[USAccidents$State == "SCG"] <- 0

USAccidents$Sale_Mar_Legal[USAccidents$State == "SCG"] <- 0



#### Trend Plots ####
# Daily accidents
ggplot(USAccidents_did, aes(Date, Acc_PC, col = State)) +
  geom_line() +
  geom_vline(xintercept = USAccidents_did$Date[185], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw()

# Monthly accidents
ggplot(USAccidents_did[,list(Acc_PC = sum(Acc_PC)), 
                       by = list(Year_Month, State)], 
       aes(Year_Month, Acc_PC, col = State)) +
  geom_line(size = 1) +
  geom_vline(xintercept = USAccidents_did$Year_Month[185], linetype="dashed", 
             color = "red", size = 1) +
  theme_bw() +
  xlab("Month") +
  ylab("Accidents per million")



#### Create additionally needed dummies ####
# Create a dummy which indicates the post treatment period
USAccidents_did$Post_Treat <- USAccidents_did$Sale_Mar_Legal
USAccidents_did$Post_Treat[USAccidents_did$State == "SCG"] <-
  USAccidents_did$Sale_Mar_Legal[USAccidents_did$State == "CA"]

# Create a dummy which indicates the treatment group
USAccidents_did$Treat_Grp <- as.numeric(USAccidents_did$State == "CA")

# Create the interaction dummy between treatment group and post treatment
USAccidents_did$Post_Treat_X_Treat_Grp <- USAccidents_did$Post_Treat * USAccidents_did$Treat_Grp

# Create the interaction dummy between the source dummies and the treatment group dummy
USAccidents_did$SRC_Bing_X_Treat_Grp <- USAccidents_did$SRC_Bing * USAccidents_did$Treat_Grp
USAccidents_did$SRC_MQ_X_Treat_Grp   <- USAccidents_did$SRC_MQ * USAccidents_did$Treat_Grp

# Transform the Day_of_Week variable into a factor to be able to control for weekday FE later on
USAccidents_did$Day_of_Week <- as.factor(USAccidents_did$Day_of_Week)

# Transform the Month variable into a factor to be able to control for month FE later on
USAccidents_did$Month <- as.factor(USAccidents_did$Month)

# Transform the Year variable into a factor to be able to control for year FE later on
USAccidents_did$Year <- as.factor(USAccidents_did$Year)



#### Estimate the Diff-in-Diff model ####
# Basic model
did_daily_PC.1 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp, 
                     USAccidents_did)

stargazer(did_daily_PC.1, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Adding Time FE
did_daily_PC.2 <- lm(Acc_PC ~ 1 + Post_Treat + Treat_Grp + Post_Treat_X_Treat_Grp + 
                       Day_of_Week + Month + Year, 
                     USAccidents_did)

stargazer(did_daily_PC.2, type = "text", 
          title = "Did estimation", 
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)


# Overview
# Overview of basic model for timeframe (Aug17 - May19) vs. time FE added
stargazer(did_daily_PC.1, did_daily_PC.2,
          type = "text", 
          title = "Did estimation", 
          column.labels = c("Model (12)","Model (13)"),
          report = "vc*s", single.row = TRUE, no.space = TRUE, header = FALSE, 
          intercept.bottom = FALSE, omit.table.layout = "n", digits = 3)



