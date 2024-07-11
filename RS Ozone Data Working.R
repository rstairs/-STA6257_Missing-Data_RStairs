library(mlbench)
library(tidyr)
library(naniar)
library(ggplot2)
library(UpSetR)
library(tidyverse)
library(corrplot)
library(gridExtra)

data(Ozone)
data1 <- Ozone

#There are thirteen columns named V1-V13
head(data1)

#Date columns are coded as factors, remaining are numeric
str(data1)

#variable naming convention
#V1	Month: 1 = January, ..., 12 = December
#V2	Day of month
#V3	Day of week: 1 = Monday, ..., 7 = Sunday
#V4	Daily maximum one-hour-average ozone reading
#V5	500 millibar pressure height (m) measured at Vandenberg AFB
#V6	Wind speed (mph) at Los Angeles International Airport (LAX)
#V7	Humidity (%) at LAX
#V8	Temperature (degrees F) measured at Sandburg, CA
#V9	Temperature (degrees F) measured at El Monte, CA
#V10	Inversion base height (feet) at LAX
#V11	Pressure gradient (mm Hg) from LAX to Daggett, CA
#V12	Inversion base temperature (degrees F) at LAX
#V13	Visibility (miles) measured at LAX

#source:
#https://search.r-project.org/CRAN/refmans/mlbench/html/Ozone.html


#Rename columns in dataset
data1 <- data1 %>%
  rename(
    Month = V1,
    Day_of_month = V2,
    Day_of_week = V3,
    Ozone_reading = V4,
    Pressure_afb = V5,
    Wind_speed_LAX = V6,
    Humidity_LAX = V7,
    Temp_sandburg = V8,
    Temp_EM = V9,
    IBH_LAX = V10,
    Pressure_gradient = V11,
    IBT_LAX = V12,
    Visibility_LAX = V13
  )

#NOTE: response variable is Ozone_reading, formerly V4 (Daily maximum one-hour-average ozone reading)

#Gives the total number of NAs in the dataset
sum(is.na(data1))
#There are a total of 203 missing values in the dataset


#Gives the total number of NAs for each individual column in the dataset
colSums((is.na(data1)))
#There are 0 missing values for Month, Day_of_Month, Day_of_Week, Wind_speed_LAX, and Visibility_LAX
#There are 5 missing values for Ozone_reading
#There are 12 missing values for Pressure_afb
#There are 15 missing values for Humidity_LAX
#There are 2 missing values for Temp_sandburg
#There are 139 missing values for Temp_EM
#There are 15 missing values for IBH_LAX
#There is 1 missing values for Pressure_gradient
#There are 14 missing values for IBT_LAX

#Below plots are taken from the following site:
#https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html


#Shows what percentage of the data are missing from each column
vis_miss(data1)

#This plot gives a visual of what combinations of NAs are present and how many there are for each
#set nsets to 8 since we have 8 columns with missing data
gg_miss_upset(data1, nsets=8)
#The most common occurrence is for only V9 to be missing (127 rows)

#Another way to visualize number of missing rows per column
gg_miss_var(data1)



#The proportion of missing data looks fairly evenly distributed by month
p1 <- gg_miss_fct(data1, fct = Month)

#The proportion of missing data looks fairly random by day of month
p2 <- gg_miss_fct(data1, fct = Day_of_month)

#It looks like nearly all of the missing data for Temp_EM occurs on days 6 and 7 of the week (Saturday/Sunday)
p3 <- gg_miss_fct(data1, fct = Day_of_week)

#The proportion of missing data looks fairly random by ozone reading
p4 <- gg_miss_fct(data1, fct = Ozone_reading)

#The proportion of missing data looks fairly random by pressure_afb
p5 <- gg_miss_fct(data1, fct = Pressure_afb)

#The proportion of missing data looks fairly random by Wind_speed_LAX
p6 <- gg_miss_fct(data1, fct = Wind_speed_LAX)

#The proportion of missing data looks fairly random by Humidity_LAX
p7 <- gg_miss_fct(data1, fct = Humidity_LAX)

#The proportion of missing data looks fairly random by Temp_sandburg
p8 <- gg_miss_fct(data1, fct = Temp_sandburg)

#The proportion of missing data looks fairly random by Temp_EM
p9 <- gg_miss_fct(data1, fct = Temp_EM)

#The proportion of missing data looks fairly random by IBH_LAX
p10 <- gg_miss_fct(data1, fct = IBH_LAX)

#The proportion of missing data looks fairly random by Pressure_gradient
p11 <- gg_miss_fct(data1, fct = Pressure_gradient)

#The proportion of missing data looks fairly random by IBT_LAX
p12 <- gg_miss_fct(data1, fct = IBT_LAX)

#The proportion of missing data looks fairly random by Visibility_LAX
p13 <- gg_miss_fct(data1, fct = Visibility_LAX)

#Make a grid of the gg_miss_fct plots for simplification
grid1 <- grid.arrange(p1, p2, p3, p4, nrow = 2)
grid2 <- grid.arrange(p5, p6, p7, p8, nrow = 2)
grid3 <- grid.arrange(p9, p10, p11, p12, p13, nrow = 2)


