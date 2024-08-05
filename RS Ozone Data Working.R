---
title: "STA6257_Project: Missing Data Imputation Methods"
output: html_document
Authors: "Robert Stairs,
          Elizabeth (Liz) Miller,
          Karthik Aerra, 
          Mohit Kumar Veeraboina
          "
date: "2024-06-20"
self-contained: true
execute:
  echo: true
  warning: false
  message: false
editor: source
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
#install.packages('mlbench')

library(mlbench)
library(dplyr)
library(ggplot2)
library(vtable)
library(knitr)
library(kableExtra)
library(tidyverse)
library(tidyr)
library(naniar)
library(ggplot2)
library(UpSetR)
library(corrplot)
library(gridExtra)
library(reshape2)
library(ggpubr)
library(caTools)
library(party)
library(magrittr)
library(ggfittext)

#additional libraries for model fitting and train/test split
library(caret) # for fitting KNN models
library(e1071)
library(rsample) # for creating validation splits
library(recipes)    # for feature engineering
library(randomForest)
library(rpart)# decision tree
library(tidymodels) 
library(class) 
library(vip)t)
```


## Import Data
```{r}
# import data
data("Ozone", package = "mlbench")

# convert to data frame
ozone1 <- data.frame(Ozone)
# Reorder columns
ozone1 <- ozone1 %>%
  dplyr::select(V4,V1,V2,V3,V5,V6,V7,V8,V9,V10,V11,V12,V13)

# View summary of data
summary(ozone1))
```

## Data Processing
Clean up the data
```{r}
# Create function to check data types of data frame columns
check_data_types <- function(ozone1) {
  sapply(ozone1, class)
}

# Check data types of the columns
data_types <- check_data_types(ozone1)
print(data_types)


# Function to convert specified columns from factor to numeric
convert_factors_to_numeric <- function(data, columns) {
  data[columns] <- lapply(data[columns], function(x) as.numeric(as.character(x)))
  return(data)
}

# Convert first 3 columns from factor to numeric
columns_to_convert <- c("V1", "V2", "V3")
ozone_date <- convert_factors_to_numeric(ozone1, columns_to_convert)
ozone2 <- convert_factors_to_numeric(ozone1, columns_to_convert)

# Check data types of the columns again
data_types <- sapply(ozone_date, class)
print(data_types)

data_types2 <- sapply(ozone2, class)
print(data_types2)
```

```{r}
# Create Date column for analysis purposes

# combine Day and Month to create a Date column
ozone_date <- ozone_date %>%
  mutate(Date = as.Date(paste(1976, V1, V2, sep = "-"), format = "%Y-%m-%d"))

# Verify the new Date column
head(ozone_date, n=10))
```

```{r}
# rename columns
ozone2 <- plyr::rename(ozone1, c('V4'="Ozone_reading",
                                 'V1'="Month", 
                                 'V2'="Day_of_month",
                                 'V3'="Day_of_week", 
                                 'V5'="Pressure_afb", 
                                 'V6'="Wind_speed_LAX", 
                                 'V7'="Humidity_LAX", 
                                 'V8'="Temp_sandburg", 
                                 'V9'="Temp_EM", 
                                 'V10'="IBH_LAX", 
                                 'V11'="Pressure_gradient", 
                                 'V12'="IBT_LAX", 
                                 'V13'="Visibility_LAX"))

ozone_date2 <- plyr::rename(ozone_date, c('V4'="Ozone_reading",
                                 'V1'="Month", 
                                 'V2'="Day_of_month",
                                 'V3'="Day_of_week", 
                                 'V5'="Pressure_afb", 
                                 'V6'="Wind_speed_LAX", 
                                 'V7'="Humidity_LAX", 
                                 'V8'="Temp_sandburg", 
                                 'V9'="Temp_EM", 
                                 'V10'="IBH_LAX", 
                                 'V11'="Pressure_gradient", 
                                 'V12'="IBT_LAX", 
                                 'V13'="Visibility_LAX"))
head(ozone2, n=10)
head(ozone_date2, n=10)
```
##### **Data Legend:**

* **Ozone_reading:** Daily maximum one-hour-average ozone reading
* **Month:** 1 = January, ..., 12 = December
* **Day of month:** 1-30/31
* **Day of week:** 1 = Monday, ..., 7 = Sunday
* **Pressure_afb:** 500 millibar pressure height (m) measured at Vandenberg AFB
* **Wind_speed_LAX:** Wind speed (mph) at Los Angeles International Airport (LAX)
* **Humidity_LAX:** Humidity (%) at LAX
* **Temp_sandburg:** Temperature (degrees F) measured at Sandburg, CA
* **Temp_EM:** Temperature (degrees F) measured at El Monte, CA
* **IBH_LAX:** Inversion base height (feet) at LAX
* **Pressure_gradient:** Pressure gradient (mm Hg) from LAX to Daggett, CA
* **IBT_LAX:** Inversion base temperature (degrees F) at LAX
* **Visibility_LAX:** Visibility (miles) measured at LAX


# Data Analysis
```{r}
# Summary statistics by day of the week 
ozone_summary_by_day <- ozone2 %>%
  group_by(Day_of_week) %>%
  summarize(
    mean_ozone = mean(Ozone_reading, na.rm = TRUE),
    median_ozone = median(Ozone_reading, na.rm = TRUE),
    max_ozone = max(Ozone_reading, na.rm = TRUE),
    min_ozone = min(Ozone_reading, na.rm = TRUE)
  )

print(ozone_summary_by_day)

# Summary statistics by month
ozone_summary_by_month <- ozone2 %>%
  group_by(Month) %>%
  summarize(
    mean_ozone = mean(Ozone_reading, na.rm = TRUE),
    median_ozone = median(Ozone_reading, na.rm = TRUE),
    max_ozone = max(Ozone_reading, na.rm = TRUE),
    min_ozone = min(Ozone_reading, na.rm = TRUE)
  )

print(ozone_summary_by_month)
```
**Examine how feature variables correlate with Ozone levels**

Correlation coefficients were determined for each variable with respect to Ozone levels (response variable). A bar graph was used to summarize the correlation coefficients. The following correlations were noted:

* **Strong Positive Correlations:** Humidity_LAX, Pressure_afb, IBT_LAX, Temp_EM, and Temp_sandburg
* **Strong Negative Correlations:** IBH_LAX and Visibility_LAX

```{r, warning=FALSE, echo=T, message=FALSE}

# Make sure data is in numeric form
ozone2[] <- lapply(ozone2, as.numeric)

# Calculate correlations with Ozone
corr_coeffs <- cor(ozone2, use = "complete.obs")['Ozone_reading', ]
corr_coeffs <- corr_coeffs[!names(corr_coeffs) %in% 'Ozone_reading']

# Create a data frame for plotting
corr_df <- data.frame(Variable = names(corr_coeffs), Correlation = corr_coeffs)

# Create the bar graph
ggplot(corr_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = 'identity',fill = "skyblue") +
  xlab('Variable') +
  ylab('Correlation Coefficient') +
  ggtitle('Correlation Between Variables and Daily Average Ozone Reading') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

```

**Explore Interactions with Positively Correlated Variables**

**Strong Positive Correlations:** Humidity_LAX, Pressure_afb, IBT_LAX, Temp_EM, and Temp_sandburg

The percentage of ozone readings appears to be normally distributed with respect to Pressure_afb, IBT_LAX, and Temp_sandburg. There appears to be some negative skew with respect to humidity, with a secondary mode at the lowest humidity bin (10-20). There are a lot of missing data points for Temp_EM (139 observations out of 366), as discussed in the initial data summary. This represents 38.0% of the observations for Temp_EM.

```{r, warning=FALSE, echo=T, message=FALSE}

## Humidity
# Create bins for humidity levels
ozone3 <- ozone2 %>%
  mutate(humidity_bin = cut(Humidity_LAX, breaks = seq(10, 100, by = 10), include.lowest = TRUE))

# Calculate percentage of ozone readings for each bin
percentage_data1 <- ozone3 %>%
  group_by(humidity_bin) %>%
  summarize(Ozone_reading = n()) %>%
  mutate(percentage = (Ozone_reading / sum(Ozone_reading)) * 100)

# Create the bar graph
ggplot(percentage_data1, aes(x = humidity_bin, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Percentage of Ozone Readings by Humidity Levels",
       x = "Humidity Levels",
       y = "Percentage of Ozone Readings") +
  theme_minimal()


## Pressure
# Create bins for humidity levels
ozone3 <- ozone2 %>%
  mutate(pressure_bin = cut(Pressure_afb, breaks = seq(5300, 6000, by = 100), include.lowest = TRUE))

# Calculate percentage of ozone readings for each bin
percentage_data2 <- ozone3 %>%
  group_by(pressure_bin) %>%
  summarize(Ozone_reading = n()) %>%
  mutate(percentage = (Ozone_reading / sum(Ozone_reading)) * 100)

# Create the bar graph
ggplot(percentage_data2, aes(x = pressure_bin, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Percentage of Ozone Readings by Pressure Levels",
       x = "Pressure Levels",
       y = "Percentage of Ozone Readings") +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5, size=8))


## IBT - Inversion base temperature (degrees F) at LAX
# Create bins for Inversion base temp levels
ozone3 <- ozone2 %>%
  mutate(IBT_bin = cut(IBT_LAX, breaks = seq(20, 100, by = 10), include.lowest = TRUE))

# Calculate percentage of ozone readings for each bin
percentage_data3 <- ozone3 %>%
  group_by(IBT_bin) %>%
  summarize(Ozone_reading = n()) %>%
  mutate(percentage = (Ozone_reading / sum(Ozone_reading)) * 100)

# Create the bar graph
ggplot(percentage_data3, aes(x = IBT_bin, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Percentage of Ozone Readings by Inversion base temp Levels",
       x = "Inversion base temp Levels",
       y = "Percentage of Ozone Readings") +
  theme_minimal()


## Temp_EM - Temperature (degrees F) measured at El Monte, CA
# Create bins for temp levels
ozone3 <- ozone2 %>%
  mutate(Temp_EM_bin = cut(Temp_EM, breaks = seq(20, 100, by = 10), include.lowest = TRUE))

# Calculate percentage of ozone readings for each bin
percentage_data4 <- ozone3 %>%
  group_by(Temp_EM_bin) %>%
  summarize(Ozone_reading = n()) %>%
  mutate(percentage = (Ozone_reading / sum(Ozone_reading)) * 100)

# Create the bar graph
ggplot(percentage_data4, aes(x = Temp_EM_bin, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Percentage of Ozone Readings by Temp at El Monte Levels",
       x = "Temp Levels",
       y = "Percentage of Ozone Readings") +
  theme_minimal()


## Temp_sandburg
# Create bins for temp levels
ozone3 <- ozone2 %>%
  mutate(Temp_sd_bin = cut(Temp_sandburg, breaks = seq(20, 100, by = 10), include.lowest = TRUE))

# Calculate percentage of ozone readings for each bin
percentage_data5 <- ozone3 %>%
  group_by(Temp_sd_bin) %>%
  summarize(Ozone_reading = n()) %>%
  mutate(percentage = (Ozone_reading / sum(Ozone_reading)) * 100)

# Create the bar graph
ggplot(percentage_data5, aes(x = Temp_sd_bin, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Percentage of Ozone Readings by Temp at Sandburg Levels",
       x = "Temp Levels",
       y = "Percentage of Ozone Readings") +
  theme_minimal()

```

**Explore Interactions with Negatively Correlated Variables**

* **Strong Negative Correlations:** IBH_LAX and Visibility_LAX
The percentage of ozone readings does not appear to be normally distributed with respect to either variable. The highest proportion of Ozone readings appear at high IBH_LAX levels (4600-5100), with a secondary mode at low IBH_LAX levels (600-1100). The proportion of Ozone readings are positively skewed with respect to Visbility_LAX, with the mode occurring between 50 and 100, and a secondary mode occuring between 250 and 300. 

```{r, warning=FALSE, echo=T, message=FALSE}

## IBH_LAX - Inversion base height (feet) at LAX
# Create bins for IBH levels
ozone3 <- ozone2 %>%
  mutate(IBH_bin = cut(IBH_LAX, breaks = seq(100, 5500, by = 500), include.lowest = TRUE))

# Calculate percentage of ozone readings for each bin
percentage_data6 <- ozone3 %>%
  group_by(IBH_bin) %>%
  summarize(Ozone_reading = n()) %>%
  mutate(percentage = (Ozone_reading / sum(Ozone_reading)) * 100)

# Create the bar graph
ggplot(percentage_data6, aes(x = IBH_bin, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Percentage of Ozone Readings by IBH Levels",
       x = "IBH Levels",
       y = "Percentage of Ozone Readings") +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))


## Visibility
# Create bins for Visibility levels
ozone3 <- ozone2 %>%
  mutate(visibility_bin = cut(Visibility_LAX, breaks = seq(0, 500, by = 50), include.lowest = TRUE))

# Calculate percentage of ozone readings for each bin
percentage_data7 <- ozone3 %>%
  group_by(visibility_bin) %>%
  summarize(Ozone_reading = n()) %>%
  mutate(percentage = (Ozone_reading / sum(Ozone_reading)) * 100)

# Create the bar graph
ggplot(percentage_data7, aes(x = visibility_bin, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Percentage of Ozone Readings by Visibility Levels",
       x = "Visibility Levels",
       y = "Percentage of Ozone Readings") +
  theme_minimal()

```


**Correlation Matrix for All Columns**

The correlation matrix shows a multiple strong correlations between the predictors in the model. This also suggests high collinearity between variables. Some of the strongest correlations are as follows (|correlation| >0.5):

* Pressure_afb: Temp_Sandburg, Temp_EM, IBH_LAX, IBT_LAX
* Humidity_LAX: Pressure gradient
* Temp_Sandburg: Pressure_afb, Temp_EM, IBH_LAX, IBT_LAX
* Temp_EM: Pressure_afb, Temp_Sandburg, IBH_LAX, IBT_LAX
* IBH_LAX: Pressure_afb, Temp_Sandburg, Temp_EM, IBT_LAX
* Pressure_gradient: Humidity_LAX
* IBT_LAX: Pressure_afb, Temp_Sandburg, Temp_EM, IBH_LAX


```{r, warning=FALSE, echo=T, message=FALSE}
library("corrplot")

#Delete NA values to allow calculation of correlation coefficients
ozone_delete_NA <- na.omit(ozone2)


#Make the correlation matrix
ozone_cor = cor(ozone_delete_NA)
ozone_cor


#Make the plot of the correlation matrix
corrplot(ozone_cor)

```



#### **Exploration of the Missing Data**

The total number of missing data points for each column is shown below as a count and as a percentage. Most of the columns contain <5% missing values, with respect to the total values observed for that feature. Temp_EM contains 139 missing values, which is 38.0% of this features observations. The total number of missing values in the dataset is 203 out of 4,768 (13 columns times 366 observations). This represents 4.3% of the entire dataset. 

**Summarize missing values**
```{r, warning=FALSE, echo=T, message=FALSE}

# Function to summarize missing values in a data frame
summarize_missing_values <- function(data) {
  data %>%
    summarize_all(~ sum(is.na(.))) %>%
    gather(key = "column", value = "missing_values") %>%
    mutate(missing_percentage = (missing_values / nrow(data)) * 100)
}

missing_summary <- summarize_missing_values(ozone2)

# Print the summary of missing values
print(missing_summary)
print(sum(is.na(ozone2)))
```

#### **Visualization of the Missing Data**

Three different plots were generated to visualize the missing data. 

The first plot uses the vis_miss() function. Since the rows are arranged by date, we can see that most of the data are missing in random clusters with respect to date. Temp_EM, which has the highest proportion of missing data (38%), appears to be missing in somewhat regular intervals. This was further explored in the next section.


```{r, warning=FALSE, echo=T, message=FALSE}
# Plot missing data pattern
#Shows what percentage of the data are missing from each column
vis_miss(ozone2)
```


The second plot uses the gg_miss_upset() function. This plot shows the number of missing values not only by each column, but by each possible combination of missing values (intersections). For example, the third bar in this figure shows that there are 9 rows that are missing values for IBT_LAX, Humidity_LAX, and IBH_LAX. As stated previously, the majority of the missing values belong to Temp_EM, with 127 rows missing values for only this feature. The next highest intersection is Pressure_afb_NA, which contains 10 rows of missing values.


```{r, warning=FALSE, echo=T, message=FALSE}
#This plot gives a visual of what combinations of NAs are present and how many there are for each
#set nsets to 8 since we have 8 columns with missing data
gg_miss_upset(ozone2, nsets=8)
```
The third plot was generated using the gg_miss_var() function. Again, Temp_EM has by far the largest number of missing points (139).


```{r, warning=FALSE, echo=T, message=FALSE}
#Another way to visualize number of missing rows per column
gg_miss_var(ozone2) + labs(y = "Number of missing values") + ylim(0, 150)
```

**Test Data for Normality**
Prior to testing the randomness of the data, it must be checked for normality in order to properly interpret the randomness results. The Shapiro-Wilk and the Anderson-Darling tests were performed. In addition, a histogram and QQ plot were rendered.

```{r}
# Extract the dependent variable
ozone_levels <- ozone2$Ozone_reading

# Remove NA values
ozone_levels <- na.omit(ozone_levels)

# Perform normality tests
library(nortest)
shapiro_test <- shapiro.test(ozone_levels)
ad_test <- ad.test(ozone_levels)

# Print the results
print(shapiro_test)
print(ad_test)
```
* Null Hypothesis (H0): The data is normally distributed.
* Alternative Hypothesis (H1): The data is not normally distributed.

Based on both p-values(8.37e-14 and 2.2e-16), the null hypothesis is rejected, data is not normally distributed.


**A histogram and QQ plot were also created**
```{r}
# Histogram
ggplot(ozone2, aes(x = Ozone_reading)) + geom_histogram(binwidth = 5) + ggtitle("Histogram of Ozone")

# Q-Q plot
qqnorm(ozone2$Ozone_reading)
qqline(ozone2$Ozone_reading)
```
The histogram and QQ plot further support the conclusion that the data follows a non-normal distribution.


**Randomness Testing of Missing Data**

The Little's MCAR test, the Hawkin's test and the Non-Parametric test were performed to analyze whether the data followed an MCAR pattern or not. 

```{r, warning=FALSE, echo=T, message=FALSE}
# Perform Little's MCAR test
mcar_result <- mcar_test(ozone2)
print(mcar_result)
```
* Null Hypothesis (H0): The data is missing completely at random (MCAR).
* Alternative Hypothesis (H1): The data is not missing completely at random.

Based on the p-value(4.102052e-12) the null hypothesis is rejected at the 0.05 significance level. However, since the test assumes that the data is from a multivariate normal distribution, we can conclude that the test is unreliable as our data is non-normal.


**Run Hawkins and Non-Parametric Tests**
```{r}
library(dplyr)
explanatory = c("Temp_EM","Month", "Day_of_month","Day_of_week", "Pressure_afb", "Wind_speed_LAX", "Humidity_LAX", "Temp_sandburg", "IBH_LAX", 
                "Pressure_gradient", "IBT_LAX", "Visibility_LAX")
dependent = "Ozone_reading"

# Run randomness test
ozone2 %>%
  select(explanatory)%>%
  MissMech::TestMCARNormality()

```

**Hawkins Test**

* Null Hypothesis (H0): The data is missing completely at random (MCAR).
* Alternative Hypothesis (H1): The data is not missing completely at random.

Based on the p-value(0.0113103) the null hypothesis is rejected at the 0.05 significance level. Again, since the test assumes normality, we can also reject the results of this test.


**Non-Parametric Test**

* Null Hypothesis (H0): The data is missing completely at random (MCAR).
* Alternative Hypothesis (H1): The data is not missing completely at random.

Based on the p-value(0.2743229), there is not sufficient evidence to reject the null hypothesis at 0.05 significance level. Because this test does not assume normality, we conclude that the data is, in fact, MCAR.


**Further Visualization of the Missing Data: Missingness Patterns**

Focusing on Temp_EM since it is the column with the highest number of missing values, the data appear to be missing randomly with respect to most of the variables. With respect to the date columns, the pattern of missingness appears to be random with respect to day of the month. However, it is apparent that there is a high proportion of missing data on days 6 and 7 of the week (Saturdays and Sundays). This indicates that Temp_EM was likely not measured on the weekends throughout the year. The values for Temp_EM also appear to be missing randomly with respect to month, though there is a notable high frequency of missing data around the month of May.

For the other variables in the dataset, there does not appear to be any obvious patterns to the missing data. Most importantly, the frequency of missing data for Temp_EM does not appear to depend on the output variable (ozone reading). Therefore, we can most likely proceed to imputation with the assumption that our data are missing completely at random (MCAR).

```{r, warning=FALSE, echo=T, message=FALSE}

# Create gg_miss_fct plots with adjusted themes
p1 <- gg_miss_fct(ozone2, fct = Month) + 
  ggtitle("Missing Data by Month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=8))

p2 <- gg_miss_fct(ozone2, fct = Day_of_month) + 
  ggtitle("Missing Data by Day of Month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=8))

p3 <- gg_miss_fct(ozone2, fct = Day_of_week) + 
  ggtitle("Missing Data by Day of Week") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=8))

p4 <- gg_miss_fct(ozone2, fct = Ozone_reading) + 
  ggtitle("Missing Data by Ozone Reading") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=8))

p5 <- gg_miss_fct(ozone2, fct = Pressure_afb) + 
  ggtitle("Missing Data by Solar Radiation") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=8))

p6 <- gg_miss_fct(ozone2, fct = Wind_speed_LAX) + 
  ggtitle("Missing Data by Wind Speed") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=8))

p7 <- gg_miss_fct(ozone2, fct = Humidity_LAX) + 
  ggtitle("Missing Data by Humidity (LAX)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=8))

p8 <- gg_miss_fct(ozone2, fct = Temp_sandburg) + 
  ggtitle("Missing Data by Temperature (Sandburg)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=8))

p9 <- gg_miss_fct(ozone2, fct = Temp_EM) + 
  ggtitle("Missing Data by Temperature (EM)") +
  theme(plot.title = element_text(size=8))

p10 <- gg_miss_fct(ozone2, fct = IBH_LAX) + 
  ggtitle("Missing Data by IBH_LAX") +
  theme(plot.title = element_text(size=8))

p11 <- gg_miss_fct(ozone2, fct = Pressure_gradient) + 
  ggtitle("Missing Data by Pressure Gradient") +
  theme(plot.title = element_text(size=8))

p12 <- gg_miss_fct(ozone2, fct = IBT_LAX) + 
  ggtitle("Missing Data by IBT_LAX") +
  theme(plot.title = element_text(size=8))

p13 <- gg_miss_fct(ozone2, fct = Visibility_LAX) + 
  ggtitle("Missing Data by Visibility_LAX") +
  theme(plot.title = element_text(size=8))

# Arrange the plots into grids with proper spacing
grid1 <- grid.arrange(p1, p2, p3, p4, nrow = 2)
grid2 <- grid.arrange(p5, p6, p7, p8, nrow = 2)
grid3 <- grid.arrange(p9, p10, p11, nrow = 2)
grid4 <- grid.arrange(p12, p13, nrow = 1)

```


## Missing Data Imputation

Prior to imputation, the dataset was split into testing and training datasets using a split ratio of 25/75.. Missing value imputations were performed on the training data set.
```{r}
# Split the data into training and testing sets
set.seed(123)
ozone2_split <- initial_split(ozone2, prop = 0.75)
train_ozone2 <- training(ozone2_split)
test_ozone2 <- testing(ozone2_split)

head(train_ozone2)
head(test_ozone2)
```


#### **Missing Data Imputation Using Simple Methods**

Missing data were deleted using listwise deletion and feature selection as a baseline comparison for imputation methods. Additionally, missing data were imputed using mean, median and mode as a demonstration of simple imputation techniques. For feature removal, we decided on a missing value threshold of 20%. After each imputation, the data was checked again to make sure there were no remaining missing values. A trainin dataset was created for each method:

**Feature selection (column deletion)**
The Temp_EM feature column exceeded the set threshold and was therefore removed from the dataset and the remaining missing values were also omitted. 

```{r, warning=FALSE, echo=T, message=FALSE}

# Function to drop column if quantity of missing values is over the threshold
drop_na_columns <- function(data, threshold) {
  na_counts <- colSums(is.na(data))
  na_proportion <- na_counts / nrow(data)
  data <- data[, na_proportion <= threshold]
  return(data)
}
# Define threshold (e.g., 20% NA allowed)
threshold <- 0.20

# Drop columns based on the NA threshold for training dataset
dropCol_train <- drop_na_columns(train_ozone2, threshold) # the column Temp_EM gets dropped
dropCol_train <- data.frame(dropCol_train)
dropCol_train <- na.omit(dropCol_train)

head(dropCol_train)

# check to make sure no missing values remain
print(sum(is.na(dropCol_train)))



# Drop columns based on the NA threshold for test dataset
dropCol_test <- drop_na_columns(test_ozone2, threshold) # the column Temp_EM gets dropped
dropCol_test <- data.frame(dropCol_train)
dropCol_test <- na.omit(dropCol_train)

head(dropCol_test)

# check to make sure no missing values remain
print(sum(is.na(dropCol_test)))
```

**Listwise deletion (row deletion)**

Create a new dataset where all rows with missing values are omitted.

```{r, warning=FALSE, echo=T, message=FALSE}
# Drop all missing values in training set. Row deletion.
dropNA_train <- na.omit(train_ozone2)

head(dropNA_train)

# check to make sure no missing values remain
print(sum(is.na(dropNA_train)))


# Drop all missing values in test set. Row deletion.
dropNA_test <- na.omit(test_ozone2)

head(dropNA_test)

# check to make sure no missing values remain
print(sum(is.na(dropNA_test)))
```

**Mean imputation**

Create a new dataset where all missing values are replaced with the mean of the feature column.

```{r, warning=FALSE, echo=T, message=FALSE}
# Function for mean imputation
mean_impute <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}

# impute on training set
mean_train <- apply(train_ozone2, 2, mean_impute)
mean_train <- data.frame(mean_train)

head(mean_train)

# check to make sure no missing values remain
print(sum(is.na(mean_train)))


# impute on test set
mean_test <- apply(test_ozone2, 2, mean_impute)
mean_test <- data.frame(mean_test)

head(mean_train)

# check to make sure no missing values remain
print(sum(is.na(mean_test)))
```

**Median imputation**

Create a new dataset where all missing values are replaced with the median of the feature column.

```{r, warning=FALSE, echo=T, message=FALSE}
# Function for median imputation
median_impute <- function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}

# impute on training set
median_train <- apply(train_ozone2, 2, median_impute)
median_train <- data.frame(median_train)

head(median_train)

# check to make sure no missing values remain
print(sum(is.na(median_train)))

# impute on test set
median_test <- apply(test_ozone2, 2, median_impute)
median_test <- data.frame(median_test)

head(median_test)

# check to make sure no missing values remain
print(sum(is.na(median_test)))
```

**Mode imputation**

Create a new dataset where all missing values are replaced with the mode of the feature column.

```{r, warning=FALSE, echo=T, message=FALSE}
# Function for mode imputation (using the most common value)
mode_impute <- function(x) {
  mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  x[is.na(x)] <- mode_val
  return(x)
}

# impute on training set
mode_train <- apply(train_ozone2, 2, mode_impute)
mode_train <- data.frame(mode_train)

head(mode_train)

# check to make sure no missing values remain
print(sum(is.na(mode_train)))


# impute on test set
mode_test <- apply(test_ozone2, 2, mode_impute)
mode_test <- data.frame(mode_test)

head(mode_test)

# check to make sure no missing values remain
print(sum(is.na(mode_test)))
```


#### **Missing Data Imputation Using MICE (Multiple Imputation Method) and missForest (Machine Learning Method)**

Next, the missing data were imputed using MICE and Miss_Forest methods. 

**missForest**
```{r, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE, results='hide'}

library(missForest)
# verbose = If 'TRUE' the user is supplied with additional output between iterations
# xtrue = Complete data matrix

#  impute on training data
mf_train <- missForest(train_ozone2, xtrue = ozone2, verbose = FALSE)
# convert back to data frame
mf_train <- as.data.frame(mf_train$ximp)

head(mf_train)

# check to make sure no missing values remain
print(sum(is.na(mf_train)))



#  impute on test data
mf_test <- missForest(test_ozone2, xtrue = ozone2, verbose = FALSE)
# convert back to data frame
mf_test <- as.data.frame(mf_test$ximp)

head(mf_test)

# check to make sure no missing values remain
print(sum(is.na(mf_test)))
```

**MICE**
For determining the number of imputations, a rule of thumb is to do one imputation for every 1% of missing data. Since we have approximately 4% of missing data in the dataset, we used 4 imputations. The method chosen is the Predictive Mean Matching (pmm) which is suitable for continuous variables like temperature, wind, etc. The total iterations must be enough to reach convergence, and the typical range is 5-20. The function was run at maxit=5, 10, and 20. At maxit=20, it appears convergence has been reached. Therefore, the final number of iterations used is 20 [@vanbuuren2018flexibleimputationmissingdata].

```{r, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE, results='hide'}
library(mice)

# Impute missing values using MICE
# print = False prevents the printing of all the iterations
mice_train1 <- mice(train_ozone2, method = "pmm", m = 4, maxit = 5, print=FALSE)

# Show mean and standard deviation of the imputed values plotted against iteration number for the imputed data in order to assess whether convergence has been achieved
plot(mice_train1)
```

```{r}
mice_train2 <- mice(train_ozone2, method = "pmm", m = 4, maxit = 10, print=FALSE)
plot(mice_train2)
```

```{r}
mice_train3 <- mice(train_ozone2, method = "pmm", m = 4, maxit = 20, print=FALSE)
plot(mice_train3)
```
```{r}
# impute on training set
# extracts the completed datasets from the mice object
mice_train <- complete(mice_train3)

# Convert completed data to data frame
mice_train <- as.data.frame(mice_train)

head(mice_train)

# check to make sure no missing values remain
print(sum(is.na(mice_train)))
```

```{r}
# impute on test set
mice_test <- mice(test_ozone2, method = "pmm", m = 4, maxit = 20, print=FALSE)
#check convergence
plot(mice_test)

# extracts the completed datasets from the mice object
mice_test <- complete(mice_test)

# Convert completed data to data frame
mice_test <- as.data.frame(mice_test)

head(mice_test)

# check to make sure no missing values remain
print(sum(is.na(mice_test)))
```


#### **Model-Fitting of Imputed Datasets**

**Make Predictions Using Data Where Missing Values Were Deleted Using Listwise Deletion (Deletion of all missing rows with missing data)**

Models were fit using the following algorithms:

* Random Forest
* K-Nearest Neighbors (KNN)
* Decision Tree

After models were fit to the training data, predictions were made on the unseen test data. RMSE was then reported for each model. 

```{r, warning=FALSE, echo=T, message=FALSE}
# Function to calculate RMSE
rmse <- function(pred, actual) {
  sqrt(mean((pred - actual)^2))
}
```
**Random Forest**
```{r, warning=FALSE, echo=T, message=FALSE}
# Random forest model
rf_dropNA <- randomForest(Ozone_reading ~ ., data = dropNA_train)

# make predictions
rf_dropNA_pred <- predict(rf_dropNA, newdata = dropNA_test)

# Plot variable importance
varImpPlot(rf_dropNA, main = "Variable Importance Plot for Random Forest Model")

# calculate RMSE
rf_dropNA_rmse <- rmse(rf_dropNA_pred, dropNA_test$Ozone_reading)
cat("Random Forest RMSE:", rf_dropNA_rmse, "\n")
```
**KNN**
```{r, warning=FALSE, echo=T, message=FALSE}
# KNN model
knn_dropNA <- train(Ozone_reading ~ ., data = dropNA_train, method = "knn")

# make predictions
knn_dropNA_pred <- predict(knn_dropNA, newdata = dropNA_test)

# Plot variable importance
vi <- varImp(knn_dropNA)
plot(vi)

# Calculate importance for the KNN model
#library(hstats)
#importance <- perm_importance(knn_dropNA, dropNA_train[, -ncol(dropNA_train)], dropNA_train$Ozone_reading)
#vip(importance)


# calculate RMSE
knn_dropNA_rmse <- rmse(knn_dropNA_pred, dropNA_test$Ozone_reading)
cat("KNN RMSE:", knn_dropNA_rmse, "\n")
```

**Decision Tree**
```{r, warning=FALSE, echo=T, message=FALSE}
# Decision tree model
tree_dropNA <- rpart(Ozone_reading ~ ., data = dropNA_train)

# make predictions
tree_dropNA_pred <- predict(tree_dropNA, newdata = dropNA_test)

# Plot variable importance
var_importance <- varImp(tree_dropNA)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# calculate RMSE
tree_dropNA_rmse <- rmse(tree_dropNA_pred, dropNA_test$Ozone_reading)
cat("Decision Tree RMSE:", tree_dropNA_rmse, "\n")
```

**Make Predictions Using Data Where Columns with >20% Missing Data Were Deleted (Temp_EM column)**

Models were fit using the following algorithms:

* Random Forest
* K-Nearest Neighbors (KNN)
* Decision Tree

After models were fit to the training data, predictions were made on the unseen test data. RMSE was then reported for each model. 

**Random Forest**
```{r}
# Random forest model
rf_dropCol <- randomForest(Ozone_reading ~ ., data = dropCol_train)

# make predictions
rf_dropCol_pred <- predict(rf_dropCol, newdata = dropCol_test)

# Plot variable importance
varImpPlot(rf_dropCol, main = "Variable Importance Plot for Random Forest Model")

# calculate RMSE
rf_dropCol_rmse <- rmse(rf_dropCol_pred, dropCol_test$Ozone_reading)
cat("Random Forest RMSE:", rf_dropCol_rmse, "\n")
```
**KNN**
```{r}
# KNN model
knn_dropCol <- train(Ozone_reading ~ ., data = dropCol_train, method = "knn")

# make predictions
knn_dropCol_pred <- predict(knn_dropCol, newdata = dropCol_test)

# Plot variable importance
vi <- varImp(knn_dropCol)
plot(vi)

# calculate RMSE
knn_dropCol_rmse <- rmse(knn_dropCol_pred, dropCol_test$Ozone_reading)
cat("KNN RMSE:", knn_dropCol_rmse, "\n")
```
**Decision Tree**
```{r}
# Decision tree model
tree_dropCol <- rpart(Ozone_reading ~ ., data = dropCol_train)

# make predictions
tree_dropCol_pred <- predict(tree_dropCol, newdata = dropCol_test)

# Plot variable importance
var_importance <- varImp(tree_dropCol)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# calculate RMSE
tree_dropCol_rmse <- rmse(tree_dropCol_pred, dropCol_test$Ozone_reading)
cat("Decision Tree RMSE:", tree_dropCol_rmse, "\n")
```

**Make Predictions Using Data Where Missing Values were Imputed using the Mean**

Models were fit using the following algorithms:

* Random Forest
* K-Nearest Neighbors (KNN)
* Decision Tree

After models were fit to the training data, predictions were made on the unseen test data. RMSE was then reported for each model.

**Random Forest**
```{r}
# Random forest model
rf_mean <- randomForest(Ozone_reading ~ ., data = mean_train)

# make predictions
rf_mean_pred <- predict(rf_mean, newdata = mean_test)

# Plot variable importance
varImpPlot(rf_mean, main = "Variable Importance Plot for Random Forest Model")

# calculate RMSE
rf_mean_rmse <- rmse(rf_mean_pred, mean_test$Ozone_reading)
cat("Random Forest RMSE:", rf_mean_rmse, "\n")
```

**KNN**
```{r}
# KNN model
knn_mean <- train(Ozone_reading ~ ., data = mean_train, method = "knn")

# make predictions
knn_mean_pred <- predict(knn_mean, newdata = mean_test)

# Plot variable importance
vi <- varImp(knn_mean)
plot(vi)

# calculate RMSE
knn_mean_rmse <- rmse(knn_mean_pred, mean_test$Ozone_reading)
cat("KNN RMSE:", knn_mean_rmse, "\n")
```

**Decision Tree**
```{r}
# Decision tree model
tree_mean <- rpart(Ozone_reading ~ ., data = mean_train)

# make predictions
tree_mean_pred <- predict(tree_mean, newdata = mean_test)

# Plot variable importance
var_importance <- varImp(tree_mean)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# calculate RMSE
tree_mean_rmse <- rmse(tree_mean_pred, mean_test$Ozone_reading)
cat("Decision Tree RMSE:", tree_mean_rmse, "\n")
```

**Make Predictions Using Data Where Missing Values were Imputed using the Median**

Models were fit using the following algorithms:

* Random Forest
* K-Nearest Neighbors (KNN)
* Decision Tree

After models were fit to the training data, predictions were made on the unseen test data. RMSE was then reported for each model. 

**Random Forest**
```{r}
# Random forest model
rf_med <- randomForest(Ozone_reading ~ ., data = median_train)
rf_med_pred <- predict(rf_med, newdata = median_test)

# Plot variable importance
varImpPlot(rf_med, main = "Variable Importance Plot for Random Forest Model")

# calculate RMSE
rf_med_rmse <- rmse(rf_med_pred, median_test$Ozone_reading)
cat("Random Forest RMSE:", rf_med_rmse, "\n")
```

**KNN**
```{r}
# KNN model
knn_med <- train(Ozone_reading ~ ., data = median_train, method = "knn")
knn_med_pred <- predict(knn_med, newdata = median_test)

# Plot variable importance
vi <- varImp(knn_med)
plot(vi)

# calculate RMSE
knn_med_rmse <- rmse(knn_med_pred, median_test$Ozone_reading)
cat("KNN RMSE:", knn_med_rmse, "\n")
```

**Decision Tree**
```{r}
# Decision tree model
tree_med <- rpart(Ozone_reading ~ ., data = median_train)
tree_med_pred <- predict(tree_med, newdata = median_test)

# Plot variable importance
var_importance <- varImp(tree_med)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# calculate RMSE
tree_med_rmse <- rmse(tree_med_pred, median_test$Ozone_reading)
cat("Decision Tree RMSE:", tree_med_rmse, "\n")
```

**Make Predictions Using Data Where Missing Values were Imputed using the Mode**

Models were fit using the following algorithms:

* Random Forest
* K-Nearest Neighbors (KNN)
* Decision Tree

After models were fit to the training data, predictions were made on the unseen test data. RMSE was then reported for each model. 

**Random Forest**
```{r}
# Random forest model
rf_mode <- randomForest(Ozone_reading ~ ., data = mode_train)
rf_mode_pred <- predict(rf_mode, newdata = mode_test)

# Plot variable importance
varImpPlot(rf_mode, main = "Variable Importance Plot for Random Forest Model")

# calculate RMSE
rf_mode_rmse <- rmse(rf_mode_pred, mode_test$Ozone_reading)
cat("Random Forest RMSE:", rf_mode_rmse, "\n")
```

**KNN**
```{r}
# KNN model
knn_mode <- train(Ozone_reading ~ ., data = mode_train, method = "knn")
knn_mode_pred <- predict(knn_mode, newdata = mode_test)

# Plot variable importance
vi <- varImp(knn_mode)
plot(vi)

# calculate RMSE
knn_mode_rmse <- rmse(knn_mode_pred, mode_test$Ozone_reading)
cat("KNN RMSE:", knn_mode_rmse, "\n")
```

**Decision Tree**
```{r}
# Decision tree model
tree_mode <- rpart(Ozone_reading ~ ., data = mode_train)
tree_mode_pred <- predict(tree_mode, newdata = mode_test)

# Plot variable importance
var_importance <- varImp(tree_mode)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# calculate RMSE
tree_mode_rmse <- rmse(tree_mode_pred, mode_test$Ozone_reading)
cat("Decision Tree RMSE:", tree_mode_rmse, "\n")
```

#### **Make Predictions using data where missing values were imputed with complex methods:**

**Make Predictions Using Data Where Missing Values were Imputed using missForest Method**

Models were fit using the following algorithms:

* Random Forest
* K-Nearest Neighbors (KNN)
* Decision Tree

After models were fit to the training data, predictions were made on the unseen test data. RMSE was then reported for each model. 

**Random Forest**
```{r}
# Random forest model
rf_miss <- randomForest(Ozone_reading ~ ., data = mf_train)
rf_miss_pred <- predict(rf_miss, newdata = mf_test)

# Plot variable importance
varImpPlot(rf_miss, main = "Variable Importance Plot for Random Forest Model")

# calculate RMSE
rf_miss_rmse <- rmse(rf_miss_pred, mf_test$Ozone_reading)
cat("Random Forest RMSE:", rf_miss_rmse, "\n")
```

**KNN**
```{r}
# KNN model
knn_miss <- train(Ozone_reading ~ ., data = mf_train, method = "knn")
knn_miss_pred <- predict(knn_miss, newdata = mf_test)

# Plot variable importance
vi <- varImp(knn_miss)
plot(vi)

# calculate RMSE
knn_miss_rmse <- rmse(knn_miss_pred, mf_test$Ozone_reading)
cat("KNN RMSE:", knn_miss_rmse, "\n")
```

**Decision Tree**
```{r}
# Decision tree model
tree_miss <- rpart(Ozone_reading ~ ., data = mf_train)
tree_miss_pred <- predict(tree_miss, newdata = mf_test)

# Plot variable importance
var_importance <- varImp(tree_miss)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# calculate RMSE
tree_miss_rmse <- rmse(tree_miss_pred, mf_test$Ozone_reading)
cat("Decision Tree RMSE:", tree_miss_rmse, "\n")
```

**Make Predictions Using Data Where Missing Values were Imputed using MICE Method**

Models were fit using the following algorithms:

* Random Forest
* K-Nearest Neighbors (KNN)
* Decision Tree

After models were fit to the training data, predictions were made on the unseen test data. RMSE was then reported for each model. 

**Random Forest**
```{r}
# Random forest model
rf_mice <- randomForest(Ozone_reading ~ ., data = mice_train)
rf_mice_pred <- predict(rf_mice, newdata = mice_test)

# Plot variable importance
varImpPlot(rf_mice, main = "Variable Importance Plot for Random Forest Model")

# calculate RMSE
rf_mice_rmse <- rmse(rf_mice_pred, mice_test$Ozone_reading)
cat("Random Forest RMSE:", rf_mice_rmse, "\n")
```

**KNN**
```{r}
# KNN model
knn_mice <- train(Ozone_reading ~ ., data = mice_train, method = "knn")
knn_mice_pred <- predict(knn_mice, newdata = mice_test)

# Plot variable importance
vi <- varImp(knn_mice)
plot(vi)

# calculate RMSE
knn_mice_rmse <- rmse(knn_mice_pred, mice_test$Ozone_reading)
cat("KNN RMSE:", knn_mice_rmse, "\n")
```

**Decision Tree**
```{r}
# Decision tree model
tree_mice <- rpart(Ozone_reading ~ ., data = mice_train)
tree_mice_pred <- predict(tree_mice, newdata = mice_test)

# Plot variable importance
var_importance <- varImp(tree_mice)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# calculate RMSE
tree_mice_rmse <- rmse(tree_mice_pred, mice_test$Ozone_reading)
cat("Decision Tree RMSE:", tree_mice_rmse, "\n")
```

#### **Summarize Model Performance for Each Imputation Methodology**

The RMSE for each model and imputation method combination was summarized into a data frame. The results are shown below. The best model fit (lowest RMSE) was obtained when using feature selection (column deletion) for imputation of missing data and the random forest algorithm for model training with the resulting dataset. Mean imputation with the Decision Tree model fitting was the second best combination, followed by median imputation with Random Forest.

```{r, warning=FALSE, echo=T, message=FALSE}
models <- c('RandomForest', 'KNN', 'DecisionTree')
scores <- c(rf_dropCol_rmse, knn_dropCol_rmse, tree_dropCol_rmse,
            rf_dropNA_rmse, knn_dropNA_rmse, tree_dropNA_rmse,
            rf_mean_rmse, knn_mean_rmse, tree_mean_rmse,
            rf_med_rmse, knn_med_rmse, tree_med_rmse,
            rf_mode_rmse, knn_mode_rmse, tree_mode_rmse,
            rf_miss_rmse, knn_miss_rmse, tree_miss_rmse,
            rf_mice_rmse, knn_mice_rmse, tree_mice_rmse
)
ImpMethod <- c('DropCol','DropNA','Mean', 'Median', 'Mode','missForest','MICE')

# Create dataframe
rmse_df <- data.frame(Model = models, ImpMethod=ImpMethod,RMSE = scores)
print(rmse_df[order(rmse_df$RMSE), ])

rmse_df$Combined_Methods <- paste(rmse_df$ImpMethod, rmse_df$Model)

#Create a bar chart
library(forcats)
library(ggplot2)
library(dplyr)

#Make a bar chart
rmse_df %>%
  mutate(Combined_Methods = fct_reorder(Combined_Methods, desc(RMSE))) %>%
  ggplot (aes(x=Combined_Methods, y=RMSE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  xlab("") + 
  theme_bw()

```
