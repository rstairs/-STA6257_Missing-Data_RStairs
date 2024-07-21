---
title: "STA6257_Project: Missing Data Imputation Methods"
output: html_document
Authors: "Karthik Aerra, 
          Elizabeth (Liz) Miller, 
          Mohit Kumar Veeraboina, 
          Robert Stairs"
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
summary(ozone1)
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
head(ozone_date, n=10)
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

```{r}
# Histogram of ozone levels
ggplot(ozone2, aes(x = Ozone_reading)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Ozone Levels in Los Angeles, 1976", x = "Ozone Level", y = "Frequency")

# Boxplot of ozone levels by month
ggplot(ozone2, aes(x = factor(Month), y = Ozone_reading)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Ozone Levels by Month", x = "Month", y = "Ozone Level")

# Line plot of ozone levels over time 
ggplot(ozone_date2, aes(x = Date, y = Ozone_reading)) +
  geom_line(color = "blue") +
  labs(title = "Ozone Levels Over Time", x = "Date", y = "Ozone Level")
```


##Examine how feature variables correlate with Ozone levels
* **Strong Positive Correlations:** Humidity_LAX, Pressure_afb, IBT_LAX, Temp_EM, and Temp_sandburg
* **Strong Negative Correlations:** IBH_LAX and Visibility_LAX
```{r}
# Make sure data is in numeric form
ozone2[] <- lapply(ozone2, as.numeric)

# Calculate correlations with Ozone
corr_coeffs <- cor(ozone2, use = "complete.obs")['Ozone_reading', ]
corr_coeffs <- corr_coeffs[!names(corr_coeffs) %in% 'Ozone_reading']

# Create a data frame for plotting
corr_df <- data.frame(Variable = names(corr_coeffs), Correlation = corr_coeffs)

# Create the bar graph
ggplot(corr_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = 'identity',fill = "blue") +
  xlab('Variable') +
  ylab('Correlation Coefficient') +
  ggtitle('Correlation Between Variables and Daily Average Ozone Reading') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

# Explore Interactions with Positively and Negatively Correlated Variables

**Strong Positive Correlations:** Humidity_LAX, Pressure_afb, IBT_LAX, Temp_EM, and Temp_sandburg
```{r}
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

* **Strong Negative Correlations:** IBH_LAX and Visibility_LAX
```{r}
## IBH_LAX - Inversion base height (feet) at LAX
# Create bins for IBH levels
ozone3 <- ozone2 %>%
  mutate(IBH_bin = cut(IBH_LAX, breaks = seq(100, 5000, by = 500), include.lowest = TRUE))

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



# Missing Data Exploration

The problem is to predict the daily maximum one-hour-average ozone reading (Ozone_reading).
Run analysis to determine important variables
```{r}
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
* There is a total of 203 missing values, which constitutes approximately 5.5% of all observations.

```{r}
#Shows what percentage of the data are missing from each column
vis_miss(ozone2)


#This plot gives a visual of what combinations of NAs are present and how many there are for each
#set nsets to 8 since we have 8 columns with missing data
gg_miss_upset(ozone2, nsets=8)


#Another way to visualize number of missing rows per column
gg_miss_var(ozone2) + ylim(0, 150)
```


```{r}
# Create gg_miss_fct plots with adjusted themes
p1 <- gg_miss_fct(ozone2, fct = Month) + 
  ggtitle("Missing Data by Month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- gg_miss_fct(ozone2, fct = Day_of_month) + 
  ggtitle("Missing Data by Day of Month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- gg_miss_fct(ozone2, fct = Day_of_week) + 
  ggtitle("Missing Data by Day of Week") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- gg_miss_fct(ozone2, fct = Ozone_reading) + 
  ggtitle("Missing Data by Ozone Reading") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5 <- gg_miss_fct(ozone2, fct = Pressure_afb) + 
  ggtitle("Missing Data by Solar Radiation") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6 <- gg_miss_fct(ozone2, fct = Wind_speed_LAX) + 
  ggtitle("Missing Data by Wind Speed") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p7 <- gg_miss_fct(ozone2, fct = Humidity_LAX) + 
  ggtitle("Missing Data by Humidity (LAX)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p8 <- gg_miss_fct(ozone2, fct = Temp_sandburg) + 
  ggtitle("Missing Data by Temperature (Sandburg)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p9 <- gg_miss_fct(ozone2, fct = Temp_EM) + 
  ggtitle("Missing Data by Temperature (EM)") 

p10 <- gg_miss_fct(ozone2, fct = IBH_LAX) + 
  ggtitle("Missing Data by IBH_LAX") 

p11 <- gg_miss_fct(ozone2, fct = Pressure_gradient) + 
  ggtitle("Missing Data by Pressure Gradient") 

p12 <- gg_miss_fct(ozone2, fct = IBT_LAX) + 
  ggtitle("Missing Data by IBT_LAX") 

p13 <- gg_miss_fct(ozone2, fct = Visibility_LAX) + 
  ggtitle("Missing Data by Visibility_LAX") 

# Arrange the plots into grids with proper spacing
grid1 <- grid.arrange(p1, p2, p3, p4, nrow = 2)
grid2 <- grid.arrange(p5, p6, p7, p8, nrow = 2)
grid3 <- grid.arrange(p9, p10, p11, nrow = 2)
grid4 <- grid.arrange(p12, p13, nrow = 1)
```

# Missing Data Imputation
Simple Imputation Methods
* Drop all NA values
* Drop column if NA's >= 20%
* Mean
* Median
* Mode
```{r}
# Function to drop column if quantity of missing values is over the threshold
drop_na_columns <- function(data, threshold) {
  na_counts <- colSums(is.na(data))
  na_proportion <- na_counts / nrow(data)
  data <- data[, na_proportion <= threshold]
  return(data)
}
# Define threshold (e.g., 20% NA allowed)
threshold <- 0.20

# Drop all missing values
dropNA_data <- na.omit(ozone2)

# Function for mean imputation
mean_impute <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}

# Function for median imputation
median_impute <- function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}

# Function for mode imputation (using the most common value)
mode_impute <- function(x) {
  mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  x[is.na(x)] <- mode_val
  return(x)
}

##### Apply imputation methods
# Drop columns based on the NA threshold
dropCol_data <- drop_na_columns(ozone2, threshold) # the column Temp_EM gets dropped
dropCol_data <- apply(ozone2, 2, mean_impute)
dropCol_data <- data.frame(dropCol_data)

imputed_data_mean <- apply(ozone2, 2, mean_impute)
imputed_data_mean <- data.frame(imputed_data_mean)

imputed_data_median <- apply(ozone2, 2, median_impute)
imputed_data_median <- data.frame(imputed_data_median)

imputed_data_mode <- apply(ozone2, 2, mode_impute)
imputed_data_mode <- data.frame(imputed_data_mode)

# Print results
head(imputed_data_mean)
head(imputed_data_median)
head(imputed_data_mode)
head(dropCol_data)
head(dropNA_data)

#print(sum(is.na(imputed_data_mean)))
#print(sum(is.na(imputed_data_median)))
#print(sum(is.na(imputed_data_mode)))
```

Complex Imputation Methods
* missForest
* MICE
```{r}
library(missForest)

# verbose = If 'TRUE' the user is supplied with additional output between iterations
# xtrue = Complete data matrix
ozone2_mf <- missForest(ozone2, xtrue = ozone2, verbose = TRUE)
# convert back to data frame
ozone2_mf <- as.data.frame(ozone2_mf$ximp)
print(sum(is.na(ozone2_mf)))

## The final results can be accessed directly. The estimated error:
ozone2_mf$OOBerror

## The true imputation error (if available):
ozone2_mf$error


library(mice)

# Impute missing values using MICE
# pmm = Predictive Mean Matching (suitable for continuous variables like temperature, wind, etc.)
# m = 5: number of imputed datasets to create.
# maxit = 50: max number of iterations
ozone2_mice <- mice(ozone2, method = "pmm", m = 5, maxit = 50)
# extracts the completed datasets from the mice object
ozone2_mice <- complete(ozone2_mice)
# Convert completed data to data frame
ozone2_mice <- as.data.frame(ozone2_mice)

print(sum(is.na(ozone2_mice)))
```


## Develop Predictive Models

### Make Predictions using data where missing values were imputed with simple methods:

```{r}
library(caret) # for fitting KNN models
library(e1071) # svm model
library(rsample) # for creating validation splits
library(recipes)    # for feature engineering
library(randomForest)
library(rpart)# decision tree
library(tidymodels) 

```
****Drop column if NA's >= 20%****
```{r}
# Split the data into training and testing sets
set.seed(123)
data_split_dropCol <- initial_split(dropCol_data, prop = 0.75)
train_dropCol <- training(data_split_dropCol)
test_dropCol <- testing(data_split_dropCol)

# Split data into predictors and target
X <- train_dropCol[, -1]  # Features
y <- train_dropCol$Ozone_reading  # Target

# Random forest model
rf_dropCol <- randomForest(Ozone_reading ~ ., data = train_dropCol)
# make predictions
rf_dropCol_pred <- predict(rf_dropCol, newdata = test_dropCol)
# Plot variable importance
varImpPlot(rf_dropCol, main = "Variable Importance Plot for Random Forest Model")

# KNN model
knn_dropCol <- train(Ozone_reading ~ ., data = train_dropCol, method = "knn")
# make predictions
knn_dropCol_pred <- predict(knn_dropCol, newdata = test_dropCol)
# Plot variable importance
feature_importance <- table(y) / length(y)
barplot(feature_importance, main = "Feature Importance for KNN", xlab = "Feature", ylab = "Importance")

# Decision tree model
tree_dropCol <- rpart(Ozone_reading ~ ., data = train_dropCol)
# make predictions
tree_dropCol_pred <- predict(tree_dropCol, newdata = test_dropCol)
# Plot variable importance
var_importance <- varImp(tree_dropCol)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# SVM model
svm_dropCol <- svm(Ozone_reading ~ ., data = train_dropCol)
# make predictions
svm_dropCol_pred <- predict(svm_dropCol, newdata = test_dropCol)
# Plot variable importance
var_importance_svm <- t(abs(coef(svm_dropCol)[1, -1]))
barplot(var_importance_svm, main = "Feature Importance in SVM", xlab = "Features", ylab = "Magnitude of Coefficient")

# Function to calculate RMSE
rmse <- function(pred, actual) {
  sqrt(mean((pred - actual)^2))
}

# Define a function to calculate R-squared
r_squared <- function(actual, pred) {
  mean_actual <- mean(actual)
  ss_tot <- sum((actual - mean_actual)^2)
  ss_res <- sum((actual - pred)^2)
  
  # Check if ss_tot is 0, which would lead to division by zero
  if (ss_tot == 0) {
    r2 <- NaN  # Handle case where ss_tot is 0
  } else {
    r2 <- 1 - (ss_res / ss_tot)
  }  
  return(r2)
}
```
```{r}
# Calculate RMSE for each model
rf_dropCol_rmse <- rmse(rf_dropCol_pred, test_dropCol$Ozone_reading)
knn_dropCol_rmse <- rmse(knn_dropCol_pred, test_dropCol$Ozone_reading)
tree_dropCol_rmse <- rmse(tree_dropCol_pred, test_dropCol$Ozone_reading)
svm_dropCol_rmse <- rmse(svm_dropCol_pred, test_dropCol$Ozone_reading)

rf_dropCol_r2 <- r_squared(test_dropCol$Ozone_reading, rf_dropCol_pred)
knn_dropCol_r2 <- r_squared(test_dropCol$Ozone_reading, knn_dropCol_pred)
tree_dropCol_r2 <- r_squared(test_dropCol$Ozone_reading, tree_dropCol_pred)
svm_dropCol_r2 <- r_squared(test_dropCol$Ozone_reading, svm_dropCol_pred)

# Print RMSE values
cat("Random Forest RMSE:", rf_dropCol_rmse, "\n")
cat("KNN RMSE:", knn_dropCol_rmse, "\n")
cat("Decision Tree RMSE:", tree_dropCol_rmse, "\n")
cat("SVM RMSE:", svm_dropCol_rmse, "\n")

cat("Random Forest R-squared:", rf_dropCol_r2["Rsquared"], "\n")
cat("KNN R-squared:", knn_dropCol_r2["Rsquared"], "\n")
cat("Decision Tree R-squared:", tree_dropCol_r2["Rsquared"], "\n")
cat("SVM R-squared:", svm_dropCol_r2["Rsquared"], "\n")
```

**Drop All NA's**
```{r}
# Split the data into training and testing sets
set.seed(123)
data_split_dropNA <- initial_split(dropNA_data, prop = 0.75)
train_dropNA <- training(data_split_dropNA)
test_dropNA <- testing(data_split_dropNA)

# Split data into predictors and target
X <- train_dropNA[, -1]  # Features
y <- train_dropNA$Ozone_reading  # Target

# Random forest model
rf_dropNA <- randomForest(Ozone_reading ~ ., data = train_dropNA)
# make predictions
rf_dropNA_pred <- predict(rf_dropNA, newdata = test_dropNA)
# Plot variable importance
varImpPlot(rf_dropNA, main = "Variable Importance Plot for Random Forest Model")

# KNN model
knn_dropNA <- train(Ozone_reading ~ ., data = train_dropNA, method = "knn")
# make predictions
knn_dropNA_pred <- predict(knn_dropNA, newdata = test_dropNA)
# Plot variable importance
feature_importance <- table(y) / length(y)
barplot(feature_importance, main = "Feature Importance for KNN", xlab = "Feature", ylab = "Importance")

# Decision tree model
tree_dropNA <- rpart(Ozone_reading ~ ., data = train_dropNA)
# make predictions
tree_dropNA_pred <- predict(tree_dropNA, newdata = test_dropNA)
# Plot variable importance
var_importance <- varImp(tree_dropNA)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# SVM model
svm_dropNA <- svm(Ozone_reading ~ ., data = train_dropNA)
# make predictions
svm_dropNA_pred <- predict(svm_dropNA, newdata = test_dropNA)
# Plot variable importance
var_importance_svm <- t(abs(coef(svm_dropNA)[1, -1]))
barplot(var_importance_svm, main = "Feature Importance in SVM", xlab = "Features", ylab = "Magnitude of Coefficient")
```
```{r}
# Calculate RMSE for each model
rf_dropNA_rmse <- rmse(rf_dropNA_pred, test_dropNA$Ozone_reading)
knn_dropNA_rmse <- rmse(knn_dropNA_pred, test_dropNA$Ozone_reading)
tree_dropNA_rmse <- rmse(tree_dropNA_pred, test_dropNA$Ozone_reading)
svm_dropNA_rmse <- rmse(svm_dropNA_pred, test_dropNA$Ozone_reading)

rf_dropNA_r2 <- r_squared(test_dropNA$Ozone_reading, rf_dropNA_pred)
knn_dropNA_r2 <- r_squared(test_dropNA$Ozone_reading, knn_dropNA_pred)
tree_dropNA_r2 <- r_squared(test_dropNA$Ozone_reading, tree_dropNA_pred)
svm_dropNA_r2 <- r_squared(test_dropNA$Ozone_reading, svm_dropNA_pred)

# Print RMSE values
cat("Random Forest RMSE:", rf_dropNA_rmse, "\n")
cat("KNN RMSE:", knn_dropNA_rmse, "\n")
cat("Decision Tree RMSE:", tree_dropNA_rmse, "\n")
cat("SVM RMSE:", svm_dropNA_rmse, "\n")

cat("Random Forest R-squared:", rf_dropNA_r2["Rsquared"], "\n")
cat("KNN R-squared:", knn_dropNA_r2["Rsquared"], "\n")
cat("Decision Tree R-squared:", tree_dropNA_r2["Rsquared"], "\n")
cat("SVM R-squared:", svm_dropNA_r2["Rsquared"], "\n")
```

**Mean**
```{r}
# Split the data into training and testing sets
set.seed(123)
data_split_mean <- initial_split(imputed_data_mean, prop = 0.75)
train_mean <- training(data_split_mean)
test_mean <- testing(data_split_mean)

# Split data into predictors and target
X <- train_mean[, -1]  # Features
y <- train_mean$Ozone_reading  # Target

# Random forest model
rf_mean <- randomForest(Ozone_reading ~ ., data = train_mean)
# make predictions
rf_mean_pred <- predict(rf_mean, newdata = test_mean)
# Plot variable importance
varImpPlot(rf_mean, main = "Variable Importance Plot for Random Forest Model")

# KNN model
knn_mean <- train(Ozone_reading ~ ., data = train_mean, method = "knn")
# make predictions
knn_mean_pred <- predict(knn_mean, newdata = test_mean)
# Plot variable importance
feature_importance <- table(y) / length(y)
barplot(feature_importance, main = "Feature Importance for KNN", xlab = "Feature", ylab = "Importance")

# Decision tree model
tree_mean <- rpart(Ozone_reading ~ ., data = train_mean)
# make predictions
tree_mean_pred <- predict(tree_mean, newdata = test_mean)
# Plot variable importance
var_importance <- varImp(tree_mean)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# SVM model
svm_mean <- svm(Ozone_reading ~ ., data = train_mean)
# make predictions
svm_mean_pred <- predict(svm_mean, newdata = test_mean)
# Plot variable importance
var_importance_svm <- t(abs(coef(svm_mean)[1, -1]))
barplot(var_importance_svm, main = "Feature Importance in SVM", xlab = "Features", ylab = "Magnitude of Coefficient")
```
```{r}
# Calculate RMSE for each model
rf_mean_rmse <- rmse(rf_mean_pred, test_mean$Ozone_reading)
knn_mean_rmse <- rmse(knn_mean_pred, test_mean$Ozone_reading)
tree_mean_rmse <- rmse(tree_mean_pred, test_mean$Ozone_reading)
svm_mean_rmse <- rmse(svm_mean_pred, test_mean$Ozone_reading)

rf_mean_r2 <- r_squared(test_mean$Ozone_reading, rf_mean_pred)
knn_mean_r2 <- r_squared(test_mean$Ozone_reading, knn_mean_pred)
tree_mean_r2 <- r_squared(test_mean$Ozone_reading, tree_mean_pred)
svm_mean_r2 <- r_squared(test_mean$Ozone_reading, svm_mean_pred)

# Print RMSE values
cat("Random Forest RMSE:", rf_mean_rmse, "\n")
cat("KNN RMSE:", knn_mean_rmse, "\n")
cat("Decision Tree RMSE:", tree_mean_rmse, "\n")
cat("SVM RMSE:", svm_mean_rmse, "\n")

cat("Random Forest R-squared:", rf_mean_r2["Rsquared"], "\n")
cat("KNN R-squared:", knn_mean_r2["Rsquared"], "\n")
cat("Decision Tree R-squared:", tree_mean_r2["Rsquared"], "\n")
cat("SVM R-squared:", svm_mean_r2["Rsquared"], "\n")
```

**Median**
```{r}
# Split the data into training and testing sets
set.seed(123)
data_split_med <- initial_split(imputed_data_median, prop = 0.75)
train_med <- training(data_split_med)
test_med <- testing(data_split_med)


# Random forest model
rf_med <- randomForest(Ozone_reading ~ ., data = train_med)
rf_med_pred <- predict(rf_med, newdata = test_med)
# Plot variable importance
varImpPlot(rf_med, main = "Variable Importance Plot for Random Forest Model")

# KNN model
knn_med <- train(Ozone_reading ~ ., data = train_med, method = "knn")
knn_med_pred <- predict(knn_med, newdata = test_med)
# Plot variable importance
feature_importance <- table(y) / length(y)
barplot(feature_importance, main = "Feature Importance for KNN", xlab = "Feature", ylab = "Importance")

# Decision tree model
tree_med <- rpart(Ozone_reading ~ ., data = train_med)
tree_med_pred <- predict(tree_med, newdata = test_med)
# Plot variable importance
var_importance <- varImp(tree_med)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# SVM model
svm_med <- svm(Ozone_reading ~ ., data = train_med)
svm_med_pred <- predict(svm_med, newdata = test_med)
# Plot variable importance
var_importance_svm <- t(abs(coef(svm_med)[1, -1]))
barplot(var_importance_svm, main = "Feature Importance in SVM", xlab = "Features", ylab = "Magnitude of Coefficient")
```
```{r}
# Calculate RMSE for each model
rf_med_rmse <- rmse(rf_med_pred, test_med$Ozone_reading)
knn_med_rmse <- rmse(knn_med_pred, test_med$Ozone_reading)
tree_med_rmse <- rmse(tree_med_pred, test_med$Ozone_reading)
svm_med_rmse <- rmse(svm_med_pred, test_med$Ozone_reading)

rf_med_r2 <- r_squared(test_med$Ozone_reading, rf_med_pred)
knn_med_r2 <- r_squared(test_med$Ozone_reading, knn_med_pred)
tree_med_r2 <- r_squared(test_med$Ozone_reading, tree_med_pred)
svm_med_r2 <- r_squared(test_med$Ozone_reading, svm_med_pred)

# Print RMSE values
cat("Random Forest RMSE:", rf_med_rmse, "\n")
cat("KNN RMSE:", knn_med_rmse, "\n")
cat("Decision Tree RMSE:", tree_med_rmse, "\n")
cat("SVM RMSE:", svm_med_rmse, "\n")

cat("Random Forest R-squared:", rf_med_r2["Rsquared"], "\n")
cat("KNN R-squared:", knn_med_r2["Rsquared"], "\n")
cat("Decision Tree R-squared:", tree_med_r2["Rsquared"], "\n")
cat("SVM R-squared:", svm_med_r2["Rsquared"], "\n")
```

**Mode**
```{r}
# Split the data into training and testing sets
set.seed(123)
data_split_mode <- initial_split(imputed_data_mode, prop = 0.75)
train_mode <- training(data_split_mode)
test_mode <- testing(data_split_mode)

# Random forest model
rf_mode <- randomForest(Ozone_reading ~ ., data = train_mode)
rf_mode_pred <- predict(rf_mode, newdata = test_mode)
# Plot variable importance
varImpPlot(rf_mode, main = "Variable Importance Plot for Random Forest Model")

# KNN model
knn_mode <- train(Ozone_reading ~ ., data = train_mode, method = "knn")
knn_mode_pred <- predict(knn_mode, newdata = test_mode)
# Plot variable importance
feature_importance <- table(y) / length(y)
barplot(feature_importance, main = "Feature Importance for KNN", xlab = "Feature", ylab = "Importance")

# Decision tree model
tree_mode <- rpart(Ozone_reading ~ ., data = train_mode)
tree_mode_pred <- predict(tree_mode, newdata = test_mode)
# Plot variable importance
var_importance <- varImp(tree_mode)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# SVM model
svm_mode <- svm(Ozone_reading ~ ., data = train_mode)
svm_mode_pred <- predict(svm_mode, newdata = test_mode)
# Plot variable importance
var_importance_svm <- t(abs(coef(svm_mode)[1, -1]))
barplot(var_importance_svm, main = "Feature Importance in SVM", xlab = "Features", ylab = "Magnitude of Coefficient")
```
```{r}
# Calculate RMSE for each model
rf_mode_rmse <- rmse(rf_mode_pred, test_mode$Ozone_reading)
knn_mode_rmse <- rmse(knn_mode_pred, test_mode$Ozone_reading)
tree_mode_rmse <- rmse(tree_mode_pred, test_mode$Ozone_reading)
svm_mode_rmse <- rmse(svm_mode_pred, test_mode$Ozone_reading)

rf_mode_r2 <- r_squared(test_mean$Ozone_reading, rf_mode_pred)
knn_mode_r2 <- r_squared(test_mean$Ozone_reading, knn_mode_pred)
tree_mode_r2 <- r_squared(test_mean$Ozone_reading, tree_mode_pred)
svm_mode_r2 <- r_squared(test_mean$Ozone_reading, svm_mode_pred)

# Print RMSE values
cat("Random Forest RMSE:", rf_mode_rmse, "\n")
cat("KNN RMSE:", knn_mode_rmse, "\n")
cat("Decision Tree RMSE:", tree_mode_rmse, "\n")
cat("SVM RMSE:", svm_mode_rmse, "\n")

cat("Random Forest R-squared:", rf_mode_r2["Rsquared"], "\n")
cat("KNN R-squared:", knn_mode_r2["Rsquared"], "\n")
cat("Decision Tree R-squared:", tree_mode_r2["Rsquared"], "\n")
cat("SVM R-squared:", svm_mode_r2["Rsquared"], "\n")
```



### Make Predictions using data where missing values were imputed with complex methods:

**missForest**
```{r}
# Split the data into training and testing sets
set.seed(123)
data_split_miss <- initial_split(ozone2_mf, prop = 0.75)
train_miss <- training(data_split_miss)
test_miss <- testing(data_split_miss)

# Random forest model
rf_miss <- randomForest(Ozone_reading ~ ., data = train_miss)
rf_miss_pred <- predict(rf_miss, newdata = test_miss)
# Plot variable importance
varImpPlot(rf_miss, main = "Variable Importance Plot for Random Forest Model")

# KNN model
knn_miss <- train(Ozone_reading ~ ., data = train_miss, method = "knn")
knn_miss_pred <- predict(knn_miss, newdata = test_miss)
# Plot variable importance
feature_importance <- table(y) / length(y)
barplot(feature_importance, main = "Feature Importance for KNN", xlab = "Feature", ylab = "Importance")

# Decision tree model
tree_miss <- rpart(Ozone_reading ~ ., data = train_miss)
tree_miss_pred <- predict(tree_miss, newdata = test_miss)
# Plot variable importance
var_importance <- varImp(tree_miss)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# SVM model
svm_miss <- svm(Ozone_reading ~ ., data = train_miss)
svm_miss_pred <- predict(svm_miss, newdata = test_miss)
# Plot variable importance
var_importance_svm <- t(abs(coef(svm_miss)[1, -1]))
barplot(var_importance_svm, main = "Feature Importance in SVM", xlab = "Features", ylab = "Magnitude of Coefficient")
```
```{r}
# Calculate RMSE for each model
rf_miss_rmse <- rmse(rf_miss_pred, test_miss$Ozone_reading)
knn_miss_rmse <- rmse(knn_miss_pred, test_miss$Ozone_reading)
tree_miss_rmse <- rmse(tree_miss_pred, test_miss$Ozone_reading)
svm_miss_rmse <- rmse(svm_miss_pred, test_miss$Ozone_reading)

rf_miss_r2 <- r_squared(test_mean$Ozone_reading, rf_miss_pred)
knn_miss_r2 <- r_squared(test_mean$Ozone_reading, knn_miss_pred)
tree_miss_r2 <- r_squared(test_mean$Ozone_reading, tree_miss_pred)
svm_miss_r2 <- r_squared(test_mean$Ozone_reading, svm_miss_pred)

# Print RMSE values
cat("Random Forest RMSE:", rf_miss_rmse, "\n")
cat("KNN RMSE:", knn_miss_rmse, "\n")
cat("Decision Tree RMSE:", tree_miss_rmse, "\n")
cat("SVM RMSE:", svm_miss_rmse, "\n")

cat("Random Forest R-squared:", rf_miss_r2["Rsquared"], "\n")
cat("KNN R-squared:", knn_miss_r2["Rsquared"], "\n")
cat("Decision Tree R-squared:", tree_miss_r2["Rsquared"], "\n")
cat("SVM R-squared:", svm_miss_r2["Rsquared"], "\n")
```


**MICE**
```{r}
# Split the data into training and testing sets
set.seed(123)
data_split_mice <- initial_split(ozone2_mice, prop = 0.75)
train_mice <- training(data_split_mice)
test_mice <- testing(data_split_mice)

# Random forest model
rf_mice <- randomForest(Ozone_reading ~ ., data = train_mice)
rf_mice_pred <- predict(rf_mice, newdata = test_mice)
# Plot variable importance
varImpPlot(rf_mice, main = "Variable Importance Plot for Random Forest Model")

# KNN model
knn_mice <- train(Ozone_reading ~ ., data = train_mice, method = "knn")
knn_mice_pred <- predict(knn_mice, newdata = test_mice)
# Plot variable importance
feature_importance <- table(y) / length(y)
barplot(feature_importance, main = "Feature Importance for KNN", xlab = "Feature", ylab = "Importance")

# Decision tree model
tree_mice <- rpart(Ozone_reading ~ ., data = train_mice)
tree_mice_pred <- predict(tree_mice, newdata = test_mice)
# Plot variable importance
var_importance <- varImp(tree_mice)
barplot(var_importance$Overall, main = "Variable Importance for Decision Tree", xlab = "Variable", ylab = "Importance")

# SVM model
svm_mice <- svm(Ozone_reading ~ ., data = train_mice)
svm_mice_pred <- predict(svm_mice, newdata = test_mice)
# Plot variable importance
var_importance_svm <- t(abs(coef(svm_mice)[1, -1]))
barplot(var_importance_svm, main = "Feature Importance in SVM", xlab = "Features", ylab = "Magnitude of Coefficient")
```
```{r}
# Calculate RMSE for each model
rf_mice_rmse <- rmse(rf_mice_pred, test_mice$Ozone_reading)
knn_mice_rmse <- rmse(knn_mice_pred, test_mice$Ozone_reading)
tree_mice_rmse <- rmse(tree_mice_pred, test_mice$Ozone_reading)
svm_mice_rmse <- rmse(svm_mice_pred, test_mice$Ozone_reading)

rf_mice_r2 <- r_squared(test_mean$Ozone_reading, rf_mice_pred)
knn_mice_r2 <- r_squared(test_mean$Ozone_reading, knn_mice_pred)
tree_mice_r2 <- r_squared(test_mean$Ozone_reading, tree_mice_pred)
svm_mice_r2 <- r_squared(test_mean$Ozone_reading, svm_mice_pred)

# Print RMSE values
cat("Random Forest RMSE:", rf_mice_rmse, "\n")
cat("KNN RMSE:", knn_mice_rmse, "\n")
cat("Decision Tree RMSE:", tree_mice_rmse, "\n")
cat("SVM RMSE:", svm_mice_rmse, "\n")

cat("Random Forest R-squared:", rf_mice_r2["Rsquared"], "\n")
cat("KNN R-squared:", knn_mice_r2["Rsquared"], "\n")
cat("Decision Tree R-squared:", tree_mice_r2["Rsquared"], "\n")
cat("SVM R-squared:", svm_mice_r2["Rsquared"], "\n")
```


# Create data frame with RSME scores
```{r}
models <- c('RandomForest', 'KNN', 'DecisionTree', 'SVM')
scores <- c(rf_dropCol_rmse,knn_mean_rmse,tree_dropCol_rmse,svm_dropCol_rmse, 
            rf_dropNA_rmse,knn_dropNA_rmse,tree_dropNA_rmse,svm_dropNA_rmse,
            rf_mean_rmse,knn_mean_rmse,tree_mean_rmse,svm_mean_rmse, 
            rf_med_rmse,knn_med_rmse,tree_med_rmse,svm_med_rmse,
            rf_mode_rmse,knn_mode_rmse,tree_mode_rmse,svm_mode_rmse,
            rf_miss_rmse,knn_miss_rmse,tree_miss_rmse,svm_miss_rmse,
            rf_mice_rmse,knn_mice_rmse,tree_mice_rmse,svm_mice_rmse)
ImpMethod <- c('DropCol','DropNA','Mean', 'Median', 'Mode','missForest','MICE')

# Create dataframe
rmse_df <- data.frame(Model = models, ImpMethod=ImpMethod,RMSE = scores)
print(rmse_df)

```




