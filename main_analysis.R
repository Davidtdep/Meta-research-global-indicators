#########################
# Load libraries
#########################

library(ggplot2)
library(readxl)
library(maps)
library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)
library(circlize)
library(gridExtra)
library(ggstream)
library(scales)
library(babynames)
library(lmtest)
library(tseries)
library(urca)


#########################
# Load data
#########################

data <- read_excel("data.xlsx")
hdi <- read.csv("human-development-index.csv")
rde <- read_excel("Research and development expenditure.xls")
ct <- read.csv("clinicalTrials.csv")



#########################
# Depure data
#########################

# eliminate columns with 100% NA's
data <- data [ , 1:8]

# Change column names
data$PY <- data$`Publication Year`
data$`Publication Year` <- NULL

data$OA <- data$`Open Access`
data$`Open Access` <- NULL

data$TC <- data$`Total Citations`
data$`Total Citations` <- NULL

data$PG <- data$`Publishing Group`
data$`Publishing Group` <- NULL

data$TypeDoc <- data$`Type of Document`
data$`Type of Document` <- NULL

# Match country names (uppercase)
data <- data %>%
  mutate(Country = str_to_title(Country))

# manual correction of the 14 countries that do not match
data$Country <- gsub("Albany", "Albania", data$Country)
data$Country <- gsub("Damascus", "Syria", data$Country)
data$Country <- gsub("North Carolina", "USA", data$Country)
data$Country <- gsub("Syrian Arab Republic", "Syria", data$Country)
data$Country <- gsub("Usa", "USA", data$Country)
data$Country <- gsub("Bosnia And Herzegovina", "Bosnia and Herzegovina", data$Country)
data$Country <- gsub("Denver", "USA", data$Country)
data$Country <- gsub("Portugal.", "Portugal", data$Country)
data$Country <- gsub("United Kingdom", "UK", data$Country)
data$Country <- gsub("Viet Nam", "Vietnam", data$Country)
data$Country <- gsub("China.", "China", data$Country)
data$Country <- gsub("Hong Kong", "Hong Kong", data$Country)
data$Country <- gsub("Russian Federation", "Russia", data$Country)
data$Country <- gsub("United States", "USA", data$Country)
data$Country <- gsub("UK", "United Kingdom", data$Country)
data$Country <- gsub("USA", "United States", data$Country)
data$Country <- gsub("Czech Republic", "Czechia", data$Country)
data$Country <- gsub("Taiwan", "China", data$Country)
# For rde data
data$Country <- gsub("Iran", "Iran, Islamic Rep.", data$Country)
data$Country <- gsub("Turkey", "Turkiye", data$Country)
data$Country <- gsub("Vietnam", "Viet Nam", data$Country)
data$Country <- gsub("Russia", "Russian Federation", data$Country)
data$Country <- gsub("Syria", "Syrian Arab Republic", data$Country)
data$Country <- gsub("Hong Kong", "Hong Kong SAR, China", data$Country)
data$Country <- gsub("Egypt", "Egypt, Arab Rep.", data$Country)
data$Country <- gsub("Bahamas", "Bahamas, The", data$Country)
data$Country <- gsub("Venezuela", "Venezuela, RB", data$Country)



# Extract the columns PY and Country
data <- data[, c("PY", "Country")]

# Eliminate rows with NA values in data
data <- na.omit(data)



#########################
# CORRELATIONS FOR HUMAN DEVELOPMENT INDEX
#########################

# eliminate any row with a data$PY value <1990
data <- data[data$PY >= 1990,]

# Which years exists in data but not in hdi
setdiff(data$PY, hdi$Year)

# Eliminate the years 2023 and 2024 from data
data <- data[data$PY != 2023 & data$PY != 2024,]

# Verify which are the countries that are not in the HDI dataset
setdiff(data$Country, hdi$Entity)

# Extract all the rows from hdi that contains any value of data$Country
hdi <- hdi[hdi$Entity %in% data$Country,]

# Calculate the sum of hdi$Human.Development.Index by every year in hdi$Year
hdi.sum.year <- hdi %>%
  group_by(Year) %>%
  summarise(HDI = sum(Human.Development.Index))

# Sum how many times appear each year in data$PY
data.sum.year <- data %>%
  group_by(PY) %>%
  summarise(Count = n())




###
# LINEAR REGRESSION
###

# Merge both dataframes by year
merged_data <- merge(data.sum.year, hdi.sum.year, by.x = "PY", by.y = "Year")

# Step 1: Logarithmic transformation of the 'Count' variable
merged_data$log_Count <- log(merged_data$Count)

# Step 2: Linear regression with the transformed variable
model_log <- lm(HDI ~ log_Count, data = merged_data)
summary(model_log)

# Step 3: Plot the logarithmic regression
ggplot(merged_data, aes(x = log_Count, y = HDI)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "#d90429") +
  labs(title = "Association between Log(Number of Articles) and HDI",
       x = "Log(Number of Meta-Research Articles)",
       y = "Summed Human Development Index") +
  theme_classic()

# Step 4: Residual analysis
# Plot residuals to check for normality and homoscedasticity
par(mfrow = c(1, 2))  # To display both plots together
plot(model_log, which = 1)  # Residuals vs. fitted values
plot(model_log, which = 2)  # Q-Q plot to check residual normality
par(mfrow = c(1, 1))  # Reset the plot

# Normality test for residuals
shapiro.test(residuals(model_log))

# Heteroscedasticity test
bptest(model_log)










#########################
# CORRELATIONS FOR RESEARCH AND DEVELOPMENT EXPENDITURE
#########################

# Verify countries in both datasets
setdiff(data$Country, rde$`Data Source`)

# Eliminate the rows in data$Country that contains the value "South Korea"
data <- data[data$Country != "South Korea",]

# Eliminate the rows that contain 2024 or 2023 in data$PY
data <- data[data$PY != 2023 & data$PY != 2024,]

# Transform the values of the row number 3 of rde the names of the columns
names(rde) <- rde[3,]

# Extract all the rows from rde that contains any value of data$Country
rde <- rde[rde$`Country Name` %in% data$Country,]

# Eliminate the columns 2, 3 and 4 of rde
rde <- rde[, -c(2, 3, 4)]

# Eliminate the columns from 1960 to 1996 of rde
rde <- rde[, -c(2:37)]

# eliminate the column 2023 of rde
rde <- rde[, -c(29)]

# Eliminate the rows in data$PY that contains a value < 1996
data <- data[data$PY >= 1996,]

# Sum how many times appear each year in data$PY
data.sum.year <- data %>%
  group_by(PY) %>%
  summarise(Count = n())

# Eliminate the first column of rde
rde.sum.year <- rde[, -1]

# Apply the function as.numeric to every column in rde.sum.year
rde.sum.year <- as.data.frame(lapply(rde.sum.year, as.numeric))

# Calculate the total sum of each column in rde.sum.year
rde.sum.year <- colSums(rde.sum.year, na.rm = TRUE)
rde.sum.year <- as.data.frame(rde.sum.year)

# Create a new column in rde.sum.year with the names of every row
rde.sum.year$Year <- rownames(rde.sum.year)

# Eliminate the letter X in rde.sum.year$Year
rde.sum.year$Year <- gsub("X", "", rde.sum.year$Year)
rde.sum.year$Year <- as.numeric(rde.sum.year$Year)

# Change the number of the columns in rde.sum.year
rde.sum.year <- rde.sum.year[, c(2, 1)]





# Step 1: Merge the dataframes by year
merged_data <- merge(data.sum.year, rde.sum.year, by.x = "PY", by.y = "Year")

# Step 2: Logarithmic transformation if the data shows skewness
merged_data$log_Count <- log(merged_data$Count)
merged_data$log_rde <- log(merged_data$rde.sum.year)

# Step 3: Correlation analysis and plot
correlation <- cor(merged_data$log_Count, merged_data$log_rde, method = "pearson")
print(paste("Pearson Correlation:", correlation))

# Scatter plot with logarithmic transformation and regression line
ggplot(merged_data, aes(x = log_Count, y = log_rde)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "#d90429") +
  labs(title = "Association between Log(Number of Meta-Research Articles) and Log(R&D Expenditure)",
       x = "Log(Number of Meta-Research Articles)",
       y = "Log(R&D Expenditure (% of GDP))") +
  theme_minimal()

# Step 4: Linear regression between log_Count and log_rde
model_log <- lm(log_rde ~ log_Count, data = merged_data)
summary(model_log)

# Step 5: Residual analysis and plots
# Residuals vs fitted values plot
ggplot(data = data.frame(Fitted = fitted(model_log), Residuals = residuals(model_log)), 
       aes(x = Fitted, y = Residuals)) +
  geom_point(color = "black") +
  geom_smooth(method = "loess", color = "#d90429") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Q-Q plot to check residual normality
ggplot(data = data.frame(Sample = quantile(residuals(model_log), probs = ppoints(100)),
                         Theoretical = qnorm(ppoints(100))), 
       aes(sample = Sample)) +
  stat_qq() +
  stat_qq_line(color = "#d90429") +
  labs(title = "Q-Q Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Additional residual tests
shapiro_test <- shapiro.test(residuals(model_log))
print(shapiro_test)

bptest_test <- bptest(model_log)
print(bptest_test)

# Step 6: Time series analysis and plots
count_ts <- ts(merged_data$log_Count, start = min(merged_data$PY), end = max(merged_data$PY))
rde_ts <- ts(merged_data$log_rde, start = min(merged_data$PY), end = max(merged_data$PY))

# Plot both time series in a single graph
merged_data_long <- data.frame(Year = merged_data$PY,
                               log_Count = merged_data$log_Count,
                               log_rde = merged_data$log_rde) %>%
  tidyr::pivot_longer(cols = c(log_Count, log_rde), names_to = "Variable", values_to = "Value")

ggplot(merged_data_long, aes(x = Year, y = Value, color = Variable)) +
  geom_line(size = 1) +
  labs(title = "Time Series of Log(Count of Articles) and Log(R&D Expenditure)",
       x = "Year",
       y = "Log(Value)") +
  scale_color_manual(values = c("log_Count" = "blue", "log_rde" = "green"),
                     labels = c("Log(Number of Articles)", "Log(R&D Expenditure)")) +
  theme_minimal()

# Step 7: Cointegration test
coint_test <- ca.jo(cbind(count_ts, rde_ts), type = "trace", ecdet = "none", K = 2)
summary(coint_test)

# Step 8: Granger causality test
granger_test <- grangertest(count_ts, rde_ts, order = 1)
print(granger_test)

# Step 9: Regression with lags and lag plot
merged_data$lag_log_Count <- dplyr::lag(merged_data$log_Count, n = 1)
model_lag <- lm(log_rde ~ lag_log_Count, data = merged_data, na.action = na.omit)
summary(model_lag)

# Plot of the relationship with a 1-year lag
ggplot(merged_data, aes(x = lag_log_Count, y = log_rde)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "#d90429") +
  labs(title = "Association between Lagged Log(Articles) and Log(R&D Expenditure)",
       x = "Lagged Log(Number of Meta-Research Articles)",
       y = "Log(R&D Expenditure (% of GDP))") +
  theme_minimal()







#########################
# CORRELATIONS FOR COMPLETED CLINICAL TRIALS
#########################

# Extract all the rows with a ct$Study.Status value equal to "COMPLETED"
ct <- ct[ct$Study.Status == "COMPLETED",]

# Extract the columns "Last.Update.Posted" and "Locations" from ct
ct <- ct[, c("Last.Update.Posted", "Locations")]

# Replace the every value in ct$Last.Update.Posted with the first 4 characters of the original value
ct$Last.Update.Posted <- substr(ct$Last.Update.Posted, 1, 4)
ct$Last.Update.Posted <- as.numeric(ct$Last.Update.Posted)

# Sum how many times appear each year in ct$Last.Update.Posted
ct.sum.year <- ct %>%
  group_by(Last.Update.Posted) %>%
  summarise(Count = n())

# Sum how many times appear each year in data$PY
data.sum.year <- data %>%
  group_by(PY) %>%
  summarise(Count = n())

# Eliminate every row in data.sum.year$PY that contains a value < 2005
data.sum.year <- data.sum.year[data.sum.year$PY >= 2005,]



summary(data.sum.year$Count)
summary(ct.sum.year$Count)





# Step 1: Merge the dataframes by year
merged_data <- merge(data.sum.year, ct.sum.year, by.x = "PY", by.y = "Last.Update.Posted")

# Step 2: Logarithmic transformation of ct.sum.year$Count due to its range
merged_data$log_Articles <- log(merged_data$Count.x)
merged_data$log_Trials <- log(merged_data$Count.y)

# Step 3: Correlation analysis
correlation <- cor(merged_data$log_Articles, merged_data$log_Trials, method = "pearson")
print(paste("Pearson Correlation:", correlation))

# Scatter plot with logarithmic transformation and regression line
ggplot(merged_data, aes(x = log_Articles, y = log_Trials)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "#d90429") +
  labs(title = "Association between Log(Number of Meta-Research Articles) and Log(Completed Clinical Trials)",
       x = "Log(Number of Meta-Research Articles)",
       y = "Log(Completed Clinical Trials)") +
  theme_minimal()

# Step 4: Linear regression between log_Articles and log_Trials
model_log <- lm(log_Trials ~ log_Articles, data = merged_data)
summary(model_log)

# Step 5: Residual analysis and plots
# Residuals vs fitted values plot
ggplot(data = data.frame(Fitted = fitted(model_log), Residuals = residuals(model_log)), 
       aes(x = Fitted, y = Residuals)) +
  geom_point(color = "black") +
  geom_smooth(method = "loess", color = "#d90429") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Q-Q plot to check residual normality
ggplot(data = data.frame(Sample = quantile(residuals(model_log), probs = ppoints(100)),
                         Theoretical = qnorm(ppoints(100))), 
       aes(sample = Sample)) +
  stat_qq() +
  stat_qq_line(color = "#d90429") +
  labs(title = "Q-Q Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Additional residual tests
shapiro_test <- shapiro.test(residuals(model_log))
print(shapiro_test)

bptest_test <- bptest(model_log)
print(bptest_test)

# Step 6: Time series analysis and plots
articles_ts <- ts(merged_data$log_Articles, start = min(merged_data$PY), end = max(merged_data$PY))
trials_ts <- ts(merged_data$log_Trials, start = min(merged_data$PY), end = max(merged_data$PY))

# Plot both time series in a single graph
merged_data_long <- data.frame(Year = merged_data$PY,
                               log_Articles = merged_data$log_Articles,
                               log_Trials = merged_data$log_Trials) %>%
  tidyr::pivot_longer(cols = c(log_Articles, log_Trials), names_to = "Variable", values_to = "Value")

ggplot(merged_data_long, aes(x = Year, y = Value, color = Variable)) +
  geom_line(size = 1) +
  labs(title = "Time Series of Log(Articles) and Log(Completed Trials)",
       x = "Year",
       y = "Log(Value)") +
  scale_color_manual(values = c("log_Articles" = "blue", "log_Trials" = "green"),
                     labels = c("Log(Number of Articles)", "Log(Completed Trials)")) +
  theme_minimal()

# Step 7: Cointegration test
coint_test <- ca.jo(cbind(articles_ts, trials_ts), type = "trace", ecdet = "none", K = 2)
summary(coint_test)

# Step 8: Granger causality test
granger_test <- grangertest(articles_ts, trials_ts, order = 1)
print(granger_test)

# Step 9: Regression with lags and lag plot
merged_data$lag_log_Articles <- dplyr::lag(merged_data$log_Articles, n = 1)
model_lag <- lm(log_Trials ~ lag_log_Articles, data = merged_data, na.action = na.omit)
summary(model_lag)

# Plot of the relationship with a 1-year lag
ggplot(merged_data, aes(x = lag_log_Articles, y = log_Trials)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "#d90429") +
  labs(title = "Association between Lagged Log(Articles) and Log(Completed Trials)",
       x = "Lagged Log(Number of Meta-Research Articles)",
       y = "Log(Completed Clinical Trials)") +
  theme_minimal()
