library(tidyverse)
library(ggplot2)
install.packages('MASS')
library(MASS)
library(e1071)

counties2020 <- read.csv("us-counties-2020.csv")
counties2021 <- read.csv("us-counties-2021.csv")

head(counties2020)
head(counties2021)

# Add a "year" column to each dataset
counties2020$year <- 2020
counties2021$year <- 2021

# Combine the two datasets by stacking them vertically
combined_data <- rbind(counties2020, counties2021)

# Create boxplot for "Cases"
boxplot(cases ~ year, data = combined_data, 
        main = "Boxplot of COVID-19 Cases (2020 vs 2021)", 
        xlab = "Year", 
        ylab = "Number of Cases", 
        col = c("lightblue", "lightgreen"))  
# Create boxplot for "Deaths"
boxplot(deaths ~ year, data = combined_data, 
        main = "Boxplot of COVID-19 Deaths (2020 vs 2021)", 
        xlab = "Year", 
        ylab = "Number of Deaths", 
        col = c("lightblue", "lightgreen"))

combined_data$log_cases <- log1p(combined_data$cases)  # log1p handles 0 values well
combined_data$log_deaths <- log1p(combined_data$deaths)

# Create boxplot for log-transformed "Cases"
boxplot(log_cases ~ year, data = combined_data, 
        main = "Boxplot of Log-transformed COVID-19 Cases (2020 vs 2021)", 
        xlab = "Year", 
        ylab = "Log of Number of Cases", 
        col = c("lightblue", "lightgreen"))

# Create boxplot for log-transformed "Deaths"
boxplot(log_deaths ~ year, data = combined_data, 
        main = "Boxplot of Log-transformed COVID-19 Deaths (2020 vs 2021)", 
        xlab = "Year", 
        ylab = "Log of Number of Deaths", 
        col = c("lightblue", "lightgreen"))

# Summary statistics for "Cases"
summary(counties2020$cases)
summary(counties2021$cases)

# Summary statistics for "Deaths"
summary(counties2020$deaths)
summary(counties2021$deaths)

clean_combined_data <- combined_data[complete.cases(combined_data), ]
clean_counties2021 <- counties2021[complete.cases(counties2021), ]
clean_counties2021 <- clean_counties2021[clean_counties2021$cases > 0 & clean_counties2021$deaths > 0, ]

clean_counties2020 <- counties2020[complete.cases(counties2020), ]
clean_counties2020 <- clean_counties2020[clean_counties2020$cases > 0 & clean_counties2020$deaths > 0, ]


# Histograms for COVID-19 Cases in 2020 and 2021# counties2020Histograms for COVID-19 Cases in 2020 and 2021
ggplot(clean_combined_data, aes(x = cases ,fill = factor(year))) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.6) +
  xlim(0,1000)+
  labs(title = "Histogram of COVID-19 Cases (2020 vs 2021)",
       x = "Number of Cases",
       y = "Frequency") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()

#histogram for deaths
ggplot(clean_combined_data, aes(x = deaths ,fill = factor(year))) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.6) +
  xlim(0,3000)+
  ylim(0,2000)+
  labs(title = "Histogram of COVID-19 Cases (2020 vs 2021)",
       x = "Number of Cases",
       y = "Frequency") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()

# Fit Log-normal distribution to Cases for 2020
fit_cases_2020_lognormal <- fitdistr(clean_counties2020$cases, "lognormal")

# Fit Log-normal distribution to Deaths for 2020
fit_deaths_2020_lognormal <- fitdistr(clean_counties2020$deaths, "lognormal")

# Fit Log-normal distribution to Cases
fit_cases_2021_lognormal <- fitdistr(clean_counties2021$cases, "lognormal")

# Fit Log-normal distribution to Deaths
fit_deaths_2021_lognormal <- fitdistr(clean_counties2021$deaths, "lognormal")

dev.off()
# Plot histogram for 'cases' with Log-normal fit
hist(clean_counties2020$cases, probability = TRUE, breaks = 30, main = "Histogram of Cases with Log-normal Fit", xlab = "Cases", col = "lightblue")
curve(dlnorm(x, meanlog = fit_cases_2020_lognormal$estimate["meanlog"], sdlog = fit_cases_2020_lognormal$estimate["sdlog"]), col = "red", add = TRUE)


# Plot histogram for 'deaths' with Log-normal fit
hist(clean_counties2020$deaths, probability = TRUE, breaks = 30, main = "Histogram of Deaths with Log-normal Fit", xlab = "Deaths", col = "lightgreen")
curve(dlnorm(x, meanlog = fit_deaths_2020_lognormal$estimate["meanlog"], sdlog = fit_deaths_2020_lognormal$estimate["sdlog"]), col = "red", add = TRUE)

# Summary statistics for cases
summary(clean_counties2020$cases)

# Summary statistics for deaths
summary(clean_counties2020$deaths)

#skewness
skewness(clean_counties2020$cases)
skewness(clean_counties2020$deaths)

# ECDF for Cases
plot(ecdf(clean_counties2020$cases), main = "ECDF of Cases", xlab = "Cases", ylab = "ECDF", col = "blue")

# ECDF for Deaths
plot(ecdf(clean_counties2020$deaths), main = "ECDF of Deaths", xlab = "Deaths", ylab = "ECDF", col = "green")

# Q-Q plot for Cases with Log-normal distribution
qqnorm(clean_counties2020$cases, main = "Q-Q plot for Cases")

# Add Log-normal Q-Q line manually
meanlog <- fit_cases_2020_lognormal$estimate["meanlog"]
sdlog <- fit_cases_2020_lognormal$estimate["sdlog"]

# Generate theoretical Log-normal quantiles
theoretical_quantiles <- qlnorm(ppoints(length(clean_counties2020$cases)), meanlog = meanlog, sdlog = sdlog)

# Add the Log-normal Q-Q line
lines(sort(clean_counties2020$cases), theoretical_quantiles[order(clean_counties2020$cases)], col = "red", lwd = 2)

# Q-Q plot for Deaths with Log-normal distribution
qqnorm(clean_counties2020$deaths, main = "Q-Q plot for Deaths")
# Add Log-normal Q-Q line manually
meanlog_deaths <- fit_deaths_2020_lognormal$estimate["meanlog"]
sdlog_deaths <- fit_deaths_2020_lognormal$estimate["sdlog"]

# Generate theoretical Log-normal quantiles
theoretical_quantiles_deaths <- qlnorm(ppoints(length(clean_counties2020$deaths)), meanlog = meanlog_deaths, sdlog = sdlog_deaths)

# Add the Log-normal Q-Q line
lines(sort(clean_counties2020$deaths), theoretical_quantiles_deaths[order(clean_counties2020$deaths)], col = "red", lwd = 2)


#NY HOUSE DATA SET

ny_house <- read.csv("NY-House-Dataset.csv")
head(ny_house)
summary(ny_house)

#cleaning the data
ny_house_clean <- ny_house %>%
  filter(!is.na(PRICE) & !is.na(BEDS) & !is.na(BATH) & !is.na(PROPERTYSQFT),
         PRICE > 0, BEDS < 10, BATH < 10, PROPERTYSQFT > 0)
#fitting linear model
model_full <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = ny_house_clean)
summary(model_full)

#scatter plot with a best fit line
plot(ny_house_clean$PROPERTYSQFT, ny_house_clean$PRICE, 
     main = "House Price vs Property Square Feet",
     xlab = "Number of Beds", ylab = "House Price",
     col = "blue", pch = 16)
abline(lm(PRICE ~ BEDS, data = ny_house), col = "red", lwd = 2)

#plotting the residuals
plot(model_full$residuals, 
     main = "Residual Plot",
     ylab = "Residuals", xlab = "Index",
     col = "darkgreen", pch = 16)
abline(h = 0, col = "red", lwd = 2)

#Part B Subsetting the data and repeating the model
ny_house_subset <- subset(ny_house_clean  , PRICE > 50000)
#fitting the new linear model
model_subset <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = ny_house_subset)
summary(model_subset)

#repeating scatter plots and residuals
plot(ny_house_subset$PROPERTYSQFT, ny_house_subset$PRICE,
     main = "House Price vs Property Sqft (Subset)",
     xlab = "Property Square Footage", ylab = "House Price",
     col = "blue", pch = 16)
abline(lm(PRICE ~ PROPERTYSQFT, data = ny_house_subset), col = "red", lwd = 2)

# Residuals
plot(model_subset$residuals, 
     main = "Residual Plot (Subset)",
     ylab = "Residuals", xlab = "Index",
     col = "darkgreen", pch = 16)
abline(h = 0, col = "red", lwd = 2)


                          