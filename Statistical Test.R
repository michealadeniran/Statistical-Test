install.packages("datarium")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("rcompanion")
install.packages("ggplot2")
install.packages('mosaic')
install.packages("dplyr")
install.packages("qqplotr")
install.packages("RVAideMemoire")
library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(car)
library(mosaic)
library(dplyr)
library(qqplotr)
library(RVAideMemoire)

#Load our dataset
Life_exp <- read.csv("Life_Expectancys.csv", header = TRUE)


#Checking for missing value
is.na(Life_exp)

# Check for duplicate rows based on all columns
duplicated(Life_exp)

# Check the dimensions (shape) of the data frame
dim(Life_exp)

#first FIVE ROWS

head(Life_exp)

#last Five Row
tail(Life_exp)

#summary details of our data
summary(Life_exp)

View(Life_exp)

#Descriptive Analysis
----------------------------------------------------------------------------
  Average_life <-Life_exp %>%
  select(Country, Year,Life_expectancy_total ) %>%
  filter(
    Country %in% c("Australia", "United States", "Canada", "United Kingdom", 
                   "Finland", "Ireland", "Sweden",  "Italy","Spain", "Portugal")) %>%
  mutate(Year_Group = cut(Year, breaks = seq(min(2007), max(2016) + 10, by = 10))) %>%
  group_by(Country) %>%
  summarise(Average_life = mean(Life_expectancy_total))
print(Average_life)
  
#OVERALL LIFE EXPECTANCY IN MALE


overall_mean <- Life_exp %>%
  select(Country, Year, Life_expectancy_total) %>%
  filter(
    Country %in% c("Australia", "United States", "Canada", "United Kingdom", 
                   "Finland", "Ireland", "Sweden",  "Italy","Spain", "Portugal")
  ) %>%
  mutate(Year_Group = cut(Year, breaks = seq(2007, 2016, by = 10))) %>%
  summarise(Average_life = mean(Life_expectancy_total))

overall_mean
  
  

#mean of life Expectancy in Male
mean(Life_exp$Life_expectancy_male)

#mean of life Expectancy in Female
mean(Life_exp$Life_expectancy_female)

#median of life Expectancy(female)
median(Life_exp$Life_expectancy_female)

#median of life Expectancy(male)
median(Life_exp$Life_expectancy_male)

#median of life Expectancy(total)
median(Life_exp$Life_expectancy_total)

#standard deviation for life Expectancy in MALE
sd(Life_exp$Life_expectancy_male)


#standard deviation for life Expectancy in Female
sd(Life_exp$Life_expectancy_female)

#standard deviation 
sd(Life_exp$Life_expectancy_total)

----------------------------------------------------
  #Histogram and Scatterplot
  
  #Histogram & scatterplot For Male and Female
hist((Life_exp$Life_expectancy_female,))

hist((Life_exp$Life_expectancy_male))

#kernel Density Plot

plot(density(Life_exp$Life_expectancy_male))


plot(density(Life_exp$Life_expectancy_female))

# Normal Distribution

#normal distibution for life expect in male
plotDist('norm', mean=78.58, sd=1.4, kind='density')


#normal distibution for life expect in female
plotDist('norm', mean=83.49, sd=1.28, kind='density')

barplot(Average_life$Average_life, names.arg = Average_life$Country, 
        col = "skyblue", main = "Average Life Expectancy by Country")

# shapiro test
result_shapiro <- Life_exp %>%
     group_by(Country) %>%
       summarise(shapiro_p_value = shapiro.test(Life_expectancy_total)$p.value)
(result_shapiro)



correlation
--------------------------------------------------------------------------------------------------------------------

#Correlation between two variable
plot(Life_exp$Life_expectancy_male, Life_exp$Life_expectancy_female)


# Pearson correlation between 2 variables
cor(Life_exp$Life_expectancy_male, Life_exp$Life_expectancy_female)

# Spearman correlation between 2 variables

cor(Life_exp$Life_expectancy_male, Life_exp$Life_expectancy_female,  method ="spearman")

plot(Life_exp$Population, Life_exp$GDP)

cor(Life_exp$Population, Life_exp$GDP,  method ="spearman")

#Correlation between multiple continuous varible

continuous_lifeexp <- Life_exp %>%
  select(
    Life_expectancy_total,
    Life_expectancy_male,
    Life_expectancy_female,
    Population,
    No_maternal_deaths,
    No_of_infant_deaths,
    GDP
  )
#display 5  rows
head(continuous_lifeexp, 5)

#correlation for all continuous Variables and rounded to 2 decimals
round(cor(continuous_lifeexp), digits = 2)


#creating a Correlation matrix
corrplot(cor(continuous_lifeexp), method = "number", type = "upper")



---------------------------------------------------------------------------------
#Hypothesis test
  
 # Extract life expectancy data for each country
life_uk_male <- Life_exp$Life_expectancy_male[Life_exp$Country == "United Kingdom"]
life_uk_female <- Life_exp$Life_expectancy_female[Life_exp$Country == "United Kingdom"]

# Perform t-test for the United Kingdom
t_test_uk <- t.test(life_uk_male, life_uk_female)
print(t_test_uk)


-------------------------------------------------------------------------------
  
life_us_male <- Life_exp$Life_expectancy_male[Life_exp$Country == "United States"]
life_us_female <- Life_exp$Life_expectancy_female[Life_exp$Country == "United States"]

# Perform t-test for the United States
t_test_us <- t.test(life_us_male, life_us_female)

print(t_test_us)


-------------------------------------------------------------------------------
  
#ONE Way Anova

oneway.test(Life_expectancy_total ~ Country,  data=Life_exp, var.equal = TRUE)
 head(Life_exp)
 

 # Check the unique levels in the 'Country' variable
 unique_countries <- unique(Life_exp$Country)
 print(unique_countries)
 
 # Create the boxplot
 boxplot(
   Life_expectancy_total ~ Country,
   data = Life_exp,
   names = unique_countries,
   xlab = "Country",
   ylab = "Life Expectancy",
   main = "Comparison of Life Expectancy between Countries"
 )

byf.shapiro(Life_expectancy_total ~ Country,  data=Life_exp)

bartlett.test(Life_expectancy_total ~ Country,  data=Life_exp)

#Post hoc test

oneway.test(Life_expectancy_total ~ Country,  data=Life_exp, var.equal = FALSE)



#Regression Analysis

str(Life_exp)

continuous_lifeexp <-
  Life_exp %>% select(Life_expectancy_total,Life_expectancy_male,
                      Life_expectancy_female,Population
                      ,No_maternal_deaths,No_of_infant_deaths,GDP)


#correlation Matrix

corrplot(cor(continuous_lifeexp), method = "number", type = "upper")




#REGRESSION

# Fit linear regression model
model_1 <- lm(Population ~ GDP , data = Life_exp)

# Check the summary of the model
summary(model_1)



# Plot the scatter plot
plot(Life_expectancy_female ~ Life_expectancy_male, data = Life_exp,
     col = "blue",
     main = "Scatter Plot: Life Expectancy (Male) vs. Life Expectancy (Female)",
     xlab = "Life Expectancy (Male)",
     ylab = "Life Expectancy (Female)")

# Add the regression line to the plot
abline(model_1, col = "red")
plot(model_1,1)
plot(model_1,2)


# Fit Multiple line rez

#Multiple Regression
Y=Life_expectancy_female
X1 =Life_expectancy_male
X2 =Life_expectancy_total
X2 =No_of_maternal_deaths

colnames(continuous_lifeexp2)

pairs(continuous_lifeexp2[, c(1, 2, 4, 5 )], lower.panel = NULL, pch = 19, cex = 0.2)


model_2 <- lm(Life_expectancy_female ~ Life_expectancy_male+Life_expectancy_total+No_maternal_deaths , data = Life_exp)

# Check the summary of the model
summary(model_2)

\                                                                                                                  
plot(model_2, 1)
plot(model_2, 2)
plot(model_2, 3)
vif(model_2)


 