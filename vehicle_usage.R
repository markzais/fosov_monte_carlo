library(ggplot2)
library(dplyr)
library(writexl)
library(openxlsx)
library(readxl)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/mark.zais/OneDrive - Integration Innovation/USSOCOM/FOSOV/Modeling/Vehicle_Usage/")
path <- getwd()

excel_sheets("20220418 Miles and Hours.xlsx")
excel_sheets("20230416 Miles and Hours.xlsx")

mrap2022<-read_excel("20220418 Miles and Hours.xlsx", 
                       sheet = "MRAP")
nscv2022<-read_excel("20220418 Miles and Hours.xlsx", 
                     sheet = "NSCV")
mrzr2022<-read_excel("20220418 Miles and Hours.xlsx", 
                     sheet = "MRZR")
crows2022<-read_excel("20220418 Miles and Hours.xlsx", 
                     sheet = "CROWS")

mrap2023<-read_excel("20230416 Miles and Hours.xlsx", 
                     sheet = "SOCOM MRAP")
nscv2023<-read_excel("20230416 Miles and Hours.xlsx", 
                     sheet = "SOCOM NSCV")
mrzr2023<-read_excel("20230416 Miles and Hours.xlsx", 
                     sheet = "SOCOM MRZR")
crows2023<-read_excel("20230416 Miles and Hours.xlsx", 
                     sheet = "SOCOM CROWS")

# merge two data frames in r
# r merge by rownames
mrap_data <- merge(mrap2022, mrap2023, by = 'Serial #')
mrap_data$AnnualMiles <- (mrap_data$`Current Miles.y` - mrap_data$`Current Hours.x`)
nscv_data <- merge(nscv2022, nscv2023, by = 'Serial #')
nscv_data$AnnualMiles <- (nscv_data$`CurrentKilometers.y` - nscv_data$`CurrentKilometers.x`)
mrzr_data <- merge(mrzr2022, mrzr2023, by = 'Serial #')
mrzr_data$AnnualMiles <- (mrzr_data$`Current Miles.y` - mrzr_data$`Current Hours.x`)
crows_data <- merge(crows2022, crows2023, by = 'Serial #')
crows_data$AnnualHours <- (crows_data$`Current Hours.y` - crows_data$`Current Hours.x`)

# Rename column header
mrap_data <- rename(mrap_data, Vehicle = 'Serial #')
nscv_data <- rename(nscv_data, Vehicle = 'Serial #')
mrzr_data <- rename(mrzr_data, Vehicle = 'Serial #')
crows_data <- rename(crows_data, Vehicle = 'Serial #')

# # Plot the data (plot #1)
# plotMiles <- ggplot2:: ggplot(data = df)+aes(x=Unit.y,y= AnnualMiles) +geom_point()

# # Plot the data (plot #2)
# # Scatter plot of number of miles driven by vehicle for each unit 
# ggplot(df, aes(x = Unit.y, y = AnnualMiles)) +
#   geom_point() +
#   labs(x = "Unit", y = "Annual Miles Driven") +
#   ggtitle("Number of Miles Driven by Vehicle and Unit") +
#   theme_minimal() 

# Plot the data (plot #3)
# Boxplot of number of miles driven by vehicle for each unit
ggplot(mrap_data, aes(x = Unit.y, y = AnnualMiles)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Unit", y = "Annual Miles Driven",
  title = "MRAP Miles Driven per Vehicle by Unit",
  subtitle = "April 2022 - April 2023") + ylim(0, 5000) +
  theme_minimal()

ggplot(nscv_data, aes(x = Unit.y, y = AnnualMiles)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Unit", y = "Annual Miles Driven",
       title = "NSCV Kilometers Driven per Vehicle by Unit",
       subtitle = "April 2022 - April 2023") + ylim(0, 1000) +
  theme_minimal()

ggplot(mrzr_data, aes(x = Unit.y, y = AnnualMiles)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Unit", y = "Annual Miles Driven",
       title = "MRZR Miles Driven per Vehicle by Unit",
       subtitle = "April 2022 - April 2023") + ylim(0, 6500) +
  theme_minimal()

ggplot(crows_data, aes(x = UNIT.y, y = AnnualMiles)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Unit", y = "Annual Miles Driven",
       title = "Crows Hours per System by Unit",
       subtitle = "April 2022 - April 2023") +  ylim(0, 500) +
  theme_minimal()

# Create multiple boxplots in one window
MRAP_Miles <- mrap_data$AnnualMiles
NSCV_Miles <- nscv_data$AnnualMiles
MRZR_Miles <- mrzr_data$AnnualMiles
CROWS_Hours <- crows_data$AnnualHours
par(mfrow=c(2,2))
boxplot(MRAP_Miles, horizontal=TRUE)
boxplot(NSCV_Miles, horizontal=TRUE)
boxplot(MRZR_Miles, horizontal=TRUE)
boxplot(CROWS_Hours, horizontal=TRUE)


