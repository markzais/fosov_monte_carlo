library(ggplot2)
library(dplyr)
library(writexl)
library(openxlsx)
library(readxl)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library("gridExtra")

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

# Dotplot of Miles per vehicle
plot1 <- ggdotplot(mrap_data, x = "Unit.y", y = "AnnualMiles") + 
  # geom_boxplot(outlier.shape = NA) +
  labs(x = "Unit", y = "Annual Miles Driven",
       sort.by.groups = TRUE,      # Sort inside each group
       title = "MRAP Miles Driven per Vehicle by Unit",
       subtitle = "April 2022 - April 2023") +
  theme_minimal()

plot2 <- ggdotplot(nscv_data, x = "Unit.y", y = "AnnualMiles") + 
  labs(x = "Unit", y = "Annual Miles Driven",
       title = "NSCV Kilometers Driven per Vehicle by Unit",
       sort.by.groups = TRUE,      # Sort inside each group
       subtitle = "April 2022 - April 2023") + 
  theme_minimal()

plot3 <- ggdotplot(mrzr_data, x = "Unit.y", y = "AnnualMiles") + 
  labs(x = "Unit", y = "Annual Miles Driven",
       sort.by.groups = TRUE,      # Sort inside each group
       title = "MRZR Miles Driven per Vehicle by Unit",
       subtitle = "April 2022 - April 2023") + 
  theme_minimal()

plot4 <- ggdotplot(crows_data, x = "UNIT.y", y = "AnnualHours") + 
  labs(x = "Unit", y = "Annual Hours",
       sort.by.groups = TRUE,      # Sort inside each group
       title = "Crows Hours per System by Unit",
       subtitle = "April 2022 - April 2023") + 
  theme_minimal()

ggarrange(plot1, plot2, plot3, plot4,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


# Boxplot of number of miles driven by vehicle for each unit
plot1 <- ggplot(mrap_data, aes(x = Unit.y, y = AnnualMiles)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Unit", y = "Annual Miles Driven",
       sort.by.groups = TRUE,      # Sort inside each group
  title = "MRAP Miles Driven per Vehicle by Unit",
  subtitle = "April 2022 - April 2023") + ylim(0, 5000) +
  theme_minimal()

plot2 <- ggplot(nscv_data, aes(x = Unit.y, y = AnnualMiles)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Unit", y = "Annual Miles Driven",
       title = "NSCV Kilometers Driven per Vehicle by Unit",
       sort.by.groups = TRUE,      # Sort inside each group
       subtitle = "April 2022 - April 2023") + ylim(0, 1000) +
  theme_minimal()

plot3 <- ggplot(mrzr_data, aes(x = Unit.y, y = AnnualMiles)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Unit", y = "Annual Miles Driven",
       sort.by.groups = TRUE,      # Sort inside each group
       title = "MRZR Miles Driven per Vehicle by Unit",
       subtitle = "April 2022 - April 2023") + ylim(0, 6500) +
  theme_minimal()

plot4 <- ggplot(crows_data, aes(x = UNIT.y, y = AnnualHours)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Unit", y = "Annual Miles Driven",
       sort.by.groups = TRUE,      # Sort inside each group
       title = "Crows Hours per System by Unit",
       subtitle = "April 2022 - April 2023") +  ylim(0, 500) +
  theme_minimal()

ggarrange(plot1, plot2, plot3, plot4,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)





grid.arrange(plot1, plot2, plot3, plot4 + rremove("x.text"),  
         ncol = 2, nrow = 2)

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
