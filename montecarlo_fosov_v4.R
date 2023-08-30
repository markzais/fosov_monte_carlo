library (MonteCarlo)
library (mcreplicate)
library(mcStats)
library(ggplot2)
library(dplyr)
library(writexl)
library(openxlsx)
library(EnvStats)
library(triangle)

setwd("C:/Users/mark.zais/OneDrive - Integration Innovation/USSOCOM/FOSOV/Modeling/R_MontheCarlo/")
path <- getwd()

# scenario <-c(1,2,3) #Scenario  
# gmv_demand <-c(1382,1589,1685) #GMV demand
size <- 1


generate_vehicle_demand <- function(seed) {

  seed=seed  # set a random seed for reproducibility
  
  # # Use this sample for equal likelihood of each scenario
  # scenario <- sample(c(1, 2, 3), 1, prob = rep(1/3, 3))
  
  # Use this sample to adjust scenario probabilities for sensitivity analysis
  scenario <- sample(c(1, 2, 3), 1, prob = c(1/3,1/3,1/3))

  # Probability distributions for Unit i and Vehicle j
  # rtriangle(n, min, max, mode)
  u1v2 <- round(rtriangle(1, 2, 4, 4))
  u2v2 <- round(rtriangle(1, 2, 4, 3))
  u3v2 <- round(rtriangle(1, 0, 0, 0))
  u4v2 <- round(rtriangle(1, 0, 0, 0))
  u5v2 <- round(rtriangle(1, 0, 1, 1))
  u6v2 <- round(rtriangle(1, 0, 1, 1))
  u7v2 <- round(rtriangle(1, 0, 1, 1))
  u8v2 <- round(rtriangle(1, 15, 36, 27))
  u9v2 <- round(rtriangle(1, 2, 4, 3))
  u10v2 <- round(rtriangle(1, 2, 6, 4))
  u11v2 <- round(rtriangle(1, 2, 4, 4))
  u12v2 <- round(rtriangle(1, 2, 3, 3))
  u13v2 <- round(rtriangle(1, 0, 2, 1))
  
  u1v3 <- round(rtriangle(1, 0, 2, 1))
  u2v3 <- round(rtriangle(1, 0, 0, 0))
  u3v3 <- round(rtriangle(1, 0, 0, 0))
  u4v3 <- round(rtriangle(1, 0, 0, 0))
  u5v3 <- round(rtriangle(1, 1, 2, 1))
  u6v3 <- round(rtriangle(1, 0, 1, 1))
  u7v3 <- round(rtriangle(1, 0, 1, 1))
  u8v3 <- round(rtriangle(1, 5, 10, 7))
  u9v3 <- round(rtriangle(1, 0, 2, 1))
  u10v3 <- round(rtriangle(1, 0, 2, 1))
  u11v3 <- round(rtriangle(1, 2, 4, 3))
  u12v3 <- round(rtriangle(1, 0, 2, 1))
  u13v3 <- round(rtriangle(1, 0, 0, 0))
  
  u1v4 <- round(rtriangle(1, 1, 2, 2))
  u2v4 <- round(rtriangle(1, 0, 0, 0))
  u3v4 <- round(rtriangle(1, 0, 0, 0))
  u4v4 <- round(rtriangle(1, 0, 0, 0))
  u5v4 <- round(rtriangle(1, 1, 2, 1))
  u6v4 <- round(rtriangle(1, 0, 1, 1))
  u7v4 <- round(rtriangle(1, 0, 1, 1))
  u8v4 <- round(rtriangle(1, 5, 10, 7))
  u9v4 <- round(rtriangle(1, 0, 2, 1))
  u10v4 <- round(rtriangle(1, 0, 2, 1))
  u11v4 <- round(rtriangle(1, 2, 4, 3))
  u12v4 <- round(rtriangle(1, 0, 2, 1))
  u13v4 <- round(rtriangle(1, 1, 1, 1))
  
  u1v5 <- round(rtriangle(1, 0, 0, 0))
  u2v5 <- round(rtriangle(1, 0, 2, 1))
  u3v5 <- round(rtriangle(1, 0, 0, 0))
  u4v5 <- round(rtriangle(1, 0, 0, 0))
  u5v5 <- round(rtriangle(1, 1, 3, 2))
  u6v5 <- round(rtriangle(1, 0, 2, 1))
  u7v5 <- round(rtriangle(1, 0, 2, 1))
  u8v5 <- round(rtriangle(1, 10, 15, 12))
  u9v5 <- round(rtriangle(1, 1, 3, 2))
  u10v5 <- round(rtriangle(1, 1, 3, 2))
  u11v5 <- round(rtriangle(1, 5, 10, 7))
  u12v5 <- round(rtriangle(1, 2, 3, 2))
  u13v5 <- round(rtriangle(1, 0, 2, 1))
  
  u1v6 <- round(rtriangle(1, 2, 5, 4))
  u2v6 <- round(rtriangle(1, 3, 6, 5))
  u3v6 <- round(rtriangle(1, 1, 2, 1))
  u4v6 <- round(rtriangle(1, 1, 2, 2))
  u5v6 <- round(rtriangle(1, 1, 3, 2))
  u6v6 <- round(rtriangle(1, 1, 3, 2))
  u7v6 <- round(rtriangle(1, 1, 3, 2))
  u8v6 <- round(rtriangle(1, 20, 60, 40))
  u9v6 <- round(rtriangle(1, 2, 6, 4))
  u10v6 <- round(rtriangle(1, 3, 6, 6))
  u11v6 <- round(rtriangle(1, 3, 6, 5))
  u12v6 <- round(rtriangle(1, 2, 5, 4))
  u13v6 <- round(rtriangle(1, 0, 1, 1))
  
  # Number of units if scenario 1
  if (scenario == 1) {
    u1 <- 75
    u2 <- 14
    u3 <- 6
    u4 <- 23
    u5 <- 29
    u6 <- 9
    u7 <- 52
    u8 <- 4
    u9 <- 8
    u10 <- 25
    u11 <- 5
    u12 <- 21
    u13 <- 25
    
    demand_v2 <- u1*u1v2 + u2*u2v2 + u3*u3v2 + u4*u4v2 + u5*u5v2 + u6*u6v2 +
      u7*u7v2 + u8*u8v2 + u9*u9v2 + u10*u10v2 + u11*u11v2 + u12*u12v2 + u13*u13v2
    
    demand_v3 <- u1*u1v3 + u2*u2v3 + u3*u3v3 + u4*u4v3 + u5*u5v3 + u6*u6v3 +
      u7*u7v3 + u8*u8v3 + u9*u9v3 + u10*u10v3 + u11*u11v3 + u12*u12v3 + u13*u13v3
    
    demand_v4 <- u1*u1v4 + u2*u2v4 + u3*u3v4 + u4*u4v4 + u5*u5v4 + u6*u6v4 +
      u7*u7v4 + u8*u8v4 + u9*u9v4 + u10*u10v4 + u11*u11v4 + u12*u12v4 + u13*u13v4
    
    demand_v5 <- u1*u1v5 + u2*u2v5 + u3*u3v5 + u4*u4v5 + u5*u5v5 + u6*u6v5 +
      u7*u7v5 + u8*u8v5 + u9*u9v5 + u10*u10v5 + u11*u11v5 + u12*u12v5 + u13*u13v5
    
    demand_v6 <- u1*u1v6 + u2*u2v6 + u3*u3v6 + u4*u4v6 + u5*u5v6 + u6*u6v6 +
      u7*u7v6 + u8*u8v6 + u9*u9v6 + u10*u10v6 + u11*u11v6 + u12*u12v6 + u13*u13v6
    # Number of units if scenario 2
  } else if (scenario == 2) {

    u1 <- 71
    u2 <- 13
    u3 <- 12
    u4 <- 15
    u5 <- 35
    u6 <- 14
    u7 <- 57
    u8 <- 4
    u9 <- 7
    u10 <- 22
    u11 <- 4
    u12 <- 16
    u13 <- 25
    
    demand_v2 <- u1*u1v2 + u2*u2v2 + u3*u3v2 + u4*u4v2 + u5*u5v2 + u6*u6v2 +
      u7*u7v2 + u8*u8v2 + u9*u9v2 + u10*u10v2 + u11*u11v2 + u12*u12v2 + u13*u13v2
    
    demand_v3 <- u1*u1v3 + u2*u2v3 + u3*u3v3 + u4*u4v3 + u5*u5v3 + u6*u6v3 +
      u7*u7v3 + u8*u8v3 + u9*u9v3 + u10*u10v3 + u11*u11v3 + u12*u12v3 + u13*u13v3
    
    demand_v4 <- u1*u1v4 + u2*u2v4 + u3*u3v4 + u4*u4v4 + u5*u5v4 + u6*u6v4 +
      u7*u7v4 + u8*u8v4 + u9*u9v4 + u10*u10v4 + u11*u11v4 + u12*u12v4 + u13*u13v4
    
    demand_v5 <- u1*u1v5 + u2*u2v5 + u3*u3v5 + u4*u4v5 + u5*u5v5 + u6*u6v5 +
      u7*u7v5 + u8*u8v5 + u9*u9v5 + u10*u10v5 + u11*u11v5 + u12*u12v5 + u13*u13v5
    
    demand_v6 <- u1*u1v6 + u2*u2v6 + u3*u3v6 + u4*u4v6 + u5*u5v6 + u6*u6v6 +
      u7*u7v6 + u8*u8v6 + u9*u9v6 + u10*u10v6 + u11*u11v6 + u12*u12v6 + u13*u13v6    
    # Number of units if scenario 3
  } else {
    u1 <- 85
    u2 <- 11
    u3 <- 1
    u4 <- 13
    u5 <- 26
    u6 <- 8
    u7 <- 41
    u8 <- 3
    u9 <- 8
    u10 <- 24
    u11 <- 4
    u12 <- 16
    u13 <- 25
    
    demand_v2 <- u1*u1v2 + u2*u2v2 + u3*u3v2 + u4*u4v2 + u5*u5v2 + u6*u6v2 +
      u7*u7v2 + u8*u8v2 + u9*u9v2 + u10*u10v2 + u11*u11v2 + u12*u12v2 + u13*u13v2
    
    demand_v3 <- u1*u1v3 + u2*u2v3 + u3*u3v3 + u4*u4v3 + u5*u5v3 + u6*u6v3 +
      u7*u7v3 + u8*u8v3 + u9*u9v3 + u10*u10v3 + u11*u11v3 + u12*u12v3 + u13*u13v3
    
    demand_v4 <- u1*u1v4 + u2*u2v4 + u3*u3v4 + u4*u4v4 + u5*u5v4 + u6*u6v4 +
      u7*u7v4 + u8*u8v4 + u9*u9v4 + u10*u10v4 + u11*u11v4 + u12*u12v4 + u13*u13v4
    
    demand_v5 <- u1*u1v5 + u2*u2v5 + u3*u3v5 + u4*u4v5 + u5*u5v5 + u6*u6v5 +
      u7*u7v5 + u8*u8v5 + u9*u9v5 + u10*u10v5 + u11*u11v5 + u12*u12v5 + u13*u13v5
    
    demand_v6 <- u1*u1v6 + u2*u2v6 + u3*u3v6 + u4*u4v6 + u5*u5v6 + u6*u6v6 +
      u7*u7v6 + u8*u8v6 + u9*u9v6 + u10*u10v6 + u11*u11v6 + u12*u12v6 + u13*u13v6
  }
  
  return(list("vehicle_v2" = demand_v2, "vehicle_v3" = demand_v3, "vehicle_v4" = demand_v4, "vehicle_v5" = demand_v5,
              "vehicle_v6" = demand_v6))
}


# scenario<-c(1,2,3) #n = the number of observations
loc_grid<-c(0,1)
scale_grid<-c(0,1)
seed<-1234

param_list=list("seed"=seed)
demand_samples <- MonteCarlo(func=generate_vehicle_demand,nrep=1000, param_list=param_list, ncpus=1)

# MakeFrame: Conversion of MonteCarlo outputs to data.frame
df<-MakeFrame(demand_samples)
head(df)
# 
# demand_samples_t <- t(demand_samples) #Transpose list
# head(demand_samples_t)  # print the first few samples

demand_stats <- summary(df)

# # output_path <- paste0(path,"/Output/")
# df_demand <- data.frame(demand_samples_t)
# output_path <- paste0(path,"/")
# write_xlsx(df_demand,paste0(output_path,"monte_carlo_output.xlsx"))

# set up Excel file
wb <- createWorkbook()
addWorksheet(wb, "my_sheet")

# write list to Excel
writeData(wb, sheet = "my_sheet", x = df, startRow = 1, startCol = 1)

# save Excel file
date <- Sys.Date()
saveWorkbook(wb, paste0(date,"_monte_carlo_output.xlsx"), overwrite = TRUE)
# 
# ggplot(data.frame(demand = demand_samples), aes(x = demand)) +
#   stat_ecdf(geom = "step", color = "steelblue") +
#   labs(title = "Vehicle Demand Simulation Results", x = "Demand", y = "Cumulative Probability")
# 
# # Generate demand samples
# demand_samples <- mc_replicate(1000, generate_vehicle_demand())


# # Calculate cumulative frequency and probability
# df <- data.frame(Demand = as.vector(demand_samples))
# df <- df %>%
#   group_by(Demand) %>%
#   summarise(Frequency = n()) %>%
#   mutate(Cumulative_Frequency = cumsum(Frequency),
#          Cumulative_Probability = cumsum(Frequency) / sum(Frequency))
# 
# # Create plot
# ggplot(df, aes(x = Demand, y = Cumulative_Frequency)) +
#   geom_bar(aes(y = Frequency), stat = "identity", fill = "steelblue", width = 0.5) +
#   scale_y_continuous(
#     name = "Cumulative Frequency",
#     sec.axis = sec_axis(~., name = "Cumulative Probability", labels = scales::percent),
#     limits = c(0, max(df$Cumulative_Frequency))
#   ) +
#   labs(title = "Cumulative Frequency Distribution",
#        x = "Demand") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
#         axis.title.x = element_blank())
