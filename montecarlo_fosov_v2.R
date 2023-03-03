library (MonteCarlo)
library (mcreplicate)
library(mcStats)
library(ggplot2)
library(dplyr)
library(writexl)
library(openxlsx)
library(EnvStats)

setwd("C:/Users/mark.zais/OneDrive - Integration Innovation/USSOCOM/FOSOV/Modeling/R_MontheCarlo/")
path <- getwd()

# scenario <-c(1,2,3) #Scenario  
# gmv_demand <-c(1382,1589,1685) #GMV demand
size <- 1


# generate_vehicle_demand <- function() {
#   sample(gmv_demand, size, prob = rep(1/3, 3))
# }
generate_vehicle_demand <- function(seed) {

  seed=seed  # set a random seed for reproducibility
  # Use this sample for equal likelihood of each scenario
  scenario <- sample(c(1, 2, 3), 1, prob = rep(1/3, 3))
  # Use this sample to adjust scenario probabilities for sensitivity analysis
  # scenario <- sample(c(1, 2, 3), 1, prob = c(.25,.25,.5))
  x11 <- round(rtri(1, min = 918, max = 1439, mode = 1341))
  x12 <- round(rtri(1, min = 1094, max = 1680, mode = 1545))
  x13 <- round(rtri(1, min = 1190, max = 1734, mode = 1645))
  x31 <- round(rtri(1, min = 342, max = 673, mode = 612))
  x32 <- round(rtri(1, min = 384, max = 766, mode = 696))
  x33 <- round(rtri(1, min = 415, max = 854, mode = 776))
  x51 <- round(rtri(1, min = 262, max = 396, mode = 360))
  x52 <- round(rtri(1, min = 322, max = 503, mode = 457))
  x53 <- round(rtri(1, min = 227, max = 348, mode = 316))
  
  if (scenario == 1) {
    demand_x1 <- x11
    demand_x3 <- x31
    demand_x5 <- x51
    demand_x6 <- 41
  } else if (scenario == 2) {
    demand_x1 <- x12
    demand_x3 <- x32
    demand_x5 <- x52
    demand_x6 <- 44
  } else {
    demand_x1 <- x13
    demand_x3 <- x33
    demand_x5 <- x53
    demand_x6 <- 40
  }
  
  return(list("vehicle_x1" = demand_x1, "vehicle_x3" = demand_x3, "vehicle_x5" = demand_x5, "vehicle_x6" = demand_x6))
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
saveWorkbook(wb, "monte_carlo_output.xlsx", overwrite = TRUE)
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
