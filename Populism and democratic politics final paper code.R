##################################################
###Populism and democratic politics final paper###
##################################################


# Load the data and packages
ESS7e02_3 <- read_csv("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Masteruppsats/Inspiration, data/ESS7e02_3/ESS7e02_3.csv")
ESS10 <- read_csv("C:/Users/Marcu/OneDrive/Skrivbord/Masternivå/Masteruppsats/Inspiration, data/ESS10/ESS10.csv")

ess_round_7 <- ESS7e02_3
ess_round_10 <- ESS10

remove(ESS10)
remove(ESS7e02_3)

View(ess_round_10)
View(ess_round_7)

library(tidyverse)
library(mediation)

# Specify variables of interest:
ess7_selected_variables <- ess_round_7 %>%
  select(essround, cntry, imsmetn, imdfetn, stfeco, imbgeco, hincfel, hinctnta, edulvlb)

ess10_selected_variables <- ess_round_10 %>%
  select(essround, cntry, imsmetn, imdfetn, stfeco, imbgeco, hincfel, hinctnta, edulvlb)

# Join the datasets
ess_combined <- bind_rows(ess7_selected_variables, ess10_selected_variables)

# Exclude missing values coded as 77 (refusal), 88 (don't know), 99 (No answer), etcetera, and replace them with NA
ess_combined_cleaned <- ess_combined %>%
  mutate(across(everything(), ~ replace(., . %in% c(55, 66, 77, 88, 99, 999, 9999), NA)))

# Excluding rows with missing values
ess_combined_cleaned <- na.omit(ess_combined_cleaned)


# Create ethnocentrism variable as a predictor of anti-migratory attitudes
ess_combined_cleaned_ethnocentrism <- ess_combined_cleaned %>%
  mutate(ethnocentrism = imsmetn - imdfetn)


#Mediator model
mediator_model3 <- lm(hincfel ~ ethnocentrism + stfeco + hinctnta + edulvlb, data = ess_combined_cleaned_ethnocentrism)
outcome_model3 <- lm(imdfetn ~ ethnocentrism + hincfel + stfeco + hinctnta + edulvlb, data = ess_combined_cleaned_ethnocentrism)
mediation_results3 <- mediate(mediator_model, outcome_model, 
                              treat = "ethnocentrism", mediator = "hincfel", 
                              boot = TRUE, sims = 1000)

summary(mediation_results3)
