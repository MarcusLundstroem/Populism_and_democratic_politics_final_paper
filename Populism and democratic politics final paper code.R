##################################################
###Populism and democratic politics final paper###
##################################################


# Load the data and packages

# Data already loaded

# Rename
ess_round_7 <- ESS7e02_3
ess_round_10 <- ESS10

remove(ESS10)
remove(ESS7e02_3)

View(ess_round_10)
View(ess_round_7)

library(tidyverse)
library(mediation)


ess7_selected_variables <- ess_round_7 %>%
  select(essround, cntry, imsmetn, imdfetn, stfeco, imbgeco, hincfel, hinctnta, edulvlb)

ess10_selected_variables <- ess_round_10 %>%
  select(essround, cntry, imsmetn, imdfetn, stfeco, imbgeco, hincfel, hinctnta, edulvlb)


ess_combined <- bind_rows(ess7_selected_variables, ess10_selected_variables)


ess_combined_cleaned <- ess_combined %>%
  mutate(across(everything(), ~ replace(., . %in% c(55, 66, 77, 88, 99, 999, 9999), NA)))


ess_combined_cleaned <- na.omit(ess_combined_cleaned)


ess_combined_cleaned_ethnocentrism <- ess_combined_cleaned %>%
  mutate(ethnocentrism = imsmetn - imdfetn)


mediator_model3 <- lm(hincfel ~ ethnocentrism + stfeco + hinctnta + edulvlb, data = ess_combined_cleaned_ethnocentrism)
outcome_model3 <- lm(imdfetn ~ ethnocentrism + hincfel + stfeco + hinctnta + edulvlb, data = ess_combined_cleaned_ethnocentrism)
mediation_results3 <- mediate(mediator_model, outcome_model, 
                              treat = "ethnocentrism", mediator = "hincfel", 
                              boot = TRUE, sims = 1000)

summary(mediation_results3)
