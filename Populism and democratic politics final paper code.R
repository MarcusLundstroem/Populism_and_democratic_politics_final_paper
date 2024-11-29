##################################################
###Populism and democratic politics final paper###
##################################################


# Load the data and packages

# Data already loaded and cleaned, but see selected variables and the process below
ess_combined_cleaned_ethnocentrism


library(tidyverse)
library(mediation)


ess7_selected_variables <- ess_round_7 %>%
  select(essround, cntry, imsmetn, imdfetn, stfeco, imbgeco, hincfel, hinctnta, edulvlb, dscrgrp)

ess10_selected_variables <- ess_round_10 %>%
  select(essround, cntry, imsmetn, imdfetn, stfeco, imbgeco, hincfel, hinctnta, edulvlb, dscrgrp)


ess_combined <- bind_rows(ess7_selected_variables, ess10_selected_variables)


ess_combined_cleaned <- ess_combined %>%
  mutate(across(everything(), ~ replace(., . %in% c(55, 66, 77, 88, 99, 999, 9999), NA)))


ess_combined_cleaned <- na.omit(ess_combined_cleaned)

ess_combined_cleaned_ethnocentrism <- ess_combined_cleaned %>%
  mutate(ethnocentrism = imsmetn - imdfetn)



mediator_model3 <- lm(hincfel ~ ethnocentrism + stfeco + hinctnta + edulvlb, data = ess_combined_cleaned_ethnocentrism)
outcome_model3 <- lm(imdfetn ~ ethnocentrism + hincfel + stfeco + hinctnta + edulvlb, data = ess_combined_cleaned_ethnocentrism)
mediation_results3 <- mediate(mediator_model3, outcome_model3, 
                              treat = "ethnocentrism", mediator = "hincfel", 
                              boot = TRUE, sims = 1000)

summary(mediation_results3) 


plot(
  mediation_results3, 
  main = "Mediation Analysis Results",   
  xlab = "Effect Size",                  
  ylab = "Effect Type",                  
  cex = 1.1,                             
  lwd = 2,                               
  col = "red",                          
  las = 1                                
)

mtext("Populism and Democratic Politics Final Paper", side = 3, line = 0.5, cex = 0.8, col = "gray40")
