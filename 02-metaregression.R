# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   DTX FOR COMMON MENTAL DISORDERS                                           #
#   2024-11-08, MH                                                            #
#   02. Meta-Regression ----
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(tidyverse)
library(xlsx)
library(metapsyTools)
library(metafor)
library(meta)
library(forcats)
library(cowplot)
library(scales)
library(missForest)
library(caret)
source("utils/utils.R")


# Across all disorders --------------------------------------------------------

data$modality %>% recode("web" = "aa-web") -> data$modality
data$recruitment = as.factor(data$recruitment)
data$modality %>% recode("computer" = "aa-web") -> data$modality
data -> data.orig
preProcess(data %>% select(country:modality), method = "knnImpute") %>% 
  predict(data) -> data
runMetaAnalysis("threelevel.che", data = data, 
                which.combine = "studies") -> M

# Run regressions --------------------------------------------------------------

rbind(
  metaRegression(M$model.threelevel.che, ~ 0 + disorder + country) %>% extractor,
  metaRegression(M$model.threelevel.che, ~ 0 + disorder + intervention) %>% extractor,
  metaRegression(M$model.threelevel.che, ~ 0 + disorder + control) %>% extractor,
  metaRegression(M$model.threelevel.che, ~ 0 + disorder + rob) %>% extractor,
  metaRegression(M$model.threelevel.che, ~ 0 + disorder + year) %>% extractor,
  metaRegression(M$model.threelevel.che, ~ 0 + disorder + percent_women) %>% extractor,
  metaRegression(M$model.threelevel.che, ~ 0 + disorder + mean_age) %>% extractor,
  metaRegression(M$model.threelevel.che, ~ 0 + disorder + recruitment) %>% extractor,
  metaRegression(M$model.threelevel.che, ~ 0 + disorder + guidance) %>% extractor,
  metaRegression(M$model.threelevel.che, ~ 0 + disorder + modality) %>% extractor,
  metaRegression(M$model.threelevel.che, ~ 0 + disorder + country + intervention + 
                   control + rob + year + percent_women + 
                   mean_age + guidance + modality) %>% extractor %>% 
    mutate(pred = paste0("all_", pred))) -> res.mr

res.mr %>% 
  mutate(estimate = sprintf("%.3f", estimate),
         se = sprintf("%.3f", se),
         tval = sprintf("%.3f", tval),
         pval = ifelse(pval < 0.001, "<0.001", sprintf("%.3f", pval)))
