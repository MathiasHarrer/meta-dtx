# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   DTX FOR COMMON MENTAL DISORDERS                                           #
#   2024-11-08, MH                                                            #
#   00. Main Analysis (Code Sample) ----
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# This file shows exemplary code used, with minor adaptions, across disorders

library(tidyverse)
library(xlsx)
library(metapsyTools)
library(metafor)
library(meta)
library(forcats)
library(cowplot)
library(scales)


## Effectiveness -------------------------------------------------------------

rbind(
  # Calculate effect sizes (overall)
  data %>%
    runMetaAnalysis(c("combined", "threelevel.che", 
                      "influence", "lowest.highest",
                      "rob"), 
                    low.rob.filter = "rob.2 == 'low risk'") %>% 
    correctPublicationBias() %>% 
    {rbind(.$summary,
           .$correctPublicationBias$summary)} %>% 
    mutate(guidance = "all"),
  # Calculate effect sizes (ush)
  data %>%
    filterPoolingData(.guidance == "ush") %>%
    runMetaAnalysis(c("combined", "threelevel.che", 
                      "influence", "lowest.highest",
                      "rob"), 
                    low.rob.filter = "rob.2 == 'low risk'") %>% 
    correctPublicationBias() %>% 
    {rbind(.$summary,
           .$correctPublicationBias$summary)} %>% 
    mutate(guidance = "ush"),
  # Calculate effect sizes (gsh)
  data %>%
    filterPoolingData(.guidance == "gsh") %>%
    runMetaAnalysis(c("combined", "threelevel.che", 
                      "influence", "lowest.highest",
                      "rob"), 
                    low.rob.filter = "rob.2 == 'low risk'") %>% 
    correctPublicationBias() %>% 
    {rbind(.$summary,
           .$correctPublicationBias$summary)} %>% 
    mutate(guidance = "gsh")
) -> res.dep


## Adherence ---------------------------------------------------------------

parse_number(data$attr_arm1) -> data$dropout_n_arm1
parse_number(data$attr_arm2) -> data$dropout_n_arm2
data$rand_arm1 -> data$totaln_arm1
data$rand_arm2 -> data$totaln_arm2
data %>%
  distinct(study, .keep_all = TRUE) -> data.dis

### Per Arm --------------------------------------------------------------

rma.glmm(xi = dropout_n_arm1, ni = totaln_arm1,  
         measure = "PLO", slab = study, data = data.dis) -> m
predict(m, transf=transf.ilogit) -> m.pred
data.frame(
  which = "arm1", k = m$k.all, prop = m.pred$pred, lower = m.pred$ci.lb,
  upper = m.pred$ci.ub, pi.lower = m.pred$pi.lb, pi.upper = m.pred$pi.ub,
  i2 = m$I2, tau2 = m$tau2, guidance = "all") -> prop.all.arm1

rma.glmm(xi = dropout_n_arm2, ni = totaln_arm2,  
         measure = "PLO", slab = study, data = data.dis) -> m
predict(m, transf=transf.ilogit) -> m.pred
data.frame(
  which = "arm2", k = m$k.all, prop = m.pred$pred, lower = m.pred$ci.lb,
  upper = m.pred$ci.ub, pi.lower = m.pred$pi.lb, pi.upper = m.pred$pi.ub,
  i2 = m$I2, tau2 = m$tau2, guidance = "all") -> prop.all.arm2

rma.glmm(xi = dropout_n_arm1, ni = totaln_arm1,  
         measure = "PLO", slab = study, 
         data = data.dis %>% filter(.guidance=="ush")) -> m
predict(m, transf=transf.ilogit) -> m.pred
data.frame(
  which = "arm1", k = m$k.all, prop = m.pred$pred, lower = m.pred$ci.lb,
  upper = m.pred$ci.ub, pi.lower = m.pred$pi.lb, pi.upper = m.pred$pi.ub,
  i2 = m$I2, tau2 = m$tau2, guidance = "ush") -> prop.ush.arm1

rma.glmm(xi = dropout_n_arm2, ni = totaln_arm2,  
         measure = "PLO", slab = study, 
         data = data.dis %>% filter(.guidance=="ush")) -> m
predict(m, transf=transf.ilogit) -> m.pred
data.frame(
  which = "arm2", k = m$k.all, prop = m.pred$pred, lower = m.pred$ci.lb,
  upper = m.pred$ci.ub, pi.lower = m.pred$pi.lb, pi.upper = m.pred$pi.ub,
  i2 = m$I2, tau2 = m$tau2, guidance = "ush") -> prop.ush.arm2

rma.glmm(xi = dropout_n_arm1, ni = totaln_arm1,  
         measure = "PLO", slab = study, 
         data = data.dis %>% filter(.guidance=="gsh")) -> m
predict(m, transf=transf.ilogit) -> m.pred
data.frame(
  which = "arm1", k = m$k.all, prop = m.pred$pred, lower = m.pred$ci.lb,
  upper = m.pred$ci.ub, pi.lower = m.pred$pi.lb, pi.upper = m.pred$pi.ub,
  i2 = m$I2, tau2 = m$tau2,
  guidance = "gsh") -> prop.gsh.arm1

rma.glmm(xi = dropout_n_arm2, ni = totaln_arm2,  
         measure = "PLO", slab = study, 
         data = data.dis %>% filter(.guidance=="gsh")) -> m
predict(m, transf=transf.ilogit) -> m.pred
data.frame(
  which = "arm2", k = m$k.all, prop = m.pred$pred, lower = m.pred$ci.lb,
  upper = m.pred$ci.ub,
  pi.lower = m.pred$pi.lb, pi.upper = m.pred$pi.ub, i2 = m$I2, tau2 = m$tau2, 
  guidance = "gsh") -> prop.gsh.arm2

# Collect all results
rbind(prop.all.arm1, prop.all.arm2, prop.gsh.arm1, prop.gsh.arm2,
      prop.ush.arm1, prop.ush.arm2) -> res.dep.adh


### Differential ---------------------------------------------------------

rma.uni(ai = dropout_n_arm1, ci = dropout_n_arm2, 
        n1i = totaln_arm1, n2i = totaln_arm2,
        slab = study, measure = "RR",
        data = data.dis) -> m
predict(m, transf=exp) -> m.pred
data.frame(
  which = "diff", k = m$k.all, diff = m.pred$pred, lower = m.pred$ci.lb,
  upper = m.pred$ci.ub,
  pi.lower = m.pred$pi.lb, pi.upper = m.pred$pi.ub, i2 = m$I2, tau2 = m$tau2, 
  guidance = "all") -> diff.all

rma.uni(ai = dropout_n_arm1, ci = dropout_n_arm2, 
        n1i = totaln_arm1, n2i = totaln_arm2,
        slab = study, measure = "RR",
        data = data.dis %>% filter(.guidance=="ush")) -> m
predict(m, transf=exp) -> m.pred
data.frame(
  which = "diff", k = m$k.all, diff = m.pred$pred, lower = m.pred$ci.lb,
  upper = m.pred$ci.ub,
  pi.lower = m.pred$pi.lb, pi.upper = m.pred$pi.ub, i2 = m$I2, tau2 = m$tau2, 
  guidance = "ush") -> diff.ush

rma.uni(ai = dropout_n_arm1, ci = dropout_n_arm2, 
        n1i = totaln_arm1, n2i = totaln_arm2,
        slab = study, measure = "RR",
        data = data.dis %>% filter(.guidance=="gsh")) -> m
predict(m, transf=exp) -> m.pred
data.frame(
  which = "diff", k = m$k.all, diff = m.pred$pred, lower = m.pred$ci.lb,
  upper = m.pred$ci.ub,
  pi.lower = m.pred$pi.lb, pi.upper = m.pred$pi.ub, i2 = m$I2, tau2 = m$tau2, 
  guidance = "gsh") -> diff.gsh


## 1.3 Comparator --------------------------------------------------------------

rbind(
  data %>%
    filter(.guidance == "gsh") %>%
    filter(condition_arm2 == "cau") %>% 
    runMetaAnalysis("threelevel.che") %>% 
    {.$summary -> tmp; tmp$which = "all"; tmp},
  data %>%
    filter(.guidance == "gsh") %>%
    filter(condition_arm2 == "cau") %>% 
    runMetaAnalysis("threelevel.che") %>% 
    {.$summary -> tmp; tmp$which = "gsh"; tmp},
  data %>%
    filter(.guidance == "ush") %>%
    filter(condition_arm2 == "cau") %>% 
    runMetaAnalysis("threelevel.che") %>% 
    {.$summary -> tmp; tmp$which = "ush"; tmp}
) -> res.dep.cau

