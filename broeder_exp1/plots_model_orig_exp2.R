
library("tidyr")
library("dplyr")
library("tibble")
library("rlang")
library("reshape2")
library("ggplot2")
library("parallel")
library("MPTinR")
library("TreeBUGS")
library("runjags")
library("purrr")
library("readr")
library("broom") # for tidy
source("../scripts/auxiliary_functions.R")
source("../scripts/summary_plots.R")


load("model_orig.eqn-exp2.txt.RData")

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

gg_est <- unnest(results, est_group) %>%
  ggplot(aes(y = est, x = parameter, 
             col=interaction(method, pooling,package),
             shape=interaction(method, pooling,package))) +
  facet_wrap(~condition, ncol = 1) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw()
plot(gg_est)