
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
library("tidyverse")
source("../scripts/auxiliary_functions.R")
source("../scripts/summary_plots.R")

load("model_1.eqn-exp1.txt.RData")
results1 <- results

load("model_2.eqn-exp1.txt.RData")
results2 <- results

load("model_3.eqn-exp1.txt.RData")
results3 <- results

load("model_orig.eqn-exp1.txt.RData")
results4 <- results

par_est <- bind_rows(
  unnest(results1, est_group),
  unnest(results2, est_group),
  unnest(results3, est_group),
  unnest(results4, est_group)
)

Do<-filter(par_est, parameter== "Do")

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

Do %>%
  group_by("model")
  
ggplot(Do, aes(y = est, x = interaction(method, pooling,package),
             shape=model, color =model)) +
  facet_wrap(~condition, ncol = 1) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  coord_flip()

core <- filter(par_est, parameter %in% c("Do", "Dn", "g"))
ggplot(core, aes(y = est, x = interaction(method, pooling,package),
               shape=model, color =model)) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  coord_flip()

####mexri edw kala na allaksw kai diagrammata kai na ta kanw ola - allazw kai grammata ston x

res_between <-  unnest(results, test_between) 

gg_est2 <- ggplot(res_between, aes(y = est_diff, x = parameter, 
                                     col=interaction(method, pooling,package),
                                     shape=interaction(method, pooling,package))) +
  facet_grid(condition2~condition1) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  coord_flip()
plot(gg_est2)


gg_gof1 <-  unnest(results, gof) %>%
  # filter(focus == "mean") %>%
  ggplot(aes(y = p, 
             x = interaction(method, pooling, package))) + 
  geom_point(size=2) + ylim(0, 1) + 
  geom_hline(yintercept = .05, lty = 2)+
  theme_bw() + coord_flip() +
  facet_wrap(~focus) +
  ggtitle("Goodness of fit")
plot(gg_gof1)

gg_gof2 <- unnest(results, gof_group) %>%
  ggplot(aes(y = p, 
             x = interaction(method, pooling, package), 
             col = condition)) + 
  geom_point(size=2) + ylim(0, 1) + 
  geom_hline(yintercept = .05, lty = 2)+
  theme_bw() +
  coord_flip() +
  facet_wrap(~focus) +
  ggtitle("Goodness of fit")
plot(gg_gof2)
