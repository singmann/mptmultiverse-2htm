
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


load("model_1.eqn-exp1.txt.RData")

results1<-results

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

gg_est1 <- unnest(results, est_group) %>%
  ggplot(aes(y = est, x = parameter, 
             col=interaction(method, pooling,package),
             shape=interaction(method, pooling,package))) +
  facet_grid(.~condition) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd) + ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw()

gg_est1

load("model_2.eqn-exp1.txt.RData")

results2<-results

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

gg_est2 <- unnest(results2, est_group) %>%
  ggplot(aes(y = est, x = parameter, 
             col=interaction(method, pooling,package),
             shape=interaction(method, pooling,package))) +
  facet_grid(.~condition) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd) + ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw()

gg_est2

load("model_3.eqn-exp1.txt.RData")

results3<-results

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

gg_est3 <- unnest(results3, est_group) %>%
  ggplot(aes(y = est, x = parameter, 
             col=interaction(method, pooling,package),
             shape=interaction(method, pooling,package))) +
  facet_grid(.~condition) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd) + ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw()

gg_est3

#estimated difference

load("model_1.eqn-exp1.txt.RData")

results1<-results

res_between <-  unnest(results1, test_between) 

gg_est2 <- ggplot(res_between, aes(y = est_diff, x = parameter, 
                                     col=interaction(method, pooling,package),
                                     shape=interaction(method, pooling,package))) +
    facet_grid(condition2~condition1) +
    geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                  position = dd, width = 0.5)+
    geom_point(position = dd) + ylim(-1,1) + 
    scale_shape_manual(values=shapes) +
    theme_bw() + geom_hline(yintercept = 0, lty = 2)
  plot(gg_est2)
  
gg_gof1 <-  unnest(results1, gof) %>%
    # filter(focus == "mean") %>%
    ggplot(aes(y = p, 
               x = interaction(method, pooling, package))) + 
    geom_point() + ylim(0, 1) + 
    geom_hline(yintercept = .05, lty = 2)+
    theme_bw() + coord_flip() +
    facet_wrap(~focus) +
    ggtitle("Goodness of fit")
  plot(gg_gof1)
  
gg_gof2 <- unnest(results1, gof_group) %>%
    ggplot(aes(y = p, 
               x = interaction(method, pooling, package), 
               col = condition)) + 
    geom_point() + ylim(0, 1) + 
    geom_hline(yintercept = .05, lty = 2)+
    theme_bw() +
    coord_flip() +
    facet_wrap(~focus) +
    ggtitle("Goodness of fit")
plot(gg_gof2)

res_between <-  unnest(results1, test_between) 

gg_est2_2 <- ggplot(res_between, aes(y = est_diff, x = parameter, 
                                     col=interaction(method, pooling,package),
                                     shape=interaction(method, pooling,package))) +
  facet_grid(condition2~condition1) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  coord_flip()
plot(gg_est2_2)
