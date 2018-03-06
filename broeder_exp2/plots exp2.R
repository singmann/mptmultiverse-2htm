
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

load("model_orig_n.eqn-exp2.txt.RData")

par_est <- unnest(results, est_group)
parameters_bn <- filter(par_est, parameter %in% c("Do_bn", "Dn_bn", "g_bn","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn","r_1_bn","r_2_bn","r_3_bn","r_7_bn","r_8_bn"))
shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

ggplot(parameters_bn, aes(y = est, x = parameter, 
                          col=interaction(method, pooling,package),
                          shape=interaction(method, pooling,package))) +
  facet_wrap(~condition, ncol = 1) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw()


parameters_bo <- filter(par_est, parameter %in% c("Do_bo", "Dn_bo", "g_bo","q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","r_1_bo","r_2_bo","r_3_bo","r_7_bo","r_8_bo"))

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

ggplot(parameters_bo, aes(y = est, x = parameter, 
                          col=interaction(method, pooling,package),
                          shape=interaction(method, pooling,package))) +
  facet_wrap(~condition, ncol = 1) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw()


core <- filter(par_est, parameter %in% c("Do_bn", "Do_bo","Dn_bn", "Dn_bo","g_bn","g_bo"))
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


params_q <- filter(par_est, parameter %in% c("q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn"))
ggplot(params_q, aes(y = est, x = interaction(method, pooling,package),
                 shape=model, color =model)) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  coord_flip()

params_r <- filter(par_est, parameter %in% c("r_1_bo","r_2_bo","r_3_bo","r_7_bo","r_8_bo","r_1_bn","r_2_bn","r_3_bn","r_7_bn","r_8_bn"))
ggplot(params_r, aes(y = est, x = interaction(method, pooling,package),
                 shape=model, color =model)) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  coord_flip()


res_between <-  unnest(results, test_between) 

dd <- position_dodge(w = .75)
core2 <- filter(res_between, parameter %in% c("Do_bn", "Do_bo","Dn_bn", "Dn_bo","g_bn","g_bo"))
ggplot(core2, aes(y = est_diff, x = interaction(method, pooling,package),
                  shape=model, color=model)) +
  facet_wrap(~parameter, ncol=3)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  coord_flip()
