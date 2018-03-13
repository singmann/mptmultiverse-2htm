
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
results1 <- results

load("model_orig_n_qrest.eqn-exp2.txt.RData")
results2 <- results

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
core <- add_column(core,bias=1)
core$bias <- ifelse(core$parameter=="Dn_bn",
                    'bn',
                    ifelse(core$parameter=="Do_bn",
                    'bn',
                    ifelse(core$parameter=='g_bn',
                           'bn',
                    'bo')))

core <- add_column(core,cp=1)
core$cp <- ifelse(core$parameter=="Dn_bn",
                    'Dn',
                    ifelse(core$parameter=="Do_bn",
                           'Do',
                           ifelse(core$parameter=='g_bn',
                                  'g',
                                  ifelse(core$parameter=="Dn_bo",
                                         'Dn',
                                         ifelse(core$parameter=='Do_bo',
                                                'Do',
                                                ifelse(core$parameter=='g_bo',
                                                       'g',
                                  core$parameter))))))



ggplot(core, aes(y = est, x = interaction(method, pooling,package),
                 shape=model, color =model)) +
  facet_grid(interaction(bias,condition) ~cp) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  coord_flip()




params_q <- filter(par_est, parameter %in% c("q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn"))

params_q <- add_column(params_q,bias=1)
params_q$bias <- ifelse(params_q$parameter=="q_1_bn",
                    'bn',
                    ifelse(params_q$parameter=="q_2_bn",
                           'bn',
                           ifelse(params_q$parameter=="q_3_bn",
                                  'bn',
                                  ifelse(params_q$parameter=="q_6_bn",
                                         'bn',
                                         ifelse(params_q$parameter=="q_7_bn",
                                                'bn',
                                                ifelse(params_q$parameter=="q_8_bn",
                                                       'bn',
                                  'bo'))))))

params_q <- add_column(params_q,pq=1)
params_q$pq <- ifelse(params_q$parameter=="q_1_bn",
                  'q_1',
                  ifelse(params_q$parameter=="q_2_bn",
                         'q_2',
                         ifelse(params_q$parameter=="q_3_bn",
                                'q_3',
                                ifelse(params_q$parameter=="q_6_bn",
                                       'q_6',
                                       ifelse(params_q$parameter=="q_7_bn",
                                              'q_7',
                                              ifelse(params_q$parameter=="q_8_bn",
                                                     'q_8',
                                                     ifelse(params_q$parameter=="q_1_bo",
                                                            'q_1',
                                                            ifelse(params_q$parameter=="q_2_bo",
                                                                   'q_2',
                                                                   ifelse(params_q$parameter=="q_3_bo",
                                                                          'q_3',
                                                                          ifelse(params_q$parameter=="q_6_bo",
                                                                                 'q_6',
                                                                                 ifelse(params_q$parameter=="q_7_bo",
                                                                                        'q_7',
                                                                                        ifelse(params_q$parameter=="q_8_bo",
                                                                                               'q_8',
                                                     params_q$parameter))))))))))))

ggplot(params_q, aes(y = est, x = interaction(method, pooling,package),
                 shape=model, color =model)) +
  facet_grid(interaction(bias,condition) ~pq) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1))+ 
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  coord_flip()



params_r <- filter(par_est, parameter %in% c("r_1_bo","r_2_bo","r_3_bo","r_7_bo","r_8_bo","r_1_bn","r_2_bn","r_3_bn","r_7_bn","r_8_bn"))

params_r <- add_column(params_r,bias=1)
params_r$bias <- ifelse(params_r$parameter=="r_1_bn",
                        'bn',
                        ifelse(params_r$parameter=="r_2_bn",
                               'bn',
                               ifelse(params_r$parameter=="r_3_bn",
                                      'bn',
                                             ifelse(params_r$parameter=="r_7_bn",
                                                    'bn',
                                                    ifelse(params_r$parameter=="r_8_bn",
                                                           'bn',
                                                           'bo')))))

params_r <- add_column(params_r,pr=1)
params_r$pr <- ifelse(params_r$parameter=="r_1_bn",
                      'r_1',
                      ifelse(params_r$parameter=="r_2_bn",
                             'r_2',
                             ifelse(params_r$parameter=="r_3_bn",
                                    'r_3',
                                    ifelse(params_r$parameter=="r_7_bn",
                                                  'r_7',
                                                  ifelse(params_r$parameter=="r_8_bn",
                                                         'r_8',
                                                         ifelse(params_r$parameter=="r_1_bo",
                                                                'r_1',
                                                                ifelse(params_r$parameter=="r_2_bo",
                                                                       'r_2',
                                                                       ifelse(params_r$parameter=="r_3_bo",
                                                                              'r_3',
                                                                              ifelse(params_r$parameter=="r_7_bo",
                                                                                            'r_7',
                                                                                            ifelse(params_r$parameter=="r_8_bo",
                                                                                                   'r_8',
                                                                                                   params_r$parameter))))))))))



ggplot(params_r, aes(y = est, x = interaction(method, pooling,package),
                 shape=model, color =model)) +
  facet_grid(interaction(bias,condition) ~pr) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1))+ 
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  coord_flip()


######estimated difff

res_between <-  unnest(results, test_between) 

core <- filter(res_between, parameter %in% c("Do_bn", "Do_bo","Dn_bn", "Dn_bo","g_bn","g_bo"))
core <- add_column(core,bias=1)
core$bias <- ifelse(core$parameter=="Dn_bn",
                    'bn',
                    ifelse(core$parameter=="Do_bn",
                           'bn',
                           ifelse(core$parameter=='g_bn',
                                  'bn',
                                  'bo')))

core <- add_column(core,cp=1)
core$cp <- ifelse(core$parameter=="Dn_bn",
                  'Dn',
                  ifelse(core$parameter=="Do_bn",
                         'Do',
                         ifelse(core$parameter=='g_bn',
                                'g',
                                ifelse(core$parameter=="Dn_bo",
                                       'Dn',
                                       ifelse(core$parameter=='Do_bo',
                                              'Do',
                                              ifelse(core$parameter=='g_bo',
                                                     'g',
                                                     core$parameter))))))


dd <- position_dodge(w = .75)
ggplot(core, aes(y = est_diff, x = interaction(method, pooling,package),
                  shape=model, color=model)) +
  facet_grid(bias ~cp)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  coord_flip()

res_between <-  unnest(results, test_between)

params_q <- filter(res_between, parameter %in% c("q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn"))

params_q <- add_column(params_q,bias=1)
params_q$bias <- ifelse(params_q$parameter=="q_1_bn",
                        'bn',
                        ifelse(params_q$parameter=="q_2_bn",
                               'bn',
                               ifelse(params_q$parameter=="q_3_bn",
                                      'bn',
                                      ifelse(params_q$parameter=="q_6_bn",
                                             'bn',
                                             ifelse(params_q$parameter=="q_7_bn",
                                                    'bn',
                                                    ifelse(params_q$parameter=="q_8_bn",
                                                           'bn',
                                                           'bo'))))))

params_q <- add_column(params_q,pq=1)
params_q$pq <- ifelse(params_q$parameter=="q_1_bn",
                      'q_1',
                      ifelse(params_q$parameter=="q_2_bn",
                             'q_2',
                             ifelse(params_q$parameter=="q_3_bn",
                                    'q_3',
                                    ifelse(params_q$parameter=="q_6_bn",
                                           'q_6',
                                           ifelse(params_q$parameter=="q_7_bn",
                                                  'q_7',
                                                  ifelse(params_q$parameter=="q_8_bn",
                                                         'q_8',
                                                         ifelse(params_q$parameter=="q_1_bo",
                                                                'q_1',
                                                                ifelse(params_q$parameter=="q_2_bo",
                                                                       'q_2',
                                                                       ifelse(params_q$parameter=="q_3_bo",
                                                                              'q_3',
                                                                              ifelse(params_q$parameter=="q_6_bo",
                                                                                     'q_6',
                                                                                     ifelse(params_q$parameter=="q_7_bo",
                                                                                            'q_7',
                                                                                            ifelse(params_q$parameter=="q_8_bo",
                                                                                                   'q_8',
                                                                                                   params_q$parameter))))))))))))

gg_est2 <- ggplot(params_q, aes(y = est_diff, x = interaction(method, pooling,package),
                                shape=model, color=model)) +
  facet_grid(bias ~pq) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1))+ 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 3) +
  coord_flip()
plot(gg_est2)


res_between <-  unnest(results, test_between)

params_r <- filter(res_between, parameter %in% c("r_1_bo","r_2_bo","r_3_bo","r_7_bo","r_8_bo","r_1_bn","r_2_bn","r_3_bn","r_7_bn","r_8_bn"))

params_r <- add_column(params_r,bias=1)
params_r$bias <- ifelse(params_r$parameter=="r_1_bn",
                        'bn',
                        ifelse(params_r$parameter=="r_2_bn",
                               'bn',
                               ifelse(params_r$parameter=="r_3_bn",
                                      'bn',
                                      ifelse(params_r$parameter=="r_7_bn",
                                             'bn',
                                             ifelse(params_r$parameter=="r_8_bn",
                                                    'bn',
                                                    'bo')))))

params_r <- add_column(params_r,pr=1)
params_r$pr <- ifelse(params_r$parameter=="r_1_bn",
                      'r_1',
                      ifelse(params_r$parameter=="r_2_bn",
                             'r_2',
                             ifelse(params_r$parameter=="r_3_bn",
                                    'r_3',
                                    ifelse(params_r$parameter=="r_7_bn",
                                           'r_7',
                                           ifelse(params_r$parameter=="r_8_bn",
                                                  'r_8',
                                                  ifelse(params_r$parameter=="r_1_bo",
                                                         'r_1',
                                                         ifelse(params_r$parameter=="r_2_bo",
                                                                'r_2',
                                                                ifelse(params_r$parameter=="r_3_bo",
                                                                       'r_3',
                                                                       ifelse(params_r$parameter=="r_7_bo",
                                                                              'r_7',
                                                                              ifelse(params_r$parameter=="r_8_bo",
                                                                                     'r_8',
                                                                                     params_r$parameter))))))))))

gg_est3 <- ggplot(params_r, aes(y = est_diff, x = interaction(method, pooling,package),
                                shape=model, color=model)) +
  facet_grid(bias ~pr) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1))+ 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 3) +
  coord_flip()
plot(gg_est3)


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


shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

gg_est_1 <- unnest(results1, est_group) %>%
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
plot(gg_est_1)

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

gg_est_2 <- unnest(results2, est_group) %>%
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


par_est <- bind_rows(
  unnest(results1, est_group),
  unnest(results2, est_group)
)

params_q <- filter(par_est, parameter %in% c("q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn"))

params_q <- add_column(params_q,bias=1)
params_q$bias <- ifelse(params_q$parameter=="q_1_bn",
                        'bn',
                        ifelse(params_q$parameter=="q_2_bn",
                               'bn',
                               ifelse(params_q$parameter=="q_3_bn",
                                      'bn',
                                      ifelse(params_q$parameter=="q_6_bn",
                                             'bn',
                                             ifelse(params_q$parameter=="q_7_bn",
                                                    'bn',
                                                    ifelse(params_q$parameter=="q_8_bn",
                                                           'bn',
                                                           'bo'))))))

params_q <- add_column(params_q,pq=1)
params_q$pq <- ifelse(params_q$parameter=="q_1_bn",
                      'q_1',
                      ifelse(params_q$parameter=="q_2_bn",
                             'q_2',
                             ifelse(params_q$parameter=="q_3_bn",
                                    'q_3',
                                    ifelse(params_q$parameter=="q_6_bn",
                                           'q_6',
                                           ifelse(params_q$parameter=="q_7_bn",
                                                  'q_7',
                                                  ifelse(params_q$parameter=="q_8_bn",
                                                         'q_8',
                                                         ifelse(params_q$parameter=="q_1_bo",
                                                                'q_1',
                                                                ifelse(params_q$parameter=="q_2_bo",
                                                                       'q_2',
                                                                       ifelse(params_q$parameter=="q_3_bo",
                                                                              'q_3',
                                                                              ifelse(params_q$parameter=="q_6_bo",
                                                                                     'q_6',
                                                                                     ifelse(params_q$parameter=="q_7_bo",
                                                                                            'q_7',
                                                                                            ifelse(params_q$parameter=="q_8_bo",
                                                                                                   'q_8',
                                                                                                   params_q$parameter))))))))))))

ggplot(params_q, aes(y = est, x = interaction(method, pooling,package),
                     shape=model, color =model)) +
  facet_grid(interaction(bias,condition) ~pq) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1))+ 
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  coord_flip()
plot(gg_est_2)


par_est <- unnest(results1, est_group)

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

par_est <- unnest(results2, est_group)

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

ggplot(parameters_bo, aes(y = est,x = parameter, 
                          col=interaction(method, pooling,package),
                          shape=interaction(method, pooling,package))) +
  facet_wrap(~condition, ncol = 1) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw()


par_est <- bind_rows(
  unnest(results1, est_group),
  unnest(results2, est_group)
)

params_q <- filter(par_est, parameter %in% c("q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn"))

params_q <- add_column(params_q,bias=1)
params_q$bias <- ifelse(params_q$parameter=="q_1_bn",
                        'bn',
                        ifelse(params_q$parameter=="q_2_bn",
                               'bn',
                               ifelse(params_q$parameter=="q_3_bn",
                                      'bn',
                                      ifelse(params_q$parameter=="q_6_bn",
                                             'bn',
                                             ifelse(params_q$parameter=="q_7_bn",
                                                    'bn',
                                                    ifelse(params_q$parameter=="q_8_bn",
                                                           'bn',
                                                           'bo'))))))

params_q <- add_column(params_q,pq=1)
params_q$pq <- ifelse(params_q$parameter=="q_1_bn",
                      'q_1',
                      ifelse(params_q$parameter=="q_2_bn",
                             'q_2',
                             ifelse(params_q$parameter=="q_3_bn",
                                    'q_3',
                                    ifelse(params_q$parameter=="q_6_bn",
                                           'q_6',
                                           ifelse(params_q$parameter=="q_7_bn",
                                                  'q_7',
                                                  ifelse(params_q$parameter=="q_8_bn",
                                                         'q_8',
                                                         ifelse(params_q$parameter=="q_1_bo",
                                                                'q_1',
                                                                ifelse(params_q$parameter=="q_2_bo",
                                                                       'q_2',
                                                                       ifelse(params_q$parameter=="q_3_bo",
                                                                              'q_3',
                                                                              ifelse(params_q$parameter=="q_6_bo",
                                                                                     'q_6',
                                                                                     ifelse(params_q$parameter=="q_7_bo",
                                                                                            'q_7',
                                                                                            ifelse(params_q$parameter=="q_8_bo",
                                                                                                   'q_8',
                                                                                                   params_q$parameter))))))))))))

ggplot(params_q, aes(y = est, x = interaction(method, pooling,package),
                     shape=model, color =model)) +
  facet_grid(interaction(bias,condition) ~pq) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1))+ 
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  coord_flip()


par_est <- bind_rows(
  unnest(results1, est_group),
  unnest(results2, est_group)
)

params_r <- filter(par_est, parameter %in% c("r_1_bo","r_2_bo","r_3_bo",'r_6_bo',"r_7_bo","r_8_bo","r_1_bn","r_2_bn","r_3_bn",'r_6_bn',"r_7_bn","r_8_bn"))

params_r <- add_column(params_r,bias=1)
params_r$bias <- ifelse(params_r$parameter=="r_1_bn",
                        'bn',
                        ifelse(params_r$parameter=="r_2_bn",
                               'bn',
                               ifelse(params_r$parameter=="r_3_bn",
                                      'bn',
                                      ifelse(params_r$parameter=="r_6_bn",
                                             'bn',
                                             ifelse(params_r$parameter=="r_7_bn",
                                                    'bn',
                                                    ifelse(params_r$parameter=="r_8_bn",
                                                           'bn',
                                                           'bo'))))))

params_r <- add_column(params_r,pr=1)
params_r$pr <- ifelse(params_r$parameter=="r_1_bn",
                      'r_1',
                      ifelse(params_r$parameter=="r_2_bn",
                             'r_2',
                             ifelse(params_r$parameter=="r_3_bn",
                                    'r_3',
                                    ifelse(params_r$parameter=="r_6_bn",
                                           'r_6',
                                           ifelse(params_r$parameter=="r_7_bn",
                                                  'r_7',
                                                  ifelse(params_r$parameter=="r_8_bn",
                                                         'r_8',
                                                         ifelse(params_r$parameter=="r_1_bo",
                                                                'r_1',
                                                                ifelse(params_r$parameter=="r_2_bo",
                                                                       'r_2',
                                                                       ifelse(params_r$parameter=="r_3_bo",
                                                                              'r_3',
                                                                              ifelse(params_r$parameter=="r_6_bo",
                                                                                     'r_6',
                                                                                     ifelse(params_r$parameter=="r_7_bo",
                                                                                            'r_7',
                                                                                            ifelse(params_r$parameter=="r_8_bo",
                                                                                                   'r_8',
                                                                                                   params_r$parameter))))))))))))



ggplot(params_r, aes(y = est, x = interaction(method, pooling,package),
                     shape=model, color =model)) +
  facet_grid(interaction(bias,condition) ~pr) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1))+ 
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  coord_flip()


res_between <-  unnest(results1, test_between) 

gg_est2_1 <- ggplot(res_between, aes(y = est_diff, x = parameter, 
                                     col=interaction(method, pooling,package),
                                     shape=interaction(method, pooling,package))) +
  facet_grid(condition2~condition1) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  coord_flip()
plot(gg_est2_1)


res_between <-  unnest(results2, test_between) 

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

res_between <-  unnest(results1, test_between) 

parameters_bn <- filter(res_between, parameter %in% c("Do_bn", "Dn_bn", "g_bn","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn","r_1_bn","r_2_bn","r_3_bn","r_7_bn","r_8_bn"))

gg_est2_bn <- ggplot(parameters_bn, aes(y = est_diff, x = parameter, 
                                        col=interaction(method, pooling,package),
                                        shape=interaction(method, pooling,package))) +
  facet_grid(condition2~condition1) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  coord_flip()
plot(gg_est2_bn)

parameters_bo <- filter(res_between, parameter %in% c("Do_bo", "Dn_bo", "g_bo","q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","r_1_bo","r_2_bo","r_3_bo","r_7_bo","r_8_bo"))

gg_est2_bo <- ggplot(parameters_bo, aes(y = est_diff, x = parameter, 
                                        col=interaction(method, pooling,package),
                                        shape=interaction(method, pooling,package))) +
  facet_grid(condition2~condition1) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  coord_flip()
plot(gg_est2_bo)

res_between <-  unnest(results2, test_between) 

parameters_bn <- filter(res_between, parameter %in% c("Do_bn", "Dn_bn", "g_bn","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn","r_1_bn","r_2_bn","r_3_bn",'r_6_bn',"r_7_bn","r_8_bn"))

gg_est2_bn <- ggplot(parameters_bn, aes(y = est_diff, x = parameter, 
                                        col=interaction(method, pooling,package),
                                        shape=interaction(method, pooling,package))) +
  facet_grid(condition2~condition1) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  coord_flip()
plot(gg_est2_bn)

parameters_bo <- filter(res_between, parameter %in% c("Do_bo", "Dn_bo", "g_bo","q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","r_1_bo","r_2_bo","r_3_bo",'r_6_bo',"r_7_bo","r_8_bo"))

gg_est2_bo <- ggplot(parameters_bo, aes(y = est_diff, x = parameter, 
                                        col=interaction(method, pooling,package),
                                        shape=interaction(method, pooling,package))) +
  facet_grid(condition2~condition1) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  coord_flip()
plot(gg_est2_bo)

res_between <- bind_rows(
  unnest(results1, test_between),
  unnest(results2, test_between)
)

core <- filter(res_between, parameter %in% c("Do_bn", "Do_bo","Dn_bn", "Dn_bo","g_bn","g_bo"))
core <- add_column(core,bias=1)
core$bias <- ifelse(core$parameter=="Dn_bn",
                    'bn',
                    ifelse(core$parameter=="Do_bn",
                           'bn',
                           ifelse(core$parameter=='g_bn',
                                  'bn',
                                  'bo')))

core <- add_column(core,cp=1)
core$cp <- ifelse(core$parameter=="Dn_bn",
                  'Dn',
                  ifelse(core$parameter=="Do_bn",
                         'Do',
                         ifelse(core$parameter=='g_bn',
                                'g',
                                ifelse(core$parameter=="Dn_bo",
                                       'Dn',
                                       ifelse(core$parameter=='Do_bo',
                                              'Do',
                                              ifelse(core$parameter=='g_bo',
                                                     'g',
                                                     core$parameter))))))


dd <- position_dodge(w = .75)
ggplot(core, aes(y = est_diff, x = interaction(method, pooling,package),
                 shape=model, color=model)) +
  facet_grid(bias ~cp)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  coord_flip()


params_q <- filter(res_between, parameter %in% c("q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn"))

params_q <- add_column(params_q,bias=1)
params_q$bias <- ifelse(params_q$parameter=="q_1_bn",
                        'bn',
                        ifelse(params_q$parameter=="q_2_bn",
                               'bn',
                               ifelse(params_q$parameter=="q_3_bn",
                                      'bn',
                                      ifelse(params_q$parameter=="q_6_bn",
                                             'bn',
                                             ifelse(params_q$parameter=="q_7_bn",
                                                    'bn',
                                                    ifelse(params_q$parameter=="q_8_bn",
                                                           'bn',
                                                           'bo'))))))

params_q <- add_column(params_q,pq=1)
params_q$pq <- ifelse(params_q$parameter=="q_1_bn",
                      'q_1',
                      ifelse(params_q$parameter=="q_2_bn",
                             'q_2',
                             ifelse(params_q$parameter=="q_3_bn",
                                    'q_3',
                                    ifelse(params_q$parameter=="q_6_bn",
                                           'q_6',
                                           ifelse(params_q$parameter=="q_7_bn",
                                                  'q_7',
                                                  ifelse(params_q$parameter=="q_8_bn",
                                                         'q_8',
                                                         ifelse(params_q$parameter=="q_1_bo",
                                                                'q_1',
                                                                ifelse(params_q$parameter=="q_2_bo",
                                                                       'q_2',
                                                                       ifelse(params_q$parameter=="q_3_bo",
                                                                              'q_3',
                                                                              ifelse(params_q$parameter=="q_6_bo",
                                                                                     'q_6',
                                                                                     ifelse(params_q$parameter=="q_7_bo",
                                                                                            'q_7',
                                                                                            ifelse(params_q$parameter=="q_8_bo",
                                                                                                   'q_8',
                                                                                                   params_q$parameter))))))))))))

gg_est2 <- ggplot(params_q, aes(y = est_diff, x = interaction(method, pooling,package),
                                shape=model, color=model)) +
  facet_grid(bias ~pq) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1))+ 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 3) +
  coord_flip()
plot(gg_est2)


params_r <- filter(res_between, parameter %in% c("r_1_bo","r_2_bo","r_3_bo",'r_6_bo',"r_7_bo","r_8_bo","r_1_bn","r_2_bn","r_3_bn",'r_6_bn',"r_7_bn","r_8_bn"))

params_r <- add_column(params_r,bias=1)
params_r$bias <- ifelse(params_r$parameter=="r_1_bn",
                        'bn',
                        ifelse(params_r$parameter=="r_2_bn",
                               'bn',
                               ifelse(params_r$parameter=="r_3_bn",
                                      'bn',
                                      ifelse(params_r$parameter=="r_6_bn",
                                             'bn',
                                      ifelse(params_r$parameter=="r_7_bn",
                                             'bn',
                                             ifelse(params_r$parameter=="r_8_bn",
                                                    'bn',
                                                    'bo'))))))

params_r <- add_column(params_r,pr=1)
params_r$pr <- ifelse(params_r$parameter=="r_1_bn",
                      'r_1',
                      ifelse(params_r$parameter=="r_2_bn",
                             'r_2',
                             ifelse(params_r$parameter=="r_3_bn",
                                    'r_3',
                                    ifelse(params_r$parameter=="r_6_bn",
                                           'r_6',
                                           ifelse(params_r$parameter=="r_7_bn",
                                                  'r_7',
                                                  ifelse(params_r$parameter=="r_8_bn",
                                                         'r_8',
                                                         ifelse(params_r$parameter=="r_1_bo",
                                                                'r_1',
                                                                ifelse(params_r$parameter=="r_2_bo",
                                                                       'r_2',
                                                                       ifelse(params_r$parameter=="r_3_bo",
                                                                              'r_3',
                                                                              ifelse(params_r$parameter=="r_6_bo",
                                                                                     'r_6',
                                                                                     ifelse(params_r$parameter=="r_7_bo",
                                                                                            'r_7',
                                                                                            ifelse(params_r$parameter=="r_8_bo",
                                                                                                   'r_8',
                                                                                                   params_r$parameter))))))))))))

gg_est3 <- ggplot(params_r, aes(y = est_diff, x = interaction(method, pooling,package),
                                shape=model, color=model)) +
  facet_grid(bias ~pr) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1))+ 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 3) +
  coord_flip()
plot(gg_est3)
