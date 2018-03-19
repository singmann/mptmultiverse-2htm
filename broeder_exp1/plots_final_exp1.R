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

load("model_orig.eqn-exp1.txt.RData")
results1 <- results

load("model_orig_qrest.eqn-exp1.txt.RData")
results2 <- results

load("model_orig_rrest.eqn-exp1.txt.RData")
results3 <- results

######par est
###orig

pars <- bind_rows(unnest(results1, est_group))
pars$condition <- ifelse(pars$condition=="strong_naming",
                                    'Strong wording',
                         'Weak wording'
)
pars$pooling <- ifelse(pars$pooling=="no",
                         'No',
                       ifelse(pars$pooling=="complete",
                              'Comp',
                              'PP'))
pars$package <- ifelse(pars$package=="MPTinR",
                       'MR',
                              'TB')

pars$method <- ifelse(pars$method=="PB/MLE",
                       'PB',
                       ifelse(pars$method=="asymptotic",
                              'as',
                              ifelse(pars$method=="simple",
                                     'sim',
                                     ifelse(pars$method=="trait",
                                            't',
                                            ifelse(pars$method=="trait_uncorrelated",
                                                   't_u',
                                                   pars$method)))))

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

ggplot(pars,aes(y = est, x = parameter, 
                col=interaction(method, pooling,package),
                shape=interaction(method, pooling,package))) +
  facet_wrap(~condition, ncol = 1) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  labs(x='Parameter',y='Estimate', color='Analysis\napproach',shape='Analysis\napproach', title='Parameter Estimation for Original Model')+
  scale_shape_manual(values=shapes) +
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))

####q rest
pars <- bind_rows(unnest(results2, est_group))
pars$condition <- ifelse(pars$condition=="strong_naming",
                         'Strong wording',
                         'Weak wording'
)
pars$pooling <- ifelse(pars$pooling=="no",
                       'No',
                       ifelse(pars$pooling=="complete",
                              'Comp',
                              'PP'))
pars$package <- ifelse(pars$package=="MPTinR",
                       'MR',
                       'TB')

pars$method <- ifelse(pars$method=="PB/MLE",
                      'PB',
                      ifelse(pars$method=="asymptotic",
                             'as',
                             ifelse(pars$method=="simple",
                                    'sim',
                                    ifelse(pars$method=="trait",
                                           't',
                                           ifelse(pars$method=="trait_uncorrelated",
                                                  't_u',
                                                  pars$method)))))


ggplot(pars,aes(y = est, x = parameter, 
                col=interaction(method, pooling,package),
                shape=interaction(method, pooling,package))) +
  facet_wrap(~condition, ncol = 1) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  labs(x='Parameter',y='Estimate', color='Analysis\napproach',shape='Analysis\napproach', title='Parameter Estimation for Q Restricted')+
  scale_shape_manual(values=shapes) +
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))

#rrest
pars <- bind_rows(unnest(results3, est_group))
pars$condition <- ifelse(pars$condition=="strong_naming",
                         'Strong wording',
                         'Weak wording'
)
pars$pooling <- ifelse(pars$pooling=="no",
                       'No',
                       ifelse(pars$pooling=="complete",
                              'Comp',
                              'PP'))
pars$package <- ifelse(pars$package=="MPTinR",
                       'MR',
                       'TB')

pars$method <- ifelse(pars$method=="PB/MLE",
                      'PB',
                      ifelse(pars$method=="asymptotic",
                             'as',
                             ifelse(pars$method=="simple",
                                    'sim',
                                    ifelse(pars$method=="trait",
                                           't',
                                           ifelse(pars$method=="trait_uncorrelated",
                                                  't_u',
                                                  pars$method)))))

ggplot(pars,aes(y = est, x = parameter, 
                col=interaction(method, pooling,package),
                shape=interaction(method, pooling,package))) +
  facet_wrap(~condition, ncol = 1) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  labs(x='Parameter',y='Estimate', color='Analysis\napproach',shape='Analysis\napproach', title='Parameter Estimation for R Restricted')+
  scale_shape_manual(values=shapes) +
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))

###core
pars <- bind_rows(
  unnest(results1, est_group),
  unnest(results2, est_group),
  unnest(results3, est_group)
)

pars$condition <- ifelse(pars$condition=="strong_naming",
                         'Strong wording',
                         'Weak wording'
)
pars$pooling <- ifelse(pars$pooling=="no",
                       'No',
                       ifelse(pars$pooling=="complete",
                              'Comp',
                              'PP'))
pars$package <- ifelse(pars$package=="MPTinR",
                       'MR',
                       'TB')

pars$method <- ifelse(pars$method=="PB/MLE",
                      'PB',
                      ifelse(pars$method=="asymptotic",
                             'as',
                             ifelse(pars$method=="simple",
                                    'sim',
                                    ifelse(pars$method=="trait",
                                           't',
                                           ifelse(pars$method=="trait_uncorrelated",
                                                  't_u',
                                                  pars$method)))))
pars$model <- ifelse(pars$model=='model_orig.eqn',
                     'Original Model',
                     ifelse(pars$model=='model_orig_qrest.eqn',
                            'Q Restricted',
                            'R Restricted'))

core <- filter(pars, parameter %in% c("Do", "Dn", "g"))
ggplot(core, aes(y = est, x = model,
                 col=interaction(method, pooling,package),
                 shape=interaction(method, pooling,package))) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  labs(x='Model',y='Estimate', color='Analysis\napproach',shape='Analysis\napproach', title='Estimates of Core Parameters Across Models')+
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()

###q

params_q <- filter(pars, parameter %in% c("q_1", "q_2", "q_3", "q_6", "q_7", "q_8"))
ggplot(params_q, aes(y = est, x = model,
                     col=interaction(method, pooling,package),
                     shape=interaction(method, pooling,package))) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1))+ 
  labs(x='Model',y='Estimate', color='Analysis\napproach',shape='Analysis\napproach', title='Estimates of Q Parameters Across Models')+
  theme_bw() + 
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()

###r

params_r <- filter(pars, parameter %in% c("r_1", "r_2", "r_3", "r_6", "r_7", "r_8"))
ggplot(params_r, aes(y = est, x = model,
                     col=interaction(method, pooling,package),
                     shape=interaction(method, pooling,package))) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1))+ 
  labs(x='Model',y='Estimate', color='Analysis\napproach',shape='Analysis\napproach', title='Estimates of R Parameters Across Models')+
  theme_bw() +
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()



######## Estimated differences

##Model Original

res_between <-  unnest(results1, test_between) 

res_between$condition1 <- 'Strong wording'
res_between$condition2 <- 'Weak wording'
res_between$pooling <- ifelse(res_between$pooling=="no",
                       'No',
                       ifelse(res_between$pooling=="complete",
                              'Comp',
                              'PP'))
res_between$package <- ifelse(res_between$package=="MPTinR",
                       'MR',
                       'TB')

res_between$method <- ifelse(res_between$method=="PB/MLE",
                      'PB',
                      ifelse(res_between$method=="asymptotic",
                             'as',
                             ifelse(res_between$method=="simple",
                                    'sim',
                                    ifelse(res_between$method=="trait",
                                           't',
                                           ifelse(res_between$method=="trait_uncorrelated",
                                                  't_u',
                                                  res_between$method)))))


ggplot(res_between, aes(y = est_diff, x = parameter, 
                                     col=interaction(method, pooling,package),
                                     shape=interaction(method, pooling,package))) +
  facet_grid(condition2~condition1) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  labs(x='Parameter',y='Estimated Difference', color='Analysis\napproach',shape='Analysis\napproach', title='Parameter Differences Between Conditions: Original Model')+
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()


###Q restricted
  
res_between <-  unnest(results2, test_between) 

res_between$condition1 <- 'Strong wording'
res_between$condition2 <- 'Weak wording'
res_between$pooling <- ifelse(res_between$pooling=="no",
                              'No',
                              ifelse(res_between$pooling=="complete",
                                     'Comp',
                                     'PP'))
res_between$package <- ifelse(res_between$package=="MPTinR",
                              'MR',
                              'TB')

res_between$method <- ifelse(res_between$method=="PB/MLE",
                             'PB',
                             ifelse(res_between$method=="asymptotic",
                                    'as',
                                    ifelse(res_between$method=="simple",
                                           'sim',
                                           ifelse(res_between$method=="trait",
                                                  't',
                                                  ifelse(res_between$method=="trait_uncorrelated",
                                                         't_u',
                                                         res_between$method)))))


ggplot(res_between, aes(y = est_diff, x = parameter, 
                        col=interaction(method, pooling,package),
                        shape=interaction(method, pooling,package))) +
  facet_grid(condition2~condition1) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  labs(x='Parameter',y='Estimated Difference', color='Analysis\napproach',shape='Analysis\napproach', title='Parameter Differences Between Conditions: Q Restricted')+
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()

###R restricted
  
res_between <-  unnest(results3, test_between) 

res_between$condition1 <- 'Strong wording'
res_between$condition2 <- 'Weak wording'
res_between$pooling <- ifelse(res_between$pooling=="no",
                              'No',
                              ifelse(res_between$pooling=="complete",
                                     'Comp',
                                     'PP'))
res_between$package <- ifelse(res_between$package=="MPTinR",
                              'MR',
                              'TB')

res_between$method <- ifelse(res_between$method=="PB/MLE",
                             'PB',
                             ifelse(res_between$method=="asymptotic",
                                    'as',
                                    ifelse(res_between$method=="simple",
                                           'sim',
                                           ifelse(res_between$method=="trait",
                                                  't',
                                                  ifelse(res_between$method=="trait_uncorrelated",
                                                         't_u',
                                                         res_between$method)))))


ggplot(res_between, aes(y = est_diff, x = parameter, 
                        col=interaction(method, pooling,package),
                        shape=interaction(method, pooling,package))) +
  facet_grid(condition2~condition1) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  labs(x='Parameter',y='Estimated Difference', color='Analysis\napproach',shape='Analysis\napproach', title='Parameter Differences Between Conditions: R Restricted')+
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()

###Core parameters
  
res_between <- bind_rows(
  unnest(results1, test_between),
  unnest(results2, test_between),
  unnest(results3, test_between)
)


res_between$condition <- ifelse(res_between$condition=="strong_naming",
                         'Strong wording',
                         'Weak wording'
)
res_between$pooling <- ifelse(res_between$pooling=="no",
                       'No',
                       ifelse(res_between$pooling=="complete",
                              'Comp',
                              'PP'))
res_between$package <- ifelse(res_between$package=="MPTinR",
                       'MR',
                       'TB')

res_between$method <- ifelse(res_between$method=="PB/MLE",
                      'PB',
                      ifelse(res_between$method=="asymptotic",
                             'as',
                             ifelse(res_between$method=="simple",
                                    'sim',
                                    ifelse(res_between$method=="trait",
                                           't',
                                           ifelse(res_between$method=="trait_uncorrelated",
                                                  't_u',
                                                  res_between$method)))))
res_between$model <- ifelse(res_between$model=='model_orig.eqn',
                     'Original Model',
                     ifelse(res_between$model=='model_orig_qrest.eqn',
                            'Q Restricted',
                            'R Restricted'))
dd <- position_dodge(w = .75)
core2 <- filter(res_between, parameter %in% c("Do", "Dn", "g"))
ggplot(core2, aes(y = est_diff, x = model,
                  col=interaction(method, pooling,package),
                  shape=interaction(method, pooling,package))) +
  facet_wrap(~parameter, ncol=3)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  labs(x='Model',y='Estimated Difference', color='Analysis\napproach',shape='Analysis\napproach', title='Parameter Differences Between Conditions Across Models: Core Parameters')+
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()

###Q Parameters

params_q <- filter(res_between, parameter %in% c("q_1", "q_2", "q_3", "q_6", "q_7", "q_8"))
ggplot(params_q, aes(y = est_diff, x = model,
                     col=interaction(method, pooling,package),
                     shape=interaction(method, pooling,package))) +
  facet_wrap(~parameter, ncol=6)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) +  
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1))+ 
  labs(x='Model',y='Estimated Difference', color='Analysis\napproach',shape='Analysis\napproach', title='Parameter Differences Between Conditions Across Models: Q Parameters')+
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 3) +
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()

###R Parameters

params_r <- filter(res_between, parameter %in% c("r_1", "r_2", "r_3", "r_6", "r_7", "r_8"))
ggplot(params_r, aes(y = est_diff, x = model,
                     col=interaction(method, pooling,package),
                     shape=interaction(method, pooling,package))) +
  facet_wrap(~parameter, ncol=6)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1))+ 
  labs(x='Model',y='Estimated Difference', color='Analysis\napproach',shape='Analysis\napproach', title='Parameter Differences Between Conditions Across Models: R Parameters')+
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 3) +
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()
