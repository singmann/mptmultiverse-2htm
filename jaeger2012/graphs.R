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

load("jaeger_orig.eqn-Jaeger_2012.csv.RData")
results1 <- results

load("jaeger_qrest.eqn-Jaeger_2012.csv.RData")
results2 <- results



pars <- bind_rows(unnest(results1, est_group))

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
                                                  'b')))))

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

ggplot(pars,aes(y = est, x = parameter, 
                col=interaction(method, pooling,package),
                shape=interaction(method, pooling,package))) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  labs(x='Parameter',y='Estimate', color='Analysis approach',shape='Analysis approach', title='Parameter Estimation for Original Model')+
  scale_shape_manual(values=shapes) +
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))


pars <- bind_rows(unnest(results2, est_group))

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
                                                  'b')))))


ggplot(pars,aes(y = est, x = parameter, 
                col=interaction(method, pooling,package),
                shape=interaction(method, pooling,package))) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  labs(x='Parameter',y='Estimate', color='Analysis approach',shape='Analysis approach', title='Parameter Estimation for Q Restricted')+
  scale_shape_manual(values=shapes) +
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))


pars <- bind_rows(
  unnest(results1, est_group),
  unnest(results2, est_group)
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
                                                  'b')))))

pars$model <- ifelse(pars$model=='jaeger_orig.eqn',
                     'Original Model',
                     ifelse(pars$model=='jaeger_qrest.eqn',
                            'Q Restricted',
                            'R Restricted'))

core <- filter(pars, parameter %in% c("Do", "Dn", "g"))
ggplot(core, aes(y = est, x = model,
                 col=interaction(method, pooling,package),
                 shape=interaction(method, pooling,package))) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  ylim(0,1) + 
  labs(x='Model',y='Estimate', color='Analysis approach',shape='Analysis approach', title='Estimates of Core Parameters Across Models')+
  scale_shape_manual(values=shapes) +
  theme_bw() +
  facet_wrap(~parameter) +
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()

params_q <- filter(pars, parameter %in% c("q_1", "q_2", "q_5", "q_6"))
ggplot(params_q, aes(y = est, x = model,
                     col=interaction(method, pooling,package),
                     shape=interaction(method, pooling,package))) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  facet_wrap(~parameter) +
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1))+ 
  labs(x='Model',y='Estimate', color='Analysis approach',shape='Analysis approach', title='Estimates of Q Parameters Across Models')+
  theme_bw() + 
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()

params_r <- filter(pars, parameter %in% c("r_1", "r_2", "r_5", "r_6"))
ggplot(params_r, aes(y = est, x = model,
                     col=interaction(method, pooling,package),
                     shape=interaction(method, pooling,package))) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  facet_wrap(~parameter, ncol=4) +
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1))+ 
  labs(x='Model',y='Estimate', color='Analysis approach',shape='Analysis approach', title='Estimates of R Parameters Across Models')+
  theme_bw() +
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))+
  coord_flip()

gof <- bind_rows(unnest(results1, gof))

gof$pooling <- ifelse(gof$pooling=="no",
                      'No',
                      ifelse(gof$pooling=="complete",
                             'Comp',
                             'PP'))
gof$package <- ifelse(gof$package=="MPTinR",
                      'MR',
                      'TB')

gof$method <- ifelse(gof$method=="PB/MLE",
                     'PB',
                     ifelse(gof$method=="asymptotic",
                            'as',
                            ifelse(gof$method=="simple",
                                   'sim',
                                   ifelse(gof$method=="trait",
                                          't',
                                          ifelse(gof$method=="trait_uncorrelated",
                                                 't_u',
                                                 'b')))))
gof$focus <- ifelse(gof$focus=="mean",
                    'Mean',
                    ifelse(gof$focus=="cov",
                           'Covariance',
                           gof$focus))


ggplot(gof, aes(y = p, 
                x = interaction(method, pooling, package))) + 
  geom_point(size=2) + ylim(0, 1) + 
  geom_hline(yintercept = .05, lty = 2)+
  theme_bw() + coord_flip() +
  facet_wrap(~focus) +
  labs(x='Analysis approach',y='p value', title='Goodness of fit: Original model')+
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))


gof <- bind_rows(unnest(results2, gof))

gof$pooling <- ifelse(gof$pooling=="no",
                      'No',
                      ifelse(gof$pooling=="complete",
                             'Comp',
                             'PP'))
gof$package <- ifelse(gof$package=="MPTinR",
                      'MR',
                      'TB')

gof$method <- ifelse(gof$method=="PB/MLE",
                     'PB',
                     ifelse(gof$method=="asymptotic",
                            'as',
                            ifelse(gof$method=="simple",
                                   'sim',
                                   ifelse(gof$method=="trait",
                                          't',
                                          ifelse(gof$method=="trait_uncorrelated",
                                                 't_u',
                                                'b')))))
gof$focus <- ifelse(gof$focus=="mean",
                    'Mean',
                    ifelse(gof$focus=="cov",
                           'Covariance',
                           gof$focus))


ggplot(gof, aes(y = p, 
                x = interaction(method, pooling, package))) + 
  geom_point(size=2) + ylim(0, 1) + 
  geom_hline(yintercept = .05, lty = 2)+
  theme_bw() + coord_flip() +
  facet_wrap(~focus) +
  labs(x='Analysis approach',y='p value', title='Goodness of fit: Q restricted')+
  theme(plot.title=element_text(face = 'bold',size=12, hjust = 0.5))
