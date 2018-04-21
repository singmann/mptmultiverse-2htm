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
source("../../scripts/auxiliary_functions.R")
source("../../scripts/summary_plots.R")
load("../../broeder_exp1/model_orig.eqn-exp1.txt.RData")
results1 <- results

load("../../broeder_exp1/model_orig_qrest.eqn-exp1.txt.RData")
results2 <- results

load("../../broeder_exp1/model_orig_rrest.eqn-exp1.txt.RData")
results3 <- results

##########################    1st slide    ###############################################
##########################      params       ################################################  

gof <- bind_rows(unnest(results1, gof),
                 unnest(results2, gof),
                 unnest(results3, gof)
)

gof$pooling <- factor(gof$pooling, levels = c("no", "complete",  "partial"), 
                      labels = c("No", "Comp", "PP"))

gof$package <- factor(gof$package, levels = c("MPTinR", "TreeBUGS"), 
                      labels = c("MR", "TB"))
gof$method <- factor(gof$method, levels = c("PB/MLE", "asymptotic", "simple",
                                            "trait", "trait_uncorrelated"), 
                     labels = c("PB", "asy", "ss", "trait", "trait_u"))

gof$inter <- with(gof, interaction(method, pooling, package, drop = TRUE, sep = " "))
levels(gof$inter) <- c("No.PB", "No.asy", "Comp.asy", "No.Bayes", 
                       "Comp.Bayes", "Trait.PP", "Trait_u.PP")

gof$focus <- factor(gof$focus, levels = c('cov', 'mean'),
                    labels = c('Covariance', 'Mean'))

gof$model <- factor(gof$model, levels = c('model_orig.eqn', 'model_orig_qrest.eqn', 'model_orig_rrest.eqn'),
                    labels = c('Q & R Restr.', 'Q Restricted', 'R Restricted'))

ggplot(gof, aes(y = p, 
                x = inter, col=model)) + 
  geom_point(size=5) +  
  geom_hline(yintercept = .05, lty = 2)+
  theme_bw() + coord_flip() +
  facet_wrap(~focus) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0", "0.5", "1")) +
  labs(x='Analysis approach',y= expression(italic(p)), 
       color='Model', title='Goodness of fit')+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))

############################      2nd slide   #######################################
###########################   Core parameters  ######################################

pars <- bind_rows(
  unnest(results1, est_group),
  unnest(results2, est_group),
  unnest(results3, est_group)
)

pars$condition <- factor(pars$condition, levels = c('strong_naming', 'weak_naming'),
                         labels = c('Strong wording', 'Weak wording'))

pars$pooling <- factor(pars$pooling, levels = c('no','complete','partial'),
                       labels = c('No', 'Comp', 'PP'))

pars$package <- factor(pars$package, levels = c("MPTinR", "TreeBUGS"), 
                      labels = c("MR", "TB"))

pars$method <- factor(pars$method, levels = c("PB/MLE", "asymptotic", "simple",
                                            "trait", "trait_uncorrelated"), 
                     labels = c("PB", "asy", "ss", "trait", "trait_u"))

pars$inter <- with(pars, interaction(method, pooling, package, drop = TRUE, sep = " "))
levels(pars$inter) <- c("No.PB", "No.asy", "Comp.asy", "No.Bayes", 
                       "Comp.Bayes", "Trait.PP", "Trait_u.PP")

pars$model <- factor(pars$model, levels = c('model_orig.eqn', 'model_orig_qrest.eqn', 'model_orig_rrest.eqn'),
                    labels = c('Q & R Restr.', 'Q Restricted', 'R Restricted'))


shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .8)

core <- filter(pars, parameter %in% c("Do", "Dn", "g"))
ggplot(core, aes(y = est, x = inter,
                 shape=model, color=model)) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0", "0.5", "1")) + 
  labs(x='Analysis approach',y='Estimate', color='Model',shape='Model', title='Estimates of Core Parameters Across Models')+
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

################################    3rd slide   ##################################
############################   Core parameters   #################################
############################    Reverse graph    #################################


ggplot(core, aes(y = est, x = model,
                 col=inter,
                 shape=inter)) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0", "0.5", "1")) +  
  labs(x='Model',y='Estimate', color='Analysis approach',shape='Analysis approach', title='Estimates of Core Parameters Across Models')+
  scale_shape_manual(values=shapes) +
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

#################################   4th slide   ####################################
############################### differences core ###################################

res_between <- bind_rows(
  unnest(results1, test_between),
  unnest(results2, test_between),
  unnest(results3, test_between)
)

res_between$pooling <- factor(res_between$pooling, levels = c('no','complete','partial'),
                       labels = c('No', 'Comp', 'PP'))

res_between$package <- factor(res_between$package, levels = c("MPTinR", "TreeBUGS"), 
                       labels = c("MR", "TB"))

res_between$method <- factor(res_between$method, levels = c("PB/MLE", "asymptotic", "simple",
                                              "trait", "trait_uncorrelated"), 
                      labels = c("PB", "asy", "ss", "trait", "trait_u"))

res_between$inter <- with(res_between, interaction(method, pooling, package, drop = TRUE, sep = " "))
levels(res_between$inter) <- c("No.PB", "No.asy", "Comp.asy", "No.Bayes", 
                        "Comp.Bayes", "Trait.PP", "Trait_u.PP")

res_between$model <- factor(res_between$model, levels = c('model_orig.eqn', 'model_orig_qrest.eqn', 'model_orig_rrest.eqn'),
                     labels = c('Q & R Restr.', 'Q Restricted', 'R Restricted'))

res_between$condition1 <- 'Strong wording'
res_between$condition2 <- 'Weak wording'


shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .8)

core <- filter(res_between, parameter %in% c("Do", "Dn", "g"))
ggplot(core, aes(y = est_diff, x = model,
                  col=inter,
                  shape=inter)) +
  facet_wrap(~parameter, ncol=3)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.6)+
  geom_point(position = dd, size=3.5) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1), 
                     labels = c("-1", "0", "1")) +  
  labs(x='Model',y='Estimated Difference', color='Analysis approach',shape='Analysis approach', title='Parameter Differences Between Conditions Across Models: Core Parameters')+
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

#################################### 5th slide #################################
################################# Q parameters #################################

params_q <- filter(pars, parameter %in% c("q_1", "q_2", "q_3", "q_6", "q_7", "q_8"))
ggplot(params_q, aes(y = est, x = inter,
                     shape=model, color=model)) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Analysis approach', y='Estimate', color='Model',shape='Model', title='Estimates of Q Parameters Across Models')+
  theme_bw() + 
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()

#################################   6th slide   ####################################
############################### differences Q #####################################

params_q <- filter(res_between, parameter %in% c("q_1", "q_2", "q_3", "q_6", "q_7", "q_8"))
ggplot(params_q, aes(y = est_diff, x = model,
                     col=inter,
                     shape=inter)) +
  facet_wrap(~parameter, ncol=6)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.6)+
  geom_point(position = dd, size=3.5) +  
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1))+ 
  labs(x='Model',y='Estimated Difference', color='Analysis approach',shape='Analysis approach', title='Parameter Differences Between Conditions Across Models: Q Parameters')+
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 3) +
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

#################################### 7th slide #################################
################################# R parameters #################################

params_r <- filter(pars, parameter %in% c("r_1", "r_2", "r_3", "r_6", "r_7", "r_8"))
ggplot(params_r, aes(y = est, x = inter,
                     shape=model, color=model)) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Analysis approach', y='Estimate', color='Model',shape='Model', title='Estimates of R Parameters Across Models')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

#################################   8th slide   ####################################
############################### differences R #####################################

params_r <- filter(res_between, parameter %in% c("r_1", "r_2", "r_3", "r_6", "r_7", "r_8"))
ggplot(params_r, aes(y = est_diff, x = model,
                     col=inter,
                     shape=inter)) +
  facet_wrap(~parameter, ncol=6)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.6)+
  geom_point(position = dd, size=3.5) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1))+ 
  labs(x='Model',y='Estimated Difference', color='Analysis approach',shape='Analysis approach', title='Parameter Differences Between Conditions Across Models: R Parameters')+
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 3) +
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()


######################   GOF   ###################################
###################  all data sets  ##############################

load("../../dube2012p/dubep_orig.eqn-Dube_2012-P.csv.RData")
results1_2 <- results

load("../../dube2012p/dubep_qrest.eqn-Dube_2012-P.csv.RData")
results2_2 <- results

load("../../dube2012p/dubep_rrest.eqn-Dube_2012-P.csv.RData")
results3_2 <- results


load("../../dube2012w/dubew_orig.eqn-Dube_2012-W.csv.RData")
results1_3 <- results

load("../../dube2012w/dubeW_qrest.eqn-Dube_2012-W.csv.RData")
results2_3 <- results

load("../../dube2012w/dubeW_rrest.eqn-Dube_2012-W.csv.RData")
results3_3 <- results


load("../../heathcote2006e1/heathcote_orig.eqn-Heathcote_2006_e1.csv.RData")
results1_4 <- results

load("../../heathcote2006e1/heathcote_qrest.eqn-Heathcote_2006_e1.csv.RData")
results2_4 <- results

load("../../heathcote2006e1/heathcote_rrest.eqn-Heathcote_2006_e1.csv.RData")
results3_4 <- results


load("../../heathcote2006e2/heathcote2_orig.eqn-Heathcote_2006_e2.csv.RData")
results1_5 <- results

load("../../heathcote2006e2/heathcote2_qrest.eqn-Heathcote_2006_e2.csv.RData")
results2_5 <- results

load("../../heathcote2006e2/heathcote2_rrest.eqn-Heathcote_2006_e2.csv.RData")
results3_5 <- results


load("../../jaeger2012/jaeger_orig.eqn-Jaeger_2012.csv.RData")
results1_6 <- results

load("../../jaeger2012/jaeger_qrest.eqn-Jaeger_2012.csv.RData")
results2_6 <- results

load("../../jaeger2012/jaeger_rrest.eqn-Jaeger_2012.csv.RData")
results3_6 <- results



load("../../koen2013/koen_orig.eqn-Koen_2013_immediate.csv.RData")
results1_7 <- results

load("../../koen2013/koen_qrest.eqn-Koen_2013_immediate.csv.RData")
results2_7 <- results

load("../../koen2013/koen_rrest.eqn-Koen_2013_immediate.csv.RData")
results3_7 <- results


gof_a <- bind_rows(
  unnest(results1, gof),
  unnest(results2, gof),
  unnest(results3, gof),
  unnest(results1_2, gof),
  unnest(results2_2, gof),
  unnest(results3_2, gof),
  unnest(results1_3, gof),
  unnest(results2_3, gof),
  unnest(results3_3, gof),
  unnest(results1_4, gof),
  unnest(results2_4, gof),
  unnest(results3_4, gof),
  unnest(results1_5, gof),
  unnest(results2_5, gof),
  unnest(results3_5, gof),
  unnest(results1_6, gof),
  unnest(results2_6, gof),
  unnest(results3_6, gof),
  unnest(results1_7, gof),
  unnest(results2_7, gof),
  unnest(results3_7, gof),
)

gof_a$pooling <- factor(gof_a$pooling, levels = c("no", "complete",  "partial"), 
                      labels = c("No", "Comp", "PP"))

gof_a$package <- factor(gof_a$package, levels = c("MPTinR", "TreeBUGS"), 
                      labels = c("MR", "TB"))
gof_a$method <- factor(gof_a$method, levels = c("PB/MLE", "asymptotic", "simple",
                                            "trait", "trait_uncorrelated","beta"), 
                     labels = c("PB", "asy", "ss", "trait", "trait_u","beta"))

gof_a$inter <- with(gof_a, interaction(method, pooling, package, drop = TRUE, sep = " "))
levels(gof_a$inter) <- c("No.PB", "No.asy", "Comp.asy", "No.Bayes", 
                       "Comp.Bayes", "Trait.PP", "Trait_u.PP","Beta.PP")

gof_a$focus <- factor(gof_a$focus, levels = c('cov', 'mean'),
                    labels = c('Covariance', 'Mean'))

gof_a$model <- ifelse(gof_a$model=="model_orig.eqn",
                             'Q & R Restr.',
                             ifelse(gof_a$model=="dubep_orig.eqn",
                                    'Q & R Restr.',
                                    ifelse(gof_a$model=="jaeger_orig.eqn",
                                           'Q & R Restr.',
                                           ifelse(gof_a$model=="koen_orig.eqn",
                                                  'Q & R Restr.',
                                                  ifelse(gof_a$model=="heathcote_orig.eqn",
                                                         'Q & R Restr.',
                                                         ifelse(gof_a$model=="dubew_orig.eqn",
                                                                'Q & R Restr.',
                                                                ifelse(gof_a$model=="heathcote2_orig.eqn",
                                                                       'Q & R Restr.',
                                                                       ifelse(gof_a$model=="koen_orig.eqn",
                                                                              'Q & R Restr.',
                                                                              ifelse(gof_a$model=="model_orig_qrest.eqn",
                                                                                     'Q Restricted',
                                                                                     ifelse(gof_a$model=="dubep_qrest.eqn",
                                                                                            'Q Restricted',
                                                                                            ifelse(gof_a$model=="jaeger_qrest.eqn",
                                                                                                   'Q Restricted',
                                                                                                   ifelse(gof_a$model=="koen_qrest.eqn",
                                                                                                          'Q Restricted',
                                                                                                          ifelse(gof_a$model=="heathcote_qrest.eqn",
                                                                                                                 'Q Restricted',
                                                                                                                 ifelse(gof_a$model=="dubew_qrest.eqn",
                                                                                                                        'Q Restricted',
                                                                                                                        ifelse(gof_a$model=="heathcote2_qrest.eqn",
                                                                                                                               'Q Restricted',
                                                                                                                               ifelse(gof_a$model=="koen_qrest.eqn",
                                                                                                                                      'Q Restricted',
                                                                                                                                      ifelse(gof_a$model=="model_orig_rrest.eqn",
                                                                                                                                             'R Restricted',
                                                                                                                                             ifelse(gof_a$model=="dubep_rrest.eqn",
                                                                                                                                                    'R Restricted',
                                                                                                                                                    ifelse(gof_a$model=="jaeger_rrest.eqn",
                                                                                                                                                           'R Restricted',
                                                                                                                                                           ifelse(gof_a$model=="koen_rrest.eqn",
                                                                                                                                                                  'R Restricted',
                                                                                                                                                                  ifelse(gof_a$model=="heathcote_rrest.eqn",
                                                                                                                                                                         'R Restricted',
                                                                                                                                                                         ifelse(gof_a$model=="dubew_rrest.eqn",
                                                                                                                                                                                'R Restricted',
                                                                                                                                                                                ifelse(gof_a$model=="heathcote2_rrest.eqn",
                                                                                                                                                                                       'R Restricted',
                                                                                                                                                                                       ifelse(gof_a$model=="koen_rrest.eqn",
                                                                                                                                                                                              'R Restricted',
                                                         gof_a$model))))))))))))))))))))))))


gof_all <- filter(gof_a, focus %in% c("Mean"))
ggplot(gof_all, aes(y = p, 
                x = inter, col=dataset)) + 
  geom_point(size=5) +  
  geom_hline(yintercept = .05, lty = 2)+
  theme_bw() + coord_flip() +
  facet_wrap(~model,ncol = 3) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0", "0.5", "1")) +
  labs(x='Analysis approach',y= expression(italic(p)), 
       color='Model', title='Goodness of fit')+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))

################# only r restricted ##############################
######################   Core   ###################################
###################  all data sets  ##############################

params <- bind_rows(
  unnest(results1, est_group),
  unnest(results2, est_group),
  unnest(results3, est_group),
  unnest(results1_2, est_group),
  unnest(results2_2, est_group),
  unnest(results3_2, est_group),
  unnest(results1_3, est_group),
  unnest(results2_3, est_group),
  unnest(results3_3, est_group),
  unnest(results1_4, est_group),
  unnest(results2_4, est_group),
  unnest(results3_4, est_group),
  unnest(results1_5, est_group),
  unnest(results2_5, est_group),
  unnest(results3_5, est_group),
  unnest(results1_6, est_group),
  unnest(results2_6, est_group),
  unnest(results3_6, est_group),
  unnest(results1_7, est_group),
  unnest(results2_7, est_group),
  unnest(results3_7, est_group),
)

params$pooling <- factor(params$pooling, levels = c("no", "complete",  "partial"), 
                      labels = c("No", "Comp", "PP"))

params$package <- factor(params$package, levels = c("MPTinR", "TreeBUGS"), 
                      labels = c("MR", "TB"))
params$method <- factor(params$method, levels = c("PB/MLE", "asymptotic", "simple",
                                                "trait", "trait_uncorrelated","beta"), 
                       labels = c("PB", "asy", "ss", "trait", "trait_u","beta"))

params$inter <- with(params, interaction(method, pooling, package, drop = TRUE, sep = " "))
levels(params$inter) <- c("No.PB", "No.asy", "Comp.asy", "No.Bayes", 
                         "Comp.Bayes", "Trait.PP", "Trait_u.PP","Beta.PP")

params$model <- ifelse(params$model=="model_orig.eqn",
                      'Q & R Restr.',
                      ifelse(params$model=="dubep_orig.eqn",
                             'Q & R Restr.',
                             ifelse(params$model=="jaeger_orig.eqn",
                                    'Q & R Restr.',
                                    ifelse(params$model=="koen_orig.eqn",
                                           'Q & R Restr.',
                                           ifelse(params$model=="heathcote_orig.eqn",
                                                  'Q & R Restr.',
                                                  ifelse(params$model=="dubew_orig.eqn",
                                                         'Q & R Restr.',
                                                         ifelse(params$model=="heathcote2_orig.eqn",
                                                                'Q & R Restr.',
                                                                ifelse(params$model=="koen_orig.eqn",
                                                                       'Q & R Restr.',
                                                                       ifelse(params$model=="model_orig_qrest.eqn",
                                                                              'Q Restricted',
                                                                              ifelse(params$model=="dubep_qrest.eqn",
                                                                                     'Q Restricted',
                                                                                     ifelse(params$model=="jaeger_qrest.eqn",
                                                                                            'Q Restricted',
                                                                                            ifelse(params$model=="koen_qrest.eqn",
                                                                                                   'Q Restricted',
                                                                                                   ifelse(params$model=="heathcote_qrest.eqn",
                                                                                                          'Q Restricted',
                                                                                                          ifelse(params$model=="dubew_qrest.eqn",
                                                                                                                 'Q Restricted',
                                                                                                                 ifelse(params$model=="heathcote2_qrest.eqn",
                                                                                                                        'Q Restricted',
                                                                                                                        ifelse(params$model=="koen_qrest.eqn",
                                                                                                                               'Q Restricted',
                                                                                                                               ifelse(params$model=="model_orig_rrest.eqn",
                                                                                                                                      'R Restricted',
                                                                                                                                      ifelse(params$model=="dubep_rrest.eqn",
                                                                                                                                             'R Restricted',
                                                                                                                                             ifelse(params$model=="jaeger_rrest.eqn",
                                                                                                                                                    'R Restricted',
                                                                                                                                                    ifelse(params$model=="koen_rrest.eqn",
                                                                                                                                                           'R Restricted',
                                                                                                                                                           ifelse(params$model=="heathcote_rrest.eqn",
                                                                                                                                                                  'R Restricted',
                                                                                                                                                                  ifelse(params$model=="dubew_rrest.eqn",
                                                                                                                                                                         'R Restricted',
                                                                                                                                                                         ifelse(params$model=="heathcote2_rrest.eqn",
                                                                                                                                                                                'R Restricted',
                                                                                                                                                                                ifelse(params$model=="koen_rrest.eqn",
                                                                                                                                                                                       'R Restricted',
                                                                                                                                    params$model))))))))))))))))))))))))

params$dataset <- factor(params$dataset, levels = c("exp1.txt", "Dube_2012-P.csv", "Dube_2012-W.csv", "Heathcote_2006_e1.csv", "Heathcote_2006_e2.csv", "Jaeger_2012.csv", "Koen_2013_immediate.csv"), 
                         labels = c("Broeder et al. (2013)", "Dube & Rotello (2012, Exp. 1, Pictures)", "Dube & Rotello (2012, Exp. 1, Words)","Heathcote et al. (2006, Exp. 1)","Heathcote et al. (2006, Exp. 2)","Jaeger et al. (2012, Exp. 1, no cue)","Koen et al. (2013, Exp. 4, immediate test)"))


params$condition <- factor(params$condition, levels = c("strong_naming", "weak_naming", "Dube_2012-P", "Dube_2012-W", "Heathcote_2006_e1", "Heathcote_2006_e2", "Jaeger_2012", "Koen-2013_immediate"), 
                         labels = c("strong_naming", "weak_naming", "Dube_2012-P", "Dube_2012-W", "Heathcote_2006_e1", "Heathcote_2006_e2", "Jaeger_2012", "Koen-2013_immediate"))


#params[params$dataset == "Broeder et al. (2013)" & params$condition == "weak_naming", "dataset"] <- "Broeder et al. (2013, Exp. 1, Weak wording)"

# params$dataset <- as.character(params$dataset)
# params$condition <- as.character(params$condition)
# params$data <- ifelse(params$dataset == "Broeder et al. (2013)" & params$condition == "weak_naming",
#                          params$dataset=="Broeder et al. (2013, Exp. 1, Weak wording)",
#                          params$dataset)
# 

Core_all <- filter(params, parameter %in% c("Dn","Do","g") & model %in% c('R Restricted'))
ggplot(Core_all, aes(y = est, x = inter,
                      color=dataset)) +
  facet_wrap( ~parameter, ncol=3) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Analysis approach', y='Estimate', color='Dataset', title='Estimates of Core Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()


Q_all <- filter(params, parameter %in% c("q_1", "q_2", "q_3", "q_5", "q_6", "q_7", "q_8") & model %in% c('R Restricted'))
ggplot(Q_all, aes(y = est, x = inter,
                     color=dataset)) +
  facet_wrap( ~parameter, ncol=7) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Analysis approach', y='Estimate', color='Dataset', title='Estimates of Q Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

R_all <- filter(params, parameter %in% c("r_1", "r_2", "r_3", "r_5", "r_6", "r_7", "r_8") & model %in% c('R Restricted'))
ggplot(R_all, aes(y = est, x = inter,
                  color=dataset)) +
  facet_wrap( ~parameter, ncol=7) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Analysis approach', y='Estimate', color='Dataset', title='Estimates of R Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()
