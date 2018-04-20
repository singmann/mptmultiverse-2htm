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
##########################      GOF       ################################################  

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
########################### split per condition #####################################

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
dd <- position_dodge(w = .9)

core <- filter(pars, parameter %in% c("Do", "Dn", "g") & condition %in% c('Strong wording'))
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

core <- filter(pars, parameter %in% c("Do", "Dn", "g") & condition %in% c('Weak wording'))
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
#############################   per condition   ##################################

core <- filter(pars, parameter %in% c("Do", "Dn", "g") & condition %in% c('Strong wording'))
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

core <- filter(pars, parameter %in% c("Do", "Dn", "g") & condition %in% c('Weak wording'))
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
###############################  per condition  ################################

params_q <- filter(pars, parameter %in% c("q_1", "q_2", "q_3", "q_6", "q_7", "q_8") & condition %in% c('Strong wording'))
ggplot(params_q, aes(y = est, x = inter,
                     shape=model, color=model)) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Analysis approach', y='Estimate', color='Model',shape='Model', title='Estimates of Q Parameters Across Models')+
  theme_bw() + 
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()

params_q <- filter(pars, parameter %in% c("q_1", "q_2", "q_3", "q_6", "q_7", "q_8") & condition %in% c('Weak wording'))
ggplot(params_q, aes(y = est, x = inter,
                     shape=model, color=model)) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
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
###############################  per condition  ################################

params_r <- filter(pars, parameter %in% c("r_1", "r_2", "r_3", "r_6", "r_7", "r_8") & condition %in% c('Strong wording'))
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


params_r <- filter(pars, parameter %in% c("r_1", "r_2", "r_3", "r_6", "r_7", "r_8") & condition %in% c('Weak wording'))
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
