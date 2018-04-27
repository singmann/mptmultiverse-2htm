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

load("../../broeder_exp2/model_orig_n.eqn-exp2.txt.RData")
results1 <- results

load("../../broeder_exp2/model_orig_n_qrest.eqn-exp2.txt.RData")
results2 <- results

load("../../broeder_exp2/model_orig_n_rrest.eqn-exp2.txt.RData")
results3 <- results

######################################## gof #######################################

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

gof$model <- factor(gof$model, levels = c('model_orig_n.eqn', 'model_orig_n_qrest.eqn', 'model_orig_n_rrest.eqn'),
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


###################################### core ###########################################

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

pars$model <- factor(pars$model, levels = c('model_orig_n.eqn', 'model_orig_n_qrest.eqn', 'model_orig_n_rrest.eqn'),
                    labels = c('Q & R Restr.', 'Q Restricted', 'R Restricted'))


core <- filter(pars, parameter %in% c("Do_bn", "Do_bo","Dn_bn", "Dn_bo","g_bn","g_bo"))
core <- add_column(core,bias=1)
core$bias <- ifelse(core$parameter=="Dn_bn"|core$parameter=="Do_bn"|core$parameter=="g_bn",
                                  'biased new',
                                  'biased old')

core <- add_column(core,cp=1)
core$cp <- ifelse(core$parameter=="Dn_bn"|core$parameter=="Dn_bo",
                  'Dn',
                  ifelse(core$parameter=="Do_bn"|core$parameter=='Do_bo',
                         'Do',
                         ifelse(core$parameter=='g_bn'|core$parameter=='g_bo',
                                'g',
                                  core$parameter)))


shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .8)

ggplot(core, aes(y = est, x = inter,
                 shape=model, color =model)) +
  facet_grid(interaction(bias,condition) ~cp) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0", "0.5", "1")) +
  scale_shape_manual(values=shapes) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Estimates of Core Parameters Across Models')+
  theme_bw()+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

#######bn

core_bn <- filter(core, parameter %in% c("Do_bn", "Dn_bn", "g_bn","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn","r_1_bn","r_2_bn","r_3_bn","r_7_bn","r_8_bn"))

ggplot(core_bn, aes(y = est, x = inter,
                    shape=model, color =model)) +
  facet_grid(condition ~cp) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0", "0.5", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Core Parameters Across Models for "biased new" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()


############# bn strong
core_bn <- filter(core, parameter %in% c("Do_bn", "Dn_bn", "g_bn","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn","r_1_bn","r_2_bn","r_3_bn","r_7_bn","r_8_bn") & condition %in% c('Strong wording'))

ggplot(core_bn, aes(y = est, x = inter,
                 shape=model, color =model)) +
  facet_grid(condition ~cp) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0", "0.5", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Core Parameters Across Models for "biased new" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()

### bn weak
core_bn <- filter(core, parameter %in% c("Do_bn", "Dn_bn", "g_bn","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn","r_1_bn","r_2_bn","r_3_bn","r_7_bn","r_8_bn") & condition %in% c('Weak wording'))

ggplot(core_bn, aes(y = est, x = inter,
                    shape=model, color =model)) +
  facet_grid(condition ~cp) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0", "0.5", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Core Parameters Across Models for "biased new" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()


############# bo

core_bo <- filter(core, parameter %in% c("Do_bo", "Dn_bo", "g_bo","q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","r_1_bo","r_2_bo","r_3_bo","r_7_bo","r_8_bo"))

ggplot(core_bo, aes(y = est, x = inter,
                    shape=model, color =model)) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  facet_grid(condition ~cp) +
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0", "0.5", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Core Parameters Across Models for "biased old" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()


### bo strong
core_bo <- filter(core, parameter %in% c("Do_bo", "Dn_bo", "g_bo","q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","r_1_bo","r_2_bo","r_3_bo","r_7_bo","r_8_bo") & condition %in% c('Strong wording')) 

ggplot(core_bo, aes(y = est, x = inter,
                    shape=model, color =model)) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  facet_grid(condition ~cp) +
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0", "0.5", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Core Parameters Across Models for "biased old" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()

### bo weak
core_bo <- filter(core, parameter %in% c("Do_bo", "Dn_bo", "g_bo","q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","r_1_bo","r_2_bo","r_3_bo","r_7_bo","r_8_bo") & condition %in% c('Weak wording')) 

ggplot(core_bo, aes(y = est, x = inter,
                    shape=model, color =model)) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  facet_grid(condition ~cp) +
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0", "0.5", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Core Parameters Across Models for "biased old" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()

############################################# Q ########################################################


params_q <- filter(pars, parameter %in% c("q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn"))

params_q <- add_column(params_q,bias=1)
params_q$bias <- ifelse(params_q$parameter=="q_1_bn"|params_q$parameter=="q_2_bn"|params_q$parameter=="q_3_bn"|params_q$parameter=="q_6_bn"|params_q$parameter=="q_7_bn"|params_q$parameter=="q_8_bn",
                        'biased new',
                        'biased old')

params_q <- add_column(params_q,pq=1)
params_q$pq <- ifelse(params_q$parameter=="q_1_bn"|params_q$parameter=="q_1_bo",
                      'q_1',
                      ifelse(params_q$parameter=="q_2_bn"|params_q$parameter=="q_2_bo",
                             'q_2',
                             ifelse(params_q$parameter=="q_3_bn"|params_q$parameter=="q_3_bo",
                                    'q_3',
                                    ifelse(params_q$parameter=="q_6_bn"|params_q$parameter=="q_6_bo",
                                           'q_6',
                                           ifelse(params_q$parameter=="q_7_bn"|params_q$parameter=="q_7_bo",
                                                  'q_7',
                                                  ifelse(params_q$parameter=="q_8_bn"|params_q$parameter=="q_8_bo",
                                                         'q_8',
                                                            params_q$parameter))))))

ggplot(params_q, aes(y = est, x = inter,
                     shape=model, color =model)) +
  facet_grid(interaction(bias,condition) ~pq) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  scale_shape_manual(values=shapes) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Estimates of Q Parameters Across Models')+
  theme_bw()+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

#######bn

q_bn <- filter(params_q, parameter %in% c("Do_bn", "Dn_bn", "g_bn","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn"))

ggplot(q_bn, aes(y = est, x = inter,
                    shape=model, color =model)) +
  facet_grid(condition ~pq) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Q Parameters Across Models for "biased new" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()


############# bn strong
q_bn <- filter(params_q, parameter %in% c("Do_bn", "Dn_bn", "g_bn","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn") & condition %in% c('Strong wording'))

ggplot(q_bn, aes(y = est, x = inter,
                    shape=model, color =model)) +
  facet_grid(condition ~pq) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) +  
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Q Parameters Across Models for "biased new" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()

### bn weak
q_bn <- filter(params_q, parameter %in% c("Do_bn", "Dn_bn", "g_bn","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn") & condition %in% c('Weak wording'))

ggplot(q_bn, aes(y = est, x = inter,
                    shape=model, color =model)) +
  facet_grid(condition ~pq) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Q Parameters Across Models for "biased new" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()


############# bo

q_bo <- filter(params_q, parameter %in% c("Do_bo", "Dn_bo", "g_bo","q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo") & condition %in% c('Weak wording')) 

ggplot(q_bo, aes(y = est, x = inter,
                    shape=model, color =model)) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  facet_grid(condition ~pq) +
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Q Parameters Across Models for "biased old" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()


### bo strong
q_bo <- filter(params_q, parameter %in% c("Do_bo", "Dn_bo", "g_bo","q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo") & condition %in% c('Strong wording')) 

ggplot(q_bo, aes(y = est, x = inter,
                    shape=model, color =model)) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  facet_grid(condition ~pq) +
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Q Parameters Across Models for "biased old" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()

### bo weak
q_bo <- filter(params_q, parameter %in% c("Do_bo", "Dn_bo", "g_bo","q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo") & condition %in% c('Weak wording')) 

ggplot(q_bo, aes(y = est, x = inter,
                    shape=model, color =model)) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  facet_grid(condition ~pq) +
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Q Parameters Across Models for "biased old" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()

#########################################  R   ##############################################


params_r <- filter(pars, parameter %in% c("r_1_bo","r_2_bo","r_3_bo",'r_6_bo',"r_7_bo","r_8_bo","r_1_bn","r_2_bn","r_3_bn",'r_6_bn',"r_7_bn","r_8_bn"))

params_r <- add_column(params_r,bias=1)
params_r$bias <- ifelse(params_r$parameter=="r_1_bn"|params_r$parameter=="r_2_bn"|params_r$parameter=="r_3_bn"|params_r$parameter=="r_6_bn"|params_r$parameter=="r_7_bn"|params_r$parameter=="r_8_bn",
                        'biased new',
                        'biased old')

params_r <- add_column(params_r,pr=1)
params_r$pr <- ifelse(params_r$parameter=="r_1_bn"|params_r$parameter=="r_1_bo",
                      'r_1',
                      ifelse(params_r$parameter=="r_2_bn"|params_r$parameter=="r_2_bo",
                             'r_2',
                             ifelse(params_r$parameter=="r_3_bn"|params_r$parameter=="r_3_bo",
                                    'r_3',
                                    ifelse(params_r$parameter=="r_6_bn"|params_r$parameter=="r_6_bo",
                                           'r_6',
                                           ifelse(params_r$parameter=="r_7_bn"|params_r$parameter=="r_7_bo",
                                                  'r_7',
                                                  ifelse(params_r$parameter=="r_8_bn"|params_r$parameter=="r_8_bo",
                                                         'r_8',
                                                         params_r$parameter))))))

ggplot(params_r, aes(y = est, x = inter,
                     shape=model, color =model)) +
  facet_grid(interaction(bias,condition) ~pr) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  scale_shape_manual(values=shapes) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Estimates of R Parameters Across Models')+
  theme_bw()+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

#######bn

r_bn <- filter(params_r, parameter %in% c("Do_bn", "Dn_bn", "g_bn","r_1_bn", "r_2_bn","r_3_bn","r_6_bn","r_7_bn","r_8_bn"))

ggplot(r_bn, aes(y = est, x = inter,
                 shape=model, color =model)) +
  facet_grid(condition ~pr) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='R Parameters Across Models for "biased new" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()


############# bn strong
r_bn <- filter(params_r, parameter %in% c("Do_bn", "Dn_bn", "g_bn","r_1_bn", "r_2_bn","r_3_bn","r_6_bn","r_7_bn","r_8_bn") & condition %in% c('Strong wording'))

ggplot(r_bn, aes(y = est, x = inter,
                 shape=model, color =model)) +
  facet_grid(condition ~pr) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='R Parameters Across Models for "biased new" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()

### bn weak
r_bn <- filter(params_r, parameter %in% c("Do_bn", "Dn_bn", "g_bn","r_1_bn", "r_2_bn","r_3_bn","r_6_bn","r_7_bn","r_8_bn") & condition %in% c('Weak wording'))

ggplot(r_bn, aes(y = est, x = inter,
                 shape=model, color =model)) +
  facet_grid(condition ~pr) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='R Parameters Across Models for "biased new" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()


############# bo

r_bo <- filter(params_r, parameter %in% c("Do_bo", "Dn_bo", "g_bo","r_1_bo", "r_2_bo","r_3_bo","r_6_bo","r_7_bo","r_8_bo")) 

ggplot(r_bo, aes(y = est, x = inter,
                 shape=model, color =model)) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  facet_grid(condition ~pr) +
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='R Parameters Across Models for "biased old" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()


### bo strong
r_bo <- filter(params_r, parameter %in% c("Do_bo", "Dn_bo", "g_bo","r_1_bo", "r_2_bo","r_3_bo","r_6_bo","r_7_bo","r_8_bo") & condition %in% c('Strong wording')) 

ggplot(r_bo, aes(y = est, x = inter,
                 shape=model, color =model)) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  facet_grid(condition ~pr) +
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='R Parameters Across Models for "biased old" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()

### bo weak
r_bo <- filter(params_r, parameter %in% c("Do_bo", "Dn_bo", "g_bo","r_1_bo", "r_2_bo","r_3_bo","r_6_bo","r_7_bo","r_8_bo") & condition %in% c('Weak wording')) 

ggplot(r_bo, aes(y = est, x = inter,
                 shape=model, color =model)) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  facet_grid(condition ~pr) +
  geom_point(position = dd, size = 3.5) +  
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=1),limits=c(0,1), 
                     labels = c("0", "1")) + 
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='R Parameters Across Models for "biased old" condition')+
  theme_bw()+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  theme(text=element_text(size = 24))+
  coord_flip()


##################### ESTIMATED DIFFERENCES ###############################

res_between <- bind_rows(
  unnest(results1, test_between),
  unnest(results2, test_between),
  unnest(results3, test_between)
)

res_between$condition1 <- 'Strong wording'
res_between$condition2 <- 'Weak wording'

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

res_between$model <- factor(res_between$model, levels = c('model_orig_n.eqn', 'model_orig_n_qrest.eqn', 'model_orig_n_rrest.eqn'),
                     labels = c('Q & R Restr.', 'Q Restricted', 'R Restricted'))


core <- filter(res_between, parameter %in% c("Do_bn", "Do_bo","Dn_bn", "Dn_bo","g_bn","g_bo"))
core <- add_column(core,bias=1)
core$bias <- ifelse(core$parameter=="Dn_bn"|core$parameter=="Do_bn"|core$parameter=="g_bn",
                    'biased new',
                    'biased old')
                    
core <- add_column(core,cp=1)
core$cp <- ifelse(core$parameter=="Dn_bn"|core$parameter=="Dn_bo",
                                      'Dn',
                                      ifelse(core$parameter=="Do_bn"|core$parameter=='Do_bo',
                                             'Do',
                                             ifelse(core$parameter=='g_bn'|core$parameter=='g_bo',
                                                    'g',
                                                    core$parameter)))
                    


ggplot(core, aes(y = est_diff, x = inter,
                 shape=model, color=model)) +
  facet_grid(bias ~cp)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1), 
                     labels = c("-1", "0", "1")) +
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Parameter Differences across conditions and models')+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

#########bn
core_bn <- filter(core, parameter %in% c("Do_bn", "Dn_bn", "g_bn","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn","r_1_bn","r_2_bn","r_3_bn","r_7_bn","r_8_bn"))

ggplot(core_bn, aes(y = est_diff, x = inter,
                 shape=model, color=model)) +
  facet_grid(bias ~cp)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1), 
                     labels = c("-1", "0", "1")) +
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Parameter Differences across conditions and models')+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

#####bo

core_bo <- filter(core, parameter %in% c("Do_bo", "Dn_bo", "g_bo","q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","r_1_bo","r_2_bo","r_3_bo","r_7_bo","r_8_bo"))

ggplot(core_bo, aes(y = est_diff, x = inter,
                    shape=model, color=model)) +
  facet_grid(bias ~cp)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1), 
                     labels = c("-1", "0", "1")) +
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Parameter Differences across conditions and models')+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

################################ Q #############################

params_q <- filter(res_between, parameter %in% c("q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo","q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn"))

params_q <- add_column(params_q,bias=1)
params_q$bias <- ifelse(params_q$parameter=="q_1_bn"|params_q$parameter=="q_2_bn"|params_q$parameter=="q_3_bn"|params_q$parameter=="q_6_bn"|params_q$parameter=="q_7_bn"|params_q$parameter=="q_8_bn",
                        'biased new',
                        'biased old')

params_q <- add_column(params_q,pq=1)
params_q$pq <- ifelse(params_q$parameter=="q_1_bn"|params_q$parameter=="q_1_bo",
                      'q_1',
                      ifelse(params_q$parameter=="q_2_bn"|params_q$parameter=="q_2_bo",
                             'q_2',
                             ifelse(params_q$parameter=="q_3_bn"|params_q$parameter=="q_3_bo",
                                    'q_3',
                                    ifelse(params_q$parameter=="q_6_bn"|params_q$parameter=="q_6_bo",
                                           'q_6',
                                           ifelse(params_q$parameter=="q_7_bn"|params_q$parameter=="q_7_bo",
                                                  'q_7',
                                                  ifelse(params_q$parameter=="q_8_bn"|params_q$parameter=="q_8_bo",
                                                         'q_8',
                                                         params_q$parameter))))))

ggplot(params_q, aes(y = est_diff, x = inter,
                 shape=model, color=model)) +
  facet_grid(bias ~pq)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1), 
                     labels = c("-1", "0", "1")) +
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Parameter Differences across conditions and models')+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

########bn

q_bn <- filter(params_q, parameter %in% c("q_1_bn", "q_2_bn","q_3_bn","q_6_bn","q_7_bn","q_8_bn"))

ggplot(q_bn, aes(y = est_diff, x = inter,
                     shape=model, color=model)) +
  facet_grid(bias ~pq)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1), 
                     labels = c("-1", "0", "1")) +
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Parameter Differences across conditions and models')+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

#########bo
q_bo <- filter(params_q, parameter %in% c("q_1_bo", "q_2_bo","q_3_bo","q_6_bo","q_7_bo","q_8_bo"))

ggplot(q_bo, aes(y = est_diff, x = inter,
                 shape=model, color=model)) +
  facet_grid(bias ~pq)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1), 
                     labels = c("-1", "0", "1")) +
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Parameter Differences across conditions and models')+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

################################ R #############################

params_r <- filter(res_between, parameter %in% c("r_1_bo", "r_2_bo","r_3_bo","r_6_bo","r_7_bo","r_8_bo","r_1_bn", "r_2_bn","r_3_bn","r_6_bn","r_7_bn","r_8_bn"))

params_r <- add_column(params_r,bias=1)
params_r$bias <- ifelse(params_r$parameter=="r_1_bn"|params_r$parameter=="r_2_bn"|params_r$parameter=="r_3_bn"|params_r$parameter=="r_6_bn"|params_r$parameter=="r_7_bn"|params_r$parameter=="r_8_bn",
                        'biased new',
                        'biased old')

params_r <- add_column(params_r,pr=1)
params_r$pr <- ifelse(params_r$parameter=="r_1_bn"|params_r$parameter=="r_1_bo",
                      'r_1',
                      ifelse(params_r$parameter=="r_2_bn"|params_r$parameter=="r_2_bo",
                             'r_2',
                             ifelse(params_r$parameter=="r_3_bn"|params_r$parameter=="r_3_bo",
                                    'r_3',
                                    ifelse(params_r$parameter=="r_6_bn"|params_r$parameter=="r_6_bo",
                                           'r_6',
                                           ifelse(params_r$parameter=="r_7_bn"|params_r$parameter=="r_7_bo",
                                                  'r_7',
                                                  ifelse(params_r$parameter=="r_8_bn"|params_r$parameter=="r_8_bo",
                                                         'r_8',
                                                         params_r$parameter))))))

ggplot(params_r, aes(y = est_diff, x = inter,
                     shape=model, color=model)) +
  facet_grid(bias ~pr)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1), 
                     labels = c("-1", "0", "1")) +
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Parameter Differences across conditions and models')+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

########bn

r_bn <- filter(params_r, parameter %in% c("r_1_bn", "r_2_bn","r_3_bn","r_6_bn","r_7_bn","r_8_bn"))

ggplot(r_bn, aes(y = est_diff, x = inter,
                 shape=model, color=model)) +
  facet_grid(bias ~pr)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1), 
                     labels = c("-1", "0", "1")) +
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Parameter Differences across conditions and models')+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

#########bo
r_bo <- filter(params_r, parameter %in% c("r_1_bo", "r_2_bo","r_3_bo","r_6_bo","r_7_bo","r_8_bo"))

ggplot(r_bo, aes(y = est_diff, x = inter,
                 shape=model, color=model)) +
  facet_grid(bias ~pr)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1), 
                     labels = c("-1", "0", "1")) +
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  labs(x='Model',y='Estimate', color='Model',shape='Model', title='Parameter Differences across conditions and models')+
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()