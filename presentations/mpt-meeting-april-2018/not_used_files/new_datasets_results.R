##############################################################################################################
################################ NEW DATASETS ################################################################
###############################################################################################################


load("../../koen2010/koen2010_orig.eqn-Koen_2010_pure.csv.RData")
results1_2 <- results

load("../../koen2010/koen2010_qrest.eqn-Koen_2010_pure.csv.RData")
results2_2 <- results

load("../../koen2010/koen2010_rrest.eqn-Koen_2010_pure.csv.RData")
results3_2 <- results


load("../../koen2011/koen2011_orig.eqn-Koen_2011.csv.RData")
results1_3 <- results

load("../../koen2011/koen2011_qrest.eqn-Koen_2011.csv.RData")
results2_3 <- results

load("../../koen2011/koen2011_rrest.eqn-Koen_2011.csv.RData")
results3_3 <- results


load("../../koen2013full/koen2013f_orig.eqn-Koen-2013_full.csv.RData")
results1_4 <- results

load("../../koen2013full/koen2013f_qrest.eqn-Koen-2013_full.csv.RData")
results2_4 <- results

load("../../koen2013full/koen2013f_rrest.eqn-Koen-2013_full.csv.RData")
results3_4 <- results


load("../../pratte2010/pratte_orig.eqn-Pratte_2010.csv.RData")
results1_5 <- results

load("../../pratte2010/pratte_qrest.eqn-Pratte_2010.csv.RData")
results2_5 <- results

load("../../pratte2010/pratte_rrest.eqn-Pratte_2010.csv.RData")
results3_5 <- results


load("../../smith2004/smith_orig.eqn-Smith_2004.csv.RData")
results1_6 <- results

load("../../smith2004/smith_qrest.eqn-Smith_2004.csv.RData")
results2_6 <- results

load("../../smith2004/smith_rrest.eqn-Smith_2004.csv.RData")
results3_6 <- results

load("../../jang2009/jang_orig.eqn-Jang_2009.csv.RData")
results1_7 <- results

load("../../jang2009/jang_qrest.eqn-Jang_2009.csv.RData")
results2_7 <- results

load("../../jang2009/jang_rrest.eqn-Jang_2009.csv.RData")
results3_7 <- results


gof_a <- bind_rows(
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

gof_a$model <- ifelse(gof_a$model=="jang_orig.eqn"|gof_a$model=="koen2010_orig.eqn"|gof_a$model=="koen2011_orig.eqn"|gof_a$model=="koen2013f_orig.eqn"|gof_a$model=="pratte_orig.eqn"|gof_a$model=="smith_orig.eqn",
                      'Q & R Restr.',
                      ifelse(gof_a$model=="jang_qrest.eqn"|gof_a$model=="koen2010_qrest.eqn"|gof_a$model=="koen2011_qrest.eqn"|gof_a$model=="koen2013f_qrest.eqn"|gof_a$model=="pratte_qrest.eqn"|gof_a$model=="smith_qrest.eqn",
                             'Q Restricted',
                             ifelse(gof_a$model=="jang_rrest.eqn"|gof_a$model=="koen2010_rrest.eqn"|gof_a$model=="koen2011_rrest.eqn"|gof_a$model=="koen2013f_rrest.eqn"|gof_a$model=="pratte_rrest.eqn"|gof_a$model=="smith_rrest.eqn",
                                    'R Restricted',
                                    gof_a$model)))

gof_a$dataset <- factor(gof_a$dataset, levels = c("Jang_2009.csv", "Koen_2010_pure.csv", "Koen_2011.csv", "Koen-2013_full.csv", "Pratte_2010.csv", "Smith_2004.csv"), 
                        labels = c("Jang et al. (2009)", "Koen & Yonelinas (2010)", "Koen & Yonelinas (2011)","Koen et al. (2013, F)","Pratte et al. (2010)","Smith & Duncan (2004, Exp. 2)"))


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
       color='Dataset', title='Goodness of fit')+
  theme(text=element_text(size = 22))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))



params <- bind_rows(
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

params$model <- ifelse(params$model=="jang_orig.eqn"|params$model=="koen2010_orig.eqn"|params$model=="koen2011_orig.eqn"|params$model=="koen2013f_orig.eqn"|params$model=="pratte_orig.eqn"|params$model=="smith_orig.eqn",
                      'Q & R Restr.',
                      ifelse(params$model=="jang_qrest.eqn"|params$model=="koen2010_qrest.eqn"|params$model=="koen2011_qrest.eqn"|params$model=="koen2013f_qrest.eqn"|params$model=="pratte_qrest.eqn"|params$model=="smith_qrest.eqn",
                             'Q Restricted',
                             ifelse(params$model=="jang_rrest.eqn"|params$model=="koen2010_rrest.eqn"|params$model=="koen2011_rrest.eqn"|params$model=="koen2013f_rrest.eqn"|params$model=="pratte_rrest.eqn"|params$model=="smith_rrest.eqn",
                                    'R Restricted',
                                    params$model)))
params$dataset <- factor(params$dataset, levels = c("Jang_2009.csv", "Koen_2010_pure.csv", "Koen_2011.csv", "Koen-2013_full.csv", "Pratte_2010.csv", "Smith_2004.csv"), 
                        labels = c("Jang et al. (2009)", "Koen & Yonelinas (2010)", "Koen & Yonelinas (2011)","Koen et al. (2013, F)","Pratte et al. (2010)","Smith & Duncan (2004, Exp. 2)"))

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
  labs(x='Analysis approach', y='Estimate', color='Dataset', title='Core Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()



Core_all <- filter(params, parameter %in% c("Dn","Do","g") & model %in% c('R Restricted'))
ggplot(Core_all, aes(y = est, x = dataset,
                     color=inter, shape=inter)) +
  facet_wrap( ~parameter, ncol=3) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Dataset', y='Estimate', color='Analysis approach', shape='Analysis approach', title='Core Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()


Q_all <- filter(params, parameter %in% c("q_1", "q_2", "q_5", "q_6") & model %in% c('R Restricted'))
ggplot(Q_all, aes(y = est, x = inter,
                  color=dataset)) +
  facet_wrap( ~parameter, ncol=4) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Analysis approach', y='Estimate', color='Dataset', title='Q Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()


Q_all <- filter(params, parameter %in% c("q_1", "q_2", "q_5", "q_6") & model %in% c('R Restricted'))
ggplot(Q_all, aes(y = est, x = dataset,
                  color=inter, shape=inter)) +
  facet_wrap( ~parameter, ncol=4) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Dataset', y='Estimate', color='Analysis approach', shape='Analysis approach', title='Q Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 20))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()


R_all <- filter(params, parameter %in% c("r_1", "r_2", "r_6") & model %in% c('R Restricted'))
ggplot(R_all, aes(y = est, x = inter,
                  color=dataset)) +
  facet_wrap( ~parameter, ncol=3) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Analysis approach', y='Estimate', color='Dataset', title='R Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()


R_all <- filter(params, parameter %in% c("r_1", "r_2", "r_6") & model %in% c('R Restricted'))
ggplot(R_all, aes(y = est, x = dataset,
                  color=inter, shape=inter)) +
  facet_wrap( ~parameter, ncol=4) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Dataset', y='Estimate', color='Analysis approach', shape='Analysis approach', title='R Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 20))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()



####################################################################################################################
######################### ALL DATASETS #############################################################################
####################################################################################################################
#####################################################################################################################


load("../../koen2010/koen2010_orig.eqn-Koen_2010_pure.csv.RData")
results1_2 <- results

load("../../koen2010/koen2010_qrest.eqn-Koen_2010_pure.csv.RData")
results2_2 <- results

load("../../koen2010/koen2010_rrest.eqn-Koen_2010_pure.csv.RData")
results3_2 <- results


load("../../koen2011/koen2011_orig.eqn-Koen_2011.csv.RData")
results1_3 <- results

load("../../koen2011/koen2011_qrest.eqn-Koen_2011.csv.RData")
results2_3 <- results

load("../../koen2011/koen2011_rrest.eqn-Koen_2011.csv.RData")
results3_3 <- results


load("../../koen2013full/koen2013f_orig.eqn-Koen-2013_full.csv.RData")
results1_4 <- results

load("../../koen2013full/koen2013f_qrest.eqn-Koen-2013_full.csv.RData")
results2_4 <- results

load("../../koen2013full/koen2013f_rrest.eqn-Koen-2013_full.csv.RData")
results3_4 <- results


load("../../pratte2010/pratte_orig.eqn-Pratte_2010.csv.RData")
results1_5 <- results

load("../../pratte2010/pratte_qrest.eqn-Pratte_2010.csv.RData")
results2_5 <- results

load("../../pratte2010/pratte_rrest.eqn-Pratte_2010.csv.RData")
results3_5 <- results


load("../../smith2004/smith_orig.eqn-Smith_2004.csv.RData")
results1_6 <- results

load("../../smith2004/smith_qrest.eqn-Smith_2004.csv.RData")
results2_6 <- results

load("../../smith2004/smith_rrest.eqn-Smith_2004.csv.RData")
results3_6 <- results

load("../../jang2009/jang_orig.eqn-Jang_2009.csv.RData")
results1_7 <- results

load("../../jang2009/jang_qrest.eqn-Jang_2009.csv.RData")
results2_7 <- results

load("../../jang2009/jang_rrest.eqn-Jang_2009.csv.RData")
results3_7 <- results


load("../../dube2012p/dubep_orig.eqn-Dube_2012-P.csv.RData")
results1_8 <- results

load("../../dube2012p/dubep_qrest.eqn-Dube_2012-P.csv.RData")
results2_8 <- results

load("../../dube2012p/dubep_rrest.eqn-Dube_2012-P.csv.RData")
results3_8 <- results


load("../../dube2012w/dubew_orig.eqn-Dube_2012-W.csv.RData")
results1_9 <- results

load("../../dube2012w/dubeW_qrest.eqn-Dube_2012-W.csv.RData")
results2_9 <- results

load("../../dube2012w/dubeW_rrest.eqn-Dube_2012-W.csv.RData")
results3_9 <- results


load("../../heathcote2006e1/heathcote_orig.eqn-Heathcote_2006_e1.csv.RData")
results1_10 <- results

load("../../heathcote2006e1/heathcote_qrest.eqn-Heathcote_2006_e1.csv.RData")
results2_10 <- results

load("../../heathcote2006e1/heathcote_rrest.eqn-Heathcote_2006_e1.csv.RData")
results3_10 <- results


load("../../heathcote2006e2/heathcote2_orig.eqn-Heathcote_2006_e2.csv.RData")
results1_11 <- results

load("../../heathcote2006e2/heathcote2_qrest.eqn-Heathcote_2006_e2.csv.RData")
results2_11 <- results

load("../../heathcote2006e2/heathcote2_rrest.eqn-Heathcote_2006_e2.csv.RData")
results3_11 <- results


load("../../jaeger2012/jaeger_orig.eqn-Jaeger_2012.csv.RData")
results1_12 <- results

load("../../jaeger2012/jaeger_qrest.eqn-Jaeger_2012.csv.RData")
results2_12 <- results

load("../../jaeger2012/jaeger_rrest.eqn-Jaeger_2012.csv.RData")
results3_12 <- results

load("../../koen2013/koen_orig.eqn-Koen_2013_immediate.csv.RData")
results1_13 <- results

load("../../koen2013/koen_qrest.eqn-Koen_2013_immediate.csv.RData")
results2_13 <- results

load("../../koen2013/koen_rrest.eqn-Koen_2013_immediate.csv.RData")
results3_13 <- results

gof_a <- bind_rows(
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
  unnest(results1_8, gof),
  unnest(results2_8, gof),
  unnest(results3_8, gof),
  unnest(results1_9, gof),
  unnest(results2_9, gof),
  unnest(results3_9, gof),
  unnest(results1_10, gof),
  unnest(results2_10, gof),
  unnest(results3_10, gof),
  unnest(results1_11, gof),
  unnest(results2_11, gof),
  unnest(results3_11, gof),
  unnest(results1_12, gof),
  unnest(results2_12, gof),
  unnest(results3_12, gof),
  unnest(results1_13, gof),
  unnest(results2_13, gof),
  unnest(results3_13, gof),
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

gof_a$model <- ifelse(gof_a$model=="jang_orig.eqn"|gof_a$model=="koen2010_orig.eqn"|gof_a$model=="koen2011_orig.eqn"|gof_a$model=="koen2013f_orig.eqn"|gof_a$model=="pratte_orig.eqn"|gof_a$model=="smith_orig.eqn"|gof_a$model=="dubep_orig.eqn"|gof_a$model=="jaeger_orig.eqn"|gof_a$model=="koen_orig.eqn"|gof_a$model=="koen_orig.eqn"|gof_a$model=="koen_orig.eqn"|gof_a$model=="heathcote_orig.eqn"|gof_a$model=="heathcote_orig.eqn"|gof_a$model=="dubew_orig.eqn"|gof_a$model=="heathcote2_orig.eqn"|gof_a$model=="koen_orig.eqn",
                      'Q & R Restr.',
                      ifelse(gof_a$model=="jang_qrest.eqn"|gof_a$model=="koen2010_qrest.eqn"|gof_a$model=="koen2011_qrest.eqn"|gof_a$model=="koen2013f_qrest.eqn"|gof_a$model=="pratte_qrest.eqn"|gof_a$model=="smith_qrest.eqn"|gof_a$model=="dubep_qrest.eqn"|gof_a$model=="jaeger_qrest.eqn"|gof_a$model=="koen_qrest.eqn"|gof_a$model=="heathcote_qrest.eqn"|gof_a$model=="dubew_qrest.eqn"|gof_a$model=="heathcote2_qrest.eqn"|gof_a$model=="koen_qrest.eqn",
                             'Q Restricted',
                             ifelse(gof_a$model=="jang_rrest.eqn"|gof_a$model=="koen2010_rrest.eqn"|gof_a$model=="koen2011_rrest.eqn"|gof_a$model=="koen2013f_rrest.eqn"|gof_a$model=="pratte_rrest.eqn"|gof_a$model=="smith_rrest.eqn"|gof_a$model=="dubep_rrest.eqn"|gof_a$model=="jaeger_rrest.eqn"|gof_a$model=="koen_rrest.eqn"|gof_a$model=="heathcote_rrest.eqn"|gof_a$model=="dubew_rrest.eqn"|gof_a$model=="heathcote2_rrest.eqn"|gof_a$model=="koen_rrest.eqn",
                                    'R Restricted',
                                    gof_a$model)))

gof_a$dataset <- factor(gof_a$dataset, levels = c("Jang_2009.csv", "Koen_2010_pure.csv", "Koen_2011.csv", "Koen-2013_full.csv", "Pratte_2010.csv", "Smith_2004.csv", "exp1.txt", "Dube_2012-P.csv", "Dube_2012-W.csv", "Heathcote_2006_e1.csv", "Heathcote_2006_e2.csv", "Jaeger_2012.csv", "Koen_2013_immediate.csv"), 
                        labels = c("Jang et al. (2009)", "Koen & Yonelinas (2010)", "Koen & Yonelinas (2011)","Koen et al. (2013, F)","Pratte et al. (2010)","Smith & Duncan (2004, Exp. 2)","Broeder et al. (2013)", "Dube & Rotello (2012, P)", "Dube & Rotello (2012, W)","Heathcote et al. (2006, 1)","Heathcote et al. (2006, 2)","Jaeger et al. (2012)","Koen et al. (2013)"))


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
       color='Dataset', title='Goodness of fit')+
  theme(text=element_text(size = 22))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))



params <- bind_rows(
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
  unnest(results1_8, est_group),
  unnest(results2_8, est_group),
  unnest(results3_8, est_group),
  unnest(results1_9, est_group),
  unnest(results2_9, est_group),
  unnest(results3_9, est_group),
  unnest(results1_10, est_group),
  unnest(results2_10, est_group),
  unnest(results3_10, est_group),
  unnest(results1_11, est_group),
  unnest(results2_11, est_group),
  unnest(results3_11, est_group),
  unnest(results1_12, est_group),
  unnest(results2_12, est_group),
  unnest(results3_12, est_group),
  unnest(results1_13, est_group),
  unnest(results2_13, est_group),
  unnest(results3_13, est_group),
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

params$model <- ifelse(params$model=="jang_orig.eqn"|params$model=="koen2010_orig.eqn"|params$model=="koen2011_orig.eqn"|params$model=="koen2013f_orig.eqn"|params$model=="pratte_orig.eqn"|params$model=="smith_orig.eqn"|params$model=="dubep_orig.eqn"|params$model=="jaeger_orig.eqn"|params$model=="koen_orig.eqn"|params$model=="koen_orig.eqn"|params$model=="koen_orig.eqn"|params$model=="heathcote_orig.eqn"|params$model=="heathcote_orig.eqn"|params$model=="dubew_orig.eqn"|params$model=="heathcote2_orig.eqn"|params$model=="koen_orig.eqn",
                       'Q & R Restr.',
                       ifelse(params$model=="jang_qrest.eqn"|params$model=="koen2010_qrest.eqn"|params$model=="koen2011_qrest.eqn"|params$model=="koen2013f_qrest.eqn"|params$model=="pratte_qrest.eqn"|params$model=="smith_qrest.eqn"|params$model=="dubep_qrest.eqn"|params$model=="jaeger_qrest.eqn"|params$model=="koen_qrest.eqn"|params$model=="heathcote_qrest.eqn"|params$model=="dubew_qrest.eqn"|params$model=="heathcote2_qrest.eqn"|params$model=="koen_qrest.eqn",
                              'Q Restricted',
                              ifelse(params$model=="jang_rrest.eqn"|params$model=="koen2010_rrest.eqn"|params$model=="koen2011_rrest.eqn"|params$model=="koen2013f_rrest.eqn"|params$model=="pratte_rrest.eqn"|params$model=="smith_rrest.eqn"|params$model=="dubep_rrest.eqn"|params$model=="jaeger_rrest.eqn"|params$model=="koen_rrest.eqn"|params$model=="heathcote_rrest.eqn"|params$model=="dubew_rrest.eqn"|params$model=="heathcote2_rrest.eqn"|params$model=="koen_rrest.eqn",
                                     'R Restricted',
                                     params$model)))

params$dataset <- factor(params$dataset, levels = c("Jang_2009.csv", "Koen_2010_pure.csv", "Koen_2011.csv", "Koen-2013_full.csv", "Pratte_2010.csv", "Smith_2004.csv", "exp1.txt", "Dube_2012-P.csv", "Dube_2012-W.csv", "Heathcote_2006_e1.csv", "Heathcote_2006_e2.csv", "Jaeger_2012.csv", "Koen_2013_immediate.csv"), 
                         labels = c("Jang et al. (2009)", "Koen & Yonelinas (2010)", "Koen & Yonelinas (2011)","Koen et al. (2013, F)","Pratte et al. (2010)","Smith & Duncan (2004, Exp. 2)","Broeder et al. (2013)", "Dube & Rotello (2012, P)", "Dube & Rotello (2012, W)","Heathcote et al. (2006, 1)","Heathcote et al. (2006, 2)","Jaeger et al. (2012)","Koen et al. (2013)"))

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
  labs(x='Analysis approach', y='Estimate', color='Dataset', title='Core Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()



Core_all <- filter(params, parameter %in% c("Dn","Do","g") & model %in% c('R Restricted'))
ggplot(Core_all, aes(y = est, x = dataset,
                     color=inter, shape=inter)) +
  facet_wrap( ~parameter, ncol=3) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Dataset', y='Estimate', color='Analysis approach', shape='Analysis approach', title='Core Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()


Q_all <- filter(params, parameter %in% c("q_1", "q_2", "q_5", "q_6") & model %in% c('R Restricted'))
ggplot(Q_all, aes(y = est, x = inter,
                  color=dataset)) +
  facet_wrap( ~parameter, ncol=4) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Analysis approach', y='Estimate', color='Dataset', title='Q Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()


Q_all <- filter(params, parameter %in% c("q_1", "q_2", "q_5", "q_6") & model %in% c('R Restricted'))
ggplot(Q_all, aes(y = est, x = dataset,
                  color=inter, shape=inter)) +
  facet_wrap( ~parameter, ncol=4) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Dataset', y='Estimate', color='Analysis approach', shape='Analysis approach', title='Q Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 20))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()


R_all <- filter(params, parameter %in% c("r_1", "r_2","r_5", "r_6") & model %in% c('R Restricted'))
ggplot(R_all, aes(y = est, x = inter,
                  color=dataset)) +
  facet_wrap( ~parameter, ncol=4) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Analysis approach', y='Estimate', color='Dataset', title='R Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 24))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()


R_all <- filter(params, parameter %in% c("r_1", "r_2", "r_5", "r_6") & model %in% c('R Restricted'))
ggplot(R_all, aes(y = est, x = dataset,
                  color=inter, shape=inter)) +
  facet_wrap( ~parameter, ncol=4) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.6)+
  geom_point(position = dd, size = 3.5) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1), 
                     labels = c("0","0.5", "1")) + 
  labs(x='Dataset', y='Estimate', color='Analysis approach', shape='Analysis approach', title='R Parameters Across Data sets for R Restricted')+
  theme_bw() + 
  theme(text=element_text(size = 20))+
  theme(plot.title=element_text(face = 'bold',size=24, hjust = 0.5))+
  coord_flip()

