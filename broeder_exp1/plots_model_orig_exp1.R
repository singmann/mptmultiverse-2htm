
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

load("model_orig.eqn-exp1.txt.RData")
results1 <- results

load("model_orig_qrest.eqn-exp1.txt.RData")
results2 <- results

load("model_orig_rrest.eqn-exp1.txt.RData")
results3 <- results

par_est <- bind_rows(
  unnest(results1, est_group),
  unnest(results2, est_group),
  unnest(results3, est_group)
)

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

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

par_est <- bind_rows(
  unnest(results1, est_group),
  unnest(results2, est_group),
  unnest(results3, est_group)
)

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

params_q <- filter(par_est, parameter %in% c("q_1", "q_2", "q_3", "q_6", "q_7", "q_8"))
ggplot(params_q, aes(y = est, x = interaction(method, pooling,package),
                     shape=model, color =model)) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1))+
  theme_bw() + 
  coord_flip() 

##### tous aksones kai sta 2

par_est <- bind_rows(
  unnest(results1, est_group),
  unnest(results2, est_group),
  unnest(results3, est_group)
)

shapes <- c(16, 18, 15, 1, 0, 8, 11, 12)
dd <- position_dodge(w = .75)

params_r <- filter(par_est, parameter %in% c("r_1", "r_2", "r_3", "r_6", "r_7", "r_8"))
ggplot(params_r, aes(y = est, x = interaction(method, pooling,package),
                     shape=model, color =model)) +
  facet_grid(condition ~parameter) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                width = 0.4)+
  geom_point(position = dd, size = 2) + 
  scale_shape_manual(values=shapes) +
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1))+
  theme_bw() + 
  coord_flip()

#####EST DIFF

res_between <- bind_rows(
  unnest(results1, test_between),
  unnest(results2, test_between),
  unnest(results3, test_between)
)
dd <- position_dodge(w = .75)
core2 <- filter(res_between, parameter %in% c("Do", "Dn", "g"))
ggplot(core2, aes(y = est_diff, x = interaction(method, pooling,package),
                  shape=model, color=model)) +
  facet_wrap(~parameter, ncol=3)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + ylim(-1,1) + 
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 2) +
  coord_flip()

res_between <- bind_rows(
  unnest(results1, test_between),
  unnest(results2, test_between),
  unnest(results3, test_between)
)
params_q <- filter(res_between, parameter %in% c("q_1", "q_2", "q_3", "q_6", "q_7", "q_8"))
ggplot(params_q, aes(y = est_diff, x = interaction(method, pooling,package),
                  shape=model, color=model)) +
  facet_wrap(~parameter, ncol=6)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) + 
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1))+
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 3) +
  coord_flip()

###aksones ki edw kai sta 2

res_between <- bind_rows(
  unnest(results1, test_between),
  unnest(results2, test_between),
  unnest(results3, test_between)
)
params_r <- filter(res_between, parameter %in% c("r_1", "r_2", "r_3", "r_6", "r_7", "r_8"))
ggplot(params_r, aes(y = est_diff, x = interaction(method, pooling,package),
                     shape=model, color=model)) +
  facet_wrap(~parameter, ncol=6)+
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                position = dd, width = 0.5)+
  geom_point(position = dd, size=2) +
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1))+
  scale_shape_manual(values=shapes) +
  theme_bw() + geom_hline(yintercept = 0, lty = 3) +
  coord_flip()



##### teleiwnw edw 


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
