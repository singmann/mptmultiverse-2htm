
library("tidyr")
library("dplyr")
library("tibble")
library("rlang")
library("reshape2")
library("ggplot2")
library("parallel")
library("purrr")
library("readr")
library("broom") # for tidy
library("stringr")
library("DescTools") # for CCC

load("../koen2010/koen2010_orig.eqn-Koen_2010_pure.csv.RData")
resultl <- list(results)

load("../koen2010/koen2010_qrest.eqn-Koen_2010_pure.csv.RData")
resultl <- c(resultl, list(results))

load("../koen2010/koen2010_rrest.eqn-Koen_2010_pure.csv.RData")
resultl <- c(resultl, list(results))

load("../koen2011/koen2011_orig.eqn-Koen_2011.csv.RData")
resultl <- c(resultl, list(results))

load("../koen2011/koen2011_qrest.eqn-Koen_2011.csv.RData")
resultl <- c(resultl, list(results))

load("../koen2011/koen2011_rrest.eqn-Koen_2011.csv.RData")
resultl <- c(resultl, list(results))

load("../koen2013full/koen2013f_orig.eqn-Koen-2013_full.csv.RData")
resultl <- c(resultl, list(results))

load("../koen2013full/koen2013f_qrest.eqn-Koen-2013_full.csv.RData")
resultl <- c(resultl, list(results))

load("../koen2013full/koen2013f_rrest.eqn-Koen-2013_full.csv.RData")
resultl <- c(resultl, list(results))

load("../pratte2010/pratte_orig.eqn-Pratte_2010.csv.RData")
resultl <- c(resultl, list(results))

load("../pratte2010/pratte_qrest.eqn-Pratte_2010.csv.RData")
resultl <- c(resultl, list(results))

load("../pratte2010/pratte_rrest.eqn-Pratte_2010.csv.RData")
resultl <- c(resultl, list(results))

load("../smith2004/smith_orig.eqn-Smith_2004.csv.RData")
resultl <- c(resultl, list(results))

load("../smith2004/smith_qrest.eqn-Smith_2004.csv.RData")
resultl <- c(resultl, list(results))

load("../smith2004/smith_rrest.eqn-Smith_2004.csv.RData")
resultl <- c(resultl, list(results))

load("../jang2009/jang_orig.eqn-Jang_2009.csv.RData")
resultl <- c(resultl, list(results))

load("../jang2009/jang_qrest.eqn-Jang_2009.csv.RData")
resultl <- c(resultl, list(results))

load("../jang2009/jang_rrest.eqn-Jang_2009.csv.RData")
resultl <- c(resultl, list(results))

load("../dube2012p/dubep_orig.eqn-Dube_2012-P.csv.RData")
resultl <- c(resultl, list(results))

load("../dube2012p/dubep_qrest.eqn-Dube_2012-P.csv.RData")
resultl <- c(resultl, list(results))

load("../dube2012p/dubep_rrest.eqn-Dube_2012-P.csv.RData")
resultl <- c(resultl, list(results))

load("../dube2012w/dubew_orig.eqn-Dube_2012-W.csv.RData")
resultl <- c(resultl, list(results))

load("../dube2012w/dubeW_qrest.eqn-Dube_2012-W.csv.RData")
resultl <- c(resultl, list(results))

load("../dube2012w/dubeW_rrest.eqn-Dube_2012-W.csv.RData")
resultl <- c(resultl, list(results))

load("../heathcote2006e1/heathcote_orig.eqn-Heathcote_2006_e1.csv.RData")
resultl <- c(resultl, list(results))

load("../heathcote2006e1/heathcote_qrest.eqn-Heathcote_2006_e1.csv.RData")
resultl <- c(resultl, list(results))

load("../heathcote2006e1/heathcote_rrest.eqn-Heathcote_2006_e1.csv.RData")
resultl <- c(resultl, list(results))

load("../heathcote2006e2/heathcote2_orig.eqn-Heathcote_2006_e2.csv.RData")
resultl <- c(resultl, list(results))

load("../heathcote2006e2/heathcote2_qrest.eqn-Heathcote_2006_e2.csv.RData")
resultl <- c(resultl, list(results))

load("../heathcote2006e2/heathcote2_rrest.eqn-Heathcote_2006_e2.csv.RData")
resultl <- c(resultl, list(results))

load("../jaeger2012/jaeger_orig.eqn-Jaeger_2012.csv.RData")
resultl <- c(resultl, list(results))

load("../jaeger2012/jaeger_qrest.eqn-Jaeger_2012.csv.RData")
resultl <- c(resultl, list(results))

load("../jaeger2012/jaeger_rrest.eqn-Jaeger_2012.csv.RData")
resultl <- c(resultl, list(results))

load("../koen2013/koen_orig.eqn-Koen_2013_immediate.csv.RData")
resultl <- c(resultl, list(results))

load("../koen2013/koen_qrest.eqn-Koen_2013_immediate.csv.RData")
resultl <- c(resultl, list(results))

load("../koen2013/koen_rrest.eqn-Koen_2013_immediate.csv.RData")
resultl <- c(resultl, list(results))
str(resultl, 1)

results <- bind_rows(resultl)

results$restriction <- factor(str_replace(str_replace(
  results$model, "[[:alnum:]]+_", ""), "\\.eqn", ""), 
  levels = c("orig", "qrest", "rrest"))

all_pars <- unnest(results, est_group)
all_pars$intera <- with(all_pars, interaction(pooling, method, package, 
                                              drop = TRUE))
exclude_methods <- c("no.asymptotic.MPTinR", 
                     "no.simple.TreeBUGS", 
                     "partial.beta.TreeBUGS")
all_pars <- droplevels( filter(all_pars, !(intera %in% exclude_methods)) )

pairs <- combn(sort(levels(all_pars$intera)), 2)

all_pars_l <- vector("list", ncol(pairs))

for (i in seq_len(ncol(pairs))) {
  tmp_dat <- all_pars %>% 
    filter(intera %in% pairs[,i]) %>% 
    select(model, dataset, parameter, restriction, est, intera) %>% 
    spread(key = intera, value = est)
  colnames(tmp_dat)[(ncol(tmp_dat)-1):ncol(tmp_dat)] <- c("x", "y")
  tmp_dat$cond_x <- pairs[1,i]
  tmp_dat$cond_y <- pairs[2,i]
  all_pars_l[[i]] <- tmp_dat 
}

all_pars2 <- bind_rows(all_pars_l)

plot_text <- all_pars2 %>% 
  group_by(cond_x, cond_y, restriction) %>% 
  summarise(ccc = format(
    CCC(x, y, na.rm = TRUE)$rho.c$est, 
    digits = 2))

all_pars2 %>% 
  filter(restriction == "orig") %>% 
  ggplot(aes(x = x, y = y)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  facet_grid(cond_x~ cond_y) +
    geom_text(data=plot_text[ plot_text$restriction == "orig", ],
            aes(x = 0.2, y = 0.9, label=ccc), 
            parse = TRUE, inherit.aes=FALSE, size = 5) +
  coord_fixed() +
  ggtitle("...") +
  theme_bw()
ggsave("pairsplot1.pdf", device = "pdf", width = 8, height = 8)


all_pars2 %>% 
  filter(restriction == "orig") %>% 
  ggplot(aes(x = x, y = y)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  facet_grid(cond_y ~ cond_x) +
    geom_text(data=plot_text[ plot_text$restriction == "orig", ],
            aes(x = 0.2, y = 0.9, label=ccc), 
            parse = TRUE, inherit.aes=FALSE, size = 5) +
  coord_fixed() +
  ggtitle("...") +
  theme_bw()
ggsave("pairsplot2.pdf", device = "pdf", width = 8, height = 8)
