
require(MPTinR)
require(dplyr)
data(roc6, package = 'MPTinR')

tapply(rowSums(roc6[,-(13:14)]), INDEX = list(roc6$exp), mean)

table(roc6$exp)


head(roc6)

d1 <- roc6[ roc6$exp == "Jaeger_2012",]

d2 <- roc6[ roc6$exp == "Koen-2013_immediate",]
d3 <- roc6[ roc6$exp == "Dube_2012-P",]
d4 <- roc6[ roc6$exp == "Dube_2012-W",]

htm <- "
(1-Do)*(1-g)*(1-gn1)*(1-gn2)
(1-Do)*(1-g)*(1-gn1)*gn2
(1-Do)*(1-g)*gn1
Do*(1-do1)*(1-do2)  + (1-Do)*g*go1
Do*do1 + (1-Do)*g*(1-go1)*go2
Do*(1-do1)*do2 + (1-Do)*g*(1-go1)*(1-go2)

Dn*(1-dn1)*dn2 + (1-Dn)*(1-g)*(1-gn1)*(1-gn2)
Dn*dn1 + (1-Dn)*(1-g)*(1-gn1)*gn2
Dn*(1-dn1)*(1-dn2) + (1-Dn)*(1-g)*gn1
(1-Dn)*g*go1
(1-Dn)*g*(1-go1)*go2
(1-Dn)*g*(1-go1)*(1-go2)
"

check.mpt(textConnection(htm), list("dn2 = do2", "dn1 = do1"))

tmp <- tempfile()
mod <- readLines(textConnection(htm))
mod <- gsub("\\<do1\\>", "dr1", mod)
mod <- gsub("\\<dn1\\>", "dr1", mod)
mod <- gsub("\\<do2\\>", "dr2", mod)
mod <- gsub("\\<dn2\\>", "dr2", mod)
writeLines(mod, tmp)

check.mpt(tmp)

make.eqn(tmp, "2htm.eqn")

mod2 <- readLines("2htm.eqn")

resp_names <- colnames(d1)[1:12]
for (c in seq_along(resp_names)) {
  mod2 <- sub(paste("(\\d)", paste0("\\<", c, "\\>")), paste("\\1", resp_names[c]), mod2)
}
cat(mod2, sep = "\n")
writeLines(mod2, "2htm.eqn")

htm_order <- "
(1-Do)*(1-g)*(1-p33)*(1-gn1)*(1-p5)*(1-gn2)
(1-Do)*(1-g)*(1-p33)*(1-gn1)*p5 + (1-Do)*(1-g)*(1-p33)*(1-gn1)*(1-p5)*gn2*(1-p5)
(1-Do)*(1-g)*p33 + (1-Do)*(1-g)*(1-p33)*gn1 + (1-Do)*(1-g)*(1-p33)*(1-gn1)*(1-p5)*gn2*p5
Do*(1-do1)*(1-do2)  + (1-Do)*g*p33 + (1-Do)*g*(1-p33)*go1 + (1-Do)*g*(1-p33)*(1-go1)*(1-p5)*go2*p5
Do*(1-do1)*do2 + (1-Do)*g*(1-p33)*(1-go1)*p5 + (1-Do)*g*(1-p33)*(1-go1)*(1-p5)*go2*(1-p5)
Do*do1 + (1-Do)*g*(1-p33)*(1-go1)*(1-p5)*(1-go2)

Dn*(1-dn1)*(1-dn2) + (1-Dn)*(1-g)*(1-p33)*(1-gn1)*(1-p5)*(1-gn2)
Dn*(1-dn1)*dn2 + (1-Dn)*(1-g)*(1-p33)*(1-gn1)*p5 + (1-Dn)*(1-g)*(1-p33)*(1-gn1)*(1-p5)*gn2*(1-p5)
Dn*dn1 + (1-Dn)*(1-g)*p33 + (1-Dn)*(1-g)*(1-p33)*gn1 + (1-Dn)*(1-g)*(1-p33)*(1-gn1)*(1-p5)*gn2*p5
(1-Dn)*g*p33 + (1-Dn)*g*(1-p33)*go1 + (1-Dn)*g*(1-p33)*(1-go1)*(1-p5)*go2*p5
(1-Dn)*g*(1-p33)*(1-go1)*p5 + (1-Dn)*g*(1-p33)*(1-go1)*(1-p5)*go2*(1-p5)
(1-Dn)*g*(1-p33)*(1-go1)*(1-p5)*(1-go2)
"

check.mpt(textConnection(htm_order), list("dn2 = do2", "dn1 = do1"))

tmp <- tempfile()
mod <- readLines(textConnection(htm_order))
mod <- gsub("\\<do1\\>", "dr1", mod)
mod <- gsub("\\<dn1\\>", "dr1", mod)
mod <- gsub("\\<do2\\>", "dr2", mod)
mod <- gsub("\\<dn2\\>", "dr2", mod)
writeLines(mod, tmp)

check.mpt(tmp)

make.eqn(tmp, "2htm_ord-g.eqn")

mod2 <- readLines("2htm_ord-g.eqn")

resp_names <- colnames(d1)[1:12]
for (c in seq_along(resp_names)) {
  mod2 <- sub(paste("(\\d)", paste0("\\<", c, "\\>")), paste("\\1", resp_names[c]), mod2)
}

mod2 <- sub("\\<p33\\>", "0.333333333333", mod2)
mod2 <- sub("\\<p5\\>", "0.5", mod2)

cat(mod2, sep = "\n")
writeLines(mod2, "2htm_ord-g.eqn")

check.mpt("2htm_ord-g.eqn")

write.csv(d1, "Jaeger_2012.csv")

write.csv(d2, "Koen_2013_immediate.csv", row.names = FALSE)
write.csv(d3, "Dube_2012-P.csv", row.names = FALSE)
write.csv(d4, "Dube_2012-W.csv", row.names = FALSE)