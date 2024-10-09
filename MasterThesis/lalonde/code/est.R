# Markups and Public Procurement 
# Marek Chadim
# 2024

## Czech construction firms
## Estimation
gc()
rm(list = ls())
source("code/functions_est.R")
source("code/functions_plot.R")

library(readxl)
d <- read_excel("data/data.xlsx")
d<-data.frame(d)
d<-na.omit(d)
head(d)
names(d)
str(d)
summary(d)
class(d)
str(d)

d<-data.frame(d)
d$tr <- d$pp_dummy
d$tr1 <- ifelse(d$pp_dummy == 1 & d$year > 2016, 1, 0) 
d$tr2 <- ifelse(d$tr1 == 0 & d$pp_dummy == 1, 1, 0)
d$co <-  ifelse(d$pp_dummy == 0, 1, 0 ) # control

d$markup_1yr_before <- d$x6
table(d$tr1, d$tr2)
table(d$tr1, d$co)
table(d$tr2, d$co)

d$xmu.avg <- apply(d[, paste0("x", 4:6)], 1, mean) # avg pre outcome
#d$ymu.avg <- apply(d[, paste0("y", 1:2)], 1, mean) # avg pst outcome
d$ymu.avg <-  d$y1

s1 <- subset(d, tr1 == 1 | co == 1) 
s2 <- subset(d, tr2 == 1 | co == 1) 
table(s1$tr)
table(s2$tr)

covar <- c("year", "pp_history","dpp_mean", "sales", "costs", "assets", "wages", "empl", "nace2", "sector", "x1", "x2","x3")

# big share
set.seed(1234)
out1 <- estimate_all(s1, "ymu.avg", "tr", covar)
out2 <- estimate_all(s1, "xmu.avg", "tr", covar)
# small share
out3 <- estimate_all(s2, "ymu.avg", "tr", covar)
out4 <- estimate_all(s2, "xmu.avg", "tr", covar)


# columns are samples
a <- list(out1, out2, out3, out4)
n <- nrow(out1)   
sav <- matrix("", n, length(a)*3-1)
for (j in 1:length(a)) {
  out <- a[[j]]
  n <- nrow(out)    
  for (i in 1:nrow(out)) {
    sav[i, j*3-2] <- sprintf("%.2f", out[i, 1])
    sav[i, j*3-1] <- paste0("(", sprintf("%.2f", out[i, 2]), ")")    
  }
}
print(sav)
write.csv(sav, file = "tables/est.csv", row.names = FALSE)


# Trimmed data
# trim
s1$ps <- probability_forest(X = s1[, covar], 
                            Y = as.factor(s1$tr), seed = 1234)$predictions[,2]
s1_trim <- subset(s1, ps <= .9)
s1_trim$ps <- probability_forest(X = s1_trim[, covar], 
                                Y = as.factor(s1_trim$tr), seed = 1234, num.trees = 4000)$predictions[,2]

mout <- Match(Y = s1_trim$ymu1, Tr = s1_trim$tr, X = s1_trim$ps, estimand = "ATT", M = 1,
              BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
s1t <- s1_trim[c(mout$index.treated, mout$index.control), ]
s1t$ps <- probability_forest(X = s1t[, covar], 
                            Y = as.factor(s1t$tr), seed = 1234, num.trees = 4000)$predictions[,2]




s2$ps <- probability_forest(X = s2[, covar], 
                            Y = as.factor(s2$tr), seed = 1234)$predictions[,2]
s2_trim <- subset(s2, ps <= .9)
s2_trim$ps <- probability_forest(X = s2_trim[, covar], 
                                 Y = as.factor(s2_trim$tr), seed = 1234, num.trees = 4000)$predictions[,2]

mout <- Match(Y = s2_trim$ymu1, Tr = s2_trim$tr, X = s2_trim$ps, estimand = "ATT", M = 1,
              BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
s2t <- s2_trim[c(mout$index.treated, mout$index.control), ]
s2t$ps <- probability_forest(X = s2t[, covar], 
                             Y = as.factor(s2t$tr), seed = 1234, num.trees = 4000)$predictions[,2]


table(s1t$tr)
table(s2t$tr)

set.seed(1234)
# big share
out1t <- estimate_all(s1t, "ymu.avg", "tr", covar)
out2t <- estimate_all(s1t, "xmu.avg", "tr", covar)
# small share
out3t <- estimate_all(s2t, "ymu.avg", "tr", covar)
out4t <- estimate_all(s2t, "xmu.avg", "tr", covar)

# columns are samples
a <- list(out1t, out2t, out3t, out4t)
n <- nrow(out1t)   
sav <- matrix("", n, length(a)*3-1)
for (j in 1:length(a)) {
  out <- a[[j]]
  n <- nrow(out)    
  for (i in 1:nrow(out)) {
    sav[i, j*3-2] <- sprintf("%.2f", out[i, 1])
    sav[i, j*3-1] <- paste0("(", sprintf("%.2f", out[i, 2]), ")")    
  }
}
print(sav)
write.csv(sav, file = "tables/est_trimmed.csv", row.names = FALSE)

#           $$\            $$\     
#           $$ |           $$ |    
#  $$$$$$\  $$ | $$$$$$\ $$$$$$\   
# $$  __$$\ $$ |$$  __$$\\_$$  _|  
# $$ /  $$ |$$ |$$ /  $$ | $$ |    
# $$ |  $$ |$$ |$$ |  $$ | $$ |$$\ 
# $$$$$$$  |$$ |\$$$$$$  | \$$$$  |
# $$  ____/ \__| \______/   \____/ 
# $$ |                             
# $$ |                             
# \__|                             

ylim <- c(-.1, .3)

pdf("graphs/est_coef_s1.pdf", width = 12, height = 3)
plot_coef(out1t, ylim = ylim, main = "2017-2021 Contractor vs. Non-Contractor", main.pos = 3)
graphics.off()

pdf("graphs/est_coef_s1_pl.pdf", width = 12, height = 3)
plot_coef(out2t, ylim = ylim, main = "2017-2021 Contractor vs. Non-Contractor: Placebo Test", main.pos = 3)
graphics.off()

pdf("graphs/est_coef_s2.pdf", width = 12, height = 3)
plot_coef(out3t, ylim = ylim, main = "2006-2016 Contractor vs. Non-Contractor", main.pos = 3)
graphics.off()

pdf("graphs/est_coef_s2_pl.pdf", width = 12, height = 3)
plot_coef(out4t, ylim = ylim, main = "2006-2016 Contractor vs. Non-Contractor: Placebo Test", main.pos = 3)
graphics.off()

#  $$$$$$\                                
# $$  __$$\                               
# $$ /  \__| $$$$$$\  $$$$$$$\   $$$$$$$\ 
# \$$$$$$\  $$  __$$\ $$  __$$\ $$  _____|
#  \____$$\ $$$$$$$$ |$$ |  $$ |\$$$$$$\  
# $$\   $$ |$$   ____|$$ |  $$ | \____$$\ 
# \$$$$$$  |\$$$$$$$\ $$ |  $$ |$$$$$$$  |
#  \______/  \_______|\__|  \__|\_______/ 



library(sensemakr)
Y <- "ymu.avg"
treat <- "tr"

covar <- c("year", "pp_history", "sales", "costs", "assets", "wages", "empl", "nace2", "sector", "x1", "x2", "x3", "lpp1", "lpp2", "lpp3", "x4", "x5", "markup_1yr_before")

fml <- as.formula(paste(Y, "~", treat, "+", paste(covar, collapse = "+")))
mod1 <- lm(fml, data = s1t)
mod2 <- lm(fml, data = s2t)
summary(mod1)

bm <- c("markup_1yr_before")
sens1 <- sensemakr(model = mod1, treatment = treat, benchmark_covariates = bm, kd = 1:3, sensitivity.of = "t-value")
sens2 <- sensemakr(model = mod2, treatment = treat, benchmark_covariates = bm, kd = 1:3, sensitivity.of = "t-value")
pdf("graphs/s1_sens.pdf", width = 7, height = 7)
plot(sens1)
graphics.off()

pdf("graphs/s2_sens.pdf", width = 7, height = 7)
plot(sens2)
graphics.off()







