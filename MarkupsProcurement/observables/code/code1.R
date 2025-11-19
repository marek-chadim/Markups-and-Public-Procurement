# Markups and Public Procurement 
# Marek Chadim
# 2025

## Czech construction firms
## Estimation
gc()
rm(list = ls())
setwd("/Users/marek/Library/CloudStorage/Dropbox/Markups-and-Public-Procurement/MarkupsProcurement/observables")
source("code/functions_est.R")
source("code/functions_plot.R")

d <- read.csv("data/data.csv")
d<-data.frame(d)
dim(d)
d[is.na(d)] <- 0
save(d, file = "data/data.RData")
load("data/data.RData")
dim(d)
head(d)
names(d)
str(d)
summary(d)
class(d)
str(d)
d<-data.frame(d)
d$tr <- d$pp
d$co <-  ifelse(d$pp == 0, 1, 0)
d$markup_1yr_before <- d$x6
table(d$tr)

d$xmu.avg <- apply(d[, paste0("x", 4:6)], 1, mean) # avg pre outcome
d$ymu.avg <- apply(d[, paste0("y", 0:2)], 1, mean) # avg pst outcome
d$ymu.avg <- d$y0
# Define covariates
covar <- c("nace42", "nace43", "x1", "x2", "x3", "pp1", "pp2", "pp3", "pp4", "sales", "costs", "assets", "empl", paste0("year", 5:15))

# estimate
set.seed(1234)
out1 <- estimate_all(d, "ymu.avg", "tr", covar)
out2 <- estimate_all(d, "xmu.avg", "tr", covar)

# columns are post/pre markups
a <- list(out1, out2)
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
d$ps <- probability_forest(X = d[, covar], 
                            Y = as.factor(d$tr), seed = 1234, num.trees = 4000)$predictions[,2]
d_trim <- subset(d, ps <= .8)
d_trim$ps <- probability_forest(X = d_trim[, covar], 
                                Y = as.factor(d_trim$tr), seed = 1234, num.trees = 4000)$predictions[,2]

mout <- Match(Y = d_trim$ymu0, Tr = d_trim$tr, X = d_trim$ps, estimand = "ATT", M = 1,
              BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
dt <- d_trim[c(mout$index.treated, mout$index.control), ]
dt$ps <- probability_forest(X = dt[, covar], 
                            Y = as.factor(dt$tr), seed = 1234, num.trees = 4000)$predictions[,2]

table(dt$tr)

set.seed(1234)
# estimate on trimmed sample
out1t <- estimate_all(dt, "ymu.avg", "tr", covar)
out2t <- estimate_all(dt, "xmu.avg", "tr", covar)

# columns are samples
a <- list(out1t, out2t)
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

ylim <- c(-.15, .15)

pdf("graphs/est_coef_d.pdf", width = 12, height = 3)
plot_coef(out1t, ylim = ylim, main = "Contractor vs. Non-Contractor", main.pos = 3)
graphics.off()

pdf("graphs/est_coef_d_pl.pdf", width = 12, height = 3)
plot_coef(out2t, ylim = ylim, main = "Contractor vs. Non-Contractor: Placebo Test", main.pos = 3)
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
covar <- c("nace42", "nace43", "x1", "x2", "x3", "pp1", "pp2", "pp3", "pp4", "sales", "costs", "assets", "empl", paste0("year", 5:15))
covar <- c(covar, "x4", "x5", "markup_1yr_before")

fml <- as.formula(paste(Y, "~", treat, "+", paste(covar, collapse = "+")))
mod <- lm(fml, data = d)
summary(mod)

bm <- c("markup_1yr_before")
sens <- sensemakr(model = mod, treatment = treat, benchmark_covariates = bm, kd = 1:3, sensitivity.of = "t-value")
pdf("graphs/d_sens.pdf", width = 7, height = 7)
plot(sens)
graphics.off()





