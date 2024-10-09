
## Estimating the Effects of Older Contracts on Price markups
gc()
rm(list = ls())
source("code/functions_est.R")
source("code/functions_plot.R")

library(readxl)
d <- as.data.frame(read_excel("data/data.xlsx"))
save(d, file = "data/data.RData")

#             $$\                          $$\     
#             $$ |                         $$ |    
#  $$$$$$$\ $$$$$$\    $$$$$$\   $$$$$$\ $$$$$$\   
# $$  _____|\_$$  _|   \____$$\ $$  __$$\\_$$  _|  
# \$$$$$$\    $$ |     $$$$$$$ |$$ |  \__| $$ |    
#  \____$$\   $$ |$$\ $$  __$$ |$$ |       $$ |$$\ 
# $$$$$$$  |  \$$$$  |\$$$$$$$ |$$ |       \$$$$  |
# \_______/    \____/  \_______|\__|        \____/ 



load("data/data.RData")
head(d)
names(d)
str(d)
summary(d)
class(d)
str(d)

hist(d$year)
hist(d$y1, breaks = 50)
head(d)
names(d)
str(d)
summary(d)
class(d)
str(d)

d<-na.omit(d)
d$tr <- d$pp_dummy
d$tr1 <- ifelse(d$pp_dummy == 1 & d$year > 2016, 1, 0) 
d$tr2 <- ifelse(d$tr1 == 0 & d$pp_dummy == 1, 1, 0)
d$co <-  ifelse(d$pp_dummy == 0, 1, 0 ) # control

table(d$tr1, d$tr2)
table(d$tr1, d$co)
table(d$tr2, d$co)
#s <- subset(d, tr1 == 1 | co == 1)
s <- subset(d, tr2 == 1 | co == 1)


table(s$tr)


s$xmu.avg <- s$x2
s$ymu.avg <- s$y1
#s$xmu.avg <- apply(s[, paste0("xmu", 4:6)], 1, mean) # avg pre outcome
#s$ymu.avg <- apply(s[, paste0("ymu", 1:7)], 1, mean) # avg pst outcome
#hist(s$xmu13_avg, breaks = 50)
hist(s$xmu.avg, breaks = 50)
hist(s$ymu.avg, breaks = 50)
table(s$tr)

treat <- "tr"
covar <- c("year", "pp_history","dpp_mean", "sales", "costs", "assets", "wages", "empl", "nace2", "sector", "x1", "x2","x3")


s$ps <- probability_forest(X = s[, covar], 
                           Y = as.factor(s$tr), seed = 1234)$predictions[,2]
range(s$ps)
nrow(subset(s, co == 1 & ps > 0.9)) # only 3 controls with ps > 0.9
s_trim <- subset(s, ps <= .9)

# propensity score matching without replacement
s_trim$ps <- probability_forest(X = s_trim[, covar], 
                                Y = as.factor(s_trim$tr), seed = 1234, num.trees = 4000)$predictions[,2]

mout <- Match(Y = s_trim$ymu1, Tr = s_trim$tr, X = s_trim$ps, estimand = "ATT", M = 1,
              BiasAdjust = FALSE, replace=FALSE, ties = FALSE)
s2 <- s_trim[c(mout$index.treated, mout$index.control), ]

# estimate propensity score again
s2$ps <- probability_forest(X = s2[, covar], 
                            Y = as.factor(s2$tr), seed = 1234, num.trees = 4000)$predictions[,2]

table(s2$tr)
range(s2$ps)
round(quantile(s2$ps[which(s2$ps > 0.6)], probs = seq(0, 1, 0.1)),3)

#                                         $$\                                                                                                                                                                                                                                                                                                                           $$\                      $$\                                     
#                                         $$ |                                                                                                                                                                                                                                                                                                                          \$$\                     $$ |                                    
#  $$$$$$\ $$\    $$\  $$$$$$\   $$$$$$\  $$ | $$$$$$\   $$$$$$\                                                                                                                                                                                                                                                                                                         \$$\                    $$ |                                    
# $$  __$$\\$$\  $$  |$$  __$$\ $$  __$$\ $$ | \____$$\ $$  __$$\                                                                                                                                                                                                                                                                                                         \$$\                   \__|                                    
# $$ /  $$ |\$$\$$  / $$$$$$$$ |$$ |  \__|$$ | $$$$$$$ |$$ /  $$ |                                                                                                                                                                                                                                                                                                         \$$\                  $$\                                     
# $$ |  $$ | \$$$  /  $$   ____|$$ |      $$ |$$  __$$ |$$ |  $$ |                                                                                                                                                                                                                                                                                                          \$$\                 $$ |                                    
# \$$$$$$  |  \$  /   \$$$$$$$\ $$ |      $$ |\$$$$$$$ |$$$$$$$  |                                                                                                                                                                                                                                                                                                           \$$\                $$ |                                    
#  \______/    \_/     \_______|\__|      \__| \_______|$$  ____/                                                                                                                                                                                                                                                                                                             \__|$$$$$$\ $$$$$$\\__|                                    
#                                                       $$ |                                                                                                                                                                                                                                                                                                                      \______|\______|                                       
#                                                       $$ |                                                                                                                                                                                                                                                                                                                                                                             
#                                                       \__|                                                                                                                                                                                                                                                                                                                                                                             



pdf("graphs/s2_odds.pdf", width = 5.5, height = 5.5)
plot_hist(s, "ps", "tr", breaks = 30, odds = TRUE, xlim = c(-3, 3), ylim = c(-0.2, 0.2))
graphics.off()


pdf("graphs/s2_odds_trim.pdf", width = 5.5, height = 5.5)
s2$ps <- probability_forest(X = s2[, covar], 
                            Y = as.factor(s2$tr), seed = 1234)$predictions[,2]
plot_hist(s2, "ps", "tr", breaks = 30, odds = TRUE, xlim = c(-3, 3), ylim = c(-0.2, 0.2))
graphics.off()


## PS

pdf("graphs/s2_ps.pdf", width = 5.5, height = 5.5)
s$ps <- probability_forest(X = s[, covar], 
                           Y = as.factor(s$tr), seed = 1234)$predictions[,2]
plot_hist(s, "ps", "tr", breaks = 30, odds = FALSE, xlim = c(0, 1), ylim = c(-0.2, 0.2))
graphics.off()

pdf("graphs/s2_ps_trim.pdf", width = 5.5, height = 5.5)
s2$ps <- probability_forest(X = s2[, covar], 
                            Y = as.factor(s2$tr), seed = 1234)$predictions[,2]
plot_hist(s2, "ps", "tr", breaks = 30, odds = FALSE, xlim = c(0, 1), ylim = c(-0.2, 0.2))
graphics.off()



#                       $$\     $$\                          $$\               
#                       $$ |    \__|                         $$ |              
#  $$$$$$\   $$$$$$$\ $$$$$$\   $$\ $$$$$$\$$$$\   $$$$$$\ $$$$$$\    $$$$$$\  
# $$  __$$\ $$  _____|\_$$  _|  $$ |$$  _$$  _$$\  \____$$\\_$$  _|  $$  __$$\ 
# $$$$$$$$ |\$$$$$$\    $$ |    $$ |$$ / $$ / $$ | $$$$$$$ | $$ |    $$$$$$$$ |
# $$   ____| \____$$\   $$ |$$\ $$ |$$ | $$ | $$ |$$  __$$ | $$ |$$\ $$   ____|
# \$$$$$$$\ $$$$$$$  |  \$$$$  |$$ |$$ | $$ | $$ |\$$$$$$$ | \$$$$  |\$$$$$$$\ 
#  \_______|\_______/    \____/ \__|\__| \__| \__| \_______|  \____/  \_______|



# full dataset
outcomes <- c(paste0("x", 3:5), paste0("y", 1:1))
est <- vector("list", length(outcomes))
names(est) <- outcomes
for (i in 1:length(outcomes)) {
  est[[i]] <- estimate_all(s, outcomes[i], "tr", covar,
                           methods = c("diff", "aipw_grf"))
  cat(i, "\n")
}
# matched dataset
est2 <- vector("list", length(outcomes))
names(est2) <- outcomes
for (i in 1:length(outcomes)) {
  est2[[i]] <- estimate_all(s2, outcomes[i], "tr", covar,
                            methods = c("diff", "aipw_grf"))
  cat(i, "\n")
}


pdf("graphs/s2_dyn.pdf", width = 7, height = 5)
par(mar = c(4, 4, 1, 2))
plot(1, xlim = c(0.7, 4.3), ylim = c(-.1, .2), type = "n", axes = FALSE, 
     ylab = "Effects on Markups (percentage)", xlab = "Year Relative to Contract 2006-2021")
box(); axis(2)
axis(1, at = 1:4, labels = c(-3:0))

abline(h = 0, v= 3.5, col = "gray60", lty = 2, lwd = 2)
for (i in 1:4) {
  # full dataset with DIM
  lines(c(i-0.075, i-0.075), est[[i]][1,3:4], lty = 1, lwd = 2, col = "grey60") # CI
  points(i-0.075, est[[i]][1,1], pch = 18, col = "grey60", cex = 1.2)  # Coef 
  # full dataset
  lines(c(i+0.075, i+0.075), est[[i]][2,3:4], lwd = 2) # CI
  points(i+0.075, est[[i]][2,1], pch = 16)  # Coef     
}
legend("bottomleft", legend = c("DIM", "AIPW"), lwd = 2, cex = 1.2,
       lty = 1, pch = c(18, 16), col = c("grey60", "black"), bty = "n")
graphics.off()

pdf("graphs/s2_dyn2.pdf", width = 7, height = 5)
par(mar = c(4, 4, 1, 2))
plot(1, xlim = c(0.7, 4.3), ylim = c(-.1, .2), type = "n", axes = FALSE, 
     ylab = "Effects on Markups (percentage)", xlab = "Year Relative to Contract 2006-2021")
box(); axis(2)
axis(1, at = 1:4, labels = c(-3:0))

abline(h = 0, v= 3.5, col = "gray60", lty = 2, lwd = 2)
for (i in 1:4) {
  # full dataset with DIM
  lines(c(i-0.15, i-0.15), est[[i]][1,3:4], lty = 1, lwd = 2, col = "grey60") # CI
  points(i-0.15, est[[i]][1,1], pch = 18, col = "grey60", cex = 1.2)  # Coef 
  # full dataset
  lines(c(i, i), est[[i]][2,3:4], lwd = 2) # CI
  points(i, est[[i]][2,1], pch = 16)  # Coef 
  # matched dataset
  lines(c(i+0.15, i+0.15), est2[[i]][2,3:4], col = "maroon", lwd = 1.5) # CI
  points(i+0.15, est2[[i]][2,1], col = "maroon", pch = 17)  # Coef  
}
legend("bottomleft", legend = c("DIM,  Full (261 : 467)", "AIPW, Full (261 : 467)", 
                              "AIPW, PS Matched (147 : 147)"), lwd = 2,
       lty = c(1, 1, 1), pch = c(18, 16, 17), 
       col = c("grey50", "black", "maroon"), bty = "n")
graphics.off()

#                     $$\     $$\     
#                     $$ |    $$ |    
#  $$$$$$$\ $$$$$$\ $$$$$$\ $$$$$$\   
# $$  _____|\____$$\\_$$  _|\_$$  _|  
# $$ /      $$$$$$$ | $$ |    $$ |    
# $$ |     $$  __$$ | $$ |$$\ $$ |$$\ 
# \$$$$$$$\\$$$$$$$ | \$$$$  |\$$$$  |
#  \_______|\_______|  \____/  \____/ 


library("grf")

## Original Data
data <- s
treat <- "tr"
ntr <- sum(data[, treat] == 1)
tau <- matrix(NA, ntr, length(outcomes))
att <- rep(NA, ntr)
for (i in 1:length(outcomes)) {
  Y <- outcomes[i]
  catt.out <- catt(data, Y, treat, covar)
  tau[, i] <- catt.out$catt
  att[i] <- catt.out$att[1]     
  cat(i, "\n")
}

pdf("graphs/s2_catt.pdf", width = 7, height = 5)
par(mar = c(4, 4, 1, 2))
plot(1, xlim = c(0.7, 5.3), ylim = c(-1, 1), type = "n", axes = FALSE, 
     ylab = "Effects on Markups (percentage)", xlab = "Year Relative to Contract")
box(); axis(2)
axis(1, at = 1:4, labels = c(-3:0))
abline(h = 0, v= 3.5, col = "gray60", lty = 2, lwd = 1.5)
for (i in 1:length(outcomes)) {
  dens <- density(tau[,i], bw = 0.5)
  polygon(i + dens$y, dens$x, col = "#AAAAAA50", border = NA)
  lines(i + dens$y, dens$x, lwd = 1) 
  points(i+0.01,  att[i], pch = 16, cex = 0.8)  # Coef
}
graphics.off()



## Trimmed Data
data <- s2
treat <- "tr"
ntr <- sum(data[, treat] == 1)
tau <- matrix(NA, ntr, length(outcomes))
att <- rep(NA, ntr)
for (i in 1:length(outcomes)) {
  Y <- outcomes[i]
  catt.out <- catt(data, Y, treat, covar)
  tau[, i] <- catt.out$catt
  att[i] <- catt.out$att[1]     
  cat(i, "\n")
}

pdf("graphs/s2_catt_trim.pdf", width = 7, height = 5)
par(mar = c(4, 4, 1, 2))
plot(1, xlim = c(0.7, 5.3), ylim = c(-1, 1), type = "n", axes = FALSE, 
     ylab = "Effects on Markups (percentage)", xlab = "Year Relative to Contract")
box(); axis(2)
axis(1, at = 1:4, labels = c(-3:0))
abline(h = 0, v= 3.5, col = "gray60", lty = 2, lwd = 1.5)
for (i in 1:length(outcomes)) {
  dens <- density(tau[,i], bw = 0.5)
  polygon(i + dens$y, dens$x, col = "#AAAAAA50", border = NA)
  lines(i + dens$y, dens$x, lwd = 1) 
  points(i+0.01,  att[i], pch = 16, cex = 0.8)  # Coef
}
graphics.off()


#                                          $$\     $$\ $$\           
#                                          $$ |    \__|$$ |          
#  $$$$$$\  $$\   $$\  $$$$$$\  $$$$$$$\ $$$$$$\   $$\ $$ | $$$$$$\  
# $$  __$$\ $$ |  $$ | \____$$\ $$  __$$\\_$$  _|  $$ |$$ |$$  __$$\ 
# $$ /  $$ |$$ |  $$ | $$$$$$$ |$$ |  $$ | $$ |    $$ |$$ |$$$$$$$$ |
# $$ |  $$ |$$ |  $$ |$$  __$$ |$$ |  $$ | $$ |$$\ $$ |$$ |$$   ____|
# \$$$$$$$ |\$$$$$$  |\$$$$$$$ |$$ |  $$ | \$$$$  |$$ |$$ |\$$$$$$$\ 
#  \____$$ | \______/  \_______|\__|  \__|  \____/ \__|\__| \_______|
#       $$ |                                                         
#       $$ |                                                         
#       \__|                                                         


covar <- c("year", "pp_history","dpp_mean", "sales", "costs", "assets", "wages", "empl", "nace2", "sector", "x1", "x2","x3")

#hist(s2$xearn.avg, breaks = 50)
#hist(s2$yearn.avg, breaks = 50)

library(qte)

qte.irs.pre.unadj <- est_qte("xmu.avg", treat, NULL, data = s)
qte.irs.pst.unadj <- est_qte("ymu.avg", treat, NULL, data = s)
qte.irs.pre.trim.unadj <- est_qte("xmu.avg", treat, NULL, data = s2)
qte.irs.pst.trim.unadj <- est_qte("ymu.avg", treat, NULL, data = s2)

qte.irs.pre.adj <- est_qte("xmu.avg", treat, covar, data = s)
qte.irs.pst.adj <- est_qte("ymu.avg", treat, covar, data = s)
qte.irs.pre.trim.adj <- est_qte("xmu.avg", treat, covar, data = s2)
qte.irs.pst.trim.adj <- est_qte("ymu.avg", treat,covar, data = s2)

save(qte.irs.pre.unadj, qte.irs.pst.unadj, qte.irs.pre.trim.unadj, qte.irs.pst.trim.unadj,
     qte.irs.pre.adj, qte.irs.pst.adj, qte.irs.pre.trim.adj, qte.irs.pst.trim.adj,
     file = "output/qte_s2.rds")

####################
# Plot QTE
####################

load("output/qte_s2.rds")

pdf("graphs/s2_qte_pre.pdf", width = 6, height = 5)
ylim <- c(-0.1, .3)
plot_qte(qte.irs.pre.adj, qte.irs.pre.unadj, ylim = ylim, main = "Pre-Contract Avg. Markup")
legend("bottomleft", legend = c("Unadjusted", "Adjusted"), 
       lty = 1, pch = c(17, 16), col = c(2, 1), bty = "n")
graphics.off()

pdf("graphs/s2_qte_pst.pdf", width = 6, height = 5)
plot_qte(qte.irs.pst.adj, qte.irs.pst.unadj, ylim = ylim, main = "Post-Contract Avg. Markup")
legend("bottomleft", legend = c("Unadjusted", "Adjusted"), 
       lty = 1, pch = c(17, 16), col = c(2, 1), bty = "n")
graphics.off()


pdf("graphs/s2_qte_pre_trim.pdf", width = 6, height = 5)
plot_qte(qte.irs.pre.trim.adj, qte.irs.pre.unadj, ylim = ylim, main = "Pre-Contract Avg. Markup")
legend("bottomleft", legend = c("Unadjusted", "Adjusted"), 
       lty = 1, pch = c(17, 16), col = c(2, 1), bty = "n")
graphics.off()

pdf("graphs/s2_qte_pst_trim.pdf", width = 6, height = 5)
plot_qte(qte.irs.pst.trim.adj, qte.irs.pst.unadj, ylim = ylim, main = "Post-Contract Avg. Markup")
legend("bottomleft", legend = c("Unadjusted", "Adjusted"), 
       lty = 1, pch = c(17, 16), col = c(2, 1), bty = "n")
graphics.off()






