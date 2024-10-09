source("https://github.com/xuyiqing/lalonde/blob/main/tutorial/functions.R?raw=TRUE")
library(haven)
d <- read_dta("data.dta")
save(d, file = "data/data.RData")
load("data/data.RData")
d<-data.frame(d)
d<-na.omit(d)
head(d)
summary(d)
names(d)
d$tr <- d$female 
d$tr1 <- ifelse(d$female==1 & d$inform== 1, 1, 0) 
d$tr2 <- ifelse(d$tr1 == 0 & d$female == 1, 1, 0)
d$co1 <- ifelse(d$female == 0 & d$inform== 1, 1, 0)
d$co2 <- ifelse(d$co1 == 0 & d$female == 0, 1, 0)

table(d$tr1, d$tr2)
table(d$tr1, d$co1)
table(d$tr2, d$co2)

treat <- "female"
covar <- c("num_correct", "belief_gap","overall_gpa_non","grade","frpl", "minority")
s1 <- subset(d, tr1 == 1 | co1 == 1 ) 
s2 <- subset(d, tr2 == 1 | co2 == 1 ) 

par(mfrow = c(1,2))

s1_ps <- assess_overlap(data = s1, treat = treat, cov = covar, ylim = c(-0.4, 0.4), breaks = 30)
s2_ps <- assess_overlap(data = s2, treat = treat, cov = covar, ylim = c(-0.2, 0.2), breaks = 30)

trim <- function(data, ps = "ps_assoverlap", threshold = .9) {
  sub <- data[which(data[, ps] < threshold), ]
  
  return(sub)
}
# matching
s1_ps_match <- psmatch(data = trim(s1_ps), Y = "bucket_", treat = treat, cov = covar)
s2_ps_match <- psmatch(data = trim(s2_ps), Y = "bucket_", treat = treat, cov = covar)

# assess overlap again
ss1 <- assess_overlap(data = s1_ps_match, treat = treat, cov = covar, breaks = 30)
ss2 <- assess_overlap(data = s2_ps_match, treat = treat, cov = covar, breaks = 30)

love.plot(s1, ss1, treat = treat, covar = covar, title = "Covariate Balance trim")
love.plot(s2, ss2, treat = treat, covar = covar, title = "Covariate Balance trim")

set.seed(1234)
out1 <- estimate_all(s1_ps_match, "bucket_", "tr", covar)
out2 <- estimate_all(s2_ps_match, "bucket_", "tr", covar)
plot_coef(out2, main = "uninform", main.pos = 3)
plot_coef(out1, main = "inform", main.pos = 3)