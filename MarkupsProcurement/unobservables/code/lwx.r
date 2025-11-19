## Replication file: Liu, Wang, Xu (2022)
# Empirical example: Hainmueller and Hangartner (2015)
rm(list=ls())
gc()
require(fect)
library(grid)
library(gridExtra)
library(ggplot2)
library(fastplm)
library(panelView)
library(dplyr)
o.data <- read.csv("panel.csv")
dim <- nrow(o.data)
dim(o.data)
Y <- "Y"
unit <- "id"
period <- "year"
FE <- c(unit, period)
cl <- unit
seed <- 42
D<- "D"
d <- o.data[complete.cases(o.data[,c(Y,D,FE, "pp_dummy")]),]
d<- d[,c(Y,D,FE,"pp_dummy")]
d <- d[order(d[,unit], d[,period]),]
fect.formula <- as.formula(paste0(Y,"~",D))
dim(d)
df <- as.data.frame(d %>% group_by(id) %>% mutate(treatment_mean = mean(D,na.rm = TRUE)))
df.use <- df[which(df$treatment_mean<1),]
nboots <- 100

## fixed-effects
set.seed(1234)
out.fe <- fastplm(formula = as.formula(fect.formula), data = d,
                  index=FE, se = 1, vce = "boot", nboots = nboots, cluster = unit)
summary(out.fe)

######################################
# Do not consider carry over effects
######################################
library(paneltools)
d <- get.cohort(d,D="pp_dummy",index=c("id","year"),start0 = TRUE)

out.fect <- fect(formula = as.formula(fect.formula), data = d, method = "fe",min.T0 = 5,
 index=FE, tol = 1e-3, se = 1, nboots = nboots, r = 0,
 loo = 1, group = 'Cohort',
 CV = FALSE, force = "two-way", parallel = 1)

out.ife <- fect(formula = as.formula(fect.formula), data = d, method = "ife", 
  index=FE, tol = 1e-3, se = 1, nboots = nboots, min.T0 = 5,cv.treat = TRUE, r=2,
  loo = 1, group = 'Cohort',
  CV = FALSE, force = "two-way", parallel = 1)

out.mc <- fect(as.formula(fect.formula), data = d, method = "mc",nlambda=50,
 index=FE, tol = 1e-4, se = 1, nboots = nboots, min.T0 = 5, cv.treat = TRUE,
 loo = 1, group = 'Cohort', 
 CV = 1, force = "two-way", parallel = 1)

out.mc$eigen.all-out.mc$lambda.cv 
## Placebo tests

out.fect.p <- fect(formula = as.formula(fect.formula), data = d, method = "fe",
 index=FE, tol = 1e-3, se = 1, nboots = nboots, r = 0, min.T0 = 3,
 CV = FALSE, force = "two-way", parallel = 1,   placeboTest = 1, 
 placebo.period = c(-2, 0))

out.ife.p <- fect(formula = as.formula(fect.formula), data = d, method = "ife",
  index=FE, tol = 1e-3, se = 1, nboots = nboots, r = 2, min.T0 = 3,
  CV = 0, force = "two-way", parallel = 1,   
  placeboTest = 1, placebo.period = c(-2, 0))

out.mc.p <- fect(formula = as.formula(fect.formula), data = d, method = "mc",
 index=FE, tol = 1e-4, se = 1, nboots = nboots, lambda = out.mc$lambda.cv, 
 CV = 0, force = "two-way", parallel = 1,   min.T0 = 3, 
 placeboTest = 1, placebo.period = c(-2, 0))


## Tests for carry-over effects

out.fect.c <- fect(formula = as.formula(fect.formula), data = d, method = "fe",
 index=FE, tol = 1e-3, se = 1, nboots = nboots, force = "two-way", min.T0 = 3,
 parallel = 1,   carryoverTest = 1, carryover.period = c(1, 5))

out.ife.c <- fect(formula = as.formula(fect.formula), data = d, method = "ife",
  index=FE, tol = 1e-3, se = 1, nboots = nboots, r = 2,min.T0 = 3,
  CV = 0, force = "two-way", parallel = 1,   
  carryoverTest = TRUE, carryover.period = c(1,5))

out.mc.c <- fect(formula = as.formula(fect.formula), data = d, method = "mc",
 index=FE, tol = 1e-4, se = 1, nboots = nboots, lambda = out.mc$lambda.cv, 
 CV = 0, force = "two-way", parallel = 1,  min.T0 = 3,
 carryoverTest = 1, carryover.period = c(1, 5))


# Save
save(d, fect.formula, out.fe, 
  out.fect, out.fect.p, out.fect.c,
  out.ife, out.ife.p, out.ife.c,
  out.mc, out.mc.p, out.mc.c,
  file = "ex2.RData")

###############################
## Plotting
###############################

load("ex_FM2015.RData")
#ex2
library(ggplot2)
require(fect)
library(grid)
library(gridExtra)

Y05 <- read.csv("Y05.csv")
## Outcome
unit <- "id"
period <- "year"
FE <- c(unit, period)
p.outcome <- panelview(as.formula(fect.formula), data = df.use, index = FE, theme.bw = TRUE,display.all = TRUE,
          type = "outcome", main = "", xlab = "Year", ylab = "Specific Grants (log)", 
          legend.labs = c("w/o Government Contract", "w/ Government Contract"), by.group = TRUE)
ggsave('outcome2.pdf', p.outcome, width = 8, height = 8)


## Treatment
p.treat <- panelview(as.formula(fect.formula), data = df.use, index = FE, display.all = TRUE,
          pre.post = FALSE, by.timing = TRUE, xlab = "Year", ylab = "Construction Firm", 
          legend.labs = c("w/o Government Contract", "w/ Government Contract"),
          background= "white", axis.lab = "time",
          gridOff = TRUE) 
p.treat <- p.treat + theme(axis.text.x = element_text(angle = 90))
ggsave('treat2.pdf', p.treat, width = 8, height = 8)
mean(Y05$pp_dummy)
######################################
# Do not consider carry over effects
######################################


## Gap plots
d_ylim <- c(-0.025, 0.175)
xlab <- "Year(s) Since Government Contract"
p1.gap <- plot(out.fect, type = "gap", ylab = "The Effect of Public Procurement on Markups (log)", 
  ylim = d_ylim, proportion = 0.1, xlab = xlab,
   main = "FEct", theme.bw = TRUE, cex.text = 0.8)

p2.gap <- plot(out.ife, type = "gap", ylab = "The Effect of Public Procurement on Markups (log)", 
  ylim = d_ylim, proportion = 0.1, xlab = xlab,
    main = "IFEct", theme.bw = TRUE, cex.text = 0.8)

p3.gap <- plot(out.mc, type = "gap", ylab = "The Effect of Public Procurement on Markups (log)", 
  ylim = d_ylim, proportion = 0.1, xlab = xlab,
    main = "MC", theme.bw = TRUE, cex.text = 0.8)
margin = theme(plot.margin = unit(c(1,1,1,1), "line"))
p.gap <- grid.arrange(grobs = lapply(list(p1.gap, p2.gap, p3.gap), "+", margin),
  ncol = 3, widths = c (1, 1, 1))
ggsave('gap.pdf', p.gap, width = 17, height = 6)

## Placebo plots
stats.labs <- c("t test p-value","TOST p-value")
xlab <- "Year(s) Since Government Contract"
p1.placebo <- plot(out.fect.p, ylab = "The Effect of Partisan Alignment on Specific Grants (log)", 
  placeboTest = 1, proportion = 0.1, xlab = xlab, ylim = d_ylim, main = "FEct", theme.bw = TRUE, 
  stats = c("placebo.p","equiv.p"), stats.labs = stats.labs) 

p2.placebo <- plot(out.ife.p, ylab = "The Effect of Partisan Alignment on Specific Grants (log)", 
  placeboTest = 1, proportion = 0.1, xlab = xlab, ylim = d_ylim, main = "IFEct", theme.bw = TRUE, 
  stats = c("placebo.p","equiv.p"), stats.labs = stats.labs) 

p3.placebo <- plot(out.mc.p, ylab = "The Effect of Partisan Alignment on Specific Grants (log)", 
  placeboTest = 1, proportion = 0.1, xlab = xlab, ylim = d_ylim, main = "MC", theme.bw = TRUE, 
  stats = c("placebo.p","equiv.p"), stats.labs = stats.labs) 

margin = theme(plot.margin = unit(c(1,1,1,1), "line"))
p.placebo <- grid.arrange(grobs = lapply(list(p1.placebo, p2.placebo, p3.placebo), "+", margin),
  ncol = 3, widths = c (1, 1, 1))
ggsave('placebo.pdf', p.placebo, width = 17, height = 6)


## Equivalence plots
stats.labs <- c("F test p-value","TOST max p-value")
xlab <- "Year(s) Since Government Contract"
p1.equiv <- plot(out.fect, type = "equiv", ylab = "Average Prediction Error", ylim = d_ylim, main = "FEct", theme.bw = TRUE, legendOff = TRUE, loo = 1, xlab = xlab,
  stats.labs = stats.labs) 

p2.equiv <- plot(out.ife, type = "equiv", ylab = "Average Prediction Error", ylim = d_ylim, main = "IFEct", theme.bw = TRUE, legendOff = TRUE, loo = 1, xlab = xlab,
  stats.labs = stats.labs) 

p3.equiv <- plot(out.mc, type = "equiv", ylab = "Average Prediction Error", ylim = d_ylim, main = "MC", theme.bw = TRUE, legendOff = TRUE, loo = 1, xlab = xlab,
  stats.labs = stats.labs) 

margin = theme(plot.margin = unit(c(1,1,1,1), "line"))
p.equiv <- grid.arrange(grobs = lapply(list(p1.equiv, p2.equiv, p3.equiv), "+", margin),
  ncol = 3, widths = c (1, 1, 1))
ggsave('equiv.pdf', p.equiv, width = 17, height = 6)


## Carryover effects
c_ylim <- c(-0.075,0.2)
xlab <- "Year(s) Since Government Contract"
stats.labs <- c("  t test p-value","TOST p-value")
stats.pos <- c(.75, 0.21)
p1 <- plot(out.fect.c, type = "exit", 
  ylab = "The Effect of Partisan Alignment on Specific Grants (log)", 
  carryoverTest = 1, proportion = 0.1, xlab = xlab, stats.pos = stats.pos, ylim = c_ylim, main = "FEct", theme.bw = TRUE, 
  stats = c("carryover.p","equiv.p"), stats.labs = stats.labs) 

p2 <- plot(out.ife.c, type = "exit",
  ylab = "The Effect of Partisan Alignment on Specific Grants (log)", 
  carryoverTest = 1, proportion = 0.1, xlab = xlab, stats.pos = stats.pos, ylim = c_ylim, main = "IFEct", theme.bw = TRUE, 
  stats = c("carryover.p","equiv.p"), stats.labs = stats.labs) 

p3 <- plot(out.mc.c, type = "exit",
  ylab = "The Effect of Partisan Alignment on Specific Grants (log)", 
  carryoverTest = 1, proportion = 0.1, xlab = xlab, stats.pos = stats.pos,
  xlim = c(-2,3), ylim = c_ylim, main = "MC", theme.bw = TRUE, 
  stats = c("carryover.p","equiv.p"), stats.labs = stats.labs) 

margin = theme(plot.margin = unit(c(1,1,1,1), "line"))
p.carryover <- grid.arrange(grobs = lapply(list(p1, p2, p3), "+", margin),
  ncol = 3, widths = c (1, 1, 1))
ggsave('carryover.pdf', p.carryover, width = 17, height = 6)

out.ife$est.eff.calendar
 

out.fect$est.avg
out.ife$est.avg
out.mc$est.avg

out.fe <- fastplm(formula = as.formula(fect.formula), data = d,
                  index=FE, se = 1, vce = "boot", nboots = nboots, cluster = unit)
summary(out.fe)
