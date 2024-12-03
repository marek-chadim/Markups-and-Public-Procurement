# To install synthdid package, uncomment the next lines as appropriate.
# install.packages("devtools")  # if you don't have this installed yet.
# devtools::install_github("synth-inference/synthdid")
gc()
rm(list=ls())
library(RCurl)
library(did) # DID
library(Synth) # SC
library(synthdid) # SDID
library(cowplot) # summary stats grids
library(ggplot2) # plots
library(R.matlab) # data for SDID
library(dplyr) # piping
library(tinytex) # knitting the file
library(bookdown) # knitting
library(tidyr) # converting from long to wide dataset
library(estimatr) # for lm_robust
library(fBasics) # summary stats table
library(knitr) # kable
library(plm) # linear panel data
library(tidyverse) 
library(broom)
library(magrittr)
library(lmtest) # for coef_test() and robust std error calculation
library(multiwayvcov) #calculate std error TWFE
set.seed(42)


# Data Setup for Diff-In-Diff and Synthetic Control

# read in data
data <- read.csv("2018.csv") 
data$X <- NULL  # removing X column

# fill out these by hand
# these variables are important for summary plots and analysis
outcome.var <- "Y"
predictors <- c("betahat_tl", "empl_num") # if any
time.var <- c("year")
unit.var <- c("i")
treatment.year <- 2018
treated.unit <- 5
pretreat.period <- c(2006:2017)
time.period <- c(2006:2021)
control.units <- c(1, 11, 22, 23, 25)

# if using special predictors which are
# certain pretreatment years of the outcome variable used to 
# more accurately predict the synthetic unit
special.years <- c(2010, 2014, treatment.year)
special.predictors <- list(        
    list("outcome", special.years[1], c("mean")), 
    list("outcome", special.years[2], c("mean")),
    list("outcome", special.years[3], c("mean"))
    )

# rename variables in the dataset
data <- data %>% rename(outcome = !!sym(outcome.var),
                time = !!sym(time.var),
                unit = !!sym(unit.var))
# now the outcome, time, and unit variables are:
outcome.var <- "outcome"
time.var <- c("time")
unit.var <- c("unit")

allvars <- c("outcome", predictors)


# Data Setup for Synthetic Diff-in-Diff (synthdid package requires us to change the data structure)
# set up empty dataframe
data.sdid <- data.frame()

# first row = numbers for each unit
data.sdid <- data.frame(unit.no = unique(data$unit))


# next is covariate data = predictors and special predictors
# predictors
# will save each dataset later
for (i in 1:length(predictors)){
  covariate_column <- data %>% 
  group_by(unit) %>%
  summarize(predictor_mean = mean(!!sym(predictors[i]), na.rm = T)) %>% 
  dplyr::select(predictor_mean)
  data.sdid <- cbind(data.sdid, covariate_column)
}

# special.predictors
special_predictors_data <- data %>%
  dplyr::filter(time %in% special.years) %>%
  dplyr::select(unit, time, outcome)
# convert from long to wide dataset
special_predictors_data <- spread(special_predictors_data, time, outcome)[,-1]
data.sdid <- cbind(data.sdid, special_predictors_data)

# next is the outcome variable for each state in the time period
outcome_data <- data %>% dplyr::select(unit, time, outcome)
outcome_data <- spread(outcome_data, time, outcome)[,-1]
data.sdid <- cbind(data.sdid, outcome_data)

# transpose data
data.sdid <- t(data.sdid)

# add other data setup variables for SDID
UNIT <- data.sdid[1,] # unit numbers

X.attr <- t(data.sdid[2:6,]) # covariate data
Y <- t(data.sdid[7:22,]) # outcome variable data
colnames(Y) <- time.period # colname = year
rownames(Y) <- UNIT # rowname = unit number
units <- function(...) { which(UNIT %in% c(...)) }
Y <- Y[c(setdiff(1:nrow(Y), units(treated.unit)), units(treated.unit)), ] # make sure treated unit is in the last row
T0 <- length(pretreat.period) # number of pre-treatment years
N0 <- nrow(Y)-1

# number of observations
n <- dim(data)[1]
# number of units
N <- length(unique(data$unit))
# number of time periods
T <- length(unique(data$time))

# check that n = N x T
# if TRUE, then the data is balanced
# if FALSE, then the data is unbalanced
n == N*T

table(data$time) 
table(data$unit)

# This functions comes from the plm package.
# If TRUE, then the data is balanced.
# If FALSE, then the data is unbalanced. 
is.pbalanced(data)

distributions <- list()

for (i in 1:length(allvars)) {
  distributions[[i]] <- ggplot(data) +
  aes(x = !!sym(allvars[i])) +
  geom_histogram(fill = "#D55E00") + 
  labs(title = paste0("Distribution of \n ", allvars[i])) + 
  theme(plot.title = element_text(size=20, hjust = 0.5, face="bold"),
         axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"))
}

plot_grid(distributions[[1]], distributions[[2]],
          distributions[[3]])


pred.vs.outcome <- list()

for (i in 1:length(predictors)){
  pred.vs.outcome[[i]] <- 
    ggplot(data, 
           aes(x = !!sym(predictors[i]), 
               y = outcome)) +
    geom_point(aes(color = time)) +
    geom_smooth() + 
  labs(title = paste0("Outcome vs ", predictors[i])) + 
  theme(plot.title = element_text(size=20, hjust = 0.5, face="bold"),
         axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"))
}

plot_grid(pred.vs.outcome[[1]], pred.vs.outcome[[2]])



# Treated Unit
p1 <- data %>% dplyr::filter(unit == treated.unit) %>%
  ggplot(.,aes(x = time, y = outcome)) + 
  geom_line() + 
  geom_vline(xintercept = treatment.year, color = "red") + 
  labs(title = "Outcome of Treated Unit \n Over Time") + 
  theme(plot.title = element_text(size=20, hjust = 0.5, face="bold"),
         axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"))

# Average of Control Units
p2 <- data %>% group_by(time) %>% dplyr::filter(unit != treated.unit) %>%
  mutate(avg.outcome.var = mean(outcome)) %>%
  ggplot(., aes(x = time, y = avg.outcome.var)) + 
  geom_line() +
  geom_vline(xintercept = treatment.year, color = "red") + 
  labs(title = "Average Outcome of Control Units \n Over Time") + 
  theme(plot.title = element_text(size=20, hjust = 0.5, face="bold"),
         axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"))
  
# Control Units
p3 <- data %>% dplyr::filter(unit != treated.unit) %>%
  ggplot(., aes(x = time, y = outcome, 
                      color = unit, group = unit)) + 
  geom_line() +
  geom_vline(xintercept = treatment.year, color = "red") + 
  labs(title = "Outcome of All Control Units \n over Time") + 
  theme(plot.title = element_text(size=20, hjust = 0.5, face="bold"),
         axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"))

plot_grid(p1, p2, p3)


# add an indicator variable to identify the unit exposed to the treatment
data$treat <- ifelse(data$unit == treated.unit, 1, 0)
# add an indicator to identify the post treatment period
data$post <-  ifelse(data$time >= treatment.year, 1, 0)

# running the regression
# the independent variables are treat, post, and treat*post
did.reg <- lm_robust(outcome ~ treat*post, data = data)

# print the regression
did.reg

# use the regression cofficients to calculate the means of the outcome
# source: https://mixtape.scunning.com/difference-in-differences.html
# mean outcome for treated unit in the pre-treatment period
before_treatment <- as.numeric(did.reg$coefficients['(Intercept)'] + did.reg$coefficients['treat']) # preperiod for treated
# mean outcome for treated unit in the post-treatment period
after_treatment <- as.numeric(sum(did.reg$coefficients))
# diff-in-diff estimate
diff_diff <- as.numeric(did.reg$coefficients['treat:post'])


# creating 2x2 DID table for the plot
data_diff <- data %>% 
  group_by(post, treat) %>% 
  summarize(mean_outcome = mean(outcome))

# plot the means of the outcome for pre-treatments and post-treatment periods and for the treated and control group
# then use annotate() to create a dashed line parallel to the control group, 
# a dotted blue line for the size of the treatment effect, 
# and the treatment estimate label ATT
# source: https://api.rpubs.com/andrewheiss/did
ggplot(data_diff, aes(x = post, y = mean_outcome, color = as.factor(treat))) +
  geom_point() + 
  geom_line(aes(group = as.factor(treat))) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = before_treatment, yend = after_treatment - diff_diff,
           linetype = "dashed", color = "grey50") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = after_treatment, yend = after_treatment - diff_diff,
           linetype = "dotted", color = "blue") +
  annotate(geom = "label", x = 1, y = after_treatment - (diff_diff / 2), 
           label = "ATT", size = 3)




# added a column to identify when the treatment first started
data$first.treat <- ifelse(data$unit == treated.unit, treatment.year, 0)

# estimating group-time average treatment effects without covariates
out <- att_gt(yname = "outcome", # outcome variable
              gname = "first.treat", # year treatment was first applied aka the group identifier
              idname = "unit", # unit identifier
              tname = "time", # time variable
              xformla = ~1,
              data = data, # data set
              est_method = "reg")

# summarize the results
# shows the average treatment effect by group and time
summary(out)

# plot the results
# set ylim so that it is equidistant from zero
ggdid(out, ylim = c(-.1, .3), xgap =1)

# estimating group-time average treatment effects with covariates
out.X <- att_gt(yname = "outcome", # outcome variable
              gname = "first.treat", # year treatment was first applied aka the group identifier
              idname = "unit", # unit identifier
              tname = "time", # time variable
              xformla = ~ 1, # can use another covariate if needed
              data = data, # data set
              est_method = "reg")

# plot the results
ggdid(out.X, ylim = c(-.1, .3), xgap =1)
# calculates a weighted average of all group-time average treatment effects
# with the weights proportional to the group size
out.simple <- aggte(out.X, type = "simple")

summary(out.simple)
# averages the group-time average treatment effects
# into average treatment effects at different lengths of exposure to the treatment
out.dynamic <- aggte(out.X, type = "dynamic")
summary(out.dynamic)
ggdid(out.dynamic)

# Create the X0, X1, Z0, Z1, Y0plot, Y1plot matrices from panel data 
# Provide the inputs needed for synth()
dataprep.out <-
  dataprep(
  foo = data,                  # dataframe with the data
  predictors = predictors, 
  predictors.op = c("mean"),        # method to be used on predictors
  dependent = "outcome",         # outcome variable
  unit.variable = "unit",       # variable associated with the units
  time.variable = "time",        # variable associated with time
  special.predictors = special.predictors,  # additional predictors
  treatment.identifier = treated.unit,         # the treated unit
  controls.identifier = control.units, # other states as the control units
  time.predictors.prior = pretreat.period, # pre-treatment period
  time.optimize.ssr = pretreat.period, # MSPE minimized for pre-treatment period
  time.plot = time.period       # span of years for gaps.plot and path.plot
  )



# identify the weights that create the 
# best synthetic control unit for California 
synth.out <- synth(dataprep.out)


# Unit weights for the 39 states rounded to 4 digits
round(synth.out$solution.w, 4)

# Predictor weights for the predictors rounded to 4 digits
round(synth.out$solution.v, 4)



## plot in levels (treated and synthetic)
# dataprep.res is a list from the output of dataprep call
# synth.res is a list from the output of the synth call
path.plot(dataprep.res = dataprep.out, 
          synth.res = synth.out,
          tr.intake = treatment.year,      # time of treatment
          Ylab = c("Firm Markup (log)"), 
          Xlab = c("Year"),
          Ylim = c(0, 1),
          Legend = c("Treated Unit", "Synthetic Unit"))





# SDID estimate
sdid = synthdid_estimate(Y, N0, T0)

#summary
summary(sdid)

# time weights
round(summary(sdid)$periods, digits = 3) # for years with non-zero values

attr(sdid, 'weights')$lambda # for all pre-treatment years

# unit weights
round(summary(sdid)$controls, digits = 3) # for all units with non-zero values

attr(sdid, 'weights')$omega # for all units

# Unit Control Plot
synthdid_units_plot(sdid)

# Synthetic Unit vs Treated Unit Plot
synthdid_plot(list(sdid=sdid), facet.vertical=FALSE, control.name='sdid control', treated.name='treated unit', lambda.comparable=TRUE, 
    trajectory.linetype = 1, trajectory.alpha=.8, effect.alpha=1, diagram.alpha=1, effect.curvature=-.4, onset.alpha=.7) + 
    theme(legend.position=c(.90,.90), legend.direction='vertical', legend.key=element_blank(), legend.background=element_blank())

tau.did <- did_estimate(attr(sdid, 'setup')$Y, attr(sdid, 'setup')$N0, attr(sdid, 'setup')$T0)
tau.sc <- sc_estimate(attr(sdid, 'setup')$Y, attr(sdid, 'setup')$N0, attr(sdid, 'setup')$T0)
estimates <- list(tau.did, tau.sc, sdid)
names(estimates) <- c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')

synthdid_plot(estimates, se.method='placebo')





synthdid_plot(estimates[1:3], facet.vertical=FALSE,
              control.name='control', treated.name='california',
              lambda.comparable=TRUE, se.method = 'none',
              trajectory.linetype = 1, line.width=.75, effect.curvature=-.4,
              trajectory.alpha=.7, effect.alpha=.7,
              diagram.alpha=1, onset.alpha=.7) +
    theme(legend.direction='horizontal',
          legend.key=element_blank(), legend.background=element_blank(),
          strip.background=element_blank(), strip.text.x = element_blank())
#ggsave

#save plot
ggsave("synthdid_plot.pdf", width=6, height=8, dpi=300)

synthdid_units_plot(rev(estimates[1:3]), se.method='none') +
    theme(legend.background=element_blank(), legend.title = element_blank(),
          legend.direction='horizontal', legend.position=c(.17,.07),
          strip.background=element_blank(), strip.text.x = element_blank())


synthdid_units_plot(rev(estimates[1:3]), se.method='none') +
    theme(legend.background=element_blank(), legend.title = element_blank(),
          legend.direction='horizontal', legend.position=c(.17,.07),
          strip.background=element_blank(), strip.text.x = element_blank())






ggsave("synthdid_plot.png", width=8, height=6, dpi=300)
