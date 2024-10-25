# install packages from CRAN
packages <- c("dplyr", "readstata13", "fixest", "did", "fect", "didimputation",
              "panelView", "PanelMatch", "ggplot2", "bacondecomp", "HonestDiD")
install.packages(setdiff(packages, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")  

# install "HonestDID"
if ("HonestDID" %in% rownames(installed.packages()) == FALSE) {
  devtools:: install_github("asheshrambachan/HonestDiD")
}  

# update "fect"
devtools:: install_github("xuyiqing/fect", upgrade = "always")


# install "paneltools"
if ("paneltools" %in% rownames(installed.packages()) == FALSE) {
  devtools:: install_github("xuyiqing/paneltools")
}
library(dplyr)
library(readstata13)
library(fixest)
library(did)
library(fect)
library(panelView)
library(PanelMatch)
library(ggplot2)
library(bacondecomp)
library(paneltools)
library(didimputation)
library(doParallel)
library(HonestDiD)
library(haven)

data <- read_dta("did.dta")
data$Y <- log(data$muhat_tl)
y <- "Y"
d <- "pp_dummy"
unit <- "id"
time <- "year"
controls <- c("cogs","k")
index <- c("id", "year")

panelview(Y=y, D=d, X=controls, index = index, data = data, xlab = "Time Period", ylab = "Unit", 
          gridOff = TRUE, by.timing = TRUE, cex.legend=5, cex.axis= 5, cex.main = 10, cex.lab = 5)

model.twfe <- feols(Y ~ pp_dummy + cogs + k|id + year^nace2,
                    data=data, cluster = "id") #use the clustered standard error
print(model.twfe)

data_cohort <- get.cohort(data, index = index, D=d,start0 = TRUE)
# Generate a dummy variable treat
data_cohort$treat <- 0
data_cohort[which(data_cohort$Cohort!='Control'),'treat'] <- 1
data_cohort[which(is.na(data_cohort$Time_to_Treatment)), "treat"] <- 0

# remove observations that starts with treated status
remove <- intersect(which(is.na(data_cohort$Time_to_Treatment)), which(data_cohort[,d]==1)) 
if(length(remove)>0){data_cohort <- data_cohort[-remove,]}

# replace missingness in Time_to_Treatment with an arbitrary number
data_cohort[which(is.na(data_cohort$Time_to_Treatment)), "Time_to_Treatment"] <- 999 

twfe.est <- feols(Y ~ i(Time_to_Treatment, treat, ref = -1)  + cogs + k                | id + year^nace2,  
                  data = data_cohort, cluster = "id")
twfe.output <- as.matrix(twfe.est$coeftable[c(1:25),])
print(twfe.output)
twfe.output <- as.data.frame(twfe.output)
twfe.output$Time <- c(c(-16:-2),c(0:9))+1 
p.twfe <- esplot(twfe.output,Period = 'Time',Estimate = 'Estimate',
                 SE = 'Std. Error', xlim = c(-15,1))
p.twfe

df.pm <- data_cohort
# we need to convert the unit and time indicator to integer
df.pm[,"id"] <- as.integer(as.factor(df.pm[,"id"]))
df.pm[,"year"] <- as.integer(as.factor(df.pm[,"year"]))
df.pm[,"nace2"] <- as.integer(as.factor(df.pm[,"nace2"]))

df.pm <- df.pm[,c("id","year","nace2","pp_dummy","Y")]

PM.results <- PanelMatch(lag=10, 
                         time.id="year", 
                         unit.id = "id", 
                         treatment = "pp_dummy", 
                         refinement.method = "none", 
                         data = df.pm, 
                         qoi = "att", 
                         lead = 0, 
                         outcome.var = "Y", 
                         match.missing = TRUE)

## For pre-treatment dynamic effects
PM.results.placebo <- PanelMatch(lag=10, 
                                 time.id="id", 
                                 unit.id = "year", 
                                 treatment = "pp_dummy", 
                                 refinement.method = "none", 
                                 data = df.pm, 
                                 qoi = "att", 
                                 lead = 0, 
                                 outcome.var = "Y", 
                                 match.missing = TRUE,
                                 placebo.test = TRUE)
PE.results.pool <- PanelEstimate(PM.results, data = df.pm, pooled = TRUE)
summary(PE.results.pool)$summary

# Dynamic Treatment Effects
PE.results <- PanelEstimate(PM.results, data = df.pm)
PE.results.placebo <- placebo_test(PM.results.placebo, data = df.pm, plot = F)

est_lead <- as.vector(PE.results$estimates)
est_lag <- as.vector(PE.results.placebo$estimates)
sd_lead <- apply(PE.results$bootstrapped.estimates,2,sd)
sd_lag <- apply(PE.results.placebo$bootstrapped.estimates,2,sd)
coef <- c(est_lag, 0, est_lead)
sd <- c(sd_lag, 0, sd_lead)
pm.output <- cbind.data.frame(ATT=coef, se=sd, t=c(-1:1))
p.pm <- esplot(data = pm.output,Period = 't',
               Estimate = 'ATT',SE = 'se')
p.pm

model.fect <- fect(Y = y, D = d, data = data, nlambda = 100,
                   method = "mc", CV=TRUE,
                   index = index, se = TRUE, parallel = TRUE, seed = 1234, force = "two-way")
print(model.fect$est.avg)

fect.output <- as.matrix(model.fect$est.att)
print(fect.output)

fect.output <- as.data.frame(fect.output)
fect.output$Time <- c(-13:10)
p.fect <- esplot(fect.output,Period = 'Time',Estimate = 'ATT',
                 SE = 'S.E.',CI.lower = "CI.lower", 
                 CI.upper = 'CI.upper', xlim = c(-15,1))
p.fect
plot(model.fect, stats = "F.p")

plot(model.fect, stats = "F.p", type = 'exit')

out.fect.p <- fect(Y = y, X = controls, D = d, data = data, index = index,
                   method = 'fe', se = TRUE, placeboTest = TRUE, placebo.period = c(-2,0))
p.placebo <- plot(out.fect.p, proportion = 0.1, stats = "placebo.p")
p.placebo

out.fect.c <- fect(Y = y, X = controls, D = d, data = data, index = index,
                   method = 'fe', se = TRUE, carryoverTest = TRUE, carryover.period = c(1,2))
p.carryover <- plot(out.fect.c,  stats = "carryover.p")
p.carryover

out.fect.balance <- fect(Y = y, X = controls, D = d, data = data, index = index,
                         method = 'fe', se = TRUE, balance.period = c(-1,1))
#ATT
print(out.fect.balance$est.balance.avg)
fect.balance.output <- as.data.frame(out.fect.balance$est.balance.att)
fect.balance.output$Time <- c(-1:1)
p.fect.balance <- esplot(fect.balance.output,Period = 'Time',Estimate = 'ATT',
                         SE = 'S.E.',CI.lower = "CI.lower", 
                         CI.upper = 'CI.upper')
p.fect.balance



library(devtools) 
