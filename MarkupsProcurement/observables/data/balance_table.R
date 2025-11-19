#install.packages("haven")
gc()
rm(list=ls())
setwd("/Users/marek/Library/CloudStorage/Dropbox/Markups-and-Public-Procurement/MarkupsProcurement/observables")
d <- read.csv("data/data.csv")
dim(d)
dim(d)
str(d)
sales <- d$sales
assets <- d$assets
costs <- d$costs
nace42 <- d$nace42
nace43 <- d$nace43
markup <- d$x3
elas <- d$beta
prod <- d$omega
empl <- d$empl
year <- d$year
lpp<-apply(d[, paste0("pp", 1:4)], 1, mean)
treatment <- as.numeric(d$pp == 1)
data <- data.frame(sales, costs, assets, empl, nace42, nace43, markup, lpp, elas, prod, year, treatment)
data <- data.frame(data)
xvars <- c("sales", "costs", "assets", "empl", "nace42", "nace43", "markup", "year")

#install.packages("tableone")
library(tableone)
table1<- CreateTableOne(vars=xvars,strata="treatment", data=data, test=FALSE)
## include standardized mean difference (SMD)
print(table1,smd=TRUE)














