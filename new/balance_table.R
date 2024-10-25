#install.packages("haven")
gc()
rm(list=ls())
d <- read.csv("lalonde/data/balance.csv")
str(d)
sales <- d$go
assets <- d$k
costs <- d$cogs
wages <- d$w
nace41 <- d$nace41
nace42 <- d$nace42
nace43 <- d$nace43
public <- d$public
foreign <- d$foreign
markup <- d$muhat_tl
empl <- d$empl
treatment <- as.numeric(d$treated == 1)
data <- data.frame(sales, costs, assets, wages, nace41, nace42, nace43, public, foreign, markup, empl, treatment)
data <- data.frame(data)
xvars <- c("sales", "costs", "assets", "wages", "nace42", "nace43", "empl", "markup")

#install.packages("tableone")
library(tableone)
table1<- CreateTableOne(vars=xvars,strata="treatment", data=data, test=FALSE)
## include standardized mean difference (SMD)
print(table1,smd=TRUE)


