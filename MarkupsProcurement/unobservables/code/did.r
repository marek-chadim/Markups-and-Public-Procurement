# install.packages("devtools")
devtools::install_github("bcallaway11/did")

library(did)
data<-read.csv("raw.csv")

out <- att_gt(
  yname = "Y",
  gname = "gvar",
  idname = "id",
  tname = "t",
  xformla = ~1,
  data = data,
  est_method = "reg"
)

summary(out)
ggdid(out, ylim = c(-.1, .25))

es <- aggte(out, type = "dynamic")

ggdid(es)

group_effects <- aggte(out, type = "group")
summary(group_effects)
