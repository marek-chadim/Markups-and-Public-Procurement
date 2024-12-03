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

data <- read.csv("did.csv")
head(data)

panelview(Y ~ D, data = data, index = c("id","year"), 
  xlab = "Year", ylab = "Firm", display.all = T,
  gridOff = TRUE, by.timing = TRUE)

panelview(data = data,Y='Y',
          D='D',index=c("id","year"),
          by.timing = T, display.all = T,
          type = "outcome",by.cohort = T)


model.twfe.0 <- feols(Y~D|id+year,
                      data=data, cluster = "id") 
print(model.twfe.0)

data.complete <- data[which(!is.na(data$Y)),] 
df_bacon <- bacon(Y~D,
                  data = data.complete,
                  id_var = "id",
                  time_var = "year")
ggplot(df_bacon) +
   aes(x = weight, y = estimate, shape = factor(type), color = factor(type)) +
   labs(x = "Weight", y = "Estimate", shape = "Type", color = 'Type') +
   geom_point()
