library(tidyverse)
library(haven)
# devtools::install_github("ebenmichael/augsynth")
# devtools::install_github("bcastanho/SCtools")
library(augsynth)
library(SCtools)

data <- read.csv("panel.csv") 
data %>%
    filter(!i %in% c(6,7,12,14,16,18)) %>%
    mutate(Ei = ifelse(is.na(Ei), 
                                   Inf, Ei),
           T = 1 * (t >= Ei)) -> analysis_df
#save df
write.csv(analysis_df, "analysis_df.csv")
# with default nu
ppool_syn <- multisynth(Y ~ T, i, t,
                        analysis_df)

print(ppool_syn$nu)
#[1] 0.410388

ppool_syn
ppool_syn_summ <- summary(ppool_syn)
ppool_syn_summ

plot(ppool_syn_summ)
plot(ppool_syn_summ, levels = "Average")




# with default nu
ppool_syn_time <- multisynth(Y ~ T, id, year, n_leads = 1,
                        analysis_df, time_cohort = TRUE)

ppool_syn_time_summ <- summary(ppool_syn_time)
ppool_syn_time_summ

plot(ppool_syn_time_summ)

#save plot


