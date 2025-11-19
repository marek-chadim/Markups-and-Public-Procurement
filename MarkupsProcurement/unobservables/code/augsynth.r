library(tidyverse)
library(haven)
# devtools::install_github("ebenmichael/augsynth")
# devtools::install_github("bcastanho/SCtools")
library(augsynth)
library(SCtools)

setwd("/Users/marek/Library/CloudStorage/Dropbox/Markups-and-Public-Procurement/MarkupsProcurement/unobservables")
data <- read.csv("data/panel.csv") 
data %>%
    filter(!i %in% c(6,7,12,14,16,18)) %>%
    mutate(Ei = ifelse(is.na(Ei), 
                                   Inf, Ei),
           T = 1 * (t >= Ei)) -> analysis_df
# Filter out always treated units
analysis_df <- analysis_df %>%
    group_by(i) %>%
    filter(!any(Ei == 1)) %>%
    ungroup()
#save df
write.csv(analysis_df, "data/analysis_df.csv")
# with default nu
ppool_syn <- multisynth(Y ~ T, i, t,
                        analysis_df, fixedeff = FALSE)

print(ppool_syn$nu)
#[1] 0.410388

ppool_syn
ppool_syn_summ <- summary(ppool_syn)
ppool_syn_summ

plot(ppool_syn_summ)
plot(ppool_syn_summ, levels = "Average")


# with default nu
ppool_syn_time <- multisynth(Y ~ T, i, t, n_leads = 1,
                        analysis_df, time_cohort = TRUE, fixedeff = FALSE)

ppool_syn_time_summ <- summary(ppool_syn_time)
ppool_syn_time_summ

plot(ppool_syn_time_summ)

#save plot


