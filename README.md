# Markups and Public Procurement
Chadim, M. (2023) Markups and Public Procurement. Bachelor's thesis, Charles University, Institute of Economic Studies, Prague. doi: [dspace.cuni.cz](https://dspace.cuni.cz/handle/20.500.11956/184831#xmluiArtifactToplinksNavigationhead_all_of_dspace_links)

To provide context, I use financial statement data from firms in the Czech construction industry. By linking this data with information from a public procurement database, I have created a firm-year indicator that identifies whether a firm was involved in public procurement and the share of its sales attributable to such contracts. My thesis, initially motivated by an undergraduate Industrial Organization course, primarily addresses the econometric challenges of estimating production functions and firm-level markups. In my study, the public procurement status plays a role similar to that of export status in the work by De Loecker and Warzynski (2012).

This summer, I resolved several issues related to this estimation. Specifically, I successfully reproduced the Stata GMM routine provided by the authors, which has increased my confidence in the resulting markup estimates. Although there are numerous methodological critiques—such as Gandhi et al. (2020) on the identification of gross output production functions and Bond et al. (2021) on the estimation of production function elasticities—I am now ready to move forward with the current approach.

In my thesis, after addressing these challenges, I compare markups using both a pooled OLS regression and a two-way fixed effects model.

Currently, I am focusing on credible treatment effect estimation. I have adopted an approach that conditions on lagged outcome values, guided by recent replication materials by Yiqing Xu and Guido Imbens. In the data, firms that enter public procurement in the current year already exhibit higher markups in previous years, along with larger sales, costs of goods sold, and capital. By using matched data, I have achieved balance in these variables and have passed placebo tests, confirming no effect on markups in years prior to firms securing government contracts.

This semester, I will write my MSc thesis, where I plan to implement recent approaches to causal panel data analysis, focusing on the Difference-in-Differences (DID) methodology. This section will cover (1) heterogeneity-robust estimators, (2) tests and sensitivity analyses for parallel trends, and (3) alternative identification strategies such as synthetic DID, matrix completion, and potentially sufficient statistics/negative controls and design-robust reweighting, inspired by recent work by Guido Imbens, Dmitry Arkhangelsky, and their coauthors.


<p float="left">
  <img src="/average.png"/> 
    <img src="/att.png"/>
  <img src="/odds.png"/>
</p>



Methodology and replication package reference: 

Jan De Loecker & Frederic Warzynski AER 2012 ["Markups and Firm-Level Export Status"](https://www.aeaweb.org/articles?id=10.1257/aer.102.6.2437)

De Loecker, Eeckhout, Unger QJE 2020 [The Rise of Market Power and the Macroeconomic Implications](https://academic.oup.com/qje/article/135/2/561/5714769?login=true)
    
Guido Imbens, Yiqing Xu [LaLonde (1986) after Nearly Four Decades: Lessons Learned](https://arxiv.org/abs/2406.00827)



