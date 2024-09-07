
# Markups and Public Procurement.

Chadim, M. (2023). Bachelor's thesis, Charles University in Prague. doi: [dspace.cuni.cz](https://dspace.cuni.cz/handle/20.500.11956/184831#xmluiArtifactToplinksNavigationhead_all_of_dspace_links)



Text stored in repository document MarkupsAndPublicProcurement.pdf

- I utilize financial statement data from firms within the Czech construction industry. By integrating these data with information from a public procurement database, I have developed a firm-year indicator identifying whether a firm participated in public procurement and the proportion of its sales derived from such contracts. My thesis, initially inspired by an undergraduate course in Industrial Organization, primarily addresses the econometric challenges associated with estimating production functions and firm-level markups. In my analysis, public procurement status serves a role analogous to export status in the work of De Loecker and Warzynski (2012).

- Over the summer, I enhanced the estimation procedure by replacing the initially used user-written Stata command markupest (Rovigatti, 2020) with the original Stata GMM routine provided by De Loecker and Warzynski. Despite numerous methodological critiques—such as those by Gandhi et al. (2020) on the identification of gross output production functions and by Bond et al. (2021) on the estimation of production function elasticities—I have decided to retain this approach.

- In my thesis, after resolving these challenges, I compare markups using both a pooled OLS regression and a two-way fixed effects model.

- In revision for the [The Young Economist Award of the Czech Economic Society](https://cse.cz/en/page/31st-young-economist-of-the-year-award), my focus is on credible treatment effect estimation. I have adopted a methodology that conditions on lagged outcome values, informed by recent replication materials from Yiqing Xu and Guido Imbens. In the data, firms entering public procurement in the current year already demonstrate higher markups in previous years, alongside larger sales, costs of goods sold, and capital. By employing matched data, I have achieved balance in these variables and successfully passed placebo tests, confirming no effect on markups in the years preceding firms' acquisition of government contracts.

- This semester, I will complete my MSc thesis, in which I intend to compare the unconfoundedness approach outlined above by applying recent advances in causal panel data analysis, with particular emphasis on the Difference-in-Differences (DID) methodology. This section will encompass (1) heterogeneity-robust estimators, (2) tests and sensitivity analyses for parallel trends, and (3) alternative identification strategies such as synthetic DID, matrix completion, and potentially sufficient statistics/negative controls and design-robust reweighting, inspired by recent work by Guido Imbens, Dmitry Arkhangelsky, and their coauthors.

Methodology and replication package reference: 

1. DLW 2012 [Markups and Firm-Level Export Status](https://www.aeaweb.org/articles?id=10.1257/aer.102.6.2437), DLEU 2020 [The Rise of Market Power and the Macroeconomic Implications](https://academic.oup.com/qje/article/135/2/561/5714769?login=true)

<img src="/Fig1.png" width="80%"/> 

<p float="left">
  <img src="/Fig2a.png" width="45%"/> 
  <img src="/Fig2b.png" width="45%"/> 
</p>

<p float="left">
  <img src="/Fig3.png" width="45%"/>
  <img src="/Fig4.png" width="45%"/>
</p>





    
2. Guido Imbens, Yiqing Xu [LaLonde (1986) after Nearly Four Decades: Lessons Learned](https://arxiv.org/abs/2406.00827)

<p float="left">
  <img src="/Rplot01.png"/>
  <img src="/Rplot03.png"/> 
  <img src="/Rplot02.png"/>
</p>



