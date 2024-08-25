# Markups and Public Procurement

To provide some context:
I use financial statement data from firms in the Czech construction industry. By linking this data with information from a public procurement database, I’ve created a firm-year indicator that identifies whether a firm was involved in public procurement and the share of its sales attributable to such contracts. My thesis, originally motivated by an undergraduate Industrial Organization course, primarily addresses the econometric challenges of estimating production functions and firm-level markups. The public procurement status in my study plays a role similar to export status in the work by De Loecker and Warzynski (2012).

This summer, I resolved several issues related to this estimation. Specifically, I was finally able to reproduce the Stata GMM routine provided by the authors, giving me confidence in the resulting markup estimates (there are a lot of methodological critiques, e.g. Gandhi et al. (2020) on the identification of gross output production functions and Bond et al. (2021) on the estimation of production function elasticities, but I would now like to move forward with what I have at the moment.

In my thesis, after tackling these challenges, I simply compare markups using a pooled OLS regression and a two-way fixed effects model.

Currently, I am focusing on credible treatment effect estimation. I adopted an approach conditioning on lagged outcome values. (recent [replication material by Yiqing Xu and Guido Imbens](https://github.com/xuyiqing/lalonde) has guided my approach). In the data, firms entering public procurement in the current year already exhibit higher markups in previous years, along with larger sales, costs of goods sold, and capital. With matched data, I’ve achieved balance in these variables, and pass placebo tests of  no effect on markups in years prior to firm recovering the government contract.

This semester, I will write my MSc thesis, where I want to implement recent approaches to causal panel data analysis, focusing on DID methodology, with a section covering: (1) heterogeneity-robust estimators, (2) tests and sensitivity analyses for parallel trends, and (3) alternative identification strategies, such as synthetic DID, matrix completion, and perhaps sufficient statistics/negative controls and design robust reweighting, inspired by the recent work of Guido Imbens, Dmitry Arkhangelsky, and their coauthors.


<p float="center">
  <img src="/average.png"/> 
  <img src="/att.png"/>
  <img src="/odds.png"/>
</p>



Methodology and replication package reference: 

Jan De Loecker & Frederic Warzynski AER 2012 ["Markups and Firm-Level Export Status"](https://www.aeaweb.org/articles?id=10.1257/aer.102.6.2437)

De Loecker, Eeckhout, Unger QJE 2020 [The Rise of Market Power and the Macroeconomic Implications](https://academic.oup.com/qje/article/135/2/561/5714769?login=true)
    
Guido Imbens, Yiqing Xu [LaLonde (1986) after Nearly Four Decades: Lessons Learned](https://arxiv.org/abs/2406.00827)



