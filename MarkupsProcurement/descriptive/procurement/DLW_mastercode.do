/* CHADIM MAREK: Markups and Public Procurement 
The Czech Economic Society 31st Young Economist of the Year Award 2024

MASTER CODE adapted from JAN DE LOECKER and FREDERIC WARZYNSKI:
Markups and Firm-level Export Status, American Economic Review 2012

This file lists the set of variables used in the analysis 
and provides the code to produce tables and result in the paper.
*/

/*
Data general structure:

-Firm-level data with id and year firm and year indicator in panel structure.

-Note: all variables are deflated using industry specific Price Indices.

-Variables (lower case denotes logs): 

* output (GO, go), costs of goods sold (COGS, cogs), capital (K, k) 
* nace2 (subindustry 41,42,43), year (2006-2021), id (firm indicator)
* public procurement indicator (pp_dummy)

*/

* Start by setting panel structure:
use data, clear
xtset id year, yearly

* Run DLW_procedure.do on data and recover Translog and Cobb-Douglas parameters
* (and bootstrapped st. err.), estimates of markups (mu), and productivity (omega)
set seed 42
do DLW_procedure.do
use markups, clear

*-------------------------------------------------------------------------------*
* Table 1: Firms and Public Procurement in Czech Construction
tabstat pp_dummy, by(year) stat(N sum) format(%4.0f)
tabstat id, by(nace2) stat(N) format(%4.0f)
*------------------------------------------------------------------------------*
*Appendix Tables: Estimated Markup Distribution
tabstat markup, by(year) stat(p10 p25 p50 p75 p90 mean sd N) format(%4.2f)
tabstat markup, by(nace2) stat(p10 p25 p50 p75 p90 mean sd N) format(%4.2f)
tabstat markup, by(pp_dummy) stat(p10 p25 p50 p75 p90 mean sd N) format(%4.2f)

//translog acf + endog. process g(omega,procurement)
gen lmu=ln(markup)
gen omega= omegahat_tl

*------------------------------------------------------------------------------*
* The results in tables 2 and 3 get produced with following code:
* Table 2: Markups and Procurement Status I

* Run a single regression with interaction terms for year and nace2
xi: reg lmu pp_dummy k cogs i.year*i.nace2, cluster(id)
dis e(r2_a)
gen theta_0 = _b[_cons]
gen theta_1 = _b[pp_dummy] 
nlcom _b[pp_dummy]*exp(_b[_cons])

* mu_pp: level difference in markup
gen mu_pp = theta_1 * exp(theta_0)

* Extract coefficient for productivity
xi: reg lmu omega k cogs i.year*i.nace2, cluster(id)
gen theta_omega = _b[omega] 

* Extract procurement coefficient controlling for productivity 
xi: reg lmu pp_dummy omega k cogs i.year*i.nace2, cluster(id)
gen theta_omega_pp = _b[pp_dummy] 

*cross sectional results, markup premium procurement: percentage and levels
tabstat theta_1 mu_pp

*cross sectional results: percentage procurement premium controlling for productivity
tabstat theta_omega_pp

*cross sectional results: productivity-markup relationship
tabstat theta_omega

* Table 3: Markups and Procurement Status II: Procurement Entry Effect

* create start variables
sort id year
by id: gen start=1 if pp_dummy[_n-1]==pp_dummy-1 
replace start=0 if start==.
label var start "1 at entry time"
by id: egen starter=sum(start)

* value entry_effect is 1 post procurement entry
gen entry_effect=starter*pp_dummy
*gen entry_effect=starter*pp_share

* create stop variables
by id: gen stop=1 if pp_dummy[_n-1]==pp_dummy+1
replace stop=0 if stop==.
by id: egen stopper=sum(stop)

* value exit_effect is 1 pre procurement exit, so take - coefficient for effect.
gen exit_effect=stopper*pp_dummy
*gen exit_effect=stopper*pp_share

*create always control variables
by id: egen meanpp= mean(pp_dummy)
gen always = cond(meanpp==1,1,0)

*Exclude firms that enter or exit public procurement more than once in the sample.
gen switcher=1 if stopper>1 | starter>1

xi: reg lmu entry_effect exit_effect always cogs k i.year*i.nace2 if switcher==., cluster(id)
dis e(r2_a)
gen gamma_0=_b[_cons]
gen gamma_0_se=_se[_cons]
gen gamma_1=_b[entry_effect]
gen gamma_1_se=_se[entry_effect]
gen gamma_2=-_b[exit_effect]
gen gamma_2_se=_se[exit_effect]
esttab using timeseries.tex,  ///
    se replace nostar nomtitle nonumbers  ///
    keep(_cons entry_effect exit_effect always) order(_cons entry_effect exit_effect always ) se(%9.3f) b(%9.3f) ///
    stats(N r2_a, fmt(%9.0g %9.3f) labels("N" "R-squared"))  
*mu_start: level difference in markup
gen mu_start=gamma_1*exp(gamma_0)
nlcom _b[entry_effect]*exp(_b[_cons])

* again with productivity included:
xi: reg lmu entry_effect exit_effect always  omega cogs k i.year*i.nace2 if switcher==., cluster(id)
gen gamma_omega=_b[entry_effect]

*time series results: percentage difference before-after procurement entry/exit, 
tabstat gamma_1 gamma_2 

*time series results: percentage procurement premium controlling for productivity
tabstat gamma_omega

*time series results, level markup
tabstat mu_start
*------------------------------------------------------------------------------*

* entry result using share of sales in total sales
gen entry_effect_share=entry_effect*pp_share
xi: reg lmu entry_effect entry_effect_share exit_effect always cogs k i.year*i.nace2 if switcher==., cluster(id)
nlcom _b[entry_effect]*exp(_b[_cons])


*Markups and Public Procurement: digging deeper
xi: reg lmu i.pp_dummy*i.year cogs k i.year*i.nace2, cluster(id)
xi: reg lmu i.pp_dummy*i.nace2 cogs k i.year*i.nace2, cluster(id)

xi: reg lmu i.pp_dummy*i.sole_proprietor cogs k i.year*i.nace2, cluster(id)
* coefficients on interaction term give markup premium for a subject type and subsector

*------------------------------------------------------------------------------*
*Markups and Procurement Status: Appendix Tables
eststo clear
levelsof year, local(years)
foreach y of local years {
    xi: reg lmu pp_dummy k cogs i.nace2 if year == `y', cluster(id)
    eststo reg_year_`y'
}
esttab reg_year_* using year_table.tex, se replace nostar ///
    title("Table 3: Markups and Procurement Status I by Year")  nonumbers  ///
    mtitles( 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021)  ///
    keep(pp_dummy) order(pp_dummy) se(%9.3f) b(%9.3f) ///
    label varlabels(pp_dummy "Public Procurement Percentage Premium") ///
    stats(N r2_a, fmt(%9.0g %9.3f) labels("N" "Adjusted R-squared"))
eststo clear
levelsof nace2, local(nace2s)
foreach n of local nace2s {
    xi: reg lmu pp_dummy k cogs i.year if nace2 == `n', cluster(id)
    eststo reg_nace2_`n'
}
esttab reg_nace2_* using nace2_table.tex, replace se nostar ///
    title("Table 3: Markups and Procurement Status I by NACE2")  nonumbers  ///
    mtitles( 41 42 43)  ///
    keep(pp_dummy) order(pp_dummy) se(%9.3f)  b(%9.3f) ///
    label varlabels(pp_dummy "Public Procurement Percentage Premium") ///
    stats(N r2_a, fmt(%9.0g %9.3f) labels("N" "Adjusted R-squared"))
*-------------------------------------------------------------------------------*