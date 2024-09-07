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

*-------------------------------------------------------------------------------*
* Table 1: Table 1 Public Procurement in Czech Construction industry
tabstat pp_dummy, by(year) stat(N sum) format(%4.0f)


*------------------------------------------------------------------------------*
*Table 2: Estimated Markups
tabstat muhat_tl, by(pp_dummy) stat(p10 p25 p50 mean p75 p90) format(%4.2f)
tabstat muhat_tl, by(year) stat(p10 p25 p50 mean p75 p90) format(%4.2f)

//1 translog acf + endog. process g(omega,procurement)
gen lmu_1=ln(muhat_tl)
gen omega_1 = omegahat_tl

*------------------------------------------------------------------------------*
* The results in tables 3 and 4 get produced with following code:

* Table 3: Markups and Procurement Status I
forvalues j=1/1  {
reghdfe lmu_`j' pp_dummy, a(id year) cluster(id)

* computing markup difference for tables
gen theta_`j'_0=_b[_cons]
gen theta_`j'_1=_b[pp_dummy]

* mu_pp: level difference in markup
gen mu_pp`j'=theta_`j'_1*exp(theta_`j'_0)

* do the same with productivity
reghdfe lmu_`j' omega_`j', a(id year) cluster(id)
gen theta_omega`j'=_b[omega_`j']

* do the same with both productivity and procurement
reghdfe lmu_`j' pp_dummy omega_`j', a(id year) cluster(id) 
gen theta_`j'_omega=_b[pp_dummy]
}

*cross sectional results, markup premium procurement: precentage and levels
tabstat theta_1_1  mu_pp1 

*cross sectional results: percentage procurement premium controlling for productivity
tabstat theta_1_omega 

*cross sectional results: productivity-markup relationship
tabstat theta_omega1 


*coefficients on interaction term give markup premium for a inditutional sector
encode inst_sector, generate(ownership)
forvalues j=1/1{
reghdfe  lmu_`j' pp_dummy#ownership,  a(year nace2) cluster(id)
}

* Table 4: Markups and Procurement Status II: Procurement Entry Effect
* create start variables
sort id year
by id: gen start=1 if pp_dummy[_n-1]==pp_dummy-1 
replace start=0 if start==.
label var start "1 at entry time
by id: egen starter=sum(start)

* create stop variables
by id: gen stop=1 if pp_dummy[_n-1]==pp_dummy+1
replace stop=0 if stop==.
by id: egen stopper=sum(stop)
gen switcher=1 if stopper>2 | starter>2

gen entry_effect=starter*pp_dummy
* value entry_effect is 1 post procurement entry

gen exit_effect=stopper*pp_dummy
* value exit_effect is 1 pre procurement exit, so take - coefficient for effect.

forvalues j=1/1 {
reghdfe lmu_`j' entry_effect exit_effect if switcher==., absorb(id year) cluster(id)
gen gamma_`j'_0=_b[_cons]
gen gamma_`j'_0_se=_se[_cons]
gen gamma_`j'_1=_b[entry_effect]
gen gamma_`j'_1_se=_se[entry_effect]
gen gamma_`j'_2=-_b[exit_effect]
gen gamma_`j'_2_se=_se[exit_effect]

*mu_start: level difference in markup
gen mu_start`j'=gamma_`j'_1*exp(gamma_`j'_0)

* again with productivity included:
reghdfe lmu_`j' entry_effect exit_effect omega_`j' if switcher==., absorb(id year)  cluster(id)
gen gamma_`j'_omega=_b[entry_effect]
}

*time series results: percentage difference before-after procurement entry/exit, 
tabstat gamma_*_1  gamma_*_2 

*time series results in level differences
tabstat mu_start*
*-------------------------------------------------------------------------------*





