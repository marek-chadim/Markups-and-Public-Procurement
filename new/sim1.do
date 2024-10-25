/*
ssc install ftools
ssc install reghdfe
ssc install csdid 
ssc install drdid
*/


clear
set seed 112087
local N = 290		// Number of "municipalities"
local T = 16		// Number of time periods
local totaltreatshr = .9	// Share of municipalities ever treated
local tshr_pre = 0			// Share of treated municipalities treated before data starts
local tshr_early = .5		// Share of treated municipalities treated in first half of data years

local eff_immediate = 0		// Size of immediate effect
local eff_max = 10			// Size of "long run" effect
local eff_timetomax = 15	// How long to get to "long-run" effect
/* 
This next local controls the "shape" of the treatment effects, which is very important
For effects that show up immediately, choose 0
For effects that grow linearly over time, choose 1
For effects that grow convexly over time, choose something greater than 1 (such as 2)
For effects that grow concavely over time, choose something between 0 and 1 (such as .5)
*/
local eff_convexity	= 2


local NT = `N'*`T'
local tshr_late = 1 - `tshr_pre' - `tshr_early'		// Share of treated municipalities treated in second half of data years

set obs `NT'

* Creates a balanced panel: i counts municipalities, t counts years
gen i = ceil(_n/`T')
gen t = mod(_n-1,`T') + 1
*tab t
*tab i
list in 1/35


sort i t
gen ai = rnormal() if t == 1				// Municipality fixed effect
replace ai = ai[_n-1] if t != 1
sort t i
gen dt = rnormal() if i == 1				// Time fixed effect
replace dt = dt[_n-1] if i != 1
sort i t
list in 1/35

* These are just random variables that are useful for allocating muncipalities' treatment status
foreach n of numlist 1/3 {
  gen temp`n' = runiform() if t == 1
  replace temp`n' = temp`n'[_n-1] if t != 1
}

gen evertreated = (temp1 < `totaltreatshr')		// Is this municipality treated at some point
gen treatyear = .								// During which year is the municipality treated
replace treatyear = -floor(temp3*`T') if evertreated == 1 & temp2 < `tshr_pre'
replace treatyear = floor(temp3*(`T'/2)) if evertreated == 1 & temp2 >= `tshr_pre' & temp2 < `tshr_early'
replace treatyear = floor((`T'/2) + temp3*(`T'/2)) if evertreated == 1 & temp2 >= `tshr_early'

tab treatyear evertreated, missing		// treatyear==. means never treated

gen treated = (evertreated == 1 & t >= treatyear)				// Is the current YEAR (not municipality) treated
gen timesincetreatment = t - treatyear if evertreated == 1		// Years since treatment
tab timesincetreatment evertreated, missing	

* This is where you can choose the functional form for what the treatment effects look like
gen truebeta = `eff_immediate' + ((timesincetreatment/`eff_timetomax')^`eff_convexity')*(`eff_max'-`eff_immediate') if treated == 1
replace truebeta = `eff_max' if treated == 1 & timesincetreatment >= `eff_timetomax'
replace truebeta = 0 if treated == 0
gen estimatedbeta = .

scatter truebeta timesincetreatment			// What does the chosen "shape" of treatment effects look like?


gen Y0 = ai + dt + rnormal()				// Y0 is the potential outcome in the absence of treatment
gen Y1 = Y0 + truebeta						// Y1 is the actual outcome that you would observe in the data


sum truebeta if treated == 1 & treatyear >= 0		// What are the true average treatment effects (which you cannot observe in non-simulated data) among treated observations
reghdfe Y1 treated, absorb(i t)						// What average treatment effect would TWFE/OLS estimate?

* This is a probably useless way that De Chaisemartin-D'Houltefoeuille suggest for looking at the potential size of the problem
twowayfeweights Y1 treatyear t treated, type(feTR)
reghdfe treated, absorb(i t) resid
qui: predict rtreated, resid				// Recall negative weights happen when the residualized treatment dummy is negative: We calculate that here.
sum rtreated if treated == 1, d
gen nrt = (rtreated < 0)							
tab nrt if treated == 1						// What share of treated observations have negative weights?

* How do the negative weights compare with the true effects?
* NOTE: Without simulated data you cannot calculate this plot because you don't observe the true beta
preserve
  keep if treated == 1
  gen ones = 1
  collapse (mean) truebeta rtreated (sum) ones, by(treatyear t)
  scatter truebeta rtreated [aw=ones], msymbol(oh)
restore
* During which years do negative weights occur? Matters for how much we should worry about long-run treatment effects. (Note: Can be calculated in real data)
preserve
  keep if treated == 1
  gen ones = 1
  collapse (mean) rtreated (sum) ones, by(treatyear t)
  scatter rtreated t [aw=ones], msymbol(oh)
restore
* How long since treatment are the negative weights occurring? Matters for how much we should worry about long-run treatment effects. (Note: Can be calculated in real data)
preserve
  keep if treated == 1
  gen ones = 1
  collapse (mean) rtreated (sum) ones, by(timesincetreatment t treatyear)
  scatter rtreated timesincetreatment [aw=ones], msymbol(oh)
restore




* Basic dynamic diff-in-diff estimates (the TWFE estimator we've all been using to look at pre-trends and dynamic effects)
foreach n of numlist 2/4 {
  gen xpre`n' = (evertreated == 1 & timesincetreatment == -`n')
}
replace xpre4 = 1 if evertreated == 1 & timesincetreatment <= -4	// Binning endpoints
foreach n of numlist 0/6 {
  gen xpos`n' = (evertreated == 1 & timesincetreatment == `n')
}
replace xpos6 = 1 if evertreated == 1 & timesincetreatment >= 6		// Binning endpoints
reghdfe Y1 xpre4 xpre3 xpre2 xpos0 xpos1 xpos2 xpos3 xpos4 xpos5 xpos6, absorb(i t)
foreach v of varlist xpre* xpos* {
  replace estimatedbeta = _b[`v'] if `v' == 1
}
* Estimated effects compared to true effects (obviously true effects not observable in real data)
graph twoway (scatter truebeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5) (scatter estimatedbeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5), xline(-1)


* Looking at a couple treatment cohorts relative to never-treated observations
preserve
  keep if treatyear == 3 | treatyear == 13 | evertreated == 0
  collapse (mean) Y1, by(t treatyear evertreated)
  sort t
  graph twoway (line Y1 t if treatyear == 3, lcolor(navy)) (line Y1 t if evertreated == 0, lcolor(gs12)) ///
	(line Y1 t if treatyear == 13, lcolor(cranberry)), xline(3, lcolor(navy)) xline(13, lcolor(cranberry))
restore


* Suppose we estimated dynamic effects using only one treatment cohort at a time
replace estimatedbeta = . 
reghdfe Y1 xpre4 xpre3 xpre2 xpos0 xpos1 xpos2 xpos3 xpos4 xpos5 xpos6 if evertreated == 0 | treatyear == 5, absorb(i t)	// Using only those treated at t==5
foreach v of varlist xpre* xpos* {
  replace estimatedbeta = _b[`v'] if `v' == 1
}
graph twoway (scatter truebeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5) (scatter estimatedbeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5), name(g1, replace) xline(-1)

replace estimatedbeta = . 
reghdfe Y1 xpre4 xpre3 xpre2 xpos0 xpos1 xpos2 xpos3 xpos4 xpos5 xpos6 if evertreated == 0 | treatyear == 6, absorb(i t)	// Using only those treated at t==6
foreach v of varlist xpre* xpos* {
  replace estimatedbeta = _b[`v'] if `v' == 1
}
graph twoway (scatter truebeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5) (scatter estimatedbeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5), name(g2, replace) xline(-1)

replace estimatedbeta = . 
reghdfe Y1 xpre4 xpre3 xpre2 xpos0 xpos1 xpos2 xpos3 xpos4 xpos5 xpos6 if evertreated == 0 | treatyear == 7, absorb(i t)	// Using only those treated at t==7
foreach v of varlist xpre* xpos* {
  replace estimatedbeta = _b[`v'] if `v' == 1
}
graph twoway (scatter truebeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5) (scatter estimatedbeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5), name(g3, replace) xline(-1)

replace estimatedbeta = . 
reghdfe Y1 xpre4 xpre3 xpre2 xpos0 xpos1 xpos2 xpos3 xpos4 xpos5 xpos6 if evertreated == 0 | treatyear == 8, absorb(i t)	// Using only those treated at t==8
foreach v of varlist xpre* xpos* {
  replace estimatedbeta = _b[`v'] if `v' == 1
}
graph twoway (scatter truebeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5) (scatter estimatedbeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5), name(g4, replace) xline(-1)

replace estimatedbeta = . 
reghdfe Y1 xpre4 xpre3 xpre2 xpos0 xpos1 xpos2 xpos3 xpos4 xpos5 xpos6 if evertreated == 0 | treatyear == 9, absorb(i t)	// Using only those treated at t==9
foreach v of varlist xpre* xpos* {
  replace estimatedbeta = _b[`v'] if `v' == 1
}
graph twoway (scatter truebeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5) (scatter estimatedbeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5), name(g5, replace) xline(-1)

replace estimatedbeta = . 
reghdfe Y1 xpre4 xpre3 xpre2 xpos0 xpos1 xpos2 xpos3 xpos4 xpos5 xpos6 if evertreated == 0 | treatyear == 10, absorb(i t)	// Using only those treated at t==10
foreach v of varlist xpre* xpos* {
  replace estimatedbeta = _b[`v'] if `v' == 1
}
graph twoway (scatter truebeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5) (scatter estimatedbeta timesincetreatment if timesincetreatment >= -3 & timesincetreatment <= 5), name(g6, replace) xline(-1)
graph combine g1 g2 g3 g4 g5 g6,  graphregion(color(white)) plotregion(fcolor(white)) rows(2)






* From the csdid documentation: "Groups that are never treated should be coded as Zero."
* Since I used missing and used 0 for an actually treated cohort, this needs to be changed
replace t = t + 100
replace treatyear = treatyear + 100
replace treatyear = 0 if treatyear == .

sum truebeta if treated == 1		// True average treatment effects on treated observations
reghdfe Y1 treated, absorb(i t)		// TWFE estimated average treatment effects
csdid Y1 , ivar(i) time(t) gvar(treatyear) method(reg) agg(simple)		// CSA estimate of single post-treatment number
csdid Y1 , ivar(i) time(t) gvar(treatyear) method(reg) agg(event)		// CSA dynamic estimates
csdid_plot																// Plotting the dynamic estimates







*