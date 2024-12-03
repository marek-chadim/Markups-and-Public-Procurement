clear all
set maxvar 30000
use did
gen t = year-2005
gen markup = muhat_tl
gen Y = ln(muhat_tl)
egen Ei = min(t / (pp_dummy == 1)), by(id)
sum Ei
drop K
gen K = t-Ei // "relative time", i.e. the number periods since treated (could be missing if never-treated)
gen D = K>=0 & Ei!=. 	
bys id: gen nyear=[_N]
gen z=pp_dummy
gen timet=t if z==1
by id: egen time_of_treat=min(timet)
tab time_of_treat
gen last_treat=time_of_treat==16
replace time_of_treat = . if last_treat
xtset id t





wooldid Y id t time_of_treat



xtevent Y, panelvar(id) timevar(t) policyvar(z) window(5) impute(nuchange) ///
 cohort(time_of_treat) control_cohort(last_treat) plot

fect Y, treat(pp_dummy) unit(id) time(t) method("mc") se wald nboots(50)  cvtreat seed(42)  minT0(5)



fect Y, treat(pp_dummy) unit(id) time(t) method("both") se placeboTest placeboperiod(3) wald nboots(50)  cvtreat




egen long i = group(id)
tab Ei, gen(G)
bys id: egen meanpp=mean(pp_dummy)
gen lastcohort = Ei==r(max) // dummy for the latest- or never-treated cohort
gen gvar = cond(Ei==., 0, Ei) // dummy for the treated cohort

keep if nyear==16
export delimited panel, replace


drop i
egen long i = group(id)
egen group = cut(Ei), group(3)
export delimited panel, replace
save panel , replace
gen notD = 1-pp_dummy
bys id: gen Y0=sum(notD)
bys id: egen maxY0=max(Y0)
bys id: keep if maxY0>4
sum maxY0
save Y05, replace
export delimited Y05, replace
use panel, clear
xtset i t
sort i t
drop W
gen W = pp_dummy
bys id: egen Wbar=mean(W)
bys year: egen Wbart=mean(W)

reg Y W Wbar Wbart  ,  cluster(i)
teffects aipw (Y Wbar Wbart Xbar Xbart) (W Wbar Wbart Xbar Xbar), vce(cluster id)  atet
teffects psmatch  (Y ) (W Wbar Wbart) , atet

matlist  e(Nt)
coefplot
tab empl
tab W
panelview Y W , i(id) t(year) type(treat)  bytiming collapsehistory  displayall 
bys year: egen sharepp=mean(W)
tabstat sharepp, by(year)

fect Y, treat(W) unit(id) time(t) method("fe") se wald nboots(50) 
 *collapse (mean) Y counterfactual_of_response, by(t)
*line Y counterfactual_of_response t
fect Y, treat(W) unit(i) time(t) method("ife") cv nboots(50) se  seed(42)
 *collapse (mean) Y counterfactual_of_response, by(t)
*collapse (mean) Y counterfactual_of_response, by(t)
*line Y counterfactual_of_response t
 matlist e(ATT)
 matlist e(ATTs)
fect Y, treat(W) unit(id) time(t)  method("mc")  cv nboots(50)  se wald 
ereturn list
 matlist e(CV)
 matlist e(ATT)
*collapse (mean) Y counterfactual_of_response, by(t)
*line Y counterfactual_of_response t


fect Y, treat(W) unit(id) time(year) method("both")  r(4) nlambda(15)
 matlist e(ATT)


matlist e(ATTs)

use panel, clear
bys id: egen Wbar = mean(pp_dummy)
bys year: egen Wbart = mean(pp_dummy)
reg Y pp_dummy, cluster(id)
reg Y pp_dummy Wbar Wbart, cluster(id)
xtreg Y pp_dummy i.t, fe cluster(id)
reghdfe Y pp_dummy,  a( t) cluster(i)


import delimited analysis_df.csv, clear
set seed 42

sdid_event y id year d, vce( bootstrap ) brep(50) placebo(all) disag 
matlist e(H)
mat res = e(H)[2..23,1..5]
svmat res
gen ids = _n - 1 if !missing(res1)
replace ids = 11 - _n if _n > 11 & !missing(res1)
sort ids
keep if ids < 5
keep if ids > -7
set scheme lean2
twoway (rarea res3 res4 ids, lc(gs10) fc(gs11%50)) (scatter res1 ids, mc(blue) ms(d)), text(.275 3.25 "ATT = 0.075" " SE = (0.019)") legend(off) xtitle(Relative time to treatment change) ytitle(Firm Markup (log)) yline(0, lc(red) lp(-)) xline(0, lc(black) lp(solid))
graph export sdidevent.pdf, replace


sdid y id year d, vce(bootstrap) seed(42) 
matlist e(tau)

sdid y id year d, vce(bootstrap) graph graph_export(sdid_, .pdf)  seed(42)  reps(100)  g2_opt (ytitle("Firm Markup (log)") xtitle("") scheme(lean2) )

ereturn list















reghdfe Y pp_dummy,  a(i t#nace2) cluster(i)
xi: areg Y pp_dummy, absorb(i i.t*i.nace2) cluster(i)
xtreg Y pp_dummy i.t##nace2, fe  cluster(i)
drop W
twowayfeweights Y i t D, type(feTR)
dis beta


















sum Ei
gen lastcohort = Ei==r(max) // dummy for the latest- or 
xtset i t
xtevent Y, policy(D) window(1) control_cohort(lastcohort) cohort(gvar)


reghdfe Y D,  a(i t)

lpdid Y, time(year) unit(id) treat(pp_dummy)  pre(10) post(1) nonabs(1)  bootstrap(50) 



panelview Y pp_dummy , i(i) t(t) type(treat) bytiming collapsehistory
did_multiplegt_dyn Y i t pp_dummy, placebo(4) effects(4) cluster(i) trends_nonparam(nace2) normalized  normalized_weights same_switchers 


// Estimation with did_imputation of Borusyak et al. (2021)
did_imputation Y i t Ei,  fe(i t) horizons(1/3) delta(1) pretrends(5) autosample cluster(i)
event_plot, default_look graph_opt(legend(position(6) rows(1)) xtitle("Periods since the event") ytitle("Average causal effect") ///
	title("Borusyak et al. (2021) imputation estimator") xlabel(-5(1)3)) 
estimates store bjs // storing the estimates for later

did_imputation Y i t Ei,  fe(i t) delta(1) leaveout avgeffectsby(D) 
did2s Y, first_stage(i.id i.year) second_stage(pp_dummy) treatment(pp_dummy) cluster(i)


// Estimation with cldid of Callaway and Sant'Anna (2020)

csdid Y, ivar(i) time(t) gvar(gvar) notyet rseed(42)  long2
estat event  // this produces and stores the estimates at the same time
event_plot cs, default_look graph_opt(legend(position(6) rows(1)) xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-5(1)3) ///
	title("Callaway and Sant'Anna (2020)")) stub_lag(Tp#) stub_lead(Tm#) together
estimates store cs

// Estimation with eventstudyinteract of Sun and Abraham (2020)
never-treated cohort
forvalues l = 0/2 {
	gen L`l'event = K==`l'
}
forvalues l = 1/5 {
	gen F`l'event = K==-`l'
}
drop F1event // normalize K=-1 (and also K=-15) to zero
gen L3plus = cond(K>=3, 1, 0)
gen F5plus = cond(K<-5, 1, 0)
eventstudyinteract Y L*event L3plus F5plus F*event, vce(cluster i) absorb(i t) cohort(Ei) control_cohort(lastcohort) 
event_plot e(b_iw)#e(V_iw), default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-5(1)2) ///
	title("Sun and Abraham (2020)")) stub_lag(L#event) stub_lead(F#event) together

matrix sa_b = e(b_iw) // storing the estimates for later
matrix sa_v = e(V_iw)







stackedev outcome Y L*event L3plus F5plus F*event ref, cohort(treat_year) time(year) never_treat(no_treat)
    unit_fe(state) clust_unit(state) covariates(cov)










// TWFE OLS estimation 
reghdfe Y L*event L3plus F5plus F*event, a(i t)
event_plot, default_look stub_lag(L#event) stub_lead(F#event) together graph_opt(xtitle("Days since the event") ytitle("OLS coefficients") xlabel(-5(1)2) ///
	title("OLS"))
estimates store ols // saving the estimates for later





* Calculate efficient estimator for the simple weighted average
staggered Y, i(i) t(t) g(gvar) estimand(simple)
staggered Y,  i(i) t(t) g(gvar) estimand(eventstudy) eventTime(0/11)
tempname CI b
mata st_matrix("`CI'", st_matrix("r(table)")[5::6, .])
mata st_matrix("`b'",  st_matrix("e(b)"))
matrix colnames `CI' = `:rownames e(thetastar)'
matrix colnames `b'  = `:rownames e(thetastar)'
coefplot matrix(`b'), ci(`CI') vertical yline(0)


gen L3plus = cond(K>=3, 1, 0)
gen F4plus = cond(K<=-4, 1, 0)
reghdfe Y F4plus F3event F2event L0event-L2event L3plus, a(i t#nace2) cluster(id) nocons
pretrends power 0.5, pre(1/3) post(4/7)
matrix sigma = e(V)
matrix beta  = e(b)
matrix beta  = beta[., 1..7]
matrix sigma = sigma[1..7, 1..7]
pretrends, numpre(3) b(beta) v(sigma) slope(`r(slope)')
return list

local plotopts ytitle("Estimate and 95% Conf. Int.") title("Effect on Y")
coefplot, vertical yline(0) ciopts(recast(rcap)) xlabel(,angle(45)) `plotopts'
honestdid, pre(1/3) post(4/7) mvec(0.5(0.5)2)
local plotopts xtitle(Mbar) ytitle(95% Robust CI)
honestdid, cached coefplot `plotopts'
matrix l_vec = 0.25 \ 0.25 \ 0.25 \ 0.25 
local plotopts xtitle(Mbar) ytitle(95% Robust CI)
honestdid, l_vec(l_vec) pre(1/3) post(4/7) mvec(0(0.5)2) omit coefplot `plotopts'

honestdid, coefplot cached

csdid Y, time(t) ivar(i) gvar(gvar) long2 notyet
estat simple
estat group
estat calendar
csdid_estat event, window(-5 0) estore(csdid)
estimates restore csdid
local plotopts xtitle(Mbar) ytitle(95% Robust CI)
honestdid, pre(3/7) post(8/8) mvec(3(1)5) coefplot `plotopts'




