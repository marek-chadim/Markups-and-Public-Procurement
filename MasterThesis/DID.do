clear all
use did


bys id: egen meanpp = mean(pp_dummy)
tab meanpp
bys id: gen treated = cond(meanpp>0, 1, 0)
tab treated
gen i = id
gen t = year-2005
gen markup = muhat_tl
gen Y = ln(muhat_tl)
egen Ei = min(t / (pp_dummy == 1)), by(id)
sum Ei
drop K
gen K = t-Ei // "relative time", i.e. the number periods since treated (could be missing if never-treated)
gen D = K>=0 & Ei!=. 	
tab pp_dummy D 

drop W
twowayfeweights Y i t D, type(feTR)
dis beta

gen lastcohort = Ei==r(max) // dummy for the latest- or never-treated cohort
gen gvar = cond(Ei==., 0, Ei) // dummy for the treated cohort
xtevent Y, policy(D) window(5) impute(stag) trend(-5) plot

reghdfe Y pp_dummy,  a(i t#nace2)
reghdfe Y D,  a(i t#nace2)

lpdid Y, time(t) unit(i) treat(pp_dummy) nocomp pre(4) post(4) nonabs(1, notyet)  absorb(nace2) 


panelview Y pp_dummy , i(i) t(t) type(treat) bytiming collapsehistory
did_multiplegt_dyn Y i t pp_dummy, placebo(3) effects(4) cluster(i) trends_nonparam(nace2) normalized  normalized_weights same_switchers 


panelview Y D , i(i) t(t) type(treat) bytiming collapsehistory
bys id: gen D1=D[1]
drop if D1==1
xtset i t

// Estimation with did_imputation of Borusyak et al. (2021)
did_imputation Y i t Ei,  fe(i t#nace2) allhorizons delta(1) pretrends(13) minn(0) cluster(i)
event_plot, default_look graph_opt(legend(position(6) rows(1)) xtitle("Periods since the event") ytitle("Average causal effect") ///
	title("Borusyak et al. (2021) imputation estimator") xlabel(-13(1)14)) 
estimates store bjs // storing the estimates for later
did_imputation Y i t Ei,  fe(i t#nace2) delta(1) leaveout avgeffectsby(D) 
did2s Y, first_stage(i.id year##nace2) second_stage(D) treatment(D) cluster(i)
help fect
fect Y, treat(D) unit(id) time(t)  method("fe")   degree(1) 
matlist e(ATT)
fect Y, treat(D) unit(id) time(t)  method("ife") cv
 matlist e(ATT)
fect Y, treat(D) unit(id) time(t)  method("mc") cv 
 matlist e(ATT)


// Estimation with cldid of Callaway and Sant'Anna (2020)

csdid Y, ivar(i) time(t) gvar(gvar) notyet rseed(42)  long2
estat event, estore(cs) // this produces and stores the estimates at the same time
event_plot cs, default_look graph_opt(legend(position(6) rows(1)) xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-14(1)14) ///
	title("Callaway and Sant'Anna (2020)")) stub_lag(Tp#) stub_lead(Tm#) together
estimates store cs

// Estimation with eventstudyinteract of Sun and Abraham (2020)
sum Ei
gen lastcohort = Ei==r(max) // dummy for the latest- or never-treated cohort
forvalues l = 0/14 {
	gen L`l'event = K==`l'
}
forvalues l = 1/14 {
	gen F`l'event = K==-`l'
}
drop F1event // normalize K=-1 (and also K=-15) to zero
eventstudyinteract Y L*event F*event, vce(cluster i) absorb(i t) cohort(Ei) control_cohort(lastcohort) 
event_plot e(b_iw)#e(V_iw), default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-14(1)14) ///
	title("Sun and Abraham (2020)")) stub_lag(L#event) stub_lead(F#event) together

matrix sa_b = e(b_iw) // storing the estimates for later
matrix sa_v = e(V_iw)

// TWFE OLS estimation 
reghdfe Y L*event F*event, a(i t##nace2)
event_plot, default_look stub_lag(L#event) stub_lead(F#event) together graph_opt(xtitle("Days since the event") ytitle("OLS coefficients") xlabel(-14(1)14) ///
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
csdid_estat event, window(-4 5) estore(csdid)
estimates restore csdid
local plotopts xtitle(Mbar) ytitle(95% Robust CI)
honestdid, pre(3/6) post(7/12) mvec(0.5(0.5)2) coefplot `plotopts'


order i t Ei K D
tab Ei, gen(G)
bys id: gen nyear=[_N]
tab nyear, gen(nyear)
drop if nyear<16
tab Ei
panelview Y D , i(i) t(t) type(treat) bytiming collapsehistory
bacondecomp Y D, ddetail
sdid Y id year D, vce(bootstrap) graph graph_export(sdid_, .eps) g1_opt(xtitle(""))  seed(42)  
 matlist e(tau)
sdid_event Y id year D, vce(bootstrap) disag 

