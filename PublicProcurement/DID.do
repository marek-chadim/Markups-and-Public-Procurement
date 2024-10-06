clear all
*use estimates
drop K
gen i = id 
gen t = year-2005
gen Y = ln(muhat_tl)
egen Ei = min(t / (pp_dummy == 1)), by(id)
bys i (t): replace Ei = Ei[1]
gen K = t-Ei // "relative time", i.e. the number periods since treated (could be missing if never-treated)
gen D = K>=0 & Ei!=. 	
order i t Ei K D
sum Ei

bys id: gen x1 = go[1]
bys id: gen x2 = k[1]
bys id: gen x3 = cogs[1]
bys id: gen x4 = w[1]

*panelview Y D, i(id) t(year) type(treat) collapsehistory bytiming
*panelview Y D, i(id) t(year) type(outcome)
*panelview Y D, i(id) t(year) xlabdist(7) type(bivariate) msize(*0.5) style(c b) 

reghdfe Y D, a(id nace2##year)
preserve
bys id: gen nyear=[_N]
bys id: gen D1=D[1]
keep if nyear==16
drop if D1
sdid Y id year D, vce(bootstrap) graph graph_export(sdid_, .eps) g1_opt(xtitle(""))  covariates(nace2) 
sdid_event Y id year D, vce(bootstrap) covariates(nace2)  placebo(3)
restore


did2s Y, first_stage(i.id year##nace2 x*) second_stage(pp_dummy) treatment(pp_dummy) cluster(i)

// Estimation with did_imputation of Borusyak et al. (2021)
did_imputation Y i t Ei, horizons(1/12) pretrend(13) autosample fe(i t#nace2) unitcontrols(x*)
event_plot, default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
	title("Borusyak et al. (2021) imputation estimator") xlabel(-13(1)12))
tab cannot_impute
tab cannot_impute if pp_mean!=1
estimates store bjs // storing the estimates for later

// Estimation with cldid of Callaway and Sant'Anna (2020)
gen gvar = cond(Ei==., 0, Ei) // group variable as required for the csdid command
csdid Y, ivar(i) time(t) gvar(gvar) notyet
estat event, estore(cs) // this produces and stores the estimates at the same time
event_plot cs, default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-14(1)14) ///
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
reghdfe Y F*event L*event, a(i t) cluster(i)
event_plot, default_look stub_lag(L#event) stub_lead(F#event) together graph_opt(xtitle("Days since the event") ytitle("OLS coefficients") xlabel(-14(1)14) ///
	title("OLS"))

estimates store ols // saving the estimates for later

