/*
	This simulated example illustrates how to estimate causal effects with event studies using a range of methods
	and plot the coefficients & confidence intervals using the event_plot command.
	
	Date: 28/05/2021
	Author: Kirill Borusyak (UCL), k.borusyak@ucl.ac.uk
	
	You'll need the following commands:
		- did_imputation (Borusyak et al. 2021): available on SSC
		- did_multiplegt (de Chaisemartin and D'Haultfoeuille 2020): available on SSC
		- eventstudyinteract (San and Abraham 2020): available on SSC
		- csdid (Callaway and Sant'Anna 2020): available on SSC

*/

// Generate a complete panel of 300 units observed in 15 periods
clear all
timer clear
set seed 42
use panel
tsset i t

panelview Y D , i(i) t(t) type(treat) bytiming collapsehistory
bacondecomp Y D , ddetail  gro(legend(position(6) rows(1)) )
graph export "bacondecomp.pdf", replace



sdid Y id year D, vce(bootstrap) graph graph_export(sdid_, .pdf) g1_opt(xtitle(""))  seed(42)  
 matlist e(tau)
sdid_event Y id year D, vce(bootstrap) disag 




xtreg Y D i.t, fe cluster(i)
// Estimation with did_imputation of Borusyak et al. (2021)
did_imputation Y i t Ei, minn(0) 
did_imputation Y i t Ei, horizons(0/3) pretrend(7) minn(0) autosample
event_plot, default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
	title("Borusyak et al. (2021) imputation estimator") xlabel(-3(1)3))

estimates store bjs // storing the estimates for later

// Estimation with did_multiplegt of de Chaisemartin and D'Haultfoeuille (2020)
did_multiplegt (old) Y i t D, robust_dynamic dynamic(3) placebo(7) breps(10) cluster(i)      firstdiff_placebo
event_plot e(estimates)#e(variances), default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
	title("de Chaisemartin and D'Haultfoeuille (2020)") xlabel(-3(1)3)) stub_lag(Effect_#) stub_lead(Placebo_#) together

matrix dcdh_b = e(estimates) // storing the estimates for later
matrix dcdh_v = e(variances)

// Estimation with cldid of Callaway and Sant'Anna (2020)
gen gvar = cond(Ei==., 0, Ei) // group variable as required for the csdid command
csdid Y, ivar(i) time(t) gvar(gvar) notyet
estat event, window(-7 3) estore(cs) // this produces and stores the estimates at the same time
event_plot cs, default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-5(1)5) ///
	title("Callaway and Sant'Anna (2020)")) stub_lag(Tp#) stub_lead(Tm#) together

// Estimation with eventstudyinteract of Sun and Abraham (2020)
sum Ei
gen lastcohort = Ei==r(max) // dummy for the latest- or never-treated cohort
forvalues l = 0/10 {
	gen L`l'event = K==`l'
}
forvalues l = 1/10 {
	gen F`l'event = K==-`l'
}
drop F1event // normalize K=-1 (and also K=-15) to zero
eventstudyinteract Y L*event F*event, vce(cluster i) absorb(i t) cohort(Ei) control_cohort(lastcohort)
event_plot e(b_iw)#e(V_iw), default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-3(1)3) ///
	title("Sun and Abraham (2020)")) stub_lag(L#event) stub_lead(F#event) together

matrix sa_b = e(b_iw) // storing the estimates for later
matrix sa_v = e(V_iw)

// TWFE OLS estimation (which is correct here because of treatment effect homogeneity). Some groups could be binned.
reghdfe Y F*event L*event, a(i t) cluster(i)
event_plot, default_look stub_lag(L#event) stub_lead(F#event) together graph_opt(xtitle("Days since the event") ytitle("OLS coefficients") xlabel(-3(1)3) ///
	title("OLS"))

estimates store ols // saving the estimates for later


// Combine all plots using the stored estimates 
set scheme lean2
event_plot bjs dcdh_b#dcdh_v cs sa_b#sa_v ols, ///
	stub_lag(tau# Effect_# Tp# L#event  L#event) stub_lead(pre# Placebo_# Tm# F#event F#event) plottype(scatter) ciplottype(rcap) ///
	together perturb(-0.325(0.13)0.325) trimlead(7) trimlag(3) noautolegend ///
		graph_opt(xtitle("Periods since engagement in public procurement") ytitle("Average effect on firm markups (%)") xlabel(-7(1)3) ylabel(0(0.1)0.3) ///
		legend(order(1 "BJS" 3 "dC-DH" ///
				5 "C-S" 7 "S-A" 9 "OLS") position(6) rows(1) region(style(none))) ///
	/// the following lines replace default_look with something more elaborate
		xline(-0.5, lcolor(gs8) lpattern(dash)) yline(0, lcolor(gs8)) graphregion(color(white)) bgcolor(white) ylabel(, angle(horizontal)) ///
	) ///
	lag_opt1(msymbol(+) color(cranberry)) lag_ci_opt1(color(cranberry)) ///
	lag_opt2(msymbol(Dh) color(navy)) lag_ci_opt2(color(navy)) ///
	lag_opt3(msymbol(Th) color(forest_green)) lag_ci_opt3(color(forest_green)) ///
	lag_opt4(msymbol(Sh) color(dkorange)) lag_ci_opt4(color(dkorange)) ///
	lag_opt5(msymbol(Oh) color(purple)) lag_ci_opt5(color(purple)) 
   graph export "estimators_example.pdf", replace

	*graph_opt(title("Event study estimators in a balanced panel (26 units, 16 periods)", size(medlarge)) ///