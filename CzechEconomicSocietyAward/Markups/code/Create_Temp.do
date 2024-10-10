/* MAREK CHADIM: Markups and Public Procurement
*  Analysis: prep file for output
*/
clear
* Use trimmed sample p=1-5 [main results for p=1]
forvalues p=1/1{
use "$dropbox/temp/data_trim_`p'.dta"
}

gen mu_cs = costshare*(rGO/rCOGS)
label var mu_cs "markup firm-level costshare
bysort year: egen cs_med_t = median(costshare)
gen mu_med = cs_med_t*(rGO/rCOGS)
label var mu_med "markup median costshare 

*------------------------------------------------------------------------------*
* OUTPUT ELASTICITIES ESTIMATED VIA PF ESTIMATION - PARAMETERS F(COGS, K) 
preserve
set more off
set seed 42
qui:
*------FIRST STAGE POLYNOMIAL INTERACTED WITH PROCUREMENT-----------------------*
local M=3
local N=3
forvalues i=1/`M' {
    gen cogs`i'=cogs^(`i')
    gen k`i'=k^(`i')
    forvalues j=1/`N' {
        gen k`i'cogs`j'=k^(`i')*cogs^(`j')
    }
}
drop k1 cogs1
xi: reg go c.k*#pp_dummy c.cogs*#pp_dummy year##nace2
predict phi
predict epsilon, res
*---COMPUTE CORRECTED SHARES----------------------------------------------------*
gen y_c=go-epsilon
gen Y_c=exp(y_c)
gen alphahat=exp(cogs)/Y_c
*------------------------------------------------------------------------------*
xtset id year
gen Lphi=L.phi
gen Lpp=L.pp_dummy
gen kcogs=k*cogs
gen Lk=L.k
gen Lcogs = L.cogs
gen Lk2=Lk^2
gen Lcogs2=Lcogs^2
gen LkLcogs=Lk*Lcogs
gen kLcogs=k*Lcogs
gen const = 1
*------------------------------------------------------------------------------*
drop if k == .
drop if cogs == .
drop if Lk == .
drop if Lcogs == .
drop if phi == .
drop if Lphi == .
drop if Lpp == .
*------------- DYNAMIC PANEL FOR STARTING VALUES--------------------------------*
xtabond2 go L.go L(0/1).(k cogs k2 cogs2 k1cogs1) i.year, ///
	 gmmstyle(L.(cogs cogs2) k k2 kLcogs) ivstyle(i.year, equation(level)) 
local cons_ols_tl = _b[_cons]
local k_ols_tl = _b[k]
local cogs_ols_tl = _b[cogs]
local k2_ols_tl = _b[k2]
local cogs2_ols_tl = _b[cogs2]
local kcogs_ols_tl = _b[k1cogs1]
*-----------------------------BEGIN MATA PROGRAM-------------------------------*
clear mata
mata:
void GMM_DLW_TL(todo,betas,PHI,PHI_LAG,PP_lag,Z,X,X_lag,W,crit,g,H)
{
    PHI=st_data(.,("phi"))
    PHI_LAG=st_data(.,("Lphi"))
	PP_lag=st_data(.,("Lpp"))
    Z=st_data(.,("const","k","Lcogs","k2","Lcogs2","kLcogs"))
    X=st_data(.,("const","k","cogs","k2","cogs2","kcogs"))
    X_lag=st_data(.,("const","Lk","Lcogs","Lk2","Lcogs2","LkLcogs"))
	W = invsym(Z'Z)
    C=st_data(.,("const"))

	OMEGA=PHI-X*betas'
	OMEGA_lag=PHI_LAG-X_lag*betas'
	OMEGA_lag2 = OMEGA_lag:*OMEGA_lag
	OMEGA_lag3 = OMEGA_lag2:*OMEGA_lag
	OMEGA_lag_pol = (C,OMEGA_lag,OMEGA_lag2,OMEGA_lag3,PP_lag)
	g_b = invsym(OMEGA_lag_pol'OMEGA_lag_pol)*OMEGA_lag_pol'OMEGA
	XI=OMEGA-OMEGA_lag_pol*g_b
	crit=(Z'XI)'*W*(Z'XI)
}

void DLW_TRANSLOG()
    {
S = optimize_init()
optimize_init_argument(S, 1, PHI)
optimize_init_argument(S, 2, PHI_LAG)
optimize_init_argument(S, 3, PP_lag)
optimize_init_argument(S, 4, Z)
optimize_init_argument(S, 5, X)
optimize_init_argument(S, 6, X_lag)
optimize_init_argument(S, 7, W)
optimize_init_params(S,(`cons_ols_tl',`k_ols_tl',`cogs_ols_tl', ///
						`k2_ols_tl',`cogs2_ols_tl',`kcogs_ols_tl'))
optimize_init_evaluator(S, &GMM_DLW_TL())			
optimize_init_nmsimplexdeltas(S, 1e-5)
optimize_init_technique(S, "nm")
optimize_init_which(S,"min")
optimize_init_conv_warning(S, "off")
p=optimize(S)
optimize_init_params(S,p)
p=optimize(S)
optimize_init_params(S,p)
p=optimize(S)
p
st_matrix("beta_dlwtranslog",p)
}
end
*-------------------------------END MATA PROGRAM--------------------------------*
cap program drop dlw_tl
program dlw_tl, eclass
preserve
sort id year
mata DLW_TRANSLOG()
scalar const_tl = beta_dlwtranslog[1,1]
scalar k_tl = beta_dlwtranslog[1,2]
scalar cogs_tl = beta_dlwtranslog[1,3]
scalar k2_tl = beta_dlwtranslog[1,4]
scalar cogs2_tl = beta_dlwtranslog[1,5]
scalar kcogs_tl = beta_dlwtranslog[1,6]
matrix beta_dlwtranslog=beta_dlwtranslog
mat colnames beta_dlwtranslog = const k cogs k2 cogs2 kcogs
ereturn post beta_dlwtranslog
restore
end
*-------------------------------------------------------------------------------*
tsset, clear
bootstrap, notable cluster(id) reps($bootreps): dlw_tl
estat bootstrap
matrix beta_dlwtranslog = e(b)
gen k_tl = beta_dlwtranslog[1,2]
gen cogs_tl = beta_dlwtranslog[1,3]
gen k2_tl = beta_dlwtranslog[1,4]
gen cogs2_tl = beta_dlwtranslog[1,5]
gen kcogs_tl = beta_dlwtranslog[1,6]
gen betahat_tl = cogs_tl+2*cogs2_tl*cogs+kcogs_tl*k
gen muhat_tl = betahat_tl/alphahat
keep id year muhat_tl betahat_tl
sort id year
save "$dropbox/data/PF/estimates.dta" , replace
restore
*-------------------------------------------------------------------------------*
sort id year
merge 1:1 id year using "$dropbox/data/PF/estimates.dta", gen(estimates)
gen elasticity = betahat_tl
label var elasticity "Elasticy firm-level PF Translog
drop if elasticity ==.
gen markup = muhat_tl
label var markup "Markup firm-level PF Translog
drop if markup ==.
*-------------------------------------------------------------------------------*
sum elasticity markup 
*-------------------------------------------------------------------------------*
* sample summary statistics : Appendix
tabstat rGO rCOGS rK, stat(mean median N) 
*------------------------------------------------------------------------------*
qui: 
{ // 1  WEIGHTED
bysort year:	egen TOTCOGS	= sum(rCOGS)
bysort year:  	egen TOTSALES 	= sum(rGO)
gen share_firm_agg = rGO/TOTSALES

	* 2.1.1 costshares
gen costshare_w 					= costshare*share_firm_agg
bysort year: egen COSTSHARE_AGG 	= sum(costshare_w)
	*2.1.2 elasticities
bysort year: egen elasticity_W = sum(share_firm_agg*elasticity) 
*-------------------------------------------------------------------------------*
* 2 MARKUPS	
bysort year: egen MARKUPcs_AGG = sum(share_firm_agg*cs_med_t)
label var MARKUPcs_AGG "Agg Markup CS (Sales Weight)

bysort year: egen MARKUP_AGG = sum(share_firm_agg*markup)
label var MARKUP_AGG "Agg Markup PF (Sales Weight)

	* 2.B INPUT WEIGHTS
gen input_w = rCOGS/TOTCOGS
bysort year: egen MARKUP_AGG_w 	= sum(input_w*markup)
label var MARKUP_AGG_w "Agg Markup PF (Input Weight)

* sales weighted markup percentiles
gen mu =  markup
bysort year (mu): gen ms_cum_mu = sum(share_firm_agg) 
bysort year (mu): gen ms90 = 1 if ms_cum_mu<.9
bysort year (mu): gen ms75 = 1 if ms_cum_mu<.75
bysort year (mu): gen ms50 = 1 if ms_cum_mu<.5
bysort year (mu): gen ms25 = 1 if ms_cum_mu<.25
bysort year (mu): gen ms10 = 1 if ms_cum_mu<.1
bysort year (mu): egen mu_ms90 =	max(mu) if ms90==1
bysort year (mu): egen mu_ms75 =	max(mu) if ms75==1
bysort year (mu): egen mu_ms50 =	max(mu) if ms50==1
bysort year (mu): egen mu_ms25 =	max(mu) if ms25==1
bysort year (mu): egen mu_ms10 =	max(mu) if ms10==1
label var mu_ms90 "p90 (ms)
label var mu_ms75 "p75 (ms)
label var mu_ms50 "p50 (ms)
label var mu_ms25 "p25 (ms)
*------------------------------------------------------------------------------*
* RTS using cost share a la Syverson
bysort year: egen cogstot = sum(rCOGS)
bysort year: egen ktot = sum(rK)
bysort year: egen totcost = sum(rCOGS+rK)
bysort year: egen totsales = sum(rGO)
gen CS_TOT_C = cogstot/totcost
gen CS_TOT_K = 1-CS_TOT_C
gen INPUT1 = (rGO^costshare)*(rK^(1-costshare))
gen input1 = ln(INPUT1)
gen INPUT2 = (rGO^CS_TOT_C)*(rK^(1-CS_TOT_C))
gen input2 = ln(INPUT2)
gen gamma_RTS1 = .
gen gamma_RTS2 = .
gen y = ln(rGO)
forvalues s= 1/2 {
forvalues t = 2006 /2021 {
qui: reg y input`s' [aw=totsales] if `t'==year 
replace gamma_RTS`s' = _b[input`s'] if `t'==year 
}
}
preserve
keep year gamma_RTS* 
sort year
drop if year==year[_n-1]
save "temp/gamma_syverson.dta", replace
restore
}
*-------------------------------------------------------------------------------*
save "$dropbox/temp/temp_file.dta", replace
* temp file created