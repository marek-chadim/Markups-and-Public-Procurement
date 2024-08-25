use data, clear
set more off
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
drop k1 cogs1 k1cogs3 k2cogs2 k2cogs3 k3cogs*
xi: reg go c.k*#pp_dummy c.cogs*#pp_dummy year#pp_dummy
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
*----------OLS REGRESSION FOR STARTING VALUES----------------------------------*
qui: reg go k cogs i.year
local cons_ols_cd = _b[_cons]
mat b = e(b)
local k_ols_cd = b[1,1] 
local cogs_ols_cd = b[1,2] 

qui: reg go k cogs k2 cogs2 k1cogs1 i.year
local cons_ols_tl = _b[_cons]
mat b = e(b)
local k_ols_tl = b[1,1]
local cogs_ols_tl = b[1,2]
local k2_ols_tl = b[1,3]
local cogs2_ols_tl = b[1,4]
local kcogs_ols_tl = b[1,5]
*-----------------------------BEGIN MATA PROGRAM-------------------------------*
clear mata
mata:
void GMM_DLW(todo,betas,PHI,PHI_LAG,PP_lag,Z,X,X_lag,W,crit,g,H)
{
    PHI=st_data(.,("phi"))
    PHI_LAG=st_data(.,("Lphi"))
	PP_lag=st_data(.,("Lpp"))
    Z=st_data(.,("const","k","Lcogs"))
    X=st_data(.,("const","k","cogs"))
    X_lag=st_data(.,("const","Lk","Lcogs"))
	W=invsym(Z'Z)
	C=st_data(.,("const"))
	
    OMEGA=PHI-X*betas'
    OMEGA_lag=PHI_LAG-X_lag*betas'
	OMEGA_lag2 = OMEGA_lag:*OMEGA_lag
	OMEGA_lag3 = OMEGA_lag2:*OMEGA_lag
	PI_HAT2 = PI_HAT:*PI_HAT
	PI_HAT3 = PI_HAT2:*PI_HAT
	OMEGA_lag_pol = (C,OMEGA_lag,OMEGA_lag2,OMEGA_lag3,PP_lag)
    g_b = invsym(OMEGA_lag_pol'OMEGA_lag_pol)*OMEGA_lag_pol'OMEGA
    XI=OMEGA-OMEGA_lag_pol*g_b
    crit=(Z'XI)'*W*(Z'XI)
}


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

void DLW()
    {
S = optimize_init()
optimize_init_argument(S, 1, PHI)
optimize_init_argument(S, 2, PHI_LAG)
optimize_init_argument(S, 3, PP_lag)
optimize_init_argument(S, 4, Z)
optimize_init_argument(S, 5, X)
optimize_init_argument(S, 6, X_lag)
optimize_init_argument(S, 7, W)
optimize_init_evaluator(S, &GMM_DLW())
optimize_init_evaluatortype(S,"d0")
optimize_init_technique(S, "nm")
optimize_init_nmsimplexdeltas(S, 0.01)
optimize_init_which(S,"min")
optimize_init_params(S,(`cons_ols_cd',`k_ols_cd',`cogs_ols_cd'))
p=optimize(S)
optimize_init_params(S,p)
p=optimize(S)
optimize_init_params(S,p)
p=optimize(S)
p
st_matrix("beta_dlw",p)
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
optimize_init_nmsimplexdeltas(S, 0.01)
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
cap program drop dlw_cd
program dlw_cd, eclass
preserve
sort id year
mata DLW()
scalar const_cd = beta_dlw[1,1]
scalar k_cd 	= beta_dlw[1,2]
scalar cogs_cd 	= beta_dlw[1,3]
matrix beta_dlw=beta_dlw
mat colnames beta_dlw = const k cogs
ereturn post beta_dlw
restore
end

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
bootstrap, cluster(id) idcluster(year) reps(2): dlw_cd
estat bootstrap
matrix beta_dlw = e(b)
gen k_cd = beta_dlw[1,2]
gen cogs_cd = beta_dlw[1,3]
gen omegahat_cd = phi-k_cd*k-cogs_cd*cogs
gen betahat_cd  = cogs_cd
gen muhat_cd = betahat_cd/alphahat
*-------------------------------------------------------------------------------*
tsset, clear
bootstrap, cluster(id) idcluster(year) reps(2): dlw_tl
estat bootstrap
matrix beta_dlwtranslog = e(b)
gen k_tl = beta_dlwtranslog[1,2]
gen cogs_tl = beta_dlwtranslog[1,3]
gen k2_tl = beta_dlwtranslog[1,4]
gen cogs2_tl = beta_dlwtranslog[1,5]
gen kcogs_tl = beta_dlwtranslog[1,6]
gen omegahat_tl = phi-k_tl*k-cogs_tl*cogs-k2_tl*k2-cogs2_tl*cogs2-kcogs_tl*k1cogs1
gen betahat_tl = cogs_tl+2*cogs2_tl*cogs+kcogs_tl*k
gen muhat_tl = betahat_tl/alphahat
*-------------------------------------------------------------------------------*

rename muhat_tl markup
keep id year nace2 legal_form subject_type inst_sector empl* ///
						go cogs k w pp_dummy pp_share markup 
destring legal_form, replace
destring subject_type, replace
destring inst_sector, replace
destring empl_cat, replace
save markups, replace




