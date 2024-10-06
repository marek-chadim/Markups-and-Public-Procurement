/*
 MAKE FIGURES: PULLS IN TEMP FILE FROM CREATE_TEMP_FILE
*/
use "temp/temp_file.dta", clear
cd "output/"
*---------------------------------------------------------------------------------------------------------------------------*
cd "figures/"
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 1. Aggregate Markups
preserve
sort year
drop if year==year[_n-1]
sort year
scatter MARKUP_AGG MARKUP_AGG_w year, c(l) lcolor(red ) lpattern(solid) symbol(none) lwidth(thick) ytitle("") xlabel(2006 2009 2012 2015 2018 2021) xtitle("") legend(ring(0)  pos(5) ) 
graph export Fig1.png, replace
scatter MARKUP_AGG MARKUP_AGG_w year, c(l) lcolor(black ) lpattern(solid) symbol(none) lwidth(thick) ytitle("") xlabel(2006 2009 2012 2015 2018 2021) xtitle("") legend(ring(0)  pos(5) ) 
graph export BW/Fig1.png, replace
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 2a 2b Distribution of markups
label var mu_ms90 "P90
label var mu_ms50 "P50
label var mu_ms75 "P75
label var MARKUP_AGG "Average (Sales Weight)
scatter  MARKUP_AGG mu_ms90 mu_ms75 mu_ms50 year, connect(l l l l )  msymbol(none none none none ) color(red red red red) lpattern(solid dash shortdash longdash_dot) lwidth(thick thick thick thick) xtitle("") xlabel(2006 2009 2012 2015 2018 2021) legend(ring(0)  pos(2) )   sort 
graph export Fig2a.png, replace
scatter  MARKUP_AGG mu_ms90 mu_ms75 mu_ms50 year, connect(l l l l )   msymbol(none none none none ) color(black black black black) lpattern(solid dash shortdash longdash_dot) lwidth(thick thick thick thick) xtitle("") xlabel(2006 2009 2012 2015 2018 2021) legend(ring(0)  pos(2) )   sort name(a)
graph export BW/Fig2a.png, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
preserve
drop if mu>2.5
twoway (kdensity mu if (year==2006) , kernel(gaussian) lcolor(red) lwidth(thick) ) (kdensity mu if (year==2021),  kernel(gaussian)  lcolor(red) ytitle("") xtitle("") clpattern(dash) lwidth(thick) graphregion(color(white) )  legend(ring(0)  col(1) pos(2) label(1 "2006") label(2 "2021")  ) )
graph export Fig2b.png, replace
twoway (kdensity mu if (year==2006) , kernel(gaussian) lcolor(black) lwidth(thick) ) (kdensity mu if (year==2021),  kernel(gaussian)  lcolor(black) ytitle("") xtitle("") clpattern(dash) lwidth(thick) graphregion(color(white) )  legend(ring(0)  col(1) pos(2) label(1 "2006") label(2 "2021") ) name(b) ) 
graph export BW/Fig2b.png, replace
graph combine a b
graph export BW/Fig2.png, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
*Fig 3 Cost-share based aggregated technology 
preserve
sort year
drop if year==year[_n-1]
label var elasticity_W "Variable Input Elasticity (Sales Weight)
label var COSTSHARE_AGG "Variable Input Cost Share (Sales Weight)
scatter elasticity_W COSTSHARE_AGG year, c(l l )  lcolor(red red)  lpattern(solid dash ) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(2006 2021) xtitle("") legend(ring(0) pos(8) col(1)) 
graph export Fig3.png, replace
scatter elasticity_W COSTSHARE_AGG year, c(l l )  lcolor(black black)  lpattern(solid dash ) msymbol( none none ) lwidth(thick thick) ytitle("") xlabel(2006 2021) xtitle("") legend(ring(0) pos(8) col(1)) 
graph export BW/Fig3.png, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 4 Returns to scale
preserve
sort year
drop if year==year[_n-1]
label var gamma_RTS1 "RTS (firm)
label var gamma_RTS2 "RTS (aggregate)
scatter gamma_RTS1 gamma_RTS2 year , c(l l) lcolor(green green) ysc(r(.95 1.1)) lpattern(solid dash) yline(1) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(2006 2021) xtitle("") legend(ring(0) col(1) pos(11) ) 
graph export Fig4.png, replace
scatter gamma_RTS1 gamma_RTS2 year , c(l l) lcolor(black black) ysc(r(.95 1.1)) lpattern(solid dash) yline(1) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(2006 2021) xtitle("") legend(ring(0) col(1) pos(11) ) 
graph export BW/Fig4.png, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
/*preserve
sort year
drop if year==year[_n-1]
forvalues s=1/2 {
gen MARKUP_AGG_g`s' = MARKUPcs_AGG*gamma_RTS`s' 
}
label var MARKUP_AGG_g1 "Cost Share (firm) Returns to Scale
label var MARKUP_AGG_g2 "Cost Share (industry mean) Returns to Scale
scatter MARKUP_AGG MARKUP_AGG_g1 MARKUP_AGG_g2 year, c(l l) lcolor(blue blue) lpattern(solid dash) symbol(none none) lwidth(thick thick) ylabel() xlabel(2006 2021) xtitle("") legend(ring(0)  pos(11)) 
graph export Fig4b.png, replace 
scatter MARKUP_AGG MARKUP_AGG_g1 MARKUP_AGG_g2 year, c(l l) lcolor(black black) lpattern(solid dash) symbol(none none) lwidth(thick thick) ylabel() xlabel(2006 2021) xtitle("") legend(ring(0)  pos(11)) 
graph export BW/Fig4b.png, replace 
restore
*-------------------------------------------------------------------------------------------------------------------------*
/* APPENDIX
cd "$dropbox"
use "temp/temp_file.dta", clear
cd "output/figures/"
*-------------------------------------------------------------------------------------------------------------------------*
* Olley Pakes Output elasticities:
preserve
xtset id year, yearly
gen lns = ln(rGO)
gen v = ln(rCOGS)
gen lnk = ln(rK)
gen Investment = rK-.9*L.rK
gen lni = ln(Investment)
gen lnk2 = lnk^2
gen lni2 = lni^2
gen lnk3 = lnk^3
gen lni3 = lni^3
gen lnki = lnk*lni
gen theta_op 	= .
egen nrind = group(nace2)
forvalues t=2007/2021 {
forvalue  s = 1/3 {
qui: reg lns v lni lni2 lni3 lnk lnk2 lnk3 lnki share_firm_agg if year==`t' & nrind==`s'
replace theta_op = _b[v] if year==`t' & nrind==`s'
}
}	
gen mu_op = theta_op*rGO/rCOGS
bysort year: egen Mop = sum(share_firm_agg*mu_op)
bysort year: egen Theta_OP_st = sum(share_firm_agg*theta_op)
bysort year: egen Theta_ACF = sum(share_firm_agg*elasticity)
sort year
drop if year==year[_n-1]
label var  Theta_OP_st "elasticity OP C-D (sector specific)
label var  Theta_ACF "elasticity ACF Translog (firm specific)
scatter Theta_OP_st Theta_ACF year if year>2006, c(l l)  lcolor(red) lpattern(solid dash) msymbol(none none) legend(pos(6) col(2)) lwidth(thick thick) xtitle("") ytitle("")  xlabel(2007 2021)  legend(ring(0) col(1) pos(7) ) 
graph export FigA1.eps, replace
scatter Theta_OP_st Theta_ACF year if year>2006, c(l l) lcolor(black) lpattern(solid dash) msymbol(none none) legend(pos(6) col(2)) lwidth(thick thick) xtitle("") ytitle("")  xlabel(2007 2021)  legend(ring(0) col(1) pos(7) ) 
graph export BW/FigA1.eps, replace
restore
*-------------------------------------------------------------------------------------------------------------------------*
* Industry specific average markups using labor cost / Leontief using L
* Estimate Leontief production a la De Loecker-Scott - by sector - by year
preserve 
drop k K y
gen y  		= go
gen k  		= ln(rK)
gen k2 		= k^2
xtset id year, yearly
gen K  		= exp(k)
gen INV 	= K - (1-.1)*L.K
gen i		= ln(INV)
gen i2		= i^2
gen lnXLR 	= ln(rW)
* subsample with wage data
drop if rW==.
drop if rW<0
drop if empl_num==0
gen M = rCOGS-rW
gen M_s = M/rGO
drop if M_s>1 & M_s<0
xtset id year, yearly
* Leontief(L, K)
gen th_l_s = . 
forvalues s = 41/43 {
qui: ivregress gmm y (lnXLR = L.lnXLR) k L.k  L.k2 L.i L.i2  if `s'==nace2
replace th_l_s = _b[lnXLR] if `s'==nace2
}
gen th_l_t = . 
forvalues t = 2008/2021 {
qui: ivregress gmm y (lnXLR = L.lnXLR)  k L.k  L.k2 L.i L.i2  i.nace2 if year==`t'
replace th_l_t = _b[lnXLR] if year==`t'
}
gen muls = th_l_s*(rGO/rW)
gen mus_lab = 1/ ((1/muls)+M_s)
gen mult = th_l_t*(rGO/rW)
gen mut_lab = 1/ ((1/mult)+M_s)
keep if mut_lab<5
keep if mus_lab<5
bysort year: egen ts = sum(rGO)
gen mssub = rGO/ ts
bysort year: egen MAGG_LABs = sum(mssub*mus_lab)
bysort year: egen MAGG_LABt = sum(mssub*mut_lab)
sort year
drop if year==year[_n-1]
label var MAGG_LABs "Markup Leontief C-D (sector specific)
label var MAGG_LABt "Markup Leontief C-D (time varying)
label var MARKUP_AGG "Markup Translog (firm specific)
scatter  MAGG_LABs MAGG_LABt MARKUP_AGG year if year>2007, c(l l l  l) lcolor(green green red) msymbol( none none none none) lwidth(thick thick thick) lpattern(dot dash solid) xtitle("")  xlabel(2008 2021) legend(ring (0) pos(10))
graph export FigA2.eps, replace
scatter  MAGG_LABs MAGG_LABt MARKUP_AGG year if year>2007, c(l l l  l) lcolor(black black black) msymbol( none none none none) lwidth(thick thick thick) lpattern(dot dash solid) xtitle("")  xlabel(2008 2021) legend(ring (0) pos(10))
graph export BW/FigA2.eps, replace
restore
*-------------------------------------------------------------------------------------------------------------------------*
/* Figures created: Main text and Appendix
*-------------------------------------------------------------------------------------------------------------------------*
