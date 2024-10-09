/* Financial statements data files downloaded using protocol:
Access MagnusWeb (IES FSV UK)
> MagnusWeb - Dun & Bradstreet Czech Republic, a.s.
> Czech construction industry
> DATE RANGE 1993 - 2023
> RATIOS
>> wages/valueadded, wages/sales, contribution margin 
> FINANCIAL
>> costs, fixed assets, sales, labour productivity,
> SELECTIONS
>> id, subject_type, legal_form, inst_sector, empl_num, empl_cat, nace

>> DATA USED FOR MARKUP ESTIMATION:
GO (deflated total operating revenue), COGS (deflated variable input expenditures)
K (deflated total fixed assets in book value), nace2 (subindustry firm info)					
	
	* external datasets:
		1. master_tender_analytics_202207251530.csv (https://datlab.eu/)
		2. deflators.csv (OECD)
*/

/*--------------------------------------------------*
cd "data/raw"
log using raw, text replace
************************************************************************
*                       RAW DATA PROCESSING                            *
************************************************************************
clear all 
cd files
************************************************************************
* PREPARE DEFLATORS 
import delimited "deflators.csv", clear
save "deflators",replace
************************************************************************
* PREPARE TENDERS
insheet using "master_tender_analytics_202207251530.csv", names clear
rename bidder_id id
drop if length(id)==2 // foreign bidders
destring id, replace
rename bid_final_price pp_sales
collapse (sum) pp_sales, by(id year)
save "tenders", replace
************************************************************************
* PREPARE ANALYSIS 
import delimited using "ratios1.csv", clear
save "ratios", replace
forvalues i=2/3{
    import delimited using "ratios`i'.csv", clear
	append using "ratios"
	save "ratios.dta", replace
}
************************************************************************
import delimited using "financial1.csv", clear
save "financial", replace
forvalues i=2/6{
    import delimited using "financial`i'.csv", clear
	append using "financial"
	save "financial.dta", replace
}
merge m:m id year using "ratios.dta", nogenerate
rename ccosts costs
rename fafixedassets assets
rename salsalesoutputs sales
rename wvawagesvalueadded wva
rename wswagessales ws
rename lplabourproductivity lp
rename cmiiicontributionmargin cm 
duplicates drop
duplicates drop id year, force
save "analysis", replace
************************************************************************
import delimited using "selections1.csv", clear
save "selections", replace
forvalues i=2/5{
    import delimited using "selections`i'.csv", clear
	append using "selections"
	save "selections.dta", replace
}
rename idièo id
rename typeofsubject subject_type
rename legalform legal_form
rename institutionalsectorsesa2010 inst_sector
rename numberofemployees empl_num
rename numberofemployeesclassificationc empl_cat
drop empl_cat-v28
rename v29 empl_cat
rename mainnacecode nace
duplicates drop
save "selections", replace
************************************************************************
* MERGE & CLEAN
use "analysis", clear
merge m:1 id using "selections", nogenerate
duplicates drop 
duplicates drop id year, force
************************************************************************
*correct sales
sum sales,d
sum sales if sales<0,d
replace sales = . if sales<0   // if negative
sum sales if sales>10000000000,d
*correct ws and define as a share
sum ws,d
sum ws if ws<1,d
replace ws = ws * 100 if ws<1 & ws>0 // if 100 times too low
replace ws = . if ws<0 // if negative
replace ws = . if ws>100 // if very high
replace ws = ws/100 // define as a share
*correct wva and define as a share
sum wva if wva<1,d
replace wva = wva * 100 if wva<1 & wva>0 // if 100 times too low
replace wva = . if wva<0 // if negative
replace wva = . if wva>500 // if very high
replace wva = wva/100 // define as a share
*correct contribution margin and define as a share
sum cm if cm<1,d
replace cm = cm * 100 if cm<1 & cm>0 // if 100 times too low
replace cm = . if cm<0 // if negative
replace cm = cm/100 // define as a share
*gen cs (a ratio of costs to sales)
gen cs = costs / sales
label var cs "costs / sales
sum cs,d
replace cs = . if cs<0 // if negative
replace cs = . if cs>10 // if very high
sum cs,d
*generate intermediate inputs
//gen iis = cm-ws
gen iis = cs - ws
replace iis = . if iis<0
*generate COGS/sales
gen COGSS = 1-cm
label var cs "COGS / sales
replace COGSS = . if COGSS<0 // if negative
replace COGSS = . if COGSS>10 // if very high
sum COGSS, d
*correct lp if 1000 times too large
sum lp if lp<10^8,d
sum lp if lp>10^8,d
replace lp = lp/1000 if lp>10^8
*generate variables
gen nace2 = floor(nace/10000)
gen GO = sales
gen W = ws * sales
gen II = iis * sales
gen COGS= COGSS*sales
gen VA = GO - II 
gen L = VA / lp if VA/lp>0
gen K = assets
sort id year
*correct
replace GO = . if GO<0
replace COGS = . if COGS<0
replace II = . if II<0
replace W = . if W<0
replace K = . if K<0
*deflate
merge m:1 year nace2 using "deflators", nogenerate
duplicates drop id year, force
gen rGO = GO/deflatorprdp
gen rVA = VA/deflatorvalp
gen rII = II/deflatorintp
gen rW = W/deflatorcpi
gen rK = K/deflatorgfcp
gen rCOGS = COGS/deflatorintp
*gen log variables
gen go = ln(rGO)
gen w = ln(rW)
gen ii = ln(rII)
gen va = ln(rVA)
gen l = ln(L)
gen k = ln(rK)
gen cogs = ln(rCOGS)
************************************************************************
xtset id year
sort id year
gen lagExists = 1 if year == year[_n-1] + 1 & id == id[_n-1]
keep if lagExists == 1
bys id (year): g entry = (_n == 1)
bys id (year): g exit = (_n == _N & year < 2021)
************************************************************************
save "magnus", replace
log close
************************************************************************/

cd "$dropbox/data/"
* insert deflated financial statements data:
use "raw/files/magnus.dta", clear
drop if cogs==. | k== . | go == .

* label vars:
label var COGS "Costs of goods sold
label var K "Fixed assets
foreach var of varlist GO COGS K {
label var r`var' "Deflated `var'"
}
label var nace2 "subindustry
sort nace2 year
* merger tender data
merge 1:1 id year using "raw/files/tenders.dta", gen(tenders)
drop if tenders==2
replace pp_sales = 0 if pp_sales==.
* public procurement status
gen pp_dummy = pp_sales>0 
gen pp_share = pp_sales/sales
replace pp_share = 1 if pp_share>1
replace pp_sales = 0 if pp_sales==.

* note procurement data runs from 2006: lag needed for estimation
xtset id year
sort id year
gen lgExist = 1 if year == year[_n-1] + 1 & id == id[_n-1]
drop if lgExist == .
drop if year<2005

* main results for 1% trim (below)
* robustness 2% and 5%
gen trim = 0

* trim on sales share ration
gen s_g = rGO/rCOGS
label var s_g "sale-cogs ratio
qui {
forvalues t=1/5 {
bysort year: egen s_g_p_`t'  = pctile(s_g), p(`t')
}
forvalues s=95/99 {
bysort year: egen s_g_p_`s'  = pctile(s_g), p(`s')
}
}
keep if s_g> s_g_p_1 & s_g< s_g_p_99

* trim on costshares
gen costshare = rCOGS/(rCOGS+rK)
label var costshare "rCOGS/(rCOGS+rK)
qui {
bysort year: egen cs_p_1=pctile(costshare), p(1)
bysort year: egen cs_p_99=pctile(costshare), p(99)
}
drop if costshare==0 | costshare==.
drop if costshare > cs_p_99 
drop if costshare < cs_p_1

replace trim = 1
save "$dropbox/data/data.dta", replace
save "$dropbox/temp/data_trim_1.dta", replace
*--------------------------------------------------*
* data created 



