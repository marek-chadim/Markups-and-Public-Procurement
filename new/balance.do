use did, clear
xtset id year
sort id year
bys id: gen nyear=[_N]
tab nyear, gen(nyear)
drop if nyear<16
rename pp_dummy pp
bysort id: gen D= pp[1]
drop if D==1
gen lmu = ln(markup)
gen x1 = L6.lmu
gen x2 = L5.lmu
gen x3 = L4.lmu
gen x4 = L3.lmu
gen x5 = L2.lmu
gen x6 = L1.lmu
gen y0 = lmu
gen y1 = F.lmu
reg y1 y0 x* nace2##year 
predict y1hat
replace y1 = y1hat if y1==.
gen y2 = F2.lmu
reg y2 y1 y0 x* nace2##year 
predict y2hat
replace y2 = y2hat if y2==.
tab nace2, gen(nace4)
tab year, gen(year)
drop year1
drop costs assets sales
bysort id: gen sales = go[1]
bysort id: gen costs = cogs[1]
bysort id: gen assets= k[1]
replace empl_num = 0 if empl_num==.
bysort id: replace empl= empl_num[1]
bysort id: gen beta= betahat_tl[1]
bysort id: gen omega= omegahat_tl[1]
bysort id: gen mu= markup[1]
gen pp1=L1.pp
gen pp2=L2.pp
gen pp3=L3.pp
gen pp4=L4.pp
drop if pp4==.
tabstat id, stat(N) by(year)
keep id year year2-year16 nace42 nace43 x1-x6 y0 y1 y2 pp pp1-pp4 costs sales assets empl beta omega mu
missings report
export delimited  ///
using "data", nolabel replace
  