use markups, clear
xtset id year
sort id year
gen y1 = ln(markup) 
gen x1 = L6.y1
gen x2 = L5.y1
gen x3 = L4.y1
gen x4 = L3.y1
gen x5 = L2.y1
gen x6 = L1.y1
gen dy = y1-x6

gen dpp = pp_dummy-L.pp_dummy
gen ldpp= L3.dpp
bysort id : egen dpp_mean= mean(ldpp)

gen lpp1 = L.pp_dummy
gen lpp2 = L.lpp1
gen lpp3 = L.lpp2
gen lpp4 = L.lpp3
bysort id : egen pp_history= mean(lpp4 == 1)

drop costs assets sales
bysort id: gen costs = L4.cogs
bysort id: gen assets= L4.k
bysort id: gen wages = L4.w
bysort id: gen sales = L4.go

bysort id: keep if x1!=.


tabstat id, stat(N) by(year)

export excel id year x* y1 dy *pp*  nace2 costs sales assets wages ///
  empl sector using "data", firstrow(variables) nolabel replace
  