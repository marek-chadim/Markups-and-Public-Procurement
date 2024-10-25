use did, clear
xtset id year
sort id year
gen Y = ln(muhat_tl)

tab nace2, gen(nace4)
drop *costs *sales *assets 
bysort id: gen sales = go[1]
bysort id: gen costs = cogs[1]
bysort id: gen assets= k[1]
bysort id: gen markups= muhat_tl[1]
bysort id: egen meanpp = mean(pp_dummy)
bysort id: gen D = cond(meanpp>0, 1, 0)
bysort id: keep if _n==1
keep if meanpp<1

export delimited id year D nace* *costs *sales *assets *wages markups year ///
    using "balance", nolabel replace
  