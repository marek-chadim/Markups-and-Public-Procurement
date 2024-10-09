    clear all
	use markups
	rename pp_dummy D
	gen Y = ln(markup)
	xi 	i.nace2 i.empl i.year
	gen _Inace2_41 = cond(nace2==41,1,0)
	gen _Iempl_0 = cond(empl==0,1,0)
	gen _Iyear_2006= cond(year==2006,1,0)
	keep id Y D _Iempl* _Iyear* _Inace2*
	order id Y D  *empl* *year* *nace2*
	foreach x of varlist _Iempl_0-_Inace2_43{
	foreach y of varlist _Iempl_0-_Inace2_43{
	generate `x'X`y'=`x'*`y'
	}
	}

	vl set
	vl move (D) vlother
	vl substitute ifactors = i.vlcategorical
	display "$ifactors"

	xporegress Y D, controls($ifactors) xfolds(5) resample(15) rseed(42) selection(plugin) vce(cluster id)
	etable
	lassoinfo
	dis e(k_controls_sel)
	dis e(controls_sel)



