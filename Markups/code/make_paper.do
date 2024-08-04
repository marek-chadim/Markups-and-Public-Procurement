/* 
USER INFO 
Create folder structure:
/code/ 
/data/
/data/PF/
/temp/
/output/figures
/output/figures/BW
*/	
clear all
set more off
set scheme lean2
if "`c(username)'"=="chadi" {
	local dropbox "~/OneDrive - Handelshögskolan i Stockholm/CodeAndData/Markups/"
	}
global dropbox  = "`dropbox'"
global bootreps = 2
set seed 42

* Create dataset from latest download (with # trimmed percentages)
cd "$dropbox"
do "code/Create_Data.do"

* Create temp file with main variables for analysis
cd "$dropbox"
do "code/Create_Temp.do"

* Create figures¨
cd "$dropbox"
do "code/Create_Output.do"

* erase all files in temp file
cd "$dropbox"
cd "temp/"
local list : dir . files "*.dta"
foreach f of local list {
    erase "`f'"
}

/*Dropping black-white figures
cd "$dropbox/output/figures/BW/"
local list: dir . files "*.eps"
foreach f of local list{
	erase "`f'"
	}
