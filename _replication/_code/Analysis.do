
clear all
global path " "	// Set the path were the "AEAPP.dta" file is saved


use "${path}\AEAPP.dta", clear

xtset sector_number year
gen fenced=1 if sector_number==1 | sector_number==2 | sector_number==3 | sector_number==4 | sector_number==5 | sector_number==9
replace fenced=0 if fenced==.
gen sfa=(year>=2006)
gen fencedsfa= fenced* sfa
gen lnapp=ln( apprehensions)
gen deathsper100k= (deaths/ apprehensions)*100000
gen lagstaff=l.staff

reg lnapp fenced sfa fencedsfa if year>1997 & year~=2006 & year~=2007 & year~=2008 & year<2018, robust
est store a
reg lnapp fenced sfa fencedsfa months80 lagstaff i.year if year>1997 & year~=2006 & year~=2007 & year~=2008 & year<2018, robust
est store b
reg deathsper100k fenced sfa fencedsfa months80 lagstaff i.year if year>1997 & year~=2006 & year~=2007 & year~=2008 & year<2018, robust
est store c
reg deathsper100k fenced sfa fencedsfa if year>1997 & year~=2006 & year~=2007 & year~=2008 & year<2018, robust
est store d

esttab a b c d using table1.rtf, replace se(%10.4f) b(%10.4f) star(* 0.05 ** 0.01 *** 0.001) scalars(N F p) mtitles title("Estimation Results")
