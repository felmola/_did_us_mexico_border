
clear all
global path "C:\Users\felmo\Dropbox\1_personal\_maestria_unibo_(operacional)\_causal_inference\_did\_did_us_mexico_border\_replication"	// Set the path were the "AEAPP.dta" file is saved


use "${path}\_data\AEAPP.dta", clear

xtset sector_number year
gen fenced=1 if sector_number==1 | sector_number==2 | sector_number==3 | sector_number==4 | sector_number==5 | sector_number==9
replace fenced=0 if fenced==.
gen sfa=(year>=2006)
gen fencedsfa= fenced* sfa
gen lnapp=ln( apprehensions)
gen deathsper100k= (deaths/ apprehensions)*100000
gen lagstaff=l.staff

reg lnapp fenced sfa fencedsfa if year>1997 & year~=2006 & year~=2007 & year~=2008 & year<2018, robust
estadd local time_fe="No"
est store lnapp
reg lnapp fenced sfa fencedsfa months80 lagstaff i.year if year>1997 & year~=2006 & year~=2007 & year~=2008 & year<2018, robust
estadd local time_fe="Yes"
est store lnapp_c
reg deathsper100k fenced sfa fencedsfa if year>1997 & year~=2006 & year~=2007 & year~=2008 & year<2018, robust
estadd local time_fe="No"
est store deathsper100k
reg deathsper100k fenced sfa fencedsfa months80 lagstaff i.year if year>1997 & year~=2006 & year~=2007 & year~=2008 & year<2018, robust
estadd local time_fe="Yes"
est store deathsper100k_c

*ssc install estout, replace
esttab lnapp lnapp_c deathsper100k deathsper100k_c using "${path}\_output\table1.tex", replace keep(fenced sfa fencedsfa months80 lagstaff _cons) scalars(time_fe N F p) se(%10.4f) b(%10.4f) star(* 0.05 ** 0.01 *** 0.001) mtitles title("Estimation Results") style(tex)

