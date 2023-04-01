clear
global path " "	// Set the path were the "AEAPP.dta" file is saved


use "${path}\AEAPP.dta", clear

******************* FIGURE 1 *******************************
use "${path}\fencing_by_year.dta", clear

	rename total_p_vehicle vehicle_barrier_Permanent , replace 
	rename total_t_vehicle  vehicle_barrier_Temporary , replace 
	
	label variable vehicle_barrier_Permanent "Cumulative Vehicle Barrier - Permanent"
	label variable vehicle_barrier_Temporary "Cumulative Vehicle Barrier - Temporary"

	egen Pedestrian_Barrier = rowtotal(total_primary total_secondary total_tertiary )
	label variable Pedestrian_Barrier "Cumulative Pedestrian Barrier - Total"
	
	gen Pedestrian_Barrier_M = Pedestrian_Barrier/5280
	gen vehicle_barrier_Temporary_M = vehicle_barrier_Temporary/5280
	gen vehicle_barrier_Permanent_M = vehicle_barrier_Permanent/5280

	
		twoway line Pedestrian_Barrier_M  vehicle_barrier_Temporary_M vehicle_barrier_Permanent_M year, xtitle("Year")  sort lpattern( solid shortdash_dot dash ) xline(2006 , lpattern(solid) lcolor(red))  xlabel(,labsize(2.3)) ytitle("Total Miles Constructed") ylabel(,labsize(2.3)) legend( pos(6) lab(1 "Cumulative Pedestrian Barrier - Total") lab(2 "Cumulative Vehicle Barrier - Temporary") lab(3 "Cumulative Vehicle Barrier - Permanent")) title("Cumulative barrier construction by type") lcolor(black gs10 black )

		
		graph save "Graph" "${path}\Figure 1.gph"

************  FIGURE 2 ***********************************************************
clear

use "${path}\aggregate_apprehensions_death_by_year.dta"

twoway bar alldeath year, sort yaxis(1) yscale(range(0) axis(1)) ///
 || line allapprehension year , sort yaxis(2) yscale(range(0) axis(2)) ///
 || ,legend(order(1 2))
 
 graph save "Graph" "${path}\Figure 2.gph"
 