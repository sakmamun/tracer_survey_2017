
***************************************************
* IMPLEMENTING MINCER'S WAGE EQUATION
* BASED ON COLLEGE GRADUATE TRACER SURVEY 2017
* THE WORLD BANK
*************************************************
clear all
set more off



gen lnwage= ln(wage)
keep age age_sq location gender final_cgpa hsc_cgpa ssc_cgpa fedu medu maritial_status fsize edu_year lnwage


egen mlnwage = mean(lnwage), by(age)
label var mlnwage "Mean of log wage"
twoway (lowess mlnwage age), ///
       scheme(economist) ///
       xscale(range()) ///
       title("") ///
       ytitle("Mean Log Wage") ///
       xtitle("Age") ///
       note("Bandwidth = 0.8")


*Mincer regression without ability variable control
global x age age_sq i.gender final_cgpa
global z c.hsc_cgpa c.ssc_cgpa fedu medu
reg lnwage edu_year $x $z, vce(robust)
estimates store m



******************************************************************************
* 2SLS with inverse Mills ratio in the outcome: 
* This treats the selection correction as a 
* control in the wage equation and then performs IV there.
*******************************************************************
global x age age_sq i.gender final_cgpa 
global z c.hsc_cgpa##c.ssc_cgpa fedu medu

*Generating Mills Ratio
probit employed i.location i.marital_status fsize				
predict xb_sel
gen lambda = normalden(xb_sel)/normal(xb_sel)

*including Mills Ratio plus instrumenting edu_year by 'Z'
ivreg2 lnwage (edu_year= $z) $x lambda if lmp==1							//  Check for IV validation
ivregress 2sls lnwage (edu_year= $z) $x lambda if lmp==1					//  *Education as year of education	
estimates store iv

ivregress 2sls lnwage (i.edu_year= $z) $x lambda if lmp==1					//  *Education as degree
estimates store iv2

esttab  m m0 iv iv2 using "results.rtf", ///
    replace b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    title("Regression estimates") ///
    mtitles("OLS" "OLS_2" "IV" "IV_2") 					///
	r2 ar2	
								
