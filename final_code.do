*******************************************************
* Project: Returns to Education – Wage Equation (Mincer)
* Data   : cleaned_data.dta
* Author : Prof. Md Shamsul Arifeen Khan Mamun, PhD
* Date   : [August 2025]
*******************************************************
clear all
set more off

*-------------------------------------------------
* Data Source fro Download
*----------------------------------------------------
https://github.com/sakmamun/tracer_survey_2017/blob/main/cleaned_data.dta

*------------------------------------------------------
* 1. Keep only relevant variables
*------------------------------------------------------
keep age age_sq location gender final_cgpa hsc_cgpa ssc_cgpa ///
     fedu medu maritial_status fsize edu_year lnwage employed lmp

*------------------------------------------------------
* 2. Descriptive Visualization: Mean Log Wage by Age
*------------------------------------------------------
egen mlnwage = mean(lnwage), by(age)
label var mlnwage "Mean of log wage"

twoway (lowess mlnwage age), ///
       scheme(economist) ///
       xscale(range()) ///
       title("Mean Log Wage by Age") ///
       ytitle("Mean Log Wage") ///
       xtitle("Age") ///
       note("Bandwidth = 0.8")

*------------------------------------------------------
* 3. OLS Mincer Regression (without ability control)
*------------------------------------------------------
global x age age_sq i.gender final_cgpa
global z c.hsc_cgpa c.ssc_cgpa fedu medu

reg lnwage edu_year $x $z, vce(robust)
estimates store m

*------------------------------------------------------
* 4. Selection Correction – Probit & Inverse Mills Ratio
*------------------------------------------------------
global x age age_sq i.gender final_cgpa
global z c.hsc_cgpa##c.ssc_cgpa fedu medu

probit employed i.location i.maritial_status fsize
predict xb_sel
gen lambda = normalden(xb_sel)/normal(xb_sel)

*------------------------------------------------------
* 5. IV Regression – edu_year as continuous variable
*------------------------------------------------------
ivreg2 lnwage (edu_year = $z) $x lambda if lmp==1
ivregress 2sls lnwage (edu_year = $z) $x lambda if lmp==1
estimates store iv

*------------------------------------------------------
* 6. IV Regression – edu_year as categorical variable
*------------------------------------------------------
ivregress 2sls lnwage (i.edu_year = $z) $x lambda if lmp==1
estimates store iv2

*------------------------------------------------------
* 7. Export Results Table (similar to esttab in Stata)
*------------------------------------------------------
esttab m iv iv2 using "results.rtf", ///
       replace b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
       title("Regression Estimates") ///
       mtitles("OLS" "IV" "IV (Degree)") ///
       r2 ar2
