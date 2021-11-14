// Updated June 19th, 2018
// Program to generate SGA using IG
// Original program  sgaIntergrowth.ado was created by Luke C Mullany (lmullany@jhu.edu)

** sgaIntergrowth2.ado is a modified version of the original program (sgaIntergrowth.ado)
** Modified by Eric Ohuma (eric.ohuma@lshtm.ac.uk) on 30th March, 2021
** # Description: Estimates SGA, LGA, and AGA according to Intergrowth-21st Standards (https://intergrowth21.tghn.org/standards-tools/)


cap program drop sgaIntergrowth2
program define sgaIntergrowth2
	version 16.0
	syntax varlist (num min=3 max=3), sgavar(string) [includeOutliers] [useImputedWeight]
	quietly {
	tokenize `varlist'
		capture assert missing(`1')==1 | inlist(`1',1,2)==1
		if _rc!=0 {
			noi di as error "Sex variable (`1') can only take values 1 (Male), 2 (Female), or missing"
			exit
		}
		capture assert inrange(`2',0,.)  | missing(`2')==1
		if _rc!=0 {
			noi di as error "Gestational age variable (`2') must be a positive number expressed in weeks"
			exit
		}
		capture assert inrange(`3',0,.)  | missing(`3')==1
		if _rc!=0 {
			noi di as error "Weight variable (`3') must be a positive number expressed in grams"
			exit
		}
		confirm new variable `sgavar'


		tempvar clonegest
		clonevar `clonegest' = `2'

		
		if "`includeOutliers'"!= "" {
			replace `clonegest' = 24 if `clonegest'<24
			replace `clonegest' = 42.9 if `clonegest'>=43 & missing(`clonegest')==0
		}
		
	


mata: boys_10 = 0.50,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.60,0.61,0.63,0.64,0.65,0.66,0.67,0.69,0.70,0.71,0.72,0.74,0.75,0.77,0.78,0.79,0.81,0.82,0.84,0.85,0.87,0.88,0.90, 	0.92,0.93,0.95,0.97,0.98,1.00,1.02,1.03,1.05,1.07,1.09,1.11,1.13,1.15,1.17,1.19,1.21,1.23,1.25,1.27,1.29,1.31,1.34,1.36,1.38,1.41,1.43,1.45,1.48,1.50,1.43,1.47,1.51,1.55,1.59,1.63,1.67,1.71,1.74,1.78,1.82,1.85,1.89,1.92,1.95,1.99,2.02,2.05,2.09,2.12,2.15,2.18,2.21,2.24,2.27,2.30,2.33,2.36,2.38,2.41,2.44,2.47,2.49,2.52,2.54,2.57,2.59,2.62,2.64,2.67,2.69,2.71,2.73,2.76,2.78,2.80,2.82,2.84,2.86,2.88,2.90,2.92,2.94,2.96,2.98,2.99,3.01,3.03,3.05,3.06,3.08,3.09,3.11,3.12,3.14,3.15,3.17,3.18,3.20,3.21


mata: boys_90 = 0.82, 0.83, 0.85, 0.87, 0.88, 0.90, 0.92, 0.93, 0.95, 0.97, 0.99, 1.01, 1.03, 1.04, 1.06, 1.08, 1.10, 1.13, 1.15, 1.17, 1.19, 1.21, 1.23, 1.26, 1.28, 1.30, 1.33, 1.35, 1.37, 1.40, 1.42, 1.45,1.48, 1.50, 1.53, 1.56, 1.58, 1.61, 1.64, 1.67, 1.70, 1.73, 1.76, 1.79, 1.82, 1.85, 1.88, 1.92, 1.95, 1.98, 2.02, 2.05, 2.09, 2.12, 2.16, 2.19, 2.23, 2.27, 2.31, 2.35, 2.38, 2.42, 2.46, 2.52, 2.56, 2.60, 2.64, 2.67, 2.71, 2.75, 2.79, 2.82, 2.86, 2.89, 2.93, 2.96, 3.00, 3.03, 3.06, 3.09, 3.13, 3.16, 3.19, 3.22, 3.25, 3.28, 3.31, 3.34, 3.37, 3.39, 3.42, 3.45, 3.48, 3.50, 3.53, 3.55, 3.58, 3.61, 3.63, 3.65, 3.68, 3.70, 3.72, 3.75, 3.77, 3.79, 3.81, 3.83, 3.86, 3.88, 3.90, 3.92, 3.94, 3.95, 3.97, 3.99, 4.01, 4.03, 4.04, 4.06, 4.08, 4.09, 4.11, 4.13, 4.14, 4.16, 4.17, 4.19, 4.20, 4.21, 4.23, 4.24, 4.25

	
mata:girls_10 = 0.47,0.48,0.49,0.50,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59, 0.60,0.61,0.62,0.64,0.65,0.66,0.67,0.68,0.70,0.71,0.72,0.74,0.75,0.76,0.78,0.79,0.81,0.82,0.83,0.85,0.86,0.88,0.90,0.91,0.93,0.94, 0.96,0.98,0.99,1.01,1.03,1.05,1.07,1.08,1.10,1.12,1.14,1.16,1.18,1.20,1.22,1.24,1.26,1.28,1.31,1.33,1.35,1.37,1.40,1.42, 1.41,1.45,1.49,1.53,1.57,1.61,1.65,1.68,1.72,1.75,1.79,1.82,1.86,1.89,1.92,1.96,1.99,2.02,2.05,2.08,2.11,2.14,2.17,2.20,2.23,2.25,2.28,2.31,2.33,2.36,2.38,2.41,2.43,2.46,2.48,2.50,2.53,2.55,2.57,2.59,2.61,2.63,2.65,2.67,2.69,2.71,2.73,2.74,2.76,2.78,2.80,2.81, 2.83,2.84,2.86,2.87,2.89,2.90,2.91,2.93,2.94,2.95,2.96,2.98,2.99,3.00,3.01,3.02,3.03,3.04


mata: girls_90 = 0.77, 0.79, 0.80, 0.82, 0.83, 0.85, 0.87, 0.88, 0.90, 0.92, 0.93, 0.95, 0.97, 0.99, 1.01, 1.02, 1.04, 1.06, 1.08, 1.10, 1.12, 1.14, 1.16, 1.19, 1.21, 1.23,1.25, 1.27, 1.30, 1.32, 1.34, 1.37, 1.39, 1.42, 1.44, 1.47, 1.50, 1.52, 1.55,1.58, 1.60, 1.63, 1.66, 1.69, 1.72, 1.75, 1.78, 1.81, 1.84, 1.87, 1.90, 1.94, 1.97, 2.00, 2.04, 2.07, 2.11, 2.14, 2.18, 2.21, 2.25, 2.29, 2.33, 2.35, 2.40, 2.44, 2.48, 2.52, 2.56, 2.60, 2.64, 2.67, 2.71, 2.75, 2.79, 2.82, 2.86, 2.89, 2.93, 2.96, 2.99, 3.03, 3.06, 3.09, 3.12, 3.15, 3.18, 3.21, 3.24, 3.27, 3.30, 3.32, 3.35, 3.38, 3.40, 3.43, 3.46, 3.48, 3.51, 3.53, 3.55, 3.58, 3.60, 3.62, 3.64, 3.66, 3.68, 3.70, 3.72, 3.74, 3.76, 3.78, 3.80, 3.82, 3.84, 3.85, 3.87, 3.89, 3.90, 3.92, 3.93, 3.95, 3.96, 3.97, 3.99, 4.00, 4.01, 4.03, 4.04, 4.05,4.06, 4.07, 4.08

					
	

		mata:st_matrix("boys_90", boys_90*1000)
		mata:st_matrix("girls_90", girls_90*1000)
		mata:st_matrix("boys_10", boys_10*1000)
		mata: st_matrix("girls_10", girls_10*1000)
	
	
		local ct = 0
		forvalues week = 24/42 {
			forvalues day = 0/6 {
				local ct = `ct'+1
				if `ct'==1 mat gac = `week' + `day'/7
				else mat gac = gac,(`week'+`day'/7)
			}
		}
		mat gac = gac,43

		if "`useImputedWeight'"!="" local passtem "mi passive:"	
		
		local stems "boys_10 girls_10"
		local stems2 "boys_90 girls_90"
		foreach stem of local stems {
			tempvar `stem'_sga

			if substr("`stem'",1,1)=="b" local sexval = 1
			else local sexval=2

			`passtem' gen ``stem'_sga'=.
			forvalues i=1/133 {
				`passtem' replace ``stem'_sga' = 1 if `1'==`sexval' & `clonegest'>=gac[1,`i'] & `clonegest'<gac[1,`=`i'+1'] & `3'<`stem'[1,`i']
			}	
			`passtem' replace ``stem'_sga' = 0 if missing(`1')==0 & missing(`clonegest')==0 & missing(`3')==0 & missing(``stem'_sga')==1
			`passtem' replace ``stem'_sga' = . if `clonegest'>=43 | `clonegest'<24
		}

		foreach stem of local stems2 {
			tempvar `stem'_lga

			if substr("`stem'",1,1)=="b" local sexval = 1
			else local sexval=2

			`passtem' gen ``stem'_lga'=.
			forvalues i=1/133 {
				`passtem' replace ``stem'_lga' = 1 if `1'==`sexval' & `clonegest'>=gac[1,`i'] & `clonegest'<gac[1,`=`i'+1'] & `3'>`stem'[1,`i']
			}	
			`passtem' replace ``stem'_lga' = 0 if missing(`1')==0 & missing(`clonegest')==0 & missing(`3')==0 & missing(``stem'_lga')==1
			`passtem' replace ``stem'_lga' = . if `clonegest'>=43 | `clonegest'<24
		}


		capture label drop `sgavar'
		label define `sgavar' 0 "AGA" 1 "SGA" 2 "LGA"


		`passtem' gen `sgavar' = 1 if (`boys_10_sga'==1 | `girls_10_sga'==1)
		`passtem' replace `sgavar' = 2 if (`boys_90_lga'==1 | `girls_90_lga'==1)
		`passtem' replace `sgavar' = 0 if `sgavar'==.

	
		if "`useImputedWeight'"==""{
		 	label values `sgavar' `sgavar'
		}
		else{
			label values `sgavar' _*`sgavar' `sgavar'
		 	drop `boys_10_sga' `boys_90_sga' `girls_10_lga' `girls_90_lga'
		 	mi update
		}

		
		label var `sgavar' "SGA (Intergrowth)"

 } 

end
