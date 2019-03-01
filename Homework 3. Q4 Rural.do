**** HOMEWORK 3. QUESTION 4. URBAN
clear
cd "C:\Users\beras\OneDrive\Escritorio\Homework3\Output"
use "C:\Users\beras\OneDrive\Escritorio\Homework3\Dataset\DataUga.dta"
keep if urban==0

**** PREPARE DATA FOR EXERCISE
*** Adapt year variable following mail instructions
bysort year hh: gen ynumb = _N
replace year = 2010 if wave=="2010-2011" & year==2011 & ynumb==2
drop ynumb
bysort year hh: gen ynumb = _N
replace year = 2009 if wave=="2009-2010" & year==2010 & ynumb==2
drop ynumb
*** Compute the consumption aggregation
egen t_c = sum(lnc), by(year)	
*** Compute the residuals for consumption and income
reg lnc age age_sq familysize i.year i.ethnic i.sex i.urban
predict res
rename res resid_c
reg lninctotal_trans age age_sq familysize i.year i.ethnic i.sex i.urban
predict res
rename res resid_i
*** Fix to panel
keep resid_c resid_i t_c hh year lninctotal_trans
xtset hh year
reshape wide resid_c resid_i t_c lninctotal_trans, i(hh) j(year)
forvalues y = 10(1)14 {
egen t_c20`y'_t = mean(t_c20`y')
drop t_c20`y'
rename t_c20`y'_t t_c20`y'
}
egen t_c2009_t = mean(t_c2009)
drop t_c2009
rename t_c2009_t t_c2009
reshape long resid_c resid_i t_c lninctotal_trans, i(hh)
rename _j year
*** Interpolate and extrapolate to establish complete the pannel
ipolate resid_c year, generate(c_resid_c) epolate by(hh)
ipolate resid_i year, generate(c_resid_i) epolate by(hh)
ipolate lninctotal_trans year, generate(lninctotal_trans_i) epolate by(hh)
*** Elliminate household with one observation
gen ones = 1
replace ones = 0 if c_resid_c ==.
replace ones = 0 if c_resid_i ==.
replace ones = 0 if lninctotal_trans_i ==.
egen numyears = sum(ones), by(hh)
drop if numyears <= 1
drop resid_c resid_i ones numyears

**** QUESTION 1
sort hh year
egen ni = group(hh) 
generate beta = .
generate phi = .
forvalues i = 1(1)2226 {
	reg d.c_resid_c d.c_resid_i d.t_c if ni==`i', nocons
	replace beta = _b[d.c_resid_i] if ni==`i'
	replace phi = _b[d.t_c] if ni==`i'
}
*** Regressions for each household
preserve
collapse beta phi, by(hh)
drop if beta <= -3
drop if beta >= 3
drop if phi <= -0.5
drop if phi >= 0.5
histogram beta, title("Beta coefficient. Rural", color(black)) xtitle ("Beta") graphregion(color(white)) bcolor(maroon)
graph export "C:\Users\beras\OneDrive\Escritorio\Homework3\Output\histogrambetarural.png", replace
histogram phi, title("Phi coefficient. Rural", color(black)) xtitle ("Phi") graphregion(color(white)) bcolor(navy)
graph export "C:\Users\beras\OneDrive\Escritorio\Homework3\Output\histogramphirural.png", replace
summarize beta, detail
summarize phi, detail
restore

**** QUESTION 3
reg d.c_resid_c d.c_resid_i d.t_c, nocons
display _b[d.c_resid_i]
display _b[d.t_c]

**** QUESTION 2
collapse (mean) lninctotal_trans_i beta, by(hh)
**** QUESTION 2.a)
*** 5 income groups
sort lninctotal_trans_i
gen hhnumber = _n 
gen group_i = 0
replace group_i = 5 if hhnumber > 1781 & hhnumber <= 2226
replace group_i = 4 if hhnumber > 1336 & hhnumber <= 1781
replace group_i = 3 if hhnumber > 890 & hhnumber <= 1336
replace group_i = 2 if hhnumber > 445 & hhnumber <= 890
replace group_i = 1 if hhnumber <= 445
*** Mean and median betas
forvalues i = 1(1)5 {
sum beta if group_i==`i', detail
}
drop hhnumber
**** QUESTION 2.b)
*** 5 beta groups
sort beta
gen hhnumber = _n 
gen group_beta = 0
replace group_beta = 5 if hhnumber > 1781 & hhnumber <= 226
replace group_beta = 4 if hhnumber > 1336 & hhnumber <= 1781
replace group_beta = 3 if hhnumber > 890 & hhnumber <= 1336
replace group_beta = 2 if hhnumber > 445 & hhnumber <= 890
replace group_beta = 1 if hhnumber <= 445
*** Mean and median betas
forvalues i = 1(1)5 {
sum lninctotal_trans_i if group_beta==`i', detail
}
