** Angrist, Lavy, Leder-Luis, Shany
** Maimondies Rule Redux 

** This code produces Table A3
** First Stage Estimates Using Birthday-based Imputed Enrollment (cohort variable)

************************

****************
use "Data\MRuleRedux_new_DB", clear

* Generate var for cluster se
egen school_year= group(schlcode year)
* Generate year FE
tabulate year, generate(yr)
* Generate SES interactions
gen tip_2002_3=tipuach if year==2002 | year==2003
replace tip_2002_3=0 if tip_2002_3==.
gen tip_2008=tipuach if year>=2008
replace tip_2008=0 if tip_2008==.



***Creating enrollment variables
*Based on birthday-based enrollment
g func1_cohort= cohort/(int(cohort/40)+1)
gen cohort2 =(cohort^2)/100
gen trend_cohort= cohort if cohort>=0 & cohort<=40
replace  trend_cohort= 20+(cohort/2) if cohort>=41 & cohort<=80
replace  trend_cohort= (100/3)+(cohort/3) if cohort>=81 & cohort<=120
replace  trend_cohort= (130/3)+(cohort/4) if cohort>=121 & cohort<=160
replace  trend_cohort= (160/3)+(cohort/5) if cohort>=161


** Run regressions
local subject "math verb"
local controls "boy relig fatheduc miss_fatheduc motheduc miss_motheduc born_isr eth_israel eth_otrimm eth_fsu eth_ethimm eth_asiafr eth_euram siblings miss_siblings"
local year_dummy "yr2 yr3 yr4 yr5 yr6 yr7 yr8 yr9 yr10"
local tip "tipuach tip_2002 tip_2008"


***REGRESSIONS***
foreach sam in `subject' {

preserve

keep if `sam'==1


reg classize func1_cohort cohort `year_dummy' `controls' `tip', clu(school_year) 
est sto A_`sam'
test func1_c_size
estadd scalar F_st=r(F)


reg classize func1_cohort cohort cohort2 `year_dummy' `controls' `tip', clu(school_year) 
est sto B_`sam'
test func1_`r'
estadd scalar F_st=r(F)

reg classize func1_cohort trend_cohort `year_dummy' `controls'  `tip', clu(school_year) 
est sto C_`sam'
test func1_c_size
estadd scalar F_st=r(F)


restore
}

esttab A_verb B_verb C_verb A_math B_math C_math, keep(func1_cohort tipuach cohort cohort2 trend_cohort) stats(r2 F_st N) nocon se, using "Tables\TableA3.tex", replace




