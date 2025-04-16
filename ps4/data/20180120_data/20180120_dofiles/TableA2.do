** Angrist, Lavy, Leder-Luis, Shany
** Maimondies Rule Redux 

** This code produces Table A2
** First Stage Estimates Using November Enrollment Instruments (c_size variable)
************************

****************
*** Load New Data
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
*Based on November enrollment
gen func1_c_size= c_size/(int((c_size-1)/40)+1)
gen c_size2 =(c_size^2)/100
gen trend_c_size= c_size if c_size>=0 & c_size<=40
replace  trend_c_size= 20+(c_size/2) if c_size>=41 & c_size<=80
replace  trend_c_size= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
replace  trend_c_size= (130/3)+(c_size/4) if c_size>=121 & c_size<=160
replace  trend_c_size= (160/3)+(c_size/5) if c_size>=161


** Run regressions
local subject "math verb"
local controls "boy relig fatheduc miss_fatheduc motheduc miss_motheduc born_isr eth_israel eth_otrimm eth_fsu eth_ethimm eth_asiafr eth_euram siblings miss_siblings"
local year_dummy "yr2 yr3 yr4 yr5 yr6 yr7 yr8 yr9 yr10"
local tip "tipuach tip_2002_3 tip_2008"


***REGRESSIONS***
foreach sam in `subject' {

preserve

keep if `sam'==1


reg classize func1_c_size c_size `year_dummy' `controls' `tip', clu(school_year) 
est sto A_`sam'
test func1_c_size
estadd scalar F_st=r(F)


reg classize func1_c_size c_size c_size2 `year_dummy' `controls' `tip', clu(school_year) 
est sto B_`sam'
test func1_`r'
estadd scalar F_st=r(F)

reg classize func1_c_size trend_c_size `year_dummy' `controls'  `tip', clu(school_year) 
est sto C_`sam'
test func1_c_size
estadd scalar F_st=r(F)


restore
}

esttab A_verb B_verb C_verb A_math B_math C_math, keep(func1_c_size tipuach c_size c_size2 trend_c_size) stats(r2 F_st N) nocon se, using "Tables\TableA2.tex", replace




