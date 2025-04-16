** Angrist, Lavy, Leder-Luis, Shany
** Maimondies Rule Redux 

** This code produces Table A7
** Class Size Effects Estimated, OLS and 2SLS in a Sample Omitting Data from 2004-6
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
*November enrollment
gen func1_c_size= c_size/(int((c_size-1)/40)+1)
gen c_size2 =(c_size^2)/100
gen trend_c_size= c_size if c_size>=0 & c_size<=40
replace  trend_c_size= 20+(c_size/2) if c_size>=41 & c_size<=80
replace  trend_c_size= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
replace  trend_c_size= (130/3)+(c_size/4) if c_size>=121 & c_size<=160
replace  trend_c_size= (160/3)+(c_size/5) if c_size>=161

*Birthday-based enrollment
g func1_cohort= cohort/(int(cohort/40)+1)
gen cohort2 =(cohort^2)/100
gen trend_cohort= cohort if cohort>=0 & cohort<=40
replace  trend_cohort= 20+(cohort/2) if cohort>=41 & cohort<=80
replace  trend_cohort= (100/3)+(cohort/3) if cohort>=81 & cohort<=120
replace  trend_cohort= (130/3)+(cohort/4) if cohort>=121 & cohort<=160
replace  trend_cohort= (160/3)+(cohort/5) if cohort>=161


** Omitting Data from 2004-6
drop if year>=2004 & year<=2006

** Run regressions
local subject "math verb"
local rule "c_size cohort"
local controls "boy relig fatheduc miss_fatheduc motheduc miss_motheduc born_isr eth_israel eth_otrimm eth_fsu eth_ethimm eth_asiafr eth_euram siblings miss_siblings"
local year_dummy "yr2 yr3 yr4 yr5 yr6 yr7 yr8 yr9 yr10"
local tip "tipuach tip_2002 tip_2008"


***REGRESSIONS***
foreach sam in `subject' {

preserve

keep if `sam'==1

foreach r in `rule' {

xi: reg `sam'score classize `r' `year_dummy' `controls'  `tip', clu(school_year) 

est sto A_`sam'_`r'_ols

xi: ivreg `sam'score (classize=func1_`r') `r' `year_dummy' `controls'  `tip', clu(school_year) 

est sto B_`sam'_`r'_2sls

xi: ivreg `sam'score (classize=func1_`r') `r' `r'2 `year_dummy' `controls'  `tip', clu(school_year) 

est sto C_`sam'_`r'_2sls

xi: ivreg `sam'score (classize=func1_`r') trend_`r' `year_dummy' `controls'  `tip', clu(school_year) 

est sto D_`sam'_`r'_2sls

}
restore
}

esttab A_verb_c_size_ols B_verb_c_size_2sls C_verb_c_size_2sls D_verb_c_size_2sls  A_math_c_size_ols B_math_c_size_2sls C_math_c_size_2sls D_math_c_size_2sls, keep(classize tipuach c_size c_size2 trend_c_size) stats(N) nocon se, using "Tables\TableA7.tex", replace
esttab A_verb_cohort_ols B_verb_cohort_2sls C_verb_cohort_2sls D_verb_cohort_2sls A_math_cohort_ols B_math_cohort_2sls C_math_cohort_2sls D_math_cohort_2sls, keep(classize tipuach cohort cohort2 trend_cohort) stats(N) nocon se, using "Tables\TableA7.tex", append





