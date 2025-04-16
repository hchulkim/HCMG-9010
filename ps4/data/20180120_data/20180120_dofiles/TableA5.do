** Angrist, Lavy, Leder-Luis, Shany
** Maimondies Rule Redux 

** This code produces Table A5
** Maimonides Rule Effects on Socioeconomic Status (2002-2011)

************************

****************
*** Load New Data
use "Data\MRuleRedux_new_DB", clear


** Collapse by school (SES is at the school level)
collapse classize cohort c_size boy relig fatheduc miss_fatheduc motheduc miss_motheduc born_isr eth_israel eth_otrimm eth_fsu eth_ethimm eth_asiafr eth_euram siblings miss_siblings tipuach, by(year  schlcode)


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


* Year FE
tabulate year, generate(yr)


** Run regressions
local rule "c_size cohort"
local year_dummy "yr2 yr3 yr4 yr5 yr6 yr7 yr8 yr9 yr10"


***REGRESSIONS***

foreach r in `rule' {

gen func1=func1_`r'
gen enroll=`r'
gen enroll2=`r'2
gen trend_enroll=trend_`r'

reg tipuach func1 enroll `year_dummy' relig, clu(schlcode) 

est sto A_`r'

reg tipuach func1 enroll enroll2 `year_dummy' relig, clu(schlcode) 

est sto B_`r'

reg tipuach func1 trend_enroll `year_dummy' relig, clu(schlcode) 

est sto C_`r'

reg tipuach func1 enroll `year_dummy' relig fatheduc siblings, clu(schlcode) 

est sto D_`r'

reg tipuach func1 enroll enroll2 `year_dummy' relig fatheduc siblings , clu(schlcode) 

est sto E_`r'

reg tipuach func1 trend_enroll `year_dummy' relig fatheduc siblings, clu(schlcode) 

est sto F_`r'


drop func1 enroll enroll2 trend_enroll

}


esttab A_c_size B_c_size C_c_size A_acohort B_cohort C_cohort, keep(func1 enroll enroll2 trend_enroll) stats(r2 N) nocon se, using "Tables\TableA5.tex", replace
esttab D_c_size E_c_size F_c_size D_cohort E_cohort F_cohort, keep(func1 enroll enroll2 trend_enroll) stats(r2 N) nocon se, using "Tables\TableA5.tex", append





