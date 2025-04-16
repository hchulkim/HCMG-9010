** Angrist, Lavy, Leder-Luis, Shany
** Maimonides Rule Redux 

** This file produces Figure A5
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


local rule "c_size cohort"
local subject "math verb"
local controls "tipuach boy relig fatheduc miss_fatheduc motheduc miss_motheduc born_isr eth_israel eth_otrimm eth_fsu eth_ethimm eth_asiafr eth_euram siblings miss_siblings"


foreach sam in `subject' {

preserve

keep if `sam'==1


forvalues y=2002 (1) 2011 {
foreach r in `rule' {
xi: ivreg `sam'score (classize=func1_`r') `r' `controls' if year==`y', clu(school_year) 

est sto `sam'_`r'_`y'

}
}
restore
}



coefplot verb_c_size_2002 || verb_c_size_2003 || verb_c_size_2004|| verb_c_size_2005 || verb_c_size_2006 || verb_c_size_2007 || verb_c_size_2008 || verb_c_size_2009 || verb_c_size_2010 || verb_c_size_2011, /*
*/keep(classize) vertical bycoefs yline(0, lcolor(red)) xlabel(1 "2002" 2 "2003" 3 "2004" 4 "2005" 5 "2006" 6 "2007" 7 "2008" 8 "2009" 9 "2010" 10 "2011") ytitle("Language", size(large))
graph  save "Figures\FigA5_A_verb.gph", replace

coefplot math_c_size_2002 || math_c_size_2003 || math_c_size_2004|| math_c_size_2005 || math_c_size_2006 || math_c_size_2007 || math_c_size_2008 || math_c_size_2009 || math_c_size_2010 || math_c_size_2011, /*
*/keep(classize) vertical bycoefs yline(0, lcolor(red)) xlabel(1 "2002" 2 "2003" 3 "2004" 4 "2005" 5 "2006" 6 "2007" 7 "2008" 8 "2009" 9 "2010" 10 "2011") ytitle("Math", size(large))
graph  save "Figures\FigA5_A_math.gph", replace

coefplot verb_cohort_2002 || verb_cohort_2003 || verb_cohort_2004|| verb_cohort_2005 || verb_cohort_2006 || verb_cohort_2007 || verb_cohort_2008 || verb_cohort_2009 || verb_cohort_2010 || verb_cohort_2011, /*
*/keep(classize) vertical bycoefs yline(0, lcolor(red)) xlabel(1 "2002" 2 "2003" 3 "2004" 4 "2005" 5 "2006" 6 "2007" 7 "2008" 8 "2009" 9 "2010" 10 "2011")  ytitle("Language", size(large))
graph  save "Figures\FigA5_B_verb.gph", replace

coefplot math_cohort_2002 || math_cohort_2003 || math_cohort_2004|| math_cohort_2005 || math_cohort_2006 || math_cohort_2007 || math_cohort_2008 || math_cohort_2009 || math_cohort_2010 || math_cohort_2011, /*
*/keep(classize) vertical bycoefs yline(0, lcolor(red)) xlabel(1 "2002" 2 "2003" 3 "2004" 4 "2005" 5 "2006" 6 "2007" 7 "2008" 8 "2009" 9 "2010" 10 "2011")  ytitle("Math", size(large))
graph  save "Figures\FigA5_B_math.gph", replace


graph combine "Figures\FigA5_A_verb.gph" "Figures\FigA5_A_math.gph", title("A. Estimates Using November Enrollment Instrument")
graph save "Figures\A5PanelA.gph" , replace
graph combine "Figures\FigA5_B_verb.gph" "Figures\FigA5_B_math.gph", title("B. Estimates Using Birthday-based Enrollment Instrument")
graph save "Figures\A5PanelB.gph" , replace

* Combine panel A and panel B
graph combine "Figures\A5PanelA.gph" "Figures\A5PanelB.gph", rows(2)

************************************************************
************************************************************

** Touch up colors manually in Stata graph editor
graph save "Figures\FigA5.gph" , replace
graph export "Figures\FigA5.pdf", replace


