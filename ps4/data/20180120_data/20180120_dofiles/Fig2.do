** Angrist, Lavy, Leder-Luis, Shany
** Maimonides Rule Redux 

** This file produces Figure 2
** Which contains a histogram of the fifth grade birthday-based enrollment distribution

****************
*** Load New Data
use "Data\MRuleRedux_new_DB", clear


** Have to collapse by school
** Or multiple classrooms that draw from the same enrollment confound bunching
collapse(first) cohort, by(schlcode year)
label var cohort "Enrollment by Birthday Rule, by School"

** Plot for bunching test in enrollment
** With lines
hist cohort, discrete freq xlabel(40 80 120) addplot(pci 0 40 100 40||pci 0 80 100 80 ||pci 0 120 100 120) legend(off) xtitle("Birthday-Based Enrollment") saving("Figures\Fig2", replace)


************************************************************
************************************************************

** Touch up colors manually in Stata graph editor
graph save "Figures\Fig2.gph" , replace
graph export "Figures\Fig2.pdf", replace
