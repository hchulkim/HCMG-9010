** Angrist, Lavy, Leder-Luis, Shany
** Maimonides Rule Redux 

** This file produces Figure 1
** Which contains a histogram of the fifth grade November enrollment distribution 2002-2011

****************
*** Load New Data
use "Data\MRuleRedux_new_DB", clear


** Have to collapse by school
** Or multiple classrooms that draw from the same enrollment confound bunching
collapse(first) c_size, by(schlcode year)
label var c_size "November Enrollment"

** Plot for bunching test in enrollment
** With lines
hist c_size, discrete freq xlabel(40 80 120) addplot(pci 0 40 100 40||pci 0 80 100 80 ||pci 0 120 100 120) legend(off) xtitle("November Enrollment") saving("Figures\Fig1", replace)

************************************************************
************************************************************

** Touch up colors manually in Stata graph editor
graph save "Figures\Fig1.gph" , replace
graph export "Figures\Fig1.pdf", replace






