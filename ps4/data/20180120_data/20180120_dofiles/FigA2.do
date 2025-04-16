** Angrist, Lavy, Leder-Luis, Shany
** Maimonides Rule Redux 

** This file produces Figure A2
** Which contains a histogram of the fifth grade number of tested distribution

****************
*** Load New Data
use "Data\MRuleRedux_new_DB", clear

** Have to collapse by school
** Or multiple classrooms that draw from the same enrollment confound bunching
collapse(first) s_num_tested sum_classize, by(schlcode year)
label var s_num_tested  "Number Tested, by School"

** Plot for bunching test in enrollment
** With lines
hist s_num_tested , discrete freq xlabel(40 80 120) addplot(pci 0 40 100 40||pci 0 80 100 80 ||pci 0 120 100 120) legend(off) xtitle("Number Tested") saving("Figures\FigA2", replace)


************************************************************
************************************************************

** Touch up colors manually in Stata graph editor
graph save "Figures\FigA2.gph" , replace
graph export "Figures\FigA2.pdf", replace
