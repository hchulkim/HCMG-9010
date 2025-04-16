** Angrist, Lavy, Leder-Luis, Shany
** Maimonides Rule Redux 

** This file produces Figure A1

****************
*** Load New Data
use "Data\MRuleRedux_new_DB", clear


** Collapse by enrollment
* Make 1 value of enrollment and class size per classxschoolxyear
collapse c_size classize cohort, by( classid schlcode year)
gen func1_c_size= c_size/(int((c_size-1)/40)+1)
* Average actual and predicted class size by enrollment
sort c_size
collapse func1_c_size classize, by(c_size)

label var func1_c_size "Maimonides Rule"
label var classize "Actual Class Size"

** Generate the figure
twoway (line func1_c_size c_size, xlabel(20 40 60 80 100 120 140 160 180 200)) (line classize c_size, xlabel(20 40 60 80 100 120 140 160 180 200)), xtitle("Enrollment") ytitle("Average Class Size") saving("Figures\FigA1", replace)


************************************************************
************************************************************

** Touch up colors manually in Stata graph editor
graph save "Figures\FigA1.gph" , replace
graph export "Figures\FigA1.pdf", replace


