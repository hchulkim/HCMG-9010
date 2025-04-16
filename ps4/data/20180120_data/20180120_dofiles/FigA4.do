** Angrist, Lavy, Leder-Luis, Shany
** Maimonides Rule Redux 

** This file produces Figure A4

****************
*** Load New Data
use "Data\MRuleRedux_new_DB", clear


** Collapse by November enrollment 
preserve
collapse c_size classize cohort, by( classid schlcode year)
gen func1_c_size= c_size/(int((c_size-1)/40)+1)

sort c_size
collapse func1_c_size classize, by(c_size)
rename classize classize_c_size

save "Data\figA4_data.dta", replace

restore


** Collapse by birthday-based imputed enrollment
collapse cohort classize, by( classid schlcode year)
gen func1_c_cohort= cohort/(int((cohort-1)/40)+1)
sort cohort
collapse func1_c_cohort classize, by(cohort)
gen c_size=cohort 
rename classize classize_cohort

** merge November enrollment and birthday-based imputed enrollment 
merge 1:1 c_size using "Data\figA4_data.dta"


gen enroll= c_size
replace enroll= cohort if enroll==.
gen rule= enroll/(int((enroll-1)/40)+1)
sort enroll

** Generate the figure

label var rule "Maimonides Rule"
label var classize_c_size "by November Enrollment"
label var classize_cohort  "by Imputed Birthday-based Enrollment"


twoway (line rule enroll , xlabel(20 40 60 80 100 120 140 160 180 200)) (line classize_c_size enroll , xlabel(20 40 60 80 100 120 140 160 180 200)) (line classize_cohort enroll , xlabel(20 40 60 80 100 120 140 160 180 200)), xtitle("Enrollment") ytitle("Average Class Size") saving("Figures\FigA4", replace)


************************************************************
************************************************************

** Touch up colors manually in Stata graph editor
graph save "Figures\FigA4.gph" , replace
graph export "Figures\FigA4.pdf", replace


