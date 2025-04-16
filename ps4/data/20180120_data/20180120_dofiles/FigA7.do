** Angrist, Lavy, Leder-Luis, Shany
** Maimonides Rule Redux 

** This code produces Figure A7
** Which contains a histogram of the third grade enrollment distribution

****************
use "Data\3rdGrade_1992Data.dta", clear


* Drop missing
drop if schlcode == 999999
drop if classize == .

** Create an enrollment variable
bys schlcode classid: gen first_obs = _n==1
gen classenroll = first_obs * classize
egen enrollment = total(classenroll), by(schlcode)
drop first_obs
drop classenroll

** Check the collapse-merge above to be correct
preserve
collapse(first) classize (first) enrollment, by(schlcode classid)
egen enroll2 = total(classize), by(schlcode)
gen test = enrollment == enroll2
summarize test
* Min = max = 1 means every line matches; both ways 
* of computing enrollment are the same
restore

*** Plot enrollment histogram
* Note we must collapse by school 
collapse(first) enrollment, by(schlcode)
hist enrollment, discrete freq xlabel(40 80 120) addplot(pci 0 40 30 40||pci 0 80 30 80 ||pci 0 120 30 120) legend(off) xtitle("Enrollment") saving("Figures\FigA7", replace)

************************************************************
************************************************************

** Touch up colors manually in Stata graph editor
graph save "Figures\FigA7.gph" , replace
graph export "Figures\FigA7.pdf", replace



