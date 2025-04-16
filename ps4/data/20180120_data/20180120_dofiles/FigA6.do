** Angrist, Lavy, Leder-Luis, Shany
** Maimonides Rule Redux 

** This file produces Figure A6

*****************

**************************
*  Panel A - histograms  *
**************************
**** Fourth Grade
use "Data\final4.dta", clear
** Have to collapse by school
** Or multiple classrooms that draw from the same enrollment confound bunching
collapse(first) c_size (first) c_pik, by(schlcode)
label var c_size "Enrollment"

** Keep enrollments below 200 
keep if c_size < 200

** Plot for bunching test in enrollment
** With lines
hist c_size, discrete freq xlabel(20 40 60 80 100 120) addplot(pci 0 40 20 40||pci 0 80 20 80 ||pci 0 120 20 120) legend(off) title("4th Grade") saving("Figures\FigA6_4thHist")

************************************************************
************************************************************
**** Fifth Grade
use "Data\final5", clear
** Have to collapse by school
** Or multiple classrooms that draw from the same enrollment confound bunching
collapse(first) c_size (first) c_pik, by(schlcode)
label var c_size "Enrollment"

** Below 200 
keep if c_size < 200

** Plot for bunching test in enrollment
** With lines
hist c_size, discrete freq xlabel(20 40 60 80 100 120) addplot(pci 0 40 20 40||pci 0 80 20 80 ||pci 0 120 20 120) legend(off) title("5th Grade") saving("Figures\FigA6_5thHist")


*****************************
*  Panel A - McCrary tests  *
*****************************
**** Fourth Grade
use "Data\final4", clear
label var c_size "Enrollment"

*Check: one enrollment size for each school
by schlcode c_size, sort: gen nvals = _n == 1 
by schlcode: replace nvals = sum(nvals)
by schlcode: replace nvals = nvals[_N] 

* all 1, as expected

* 1 value of enrollment per school
collapse(first) c_size, by(schlcode)

** Below 200 
keep if c_size < 200

** McCrary Graph, 40 breakpoint
DCdensity c_size, breakpoint(41) generate(Xj Yj r0 fhat se_fhat) b(1) 
drop Xj Yj r0 fhat se_fhat

graph save "Figures\FigA6_4thMcCrary41.gph", replace


****************************************
** Fifth grade
use "Data\final5", clear
label var c_size "Enrollment"


*Check: one enrollment size for each school
by schlcode c_size, sort: gen nvals = _n == 1 
by schlcode: replace nvals = sum(nvals)
by schlcode: replace nvals = nvals[_N] 
* all 1, as expected


* Collapse for 1 value of enrollment per school
collapse(first) c_size, by(schlcode)

** Below 200 
keep if c_size < 200


** McCrary Graph, 40 breakpoint
DCdensity c_size, breakpoint(41) generate(Xj Yj r0 fhat se_fhat)  b(1)
graph save "Figures\FigA6_5thMcCrary41.gph", replace 

**
****************************************

** Combine

graph combine "Figures\FigA6_5thHist.gph"  "Figures\FigA6_4thHist.gph", title("A. Histograms") col(2) saving("Figures\FigA6_A.gph", replace)  
graph combine "Figures\FigA6_5thMcCrary41.gph"  "Figures\FigA6_4thMcCrary41.gph", title("B. McCrary Tests") col(2) saving("Figures\FigA6_B.gph", replace) 

* Combine panel A and panel B
graph combine "Figures\FigA6_A.gph" "Figures\FigA6_B.gph", rows(2)

************************************************************
************************************************************

** Touch up colors manually in Stata graph editor
graph save "Figures\FigA6.gph" , replace
graph export "Figures\FigA6.pdf", replace

