** Angrist, Lavy, Leder-Luis, Shany
** Maimonides Rule Redux 

** This file produces Figure A3
*  Density discontinuity tests (2002-2011)

****************
*** Load New Data
use "Data\MRuleRedux_new_DB", clear


* Make 1 value of enrollment per schoolx year
collapse(first) c_size cohort, by(schlcode year)
drop if missing(c_size)


*** Run the McCrary permutation test for November enrollment at 41 and 81

DCdensity c_size, breakpoint(41) generate(Xj Yj r0 fhat se_fhat) b(1) 
drop Xj Yj r0 fhat se_fhat
graph save "Figures\FigA3_a.gph", replace

DCdensity c_size, breakpoint(81) generate(Xj Yj r0 fhat se_fhat) b(1) 
drop Xj Yj r0 fhat se_fhat
graph  save "Figures\FigA3_b.gph", replace

gr combine "Figures\FigA3_a.gph" "Figures\FigA3_b.gph", title("A. November Enrollment") col(2) saving("Figures\FigA3_A.gph", replace) 


  
*** Run the McCrary permutation test for birthday-based enrollment at 41 and 81

DCdensity cohort, breakpoint(41) generate(Xj Yj r0 fhat se_fhat) b(1) 
drop Xj Yj r0 fhat se_fhat
graph save "Figures\FigA3_c.gph", replace

DCdensity cohort, breakpoint(81) generate(Xj Yj r0 fhat se_fhat) b(1) 
drop Xj Yj r0 fhat se_fhat
graph save "Figures\FigA3_d.gph", replace


gr combine "Figures\FigA3_c.gph" "Figures\FigA3_d.gph", title("B. Birthday-based Imputed Enrollment") col(2)  saving("Figures\FigA3_B.gph", replace)

* Combine panel A and panel B
graph combine "Figures\FigA3_A.gph" "Figures\FigA3_B.gph", rows(2)

************************************************************
************************************************************

** Touch up colors manually in Stata graph editor
graph save "Figures\FigA3.gph" , replace
graph export "Figures\FigA3.pdf", replace


