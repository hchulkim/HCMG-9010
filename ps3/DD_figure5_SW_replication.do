
use divorce_example if year>=1964 & sex==2, clear
*drop if stfips==6
gen post = year>=_nfd
replace post = 1 if nfd=="PRE"
*replace asmrs = ln(asmrs*10)*100
replace asmrs = asmrs*10

xi: reg asmrs i.stfips i.year post, robust

local DDL = _b[post]
local DD : display %03.2f _b[post]
local DDSE : display %03.2f _se[post]
local DD1 = `DD' - 1

xi: reg asmrs i.stfips i.year _Texp*, robust
outreg2 using sw_replication.xls, replace keep(_T*) noparen noaster addstat(DD, `DD', DDSE, `DDSE')

/***Get ES Coefs***/
xmluse sw_replication.xls, clear cells(A3:B56) first
replace VARIABLES = subinstr(VARIABLES,"_Texp_","",.)	
quietly destring _all, replace ignore(",")
compress
drop in 1/3
*replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
compress

ren VARIABLES exp
gen b = exp<.
replace exp = exp-10
local obs = _N
forval k = 2/`obs'{
	local j = `k'-1
	replace exp = exp[`j'] in `k' if exp[`k']==.
}

local obs =_N+1
set obs `obs'
for var _all: replace X = 0 in `obs'
replace b = 1 in `obs'
replace exp = -1 in `obs'
keep exp asmrs b 
reshape wide asmrs, i(exp) j(b)



cap drop *lb* *ub*
gen lb = asmrs1 - 1.96*asmrs0 if exp~=-19
gen ub = asmrs1 + 1.96*asmrs0 if exp~=-19

#delimit ;
twoway (scatter asmrs1 ub lb exp , 
		lpattern(solid dash dash dot dot solid solid) 
		lcolor(gray gray gray red blue) 
		lwidth(thick medium medium medium medium thick thick)
		msymbol(i i i i i i i i) msize(medlarge medlarge)
		mcolor(gray black gray gray red blue) 
		c(l l l l l l l l l) 
		cmissing(n n n n n n n) 
		xline(-1, lcolor(black) lpattern(solid))
		yline(0, lcolor(black)) 
		xlabel(-8 -4 -1 3 7 11 15, labsize(medium))
		ylabel(, nogrid labsize(medium))
		xsize(7.5) ysize(5.5) 			
		legend(off)
		xtitle("Years Relative to Divorce Reform", size(medium))
		ytitle("Suicides per 1m women", size(medium))
		graphregion(fcolor(white) color(white) icolor(white) margin(zero))
		yline(`DDL', lcolor(red) lwidth(thick)) text(`DD1' -2.55 "DD Coefficient = `DD' (s.e. = `DDSE')")
		)
		;

#delimit cr;		
graph export "f5_SW_replication.png", replace as(png) 
 
exit



