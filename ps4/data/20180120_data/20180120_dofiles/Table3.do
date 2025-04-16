
** Angrist, Lavy, Leder-Luis, Shany
** Maimondies Rule Redux 

** This code produces Table 3
*** We put PD (socioeconomic status variable) on the LHS to test for issues with instrument

******************
*** Fifth Grade

** Load data
use "Data\final5.dta", clear

** Original Cleaning
replace avgverb= avgverb-100 if avgverb>100
replace avgmath= avgmath-100 if avgmath>100

g func1= c_size/(int((c_size-1)/40)+1)
g func2= cohsize/(int(cohsize/40)+1)

replace avgverb=. if verbsize==0
replace passverb=. if verbsize==0

replace avgmath=. if mathsize==0
replace passmath=. if mathsize==0

keep if 1<classize & classize<45 & c_size>5
keep if c_leom==1 & c_pik<3
keep if avgverb~=.

g c_size2= (c_size^2)/100

* GENERATE TREND
g trend= c_size if c_size>=0 & c_size<=40
	replace trend= 20+(c_size/2) if c_size>=41 & c_size<=80
	replace trend= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
	replace trend= (130/3)+(c_size/4) if c_size>=121 & c_size<=160

******
*Make sure tipuach, c_size, c_pik are unique by schlcode

* Tipuach
by schlcode tipuach, sort: generate y = _n == 1
by schlcode:  replace y = sum(y) 
summarize y 
	* Notice min, max are 1
	* Therefore at most 1 value per schlcode
drop y 

* Repeat for c_size
by schlcode c_size, sort: generate y = _n == 1
by schlcode:  replace y = sum(y) 
summarize y 
	* Notice min, max are 1
	* Therefore at most 1 value per schlcode
drop y 

* Repeat for c_pik
by schlcode c_pik, sort: generate y = _n == 1
by schlcode:  replace y = sum(y) 
summarize y 
	* Notice min, max are 1
	* Therefore at most 1 value per schlcode
drop y 
*****


** COLLAPSE BY SCHOOL
collapse (first) tipuach (first) func1 (first) c_size (first) c_size2 (first) c_pik (first) trend, by(schlcode)

** Run regressions
reg tipuach func1 c_size c_pik
est sto A
reg tipuach func1 c_size c_size2 c_pik
est sto B
reg tipuach func1 trend c_pik
est sto C

******************
*** Fourth Grade

** Load data
use "Data\final4.dta", clear

** Original Cleaning
replace avgverb= avgverb-100 if avgverb>100
replace avgmath= avgmath-100 if avgmath>100

g func1= c_size/(int((c_size-1)/40)+1)
g func2= cohsize/(int(cohsize/40)+1)

replace avgverb=. if verbsize==0
replace passverb=. if verbsize==0

replace avgmath=. if mathsize==0
replace passmath=. if mathsize==0

keep if 1<classize & classize<45 & c_size>5
keep if c_leom==1 & c_pik<3
keep if avgverb~=.

g c_size2= (c_size^2)/100

* GENERATE TREND
g trend= c_size if c_size>=0 & c_size<=40
	replace trend= 20+(c_size/2) if c_size>=41 & c_size<=80
	replace trend= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
	replace trend= (130/3)+(c_size/4) if c_size>=121 & c_size<=160

******
*Make sure tipuach, c_size, c_pik are unique by schlcode

* Tipuach
by schlcode tipuach, sort: generate y = _n == 1
by schlcode:  replace y = sum(y) 
summarize y 
	* Notice min, max are 1
	* Therefore at most 1 value per schlcode
drop y 

* Repeat for c_size
by schlcode c_size, sort: generate y = _n == 1
by schlcode:  replace y = sum(y) 
summarize y 
	* Notice min, max are 1
	* Therefore at most 1 value per schlcode
drop y 

* Repeat for c_pik
by schlcode c_pik, sort: generate y = _n == 1
by schlcode:  replace y = sum(y) 
summarize y 
	* Notice min, max are 1
	* Therefore at most 1 value per schlcode
drop y 
*****


collapse (first) tipuach (first) func1 (first) c_size (first) c_size2 (first) c_pik (first) trend, by(schlcode)


** Run regressions
reg tipuach func1 c_size c_pik
est sto D
reg tipuach func1 c_size c_size2 c_pik
est sto E
reg tipuach func1 trend c_pik
est sto F



******************
*** Third Grade

** Load data
use "Data\3rdGrade_1992Data_merged.dta", clear


* Drop missing
drop if schlcode == 999999
drop if classize == .

** Create an enrollment variable
bys schlcode classid: gen first_obs = _n==1
gen classenroll = first_obs * classize
egen c_size = total(classenroll), by(schlcode)
drop first_obs
drop classenroll

* Define maimonides rule 
g func1= c_size/(int((c_size-1)/40)+1)
* Define square enrollment
g c_size2= (c_size^2)/100

* GENERATE TREND
g trend= c_size if c_size>=0 & c_size<=40
	replace trend= 20+(c_size/2) if c_size>=41 & c_size<=80
	replace trend= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
	replace trend= (130/3)+(c_size/4) if c_size>=121 & c_size<=160

******
*Make sure tipuach, c_size, c_pik are unique by schlcode

* Tipuach
by schlcode tipuach, sort: generate y = _n == 1
by schlcode:  replace y = sum(y) 
summarize y 
	* Notice min, max are 1
	* Therefore at most 1 value per schlcode
drop y 

* Repeat for c_size
by schlcode c_size, sort: generate y = _n == 1
by schlcode:  replace y = sum(y) 
summarize y 
	* Notice min, max are 1
	* Therefore at most 1 value per schlcode
drop y 

* Repeat for c_pik
by schlcode c_pik, sort: generate y = _n == 1
by schlcode:  replace y = sum(y) 
summarize y 
	* Notice min, max are 1
	* Therefore at most 1 value per schlcode
drop y 
*****

collapse (first) tipuach (first) func1 (first) c_size (first) c_size2 (first) c_pik (first) trend, by(schlcode)



** Run regressions

reg tipuach func1 c_size c_pik
est sto G
reg tipuach func1 c_size c_size2 c_pik
est sto H
reg tipuach func1 trend c_pik
est sto I




******************
*** Print out as 1 big table

esttab A B C D E F G H I, nocon se stat(N), using "Tables\Table3.tex", replace 





