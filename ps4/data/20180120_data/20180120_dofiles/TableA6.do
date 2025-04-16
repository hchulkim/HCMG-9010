** Angrist, Lavy, Leder-Luis, Shany
** Maimondies Rule Redux 

** This code produces Table A6

**** Replicate Maimonides Rule original results on donut sample
** Clustering standard error by school

******************

use "Data\final5.dta", clear

**** Original Table IV in AL 1999 is  Fifth Graders
*** Replcate original cleaning

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
g byte all=1
g c_size2= (c_size^2)/100

* GENERATE TREND
g trend= c_size if c_size>=0 & c_size<=40
	replace trend= 20+(c_size/2) if c_size>=41 & c_size<=80
	replace trend= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
	replace trend= (130/3)+(c_size/4) if c_size>=121 & c_size<=160


**** define donuts
g byte donut1 = c_size< 38 | c_size> 42
g byte donut2 = c_size< 37 | c_size> 43
g byte donut3 = c_size< 39 | c_size> 41

*********************************************
****** Regressions
*********************************************

*** Donuts on Linear and Quadratic

** Donut: )39, 41(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut3==1, cluster(schlcode) 
est sto A5
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut3==1, cluster(schlcode) 
est sto B5
* Math
ivreg avgmath (classize=func1) tipu c_size if donut3==1, cluster(schlcode) 
est sto C5
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut3==1, cluster(schlcode) 
est sto D5


esttab A5 B5 C5 D5, nocon se stats(N), using "Tables\TableA6.tex", replace
est clear 

** Donut: )38,42(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut1==1, cluster(schlcode) 
est sto E5
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut1==1, cluster(schlcode) 
est sto F5

* Math
ivreg2 avgmath (classize=func1) tipu c_size if donut1==1, cluster(schlcode) 
est sto G5
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut1==1, cluster(schlcode) 
est sto H5

esttab E5 F5 G5 H5, nocon se stats(N), using "Tables\TableA6.tex", append  
est clear 
***** 

** Donut:  )37, 43(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut2==1, cluster(schlcode) 
est sto I5
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut2==1, cluster(schlcode) 
est sto J5
* Math
ivreg avgmath (classize=func1) tipu c_size if donut2==1, cluster(schlcode) 
est sto K5
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut2 == 1, cluster(schlcode) 
est sto L5
**
esttab I5 J5 K5 L5, nocon se stats(N), using "Tables\TableA6.tex", append  
est clear 

**
*****************************************************************************************
*****************************************************************************************
*****************************************************************************************
*****************************************************************************************

** Fourth graders, same 

use "Data\final4.dta", clear


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
g byte all=1
g c_size2= (c_size^2)/100

* GENERATE TREND
g trend= c_size if c_size>=0 & c_size<=40
	replace trend= 20+(c_size/2) if c_size>=41 & c_size<=80
	replace trend= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
	replace trend= (130/3)+(c_size/4) if c_size>=121 & c_size<=160


**** define donuts
g byte donut1 = c_size< 38 | c_size> 42
g byte donut2 = c_size< 37 | c_size> 43
g byte donut3 = c_size< 39 | c_size> 41

*********************************************
****** Regressions
*********************************************

*** Donuts on Linear and Quadratic

** Donut: )39, 41(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut3==1, cluster(schlcode) 
est sto A4
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut3==1, cluster(schlcode) 
est sto B4
* Math
ivreg avgmath (classize=func1) tipu c_size if donut3==1, cluster(schlcode) 
est sto C4
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut3==1, cluster(schlcode) 
est sto D4


esttab A4 B4 C4 D4, nocon se stats(N), using "Tables\TableA6.tex", append
est clear 

** Donut: )38,42(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut1==1, cluster(schlcode) 
est sto E4
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut1==1, cluster(schlcode) 
est sto F4

* Math
ivreg avgmath (classize=func1) tipu c_size if donut1==1, cluster(schlcode) 
est sto G4
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut1==1, cluster(schlcode) 
est sto H4

esttab E4 F4 G4 H4, nocon se stats(N),  using "Tables\TableA6.tex", append
est clear 
***** 

** Donut:  )37, 43(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut2==1, cluster(schlcode) 
est sto I4
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut2==1, cluster(schlcode) 
est sto J4
* Math
ivreg avgmath (classize=func1) tipu c_size if donut2==1, cluster(schlcode) 
est sto K4 
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut2 == 1, cluster(schlcode) 
est sto L4
**
esttab I4 J4 K4 L4, nocon se stats(N), using "Tables\TableA6.tex", append
est clear 
