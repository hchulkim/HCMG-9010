** Angrist, Lavy, Leder-Luis, Shany
** Maimondies Rule Redux 

** This code produces Table A4

**** Replicate Maimonides Rule original results on full sample
** Clustering standard error by school

******************


**** Original Table IV: Fifth Graders
*** Original cleaning
use "Data\final5.dta", clear

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

* This generates the discontinuity sample (not used here)
*g byte disc= (c_size>=36 & c_size<=45) | (c_size>=76 & c_size<=85) | ///
*	(c_size>=116 & c_size<=125)

g byte all=1
g c_size2= (c_size^2)/100

* GENERATE TREND
g trend= c_size if c_size>=0 & c_size<=40
	replace trend= 20+(c_size/2) if c_size>=41 & c_size<=80
	replace trend= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
	replace trend= (130/3)+(c_size/4) if c_size>=121 & c_size<=160


*** Replicate columns 2-4 in Table IV
*OMIT column 1 ivreg2 avgverb (classize=func1) tipu, clu(schlcode) 2sls
ivreg avgverb (classize=func1) tipu c_size, cluster(schlcode) 
est sto A
ivreg avgverb (classize=func1) tipu c_size c_size2, cluster(schlcode) 
est sto B
ivreg avgverb (classize=func1) trend, cluster(schlcode) 
est sto C
*** Replicate columns 8-10 in Table IV
ivreg avgmath (classize=func1) tipu c_size, cluster(schlcode) 
est sto D
ivreg avgmath (classize=func1) tipu c_size c_size2, cluster(schlcode) 
est sto E
ivreg avgmath (classize=func1) trend, cluster(schlcode) 
est sto F
***** 


***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** 
**** Original Table V: Fourth Graders
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

* This generates the discontinuity sample (not used here)
*g byte disc= (c_size>=36 & c_size<=45) | (c_size>=76 & c_size<=85) | ///
*	(c_size>=116 & c_size<=125)

g byte all=1
g c_size2= (c_size^2)/100

* GENERATE TREND
g trend= c_size if c_size>=0 & c_size<=40
	replace trend= 20+(c_size/2) if c_size>=41 & c_size<=80
	replace trend= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
	replace trend= (130/3)+(c_size/4) if c_size>=121 & c_size<=160
	* To include 160-200: replace trend= (154/3)+(c_size/5) if c_size>=161 & c_size<=200

*** Replicate columns 2-4 in Table V
*OMIT column 1 ivreg2 avgverb (classize=func1) tipu, clu(schlcode) 2sls
ivreg avgverb (classize=func1) tipu c_size, cluster(schlcode) 
est sto G
ivreg avgverb (classize=func1) tipu c_size c_size2, cluster(schlcode) 
est sto H
ivreg avgverb (classize=func1) trend, cluster(schlcode) 
est sto I
*** Replicate columns 8-10 in Table V
ivreg avgmath (classize=func1) tipu c_size, cluster(schlcode) 
est sto J
ivreg avgmath (classize=func1) tipu c_size c_size2, cluster(schlcode) 
est sto K
ivreg avgmath (classize=func1) trend, cluster(schlcode) 
est sto L
***** 

esttab A B C D E F,  nocon se stats(N), using "Tables\TableA4.tex", replace  
esttab G H I J K L,  nocon se stats(N), using "Tables\TableA4.tex", append   


