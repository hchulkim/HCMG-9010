** Angrist, Lavy, Leder-Luis, Shany
** Maimondies Rule Redux 


** This code produces the database for the analysis of 2002-2011

*****************************************************************

/*--------------------------------------------------------------*/
/*          Imports the raw data provided by the MOE            */
/*--------------------------------------------------------------*/

* Import merged file of all students files for the years 2001-2012
use "Data\STUDENTS FILES 2001_2012.dta", clear

* Keep only elementry school students
keep if status_noar==7
* Keep only 4th, 5th and 6th grades
keep if kita==4 | kita==5 | kita==6

* Import merged file of all schools files for the years 2001-2012
sort code_mosad year 
merge m:1 code_mosad year "Data\SCHOOLS FILES 2001_2012.dta"
drop if _m==2
drop _m

* Keep only Jewish regular elementary schools
/*Only Jewish*/
keep if code_migzar==1
/*Only Regular*/
keep if code_sug_mosad==1 |  code_sug_mosad==0
keep if code_sug_chinuch==1
keep if code_maamad_mishpati==1

* Import the standard classes file
/* the file include November enrollment by school, grade, class and year
the variable c_size was generate in the rechearch lab according to fifth grade November enrollment for each school and year*/  
sort code_mosad year kita makbila
merge m:1 code_mosad year kita makbila using "Data\STANDARD CLASSES.dta"
keep if _m==3
drop _m

* Import enrollment file
/* the file include June (actual) enrollment by school, grade, class and year
the variable classize was generate in the rechearch lab according to actual enrollment in each class of fifth grade for each school and year*/  
sort code_mosad year kita makbila
merge m:1 code_mosad year kita makbila using "Data\ENROLLMENT.dta"
keep if _m==3
drop _m


* Import SES index file
sort  code_mosad_new year
merge m:1 code_mosad_new year using "Data\SES INDEX.dta"
keep if _m==3
drop _m

/*--------------------------------------------------------------*/
/*                   Cleans the data                            */
/*--------------------------------------------------------------*/

rename code_mosad schlcode /*school id*/
rename kita grade /*grade level*/ 
rename makbila classid /*class sequence number*/
rename code_zehut_talmid_new student_id /*student id*/
rename taarich_leda birth_date /*date of birth*/
format birth_date %td
rename asiron tipuach /*ses index*/

label var classize "actual class size"
label var c_size "planned enrollment (November enrollment)"
label var tipuach "ses index 1 to 10"


* drop special education classes 
keep if (sug_kita==1 |  sug_kita==0 | sug_kita==25 | sug_kita==99 | sug_kita==.)
drop if classid>=20

*fixing date of birth
/*Cases where immigrants get default birth date, fixing when birth date is updated*/
egen m_birth_date=mean(birth_date), by(student_id)
/*indicates students with updated birth date during the years*/
gen help1=m_birth_date!=birth_date
egen help2=max(help1), by(student_id)
/*the last year is the most updated*/
egen max_year=max(year), by(student_id)
gen help3=birth_date if year==max_year & help2==1
replace help3=0 if help3==.
egen fix_birth_date=max(help3), by(student_id)
replace birth_date=fix_birth_date if help2==1
drop help* max_year fix_birth_date

*generate year, month and day of birth
gen yob=year(birth_date)
gen mob=month(birth_date)
gen dob=day(birth_date)

* drop age outliers
gen age=year-yob
keep if age>8 & age<15

/*generate grade enrollment var according to students files*/
gen n=1
egen sum_classize=sum(n), by(schlcode year grade)

* keep repeaters only in the first year 
duplicates tag student_id grade, gen(repeaters)
egen min_year=min(year), by(student_id grade)
drop if repeaters!=0 & min_year!=year
drop min_year


/*--------------------------------------------------------------*/
/*   Generate the birthday-based imputed enrollment             */
/*--------------------------------------------------------------*/

gen cohort2002= ((yob==1990 &  mob==12 &  dob>=18) |  yob==1991)
replace cohort2002=0 if  (yob==1991 &  mob==12 &  dob>7)

gen cohort2003=((yob==1991 &  mob==12 &  dob>=8) |  yob==1992)
replace cohort2003=0 if  (yob==1992 &  mob==12 &  dob>24)

gen cohort2004= ((yob==1992 &  mob==12 &  dob>=25) |  yob==1993)
replace cohort2004=0 if  (yob==1993 &  mob==12 &  dob>14)

gen cohort2005= ((yob==1993 &  mob==12 &  dob>=15) |  yob==1994)
replace cohort2005=0 if  (yob==1994 &  mob==12 &  dob>3)

gen cohort2006=((yob==1994 &  mob==12 &  dob>=4) |  yob==1995)
replace cohort2006=0 if  (yob==1995 &  mob==12 &  dob>23)

gen cohort2007= ((yob==1995 &  mob==12 &  dob>=24) |  yob==1996)
replace cohort2007=0 if  (yob==1996 &  mob==12 &  dob>10)

gen cohort2008= ((yob==1996 &  mob==12 &  dob>=11) |  yob==1997)
replace cohort2008=0 if (yob==1997 &  mob==12 &  dob>29)

gen cohort2009= ((yob==1997 &  mob==12 &  dob>=30) |  yob==1998)
replace cohort2009=0 if  (yob==1998 &  mob==12 &  dob>19)

gen cohort2010= ((yob==1998 &  mob==12 &  dob>=20) |  yob==1999)
replace cohort2010=0  if (yob==1999 &  mob==12 &  dob>9)

gen cohort2011=((yob==1999 &  mob==12 &  dob>=10) |  yob==2000)
replace cohort2011=0 if (yob==2000 &  mob==12 &  dob>26)

gen cohort=cohort2002==1 | cohort2003==1 | cohort2004==1 | cohort2005==1 | cohort2006==1 | cohort2007==1 | cohort2008==1 | cohort2009==1 | cohort2010==1 | cohort2011==1


preserve

collapse  cohort2002 cohort2003 cohort2004 cohort2005 cohort2006 cohort2007 cohort2008 cohort2009 cohort2010 cohort2011, by(student_id schlcode)
collapse (sum) cohort2002 cohort2003 cohort2004 cohort2005 cohort2006 cohort2007 cohort2008 cohort2009 cohort2010 cohort2011, by(schlcode)
reshape long  cohort, i(schlcode) j(year)
drop if  cohort==0

label var cohort "birthday-based enrollment"

save "Data\BIRTHDAYBASED ENROLLMENT.dta", replace

restore
drop cohort*


/*--------------------------------------------------------------*/
/*              Fifth grade 2002-2011 only                      */
/*--------------------------------------------------------------*/

keep if year>=2002 & year<=2011
keep if grade==5


/*--------------------------------------------------------------*/
/*         Generate cleaned controls variables                  */
/*--------------------------------------------------------------*/

rename shnot_limud_av fatheduc /*father's years of schooling*/
rename shnot_limud_em motheduc /*mother's years of schooling*/
rename erets_leda birth_country /*country of birth*/
rename erets_leda_av fath_birth_country /*father's country of birth*/
rename erets_leda_em moth_birth_country /*mother's country of birth*/
rename mispar_achim_tsad_em siblings/*siblings from the same mother*/
rename code_sug_pikuach c_pik /*=1 for secular school, =2 for religious school*/
rename code_min gender /*=1 for boy, =2 for girl, =9 unknown*/

* gender dummy
drop if gender==9
gen boy=gender==1

* parents’ years of schooling and number of siblings
foreach var in fatheduc motheduc siblings{
replace `var'=0 if `var'==99
gen miss_`var'=`var'==88 | `var'==.
replace `var'=0 if `var'==88 | `var'==.
replace `var'=25 if `var'>25
}

*  ethnic-origin indicators
gen born_isr=birth_country==900
gen per_birth_country=fath_birth_country
replace per_birth_country=moth_birth_country if (fath_birth_country==0 | fath_birth_country>=900)
replace per_birth_country=birth_country if (fath_birth_country==0 | fath_birth_country>=900) & (moth_birth_country==0 | moth_birth_country>=900)
gen eth_israel=per_birth_country==900 
gen eth_fsu=(per_birth_country==300 | per_birth_country==3000 |(per_birth_country>=301 & per_birth_country<=307) | (per_birth_country>=311 & per_birth_country<=316) | per_birth_country==9 | per_birth_country==308)
gen eth_ethimm=per_birth_country==250
gen eth_asiafr=((per_birth_country>=10 & per_birth_country<250) | (per_birth_country>250 & per_birth_country<259) | (per_birth_country>260 & per_birth_country<270) |  (per_birth_country>270 & per_birth_country<=290))
gen eth_euram=((per_birth_country==259 | per_birth_country==260 | per_birth_country==270 | per_birth_country==310 | (per_birth_country>=400 & per_birth_country<=889)))	
gen eth_otrimm=eth_fsu==0 & eth_ethimm==0 & eth_asiafr==0 & eth_euram==0 & eth_israel==0 

* indicator for religious schools
gen relig=c_pik==2


/*--------------------------------------------------------------*/
/*          Merge birthday-based imputed enrollment             */
/*--------------------------------------------------------------*/

sort schlcode year 
merge m:1 schlcode year using "Data\BIRTHDAYBASED ENROLLMENT.dta"
keep if _m==3
drop _m


/*--------------------------------------------------------------*/
/*       Imports GEMS test scores provided by the MOE           */
/*--------------------------------------------------------------*/

rename student_id code_zehut_talmid_new
sort code_zehut_talmid_new
merge m:1 code_zehut_talmid_new using "Data\GEMS TEST SCORES.dta"
drop if _m==2
drop _m

rename code_zehut_talmid_new student_id
rename matsum5 mathscore /*math score*/
rename hebsum5 verbscore /*languag score*/

*clean test scores
replace mathscore =0 if mathscore <0
replace mathscore =100 if mathscore >100 & mathscore!=.
replace verbscore =0 if verbscore <0
replace verbscore =100 if verbscore >100 & verbscore!=.

* define number tested
gen math=(mathscore!=.)
gen verb=(verbscore!=.)
egen c_mathsize=sum(math), by(year schlcode classid) /*number tested in math at each class*/
egen c_verbsize=sum(verb), by(year schlcode classid) /*number tested in languag at each class*/
egen s_mathsize=sum(math), by(year schlcode) /*number tested in math at each school*/
egen s_verbsize=sum(verb), by(year schlcode) /*number tested in languag at each school*/
egen c_num_tested=rowmax(c_mathsize c_verbsize) 
egen s_num_tested=rowmax(s_mathsize s_verbsize)


*keep only data with test score
keep if mathscore!=. | verbscore!=.


*generate percet tested in each school and class
gen p_takers_grade=s_num_tested/sum_classize
gen p_takers_class=c_num_tested/classize

*after checking... less than 0.25 is due to coding file mistakes
keep if p_test_grade>0.25

** Schools tested in two following years, keep the first
egen max_year=max(year), by( schlcode)
egen min_year=min(year), by( schlcode)
drop if year==2003 & min_year==2002 
drop if year==2004 & min_year==2003
drop if schlcode==3 & year==2006
drop if schlcode==11 & year==2008
drop if schlcode==33 & year==2006
drop if schlcode==96 & year==2006
drop if schlcode==108 & year==2006
drop if schlcode==123 & year==2006
drop if schlcode==1026 & year==2011
drop if schlcode==1323 & year==2006

* clean data from outliers
keep if classize>1 & classize<45 & sum_classize>5 & c_size>0


save "Data\MRuleRedux_new_DB.dta", replace



