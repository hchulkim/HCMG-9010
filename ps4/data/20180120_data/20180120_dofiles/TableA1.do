** Angrist, Lavy, Leder-Luis, Shany
** Maimondies Rule Redux 

** This code produces Table A1
** Descriptive Statistics 
************************

****************
*** Load New Data
use "Data\MRuleRedux_new_DB", clear


* Generate number of classes per school/year
egen num_class=nvals(classid), by(schlcode year)


** Generate the table
***By class (panel A)
preserve
collapse classize  c_mathsize c_verbsize verbscore mathscore, by(year  schlcode classid)
estpost tabstat verbscore mathscore classize  c_mathsize c_verbsize, statistics(n mean sd p10 p25 median p75 p90 ) columns(statistics)
esttab using Tables\TableA1.tex, cells("count mean sd p10 p25 p50 p75 p90") replace nonum noobs
restore

***By school (panel B)
preserve
collapse sum_classize c_size cohort tipuach num_class relig, by(year  schlcode)
estpost tabstat sum_classize c_size cohort tipuach num_class  relig, statistics(n mean sd p10 p25 median p75 p90 ) columns(statistics)
esttab using Tables\TableA1.tex, cells("count mean sd p10 p25 p50 p75 p90") append nonum noobs
restore

***By student (panel C)
estpost tabstat verbscore mathscore fatheduc motheduc siblings boy born_isr eth_israel eth_ethimm eth_fsu eth_asiafr eth_euram , statistics(n mean sd p10 p25 median p75 p90 ) columns(statistics) 
esttab using Tables\TableA1.tex, cells("count mean sd p10 p25 p50 p75 p90") append nonum noobs



