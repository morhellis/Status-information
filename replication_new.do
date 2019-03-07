
local path="C:\Users\alexei.zakharov\Dropbox\research\status_experiment\2018\"

*Creates an index for social engagement from questions b1-b8. Treats "Don't know" answers as missing data, and imputes the data.
use "`path'replication_dataset_2019.dta", clear
drop if include_data==0
keep subj_id b11 b21 b31 b41 b51 b61 b71 b81 trust gender1 family1
drop if b21==.
replace b21=. if b21==6
replace b31=. if b31==8
replace b41=. if b41==7
replace b51=. if b51==6
replace b61=. if b61==3
replace b71=. if b71==6
replace b81=. if b81==6

export delimited using "`path's1.csv", replace
import delimited "`path's1.csv", clear


mi unset    
tsset, clear
duplicates drop
mi set mlong
mi register imputed b11 b21 b31 b41 b51 b61 b71 b81
mi impute mvn b11 b21 b31 b41 b51 b61 b71 b81, add(10)
drop if _mi_miss==1
collapse b11 b21 b31 b41 b51 b61 b71 b81 trust, by(subj_id)
pca b21-b81
alpha b21-b81
predict soc_f
tabstat soc_f, stats(sd) save
mat rr=r(StatTotal)
local rrr=rr[1,1]
replace soc_f=-soc/`rrr'

keep subj_id soc_f
export delimited using "`path'socindex.csv", replace
import delimited "`path'socindex.csv", clear

save "`path'soc_index.dta", replace
use "`path'replication_dataset_2019.dta", clear
drop _merge
drop soc_f
merge m:1 subj_id using "`path'soc_index.dta"
drop _merge
lab var soc_f "Socialization index"

drop x_y1_soc x21_soc
gen x_y1_soc=x_y1*soc_f
gen x21_soc=x21*soc_f
lab var x_y1_soc "Private\space{}signal$\times$\space{}Socialization"
lab var x21_soc "Partner's\space{}action$\times$\space{}Socialization"





****************************************************************************************************************************************
*** Table: Summary Statistics.
****************************************************************************************************************************************

tabstat age gender1 if period==1&include_data==1, stats(mean median)

pca own1-own8 if include_data==1
alpha own1-own8 if include_data==1

pca ownothers1-ownothers8 if include_data==1
alpha ownothers1-ownothers8 if include_data==1

pca c1-c9 if include_data==1
alpha c1-c9 if include_data==1

pca x1n x2n x3n x4n x5n if include_data==1
alpha x1n x2n x3n x4n x5n if include_data==1

local fname="`path'table_sum.tex"
eststo clear
eststo: estpost sum gender1 age parents_edhi fam_ys fam_os fam_oc safe cog afpos afneg own_f ownothers_f lead_f civ_f soc_f active_part sport employed health wealth1 wealthchange1 expectedwealthchange1 trust if include_data==1
esttab using "`fname'", cells("mean(fmt(%3.2f)) sd(fmt(%3.2f)) count(fmt(%3.0f))") label replace nonum noobs



*\ref{stata:x1rat}
quietly gen x1_rational=0
replace x1_rational=1 if (x1>=x_y1-7)&(x1<=x_y1+7)

quietly gen x1_conserv=0
quietly replace x1_conserv=1 if x1_rational==1&(x1==0&x_y1==0)|(x1>=x_y1&x1<=0&x_y1<0)|(x1<=x_y1&x1>=0&x_y1>0)
tabstat x1_conserv if include_data==1

quietly gen x1_regular=0
quietly replace x1_regular=1 if inlist(x1,floor(x_y1/2),ceil(x_y1))
tabstat x1_regular if include_data==1

quietly gen x1_eq=0
quietly gen x1_eq=x1_regular
quietly replace x1_eq=1 if x_y1==0&x1>=-3&x1<=3
quietly replace x1_eq=1 if x_y1==1&x1>=-2&x1<=3
quietly replace x1_eq=1 if x_y1==2&x1>=-1&x1<=3
quietly replace x1_eq=1 if x_y1==3&x1>=0&x1<=3
quietly replace x1_eq=1 if x_y1==4&x1>=1&x1<=3
quietly replace x1_eq=1 if x_y1==-1&x1>=-3&x1<=2
quietly replace x1_eq=1 if x_y1==-2&x1>=-3&x1<=1
quietly replace x1_eq=1 if x_y1==-3&x1>=-3&x1<=0
quietly replace x1_eq=1 if x_y1==-4&x1>=-3&x1<=-1
tabstat x1_eq if include_data==1

quietly gen x2_rational=0
quietly replace x2_rational=1 if (x2>=x_y1-7)&(x2<=x_y1+7)
tabstat x1_rational x2_rational if include_data==1
quietly gen x2_inner=0
quietly replace x2_inner=1 if (x_y1<=x2&x2<=x21)|(x_y1>=x2&x2>=x21)

tabstat x2_inner if include_data==1


****************************************************************************************************************************************
*** Table: The effects of treatment, subjective status, and objective status on first-period action.
****************************************************************************************************************************************
local varlist "x_y1_1 var1"
local nn: word count of `varlist'
local nn=`nn'-1
forval i=1/`nn' {
local vname: word `i' of `varlist'
capture confirm variable `vname'
if !_rc {
                       drop `vname'
}
}

//
local fname="`path'table_new_x1.tex"
local note="OLS regressions. Dependent variable is individual's first-period action. Standard errors clustered by subject. Other covariates not shown. [Var.] is as follows. Column 1: Dictator (0 or 1); Column 2: Subjective status index; Column3: Peer's subjective status index; Column 4: Income category (1-6); Column 5: Expecteded change in well-being (1-5); Column 6: Retrospective change in well-being (1-5); Column 7: Both parents have higher education (0 or 1); Column 8: has an older sibling (0 or 1); Column 9: Has a younger sibling (0 or 1); Column 10: In only child (0 or 1)"
est clear
gen x_y1_1=.
lab var x_y1_1 "Priv.\space{}sig.$\times$\space{}[Var.]"

gen var1=.
local varl="DG_role own_f ownothers_f wealth1 expectedwealthchange1 wealthchange1 parents_edhi fam_ys fam_os fam_oc"
estimates clear
forval i=1/10 {
	local vv: word `i' of `varl'
	replace var1=`vv'
	replace x_y1_1=x_y1*`vv'
	reg x1 x_y1 x_y1_1 var1 if include_data==1, clu(subj_id)
	est sto m`i'
}

local tokeep="x_y1_1"

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 using "`fname'", label mtitle("Dictator" "Subj-own" "Subj-other" "Income" "Inc. (exp)" "Inc (retr)" "Parental ed." "Yo.  sib." "Old. sib." "Only child") keep(`tokeep') stats(r2 N) nogap compress se star(* 0.10 ** 0.05 *** 0.01) replace note("`note'") nonum




****************************************************************************************************************************************
*** Table: The effects of other covariates on first-period action.
****************************************************************************************************************************************
local varlist "x_y1_1 var1"
local nn: word count of `varlist'
local nn=`nn'-1
forval i=1/`nn' {
local vname: word `i' of `varlist'
capture confirm variable `vname'
if !_rc {
                       drop `vname'
}
}

//

local fname="`path'table_new_x1_1.tex"
local note="OLS regressions. Dependent variable is individual's first-period action. Standard errors clustered by subject. Other covariates not shown. [Var.] is as follows. Column 1: Fraction of safe choices on the risk task; Column 2: Leadership skills; Column 3: Active in a sports/environmental/professional organization, labor union, or political party (0 or 1); Column 3: Sociablity index; Column 5: Subjective health (1-10); Column 6: Interpersonal Trust (0 or 1); Column 7: Civicness index; Column 8: Male (0 or 1); Column 9: Employed part-time or full-time (0 or 1); Column 10: Has a sports degree (0 or 1)"
est clear
gen x_y1_1=.
lab var x_y1_1 "Priv.\space{}sig.$\times$\space{}[Var.]"

gen var1=.
local varl="safe lead_f active_part soc_f health trust civ_f gender1 employed sport"
estimates clear
forval i=1/10 {
	local vv: word `i' of `varl'
	replace var1=`vv'
	replace x_y1_1=x_y1*`vv'
	reg x1 x_y1 x_y1_1 var1 if include_data==1, clu(subj_id)
	est sto m`i'
}

local tokeep="x_y1_1"

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 using "`fname'", label mtitle(Risk Lead Active Social Health Trust Civic Male Employed Sports) keep(`tokeep') stats(r2 N) nogap compress se star(* 0.10 ** 0.05 *** 0.01) replace note("`note'") nonum



****************************************************************************************************************************************
*** Table: The effects of treatment, subjective status, and objective status on second-period action.
****************************************************************************************************************************************
local varlist "x_y1_1 x21_1 var1"
local nn: word count of `varlist'
local nn=`nn'-1
forval i=1/`nn' {
local vname: word `i' of `varlist'
capture confirm variable `vname'
if !_rc {
                       drop `vname'
}
}

//
local fname="`path'table_new_x2.tex"
local note="OLS regressions. Dependent variable is individual's second-period action. Standard errors clustered by subject. Other covariates not shown. [Var.] is as follows. Column 1: Dictator (0 or 1); Column 2: Subjective status index; Column3: Peer's subjective status index; Column 4: Income category (1-6); Column 5: Expecteded change in well-being (1-5); Column 6: Retrospective change in well-being (1-5); Column 7: Both parents have higher education (0 or 1); Column 8: has an older sibling (0 or 1); Column 9: Has a younger sibling (0 or 1); Column 10: In only child (0 or 1)"
est clear
gen x_y1_1=.
gen x21_1=.
lab var x_y1_1 "Priv.\space{}sig.$\times$\space{}[Var.]"
lab var x21_1 "Part.\space{}act.$\times$\space{}[Var.]"


gen var1=.
local varl="DG_role own_f ownothers_f wealth1 expectedwealthchange1 wealthchange1 parents_edhi fam_ys fam_os fam_oc"
estimates clear
forval i=1/10 {
	local vv: word `i' of `varl'
	replace var1=`vv'
	replace x_y1_1=x_y1*`vv'
	replace x21_1=x21*`vv'
	reg x2 x_y1 x21 x_y1_1 x21_1 var1 if include_data==1, clu(subj_id)
	est sto m`i'
}

local tokeep="x_y1_1 x21_1"

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 using "`fname'", label mtitle("Dictator" "Subj-own" "Subj-other" "Income" "Inc. (exp)" "Inc (retr)" "Parental ed." "Yo.  sib." "Old. sib." "Only child") keep(`tokeep') stats(r2 N) nogap compress se star(* 0.10 ** 0.05 *** 0.01) replace note("`note'") nonum


****************************************************************************************************************************************
*** Table: The effects of other covariates on second-period action.
****************************************************************************************************************************************
local varlist "x_y1_1 x21_1 var1"
local nn: word count of `varlist'
local nn=`nn'-1
forval i=1/`nn' {
local vname: word `i' of `varlist'
capture confirm variable `vname'
if !_rc {
                       drop `vname'
}
}

//
local fname="`path'table_new_x2_1.tex"
local note="OLS regressions. Dependent variable is individual's second-period action. Standard errors clustered by subject. Other covariates not shown. [Var.] is as follows. Column 1: Fraction of safe choices on the risk task; Column 2: Leadership skills; Column 3: Active in a sports/environmental/professional organization, labor union, or political party (0 or 1); Column 3: Sociablity index; Column 5: Subjective health (1-10); Column 6: Interpersonal Trust (0 or 1); Column 7: Civicness index; Column 8: Male (0 or 1); Column 9: Employed part-time or full-time (0 or 1); Column 10: Has a sports degree (0 or 1)"
est clear
gen x_y1_1=.
gen x21_1=.
lab var x_y1_1 "Priv.\space{}sig.$\times$\space{}[Var.]"
lab var x21_1 "Part.\space{}act.$\times$\space{}[Var.]"


gen var1=.
local varl="safe lead_f active_part soc_f health trust civ_f gender1 employed sport"
estimates clear
forval i=1/10 {
	local vv: word `i' of `varl'
	replace var1=`vv'
	replace x_y1_1=x_y1*`vv'
	replace x21_1=x21*`vv'
	reg x2 x_y1 x21 x_y1_1 x21_1 var1 if include_data==1, clu(subj_id)
	est sto m`i'
}

local tokeep="x_y1_1 x21_1"

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 using "`fname'", label mtitle(Risk Lead Active Social Health Trust Civic Male Employed Sports) keep(`tokeep') stats(r2 N) nogap compress se star(* 0.10 ** 0.05 *** 0.01) replace note("`note'") nonum



****************************************************************************************************************************************
*** Table: The effects of treatment, subjective status, and objective status on second-period action, round 1
****************************************************************************************************************************************
local varlist "x_y1_1 x21_1 var1"
local nn: word count of `varlist'
local nn=`nn'-1
forval i=1/`nn' {
local vname: word `i' of `varlist'
capture confirm variable `vname'
if !_rc {
                       drop `vname'
}
}

//
local fname="`path'table_new_x2_r1.tex"
local note="OLS regressions. Dependent variable is individual's second-period action. Standard errors clustered by subject. Other covariates not shown. [Var.] is as follows. Column 1: Dictator (0 or 1); Column 2: Subjective status index; Column3: Peer's subjective status index; Column 4: Income category (1-6); Column 5: Expecteded change in well-being (1-5); Column 6: Retrospective change in well-being (1-5); Column 7: Both parents have higher education (0 or 1); Column 8: has an older sibling (0 or 1); Column 9: Has a younger sibling (0 or 1); Column 10: In only child (0 or 1)"
est clear
gen x_y1_1=.
gen x21_1=.
lab var x_y1_1 "Priv.\space{}sig.$\times$\space{}[Var.]"
lab var x21_1 "Part.\space{}act.$\times$\space{}[Var.]"


gen var1=.
local varl="DG_role own_f ownothers_f wealth1 expectedwealthchange1 wealthchange1 parents_edhi fam_ys fam_os fam_oc"
estimates clear
forval i=1/10 {
	local vv: word `i' of `varl'
	replace var1=`vv'
	replace x_y1_1=x_y1*`vv'
	replace x21_1=x21*`vv'
	reg x2 x_y1 x21 x_y1_1 x21_1 var1 if include_data==1&period==1, clu(subj_id)
	est sto m`i'
}

local tokeep="x_y1_1 x21_1"

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 using "`fname'", label mtitle("Dictator" "Subj-own" "Subj-other" "Income" "Inc. (exp)" "Inc (retr)" "Parental ed." "Yo.  sib." "Old. sib." "Only child") keep(`tokeep') stats(r2 N) nogap compress se star(* 0.10 ** 0.05 *** 0.01) replace note("`note'") nonum



****************************************************************************************************************************************
*** Table: The effects of other covariates on second-period action, round 1
****************************************************************************************************************************************
local varlist "x_y1_1 x21_1 var1"
local nn: word count of `varlist'
local nn=`nn'-1
forval i=1/`nn' {
local vname: word `i' of `varlist'
capture confirm variable `vname'
if !_rc {
                       drop `vname'
}
}

//
local fname="`path'table_new_x2_1_r1.tex"
local note="OLS regressions. Dependent variable is individual's second-period action. Standard errors clustered by subject. Other covariates not shown. [Var.] is as follows. Column 1: Fraction of safe choices on the risk task; Column 2: Leadership skills; Column 3: Active in a sports/environmental/professional organization, labor union, or political party (0 or 1); Column 3: Sociablity index; Column 5: Subjective health (1-10); Column 6: Interpersonal Trust (0 or 1); Column 7: Civicness index; Column 8: Male (0 or 1); Column 9: Employed part-time or full-time (0 or 1); Column 10: Has a sports degree (0 or 1)"
est clear
gen x_y1_1=.
gen x21_1=.
lab var x_y1_1 "Priv.\space{}sig.$\times$\space{}[Var.]"
lab var x21_1 "Part.\space{}act.$\times$\space{}[Var.]"


gen var1=.
local varl="safe lead_f active_part soc_f health trust civ_f gender1 employed sport"
estimates clear
forval i=1/10 {
	local vv: word `i' of `varl'
	replace var1=`vv'
	replace x_y1_1=x_y1*`vv'
	replace x21_1=x21*`vv'
	reg x2 x_y1 x21 x_y1_1 x21_1 var1 if include_data==1&period==1, clu(subj_id)
	est sto m`i'
}

local tokeep="x_y1_1 x21_1"

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 using "`fname'", label mtitle(Risk Lead Active Social Health Trust Civic Male Employed Sports) keep(`tokeep') stats(r2 N) nogap compress se star(* 0.10 ** 0.05 *** 0.01) replace note("`note'") nonum

*********************************************************************************************************************************************************************************************
********** TABLE: Determinants of second-period action.
*********************************************************************************************************************************************************************************************


local fname="`path'table_new_2.tex"
local note="OLS regressions. Dependent variable is individual's second-period action. Standard errors clustered by subject. Other covariates not shown."

est clear
reg x2 x_y1 x21 x_y1_lead x21_lead x_y1_own x21_own x_y1_safe x21_safe x_y1_trust x21_trust own_f lead_f safe trust if include_data==1, clu(subj_id)
est sto m1
reg x2 x_y1 x21 x_y1_active x21_active x_y1_own x21_own x_y1_safe x21_safe x_y1_trust x21_trust active_part own_f  safe trust if include_data==1, clu(subj_id)
est sto m2
reg x2 x_y1 x21 x_y1_lead x21_lead x_y1_own x21_own x_y1_safe x21_safe x_y1_trust x21_trust x_y1_cog x21_cog own_f lead_f safe trust cog if include_data==1, clu(subj_id)
est sto m3
reg x2 x_y1 x21 x_y1_active x21_active x_y1_own x21_own x_y1_safe x21_safe x_y1_trust x21_trust x_y1_cog x21_cog active_part own_f safe trust cog if include_data==1, clu(subj_id)
est sto m4
reg x2 x_y1 x21 x_y1_afpos x21_afpos x_y1_afneg x21_afneg  afpos afneg if include_data==1, clu(subj_id)
est sto m5
reg x2 x_y1 x21 x_y1_afpos x21_afpos x_y1_afneg x21_afneg x_y1_cog x21_cog cog afpos afneg if include_data==1, clu(subj_id)
est sto m6
reg x2 x_y1 x21 x_y1_own x21_own x_y1_afpos x21_afpos x_y1_afneg x21_afneg x_y1_cog x21_cog cog afpos own_f afneg if include_data==1, clu(subj_id)
est sto m7

local vlist="x_y1_own x21_own x_y1_lead x21_lead x_y1_active x21_active x_y1_safe x21_safe x_y1_trust x21_trust x_y1_afpos x21_afpos x_y1_afneg x21_afneg  x_y1_cog x21_cog"
esttab m1 m2 m3 m4 m5 m6 m7 using "`fname'", label mtitle(1 2 3 4 5 6 7) keep(`vlist') order(`vlist') stats(r2 N) nogap compress se star(* 0.10 ** 0.05 *** 0.01) replace note("`note'") nonum





****************************************************************************************************************************************
*** Table: The gender-specific effects of treatment, subjective status, and objective status on second-period action.
****************************************************************************************************************************************
local varlist "x_y1_1m x21_1m var1 var1_m"
local nn: word count of `varlist'
local nn=`nn'-1
forval i=1/`nn' {
local vname: word `i' of `varlist'
capture confirm variable `vname'
if !_rc {
                       drop `vname'
}
}

//
local fname="`path'table_new_x2m.tex"
local note="OLS regressions. Dependent variable is individual's second-period action. Standard errors clustered by subject. Other covariates not shown. [Var.] is as follows. Column 1: Dictator (0 or 1); Column 2: Subjective status index; Column3: Peer's subjective status index; Column 4: Income category (1-6); Column 5: Expecteded change in well-being (1-5); Column 6: Retrospective change in well-being (1-5); Column 7: Both parents have higher education (0 or 1); Column 8: has an older sibling (0 or 1); Column 9: Has a younger sibling (0 or 1); Column 10: In only child (0 or 1)"
est clear
gen x_y1_1m=.
gen x21_1m=.
lab var x_y1_1m "Priv.\space{}sig.$\times$\space{}[Var.]\space{}$\times$\space{}Male"
lab var x21_1m "Part.\space{}act.$\times$\space{}[Var.]\space{}$\times$\space{}Male"

gen var1=.
gen var1_m=.
local varl="DG_role own_f ownothers_f wealth1 expectedwealthchange1 wealthchange1 parents_edhi fam_ys fam_os fam_oc"
estimates clear
forval i=1/10 {
	local vv: word `i' of `varl'
	replace var1=`vv'
	replace var1_m=`vv'*gender1
	replace x_y1_1=x_y1*`vv'
	replace x21_1=x21*`vv'
	replace x_y1_1m=x_y1*`vv'*gender1
	replace x21_1m=x21*`vv'*gender1
	reg x2 x_y1 x21 x_y1_1 x21_1 x_y1_1m x21_1m x_y1_male x21_male var1 gender1 var1_m if include_data==1, clu(subj_id)
	est sto m`i'
}

local tokeep="x_y1_1m x21_1m"

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 using "`fname'", label mtitle("Dictator" "Subj-own" "Subj-other" "Income" "Inc. (exp)" "Inc (retr)" "Parental ed." "Yo.  sib." "Old. sib." "Only child") keep(`tokeep') stats(r2 N) nogap compress se star(* 0.10 ** 0.05 *** 0.01) replace note("`note'") nonum


****************************************************************************************************************************************
*** Table: The gender-specific effects of other covariates on second-period action.
****************************************************************************************************************************************
local varlist "x_y1_1m x21_1m var1 var1_m"
local nn: word count of `varlist'
local nn=`nn'-1
forval i=1/`nn' {
local vname: word `i' of `varlist'
capture confirm variable `vname'
if !_rc {
                       drop `vname'
}
}

//
local fname="`path'table_new_x2_1m.tex"
local note="OLS regressions. Dependent variable is individual's first-period action. Standard errors clustered by subject. Other covariates not shown. [Var.] is as follows. Column 1: Fraction of safe choices on the risk task; Column 2: Leadership skills; Column 3: Active in a sports/environmental/professional organization, labor union, or political party (0 or 1); Column 3: Sociablity index; Column 5: Subjective health (1-10); Column 6: Interpersonal Trust (0 or 1); Column 7: Civicness index; Column 8: Employed part-time or full-time (0 or 1); Column 9: Has a sports degree (0 or 1)"
est clear
gen x_y1_1m=.
gen x21_1m=.
lab var x_y1_1m "Priv.\space{}sig.$\times$\space{}[Var.]\space{}$\times$\space{}Male"
lab var x21_1m "Part.\space{}act.$\times$\space{}[Var.]\space{}$\times$\space{}Male"


gen var1=.
gen var1_m=.
local varl="safe lead_f active_part soc_f health trust civ_f employed sport"
estimates clear
forval i=1/9 {
	local vv: word `i' of `varl'
	replace var1=`vv'
	replace var1_m=`vv'*gender1
	replace x_y1_1=x_y1*`vv'
	replace x21_1=x21*`vv'
	replace x_y1_1m=x_y1*`vv'*gender1
	replace x21_1m=x21*`vv'*gender1
	reg x2 x_y1 x21 x_y1_1 x21_1 x_y1_1m x21_1m x_y1_male x21_male var1 gender1 var1_m if include_data==1, clu(subj_id)
	est sto m`i'
}

local tokeep="x_y1_1m x21_1m"

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 using "`fname'", label mtitle(Risk Lead Active Social Health Trust Civic Employed Sports) keep(`tokeep') stats(r2 N) nogap compress se star(* 0.10 ** 0.05 *** 0.01) replace note("`note'") nonum




ttest own_f if include_data==1&period==1, by(DG_role)
tabstat DG_donate_av if period==1&include_data==1&DG_role==1, stats(mean sd)
gen x_y1_donav=x_y1* DG_donate_av
gen x21_donav=x21* DG_donate_av
reg x2 x_y1 x21 x_y1_donav x21_donav  DG_donate_av if include_data==1&DG_role==0, clu(subj_id)



*************************************************
* Priming effects
*************************************************



ttest DG_donate_av if period==1&DG_type==1&include_data==1, by(end)


ttest civ_f if period==1&include_data==1, by(end)
ttest wealth1 if period==1&include_data==1, by(end)
ranksum wealth1 if period==1&include_data==1, by(end)
ranksum wealthchange1 if period==1&include_data==1, by(end)
ranksum expectedwealthchange1 if period==1&include_data==1, by(end)
ranksum health if period==1&include_data==1, by(end)
ttest health if period==1&include_data==1, by(end)
cc end trust if period==1&include_data==1, exact


ttest lead_f if period==1&include_data==1, by(end)
ttest own_f if period==1&include_data==1, by(end)
ttest ownothers_f if period==1&include_data==1, by(end)
ttest safe if period==1&include_data==1, by(end)
cc end sport if period==1&include_data==1, exact
cc end active_part if period==1&include_data==1, exact
cc end employed if period==1&include_data==1, exact
ranksum cog if period==1&include_data==1, by(end)
ttest soc_f if period==1&include_data==1, by(end)

local fname="`path'table_priming.tex"
local note="OLS regressions. Dependent variable is individual's second-period action. Standard errors clustered by subject. Other covariates not shown."

estimates clear
reg x2 x_y1 x21 x_y1_end x21_end end if include_data==1, clu(subj_id)
est sto m1
reg x2 x_y1 x21 x_y1_end x21_end end if period==1&include_data==1, clu(subj_id)
est sto m2
esttab m1 m2 using "`fname'", label mtitle("All rounds" "Round 1") keep(x_y1_end x21_end) stats(r2 N) nogap compress se star(* 0.10 ** 0.05 *** 0.01) replace note("`note'") nonum
