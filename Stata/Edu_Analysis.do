
********************************************************************************
*                                                                              *
*                    PSPS Analysis: Educational Findings                       *
*                                                                              *
*                Summary: This do file displays the quantitative               *
*				analysis conducted related to educational variables            *
*                                                                              *
*                                                                              *
*   			    	Date of last change: 04/17/2026                        *
*                                                                              *
*                                                                              *
********************************************************************************

********************************************************************************
*									CONTENTS                                   * 
********************************************************************************
*                                                                              *                                                                    
*      1. Prepare workspace  												   *
*         1.1 Household size 												   *  
*         1.2 Public Vs. Private                                               *                                                             
*                                                                              *                                                             
*                                                                              *                                                             
*      2. Educational Analysis       									       *
*         2.1 School Attendance									   			   *
*         2.2 Level of Educational Attainment								   *
*         2.3 Cost of Education                                                *
*  																			   *	
*      3. Regional Analysis                                                    *
*         3.1 School Attendance: By Province                                   *
*         3.2 Level of Educational Attainment: By Province                     *
*         3.3 Cost of Education: By Province                                   *
*                                                                              *
*      4. Fourp Analysis                                                       *
*                                                                              *
*      5. Gender                                                               *
*                                                                              *
*                                                                              *
********************************************************************************
                                                                    													
********************************************************************************
*****  Prepare Workspace   *****************************************************
********************************************************************************

// setting up working directory
use "/Users/sarahnorman/Desktop/PSPS Exploration/1_roster_edu_mig copy.dta", clear
merge m:1 brgy_code fourp_status using "/Users/sarahnorman/Desktop/PSPS Exploration/design_weights.dta", keep (1 3)


////////////////////////   1.1 Household size    ///////////////////////////////

// Average household size 
* 5.76 (5 - 6 ppl per household)
summarize count_res_members

// Number of children
* average is ~1 kid per household between 9 - 15
tab r_kids_old

* average is ~1 kid per household between 3 - 8
tab r_kids_yng

* average is ~0 kid per household between <3
tab r_kids_baby

///////////////////////   1.2 Public Vs. Private    ////////////////////////////

//Public
* 99.14%
tab edu_sch_type

********************************************************************************
*****  Educational Analysis    *************************************************
********************************************************************************

///////////////////////   2.1 School Attendance    /////////////////////////////

// Histogram of if attended primary school * Weighted
* NO 9% YES 91%

graph bar (percent) [pw=norm_hh_weight] if age_est >= 6, ///
    over(ed_presch) ///
    blabel(bar, format(%9.0f) position(outside))
	
// Histogram of if attended school (those past schooling age) * Weighted
* NO 1% Yes 99%
graph bar (percent) [pw=norm_hh_weight] if age_est >= 18, ///
    over(edu_attend) ///
    blabel(bar, format(%9.0f) position(outside))

count edu_attend == 1 & edu_current == 1

graph bar (percent) if age_est >= 25 & age_est <= 44 , ///
    over(edu_completed) ///
    blabel(bar, format(%9.0f) position(outside)) 
	
*cohort (1990s) - k10
	
// Dropouts (those of school-age, have attended school in past, but are not now)
*tab dropout: 1,678 () have dropped out
gen dropout = (edu_attend == 1 & edu_current == 0) ///
    if age_est >= 6 & age_est <= 18

    * there is high access to basic education with notable drop out size *
	
////////////////   2.2 Level of educational attainment    //////////////////////

// Higher Education
* 9.20% stopped formal schooling after elementary school
* 5,899 ~14% started but did not complete high school (k-10 - k-12 systems Grades 7–9 only)
* 9.65% are college graduates --> indicating limited teriary education

tab edu_completed if age_est >= 25 & age_est <= 44 
*cohort (1990s) - k10


//////////////////////   2.3 Costs of education    /////////////////////////////

*ssc install winsor2

// Mean Costs
* ~16,014 PHP

foreach var in edu_exp_1 edu_exp_2 edu_exp_3 edu_exp_4 ///
               edu_exp_5 edu_exp_6 edu_exp_7 edu_exp_8 {

    quietly summarize `var', detail
    
	*removes outliers
    local p1  = r(p1)
    local p99 = r(p99)

    quietly summarize `var' if `var' >= `p1' & `var' <= `p99'
    
    local m_`var' = r(mean)
}

display `m_edu_exp_1' + `m_edu_exp_2' + `m_edu_exp_3' + ///
        `m_edu_exp_4' + `m_edu_exp_5' + `m_edu_exp_6' + ///
        `m_edu_exp_7' + `m_edu_exp_8'

// expressed money as barrier to education
* 9.49% not enrolled bc lack money
tab edu_reasons1_NoMoney
* 13.32% never attended bc lack money
tab edu_reasons2_NoMoney
* 7.85 are absent bc lack money
tab edu_reasons3_NoMoney

********************************************************************************
*****  Regional Analysis    ****************************************************
********************************************************************************

//////////////////   3.1 School Attendance: By Province    /////////////////////

// Histogram of if attended school (those past schooling age)
* Above 98% for all
graph bar (mean) edu_attend if age_est >= 25 & age_est <= 44, ///
    over(province, label(angle(45))) ///
    blabel(bar, format(%9.3f) position(outside)) ///
    ytitle("Proportion who attended school")

// Percent dropout by province
* Highest: Antique 11.49% | Lowest: Aklan 3.45%
tab province dropout, row

///////////   3.2 Level of educational attainment: By Province    //////////////

// Higher Education
* stopped formal schooling after elementary school: Iloilo 637 | AKLAN 150
* Started but did not complete high school (grades 7 - 10)
	* Iloilo 1,172 | Aklan 245
	* BUT divided by province totals: Negros Occidental 15.45% ALL ~14% - ~16%
* college graduation rates: Iloilo 15.1% | Negros Occidental 9.2%

tab province edu_completed if age_est >= 25 & age_est <= 44 

/////////////////   3.3 Costs of education: By Province    /////////////////////

// Mean Costs
* Aklan 17,384.77 | Negros Occidental 13,475.07
capture frame drop costtable
frame create costtable str25 province double total_cost

levelsof province, local(provs)

foreach p of local provs {
    
    local total = 0
    
    foreach var in edu_exp_1 edu_exp_2 edu_exp_3 edu_exp_4 ///
                   edu_exp_5 edu_exp_6 edu_exp_7 edu_exp_8 {
        
        quietly summarize `var' if province == "`p'", detail
        local p1 = r(p1)
        local p99 = r(p99)

        quietly summarize `var' if province == "`p'" & `var' >= `p1' & `var' <= `p99'
        local total = `total' + r(mean)
    }

    frame post costtable ("`p'") (`total')
}

frame costtable: format total_cost %15.2fc
frame costtable: list province total_cost, noobs clean

********************************************************************************
*****  Fourp Analysis   ********************************************************
********************************************************************************

// Histogram of if attended school (those past schooling age)
* No significant difference
graph bar (mean) edu_attend if age_est >= 25 & age_est <= 44, ///
    over(fourp_status, label(angle(45))) ///
    blabel(bar, format(%9.3f) position(outside)) ///
    ytitle("Proportion who attended school")

// Percent dropout by fourp
/* Dropout rates are higher among 4Ps beneficiaries (8.83%) than 
non-beneficiaries (6.60%). Suggests the program reaches more educationally 
vulnerable populations rather than fully offsetting dropout risk. */

tab fourp_status dropout, row

* Status does not fully offset burden of money
tab fourp_status edu_reasons1_NoMoney, row
tab fourp_status edu_reasons2_NoMoney, row
tab fourp_status edu_reasons3_NoMoney, row

  * Difficult to measure if fourp_status is improving educational outcomes  *
  
********************************************************************************
*****  Gender   ****************************************************************
********************************************************************************

// Histogram of if attended school (those past schooling age)

graph bar (mean) edu_attend if age_est >= 25 & age_est <= 44 ///
    & inlist(gender, 1, 2), ///
    over(gender, label(angle(45))) ///
    blabel(bar, format(%9.3f) position(outside)) ///
    ytitle("Proportion who attended school")
	
tab gender dropout, row

/* merge m:1 brgy_code fourp_status usnig "${weights}", keep (1 3) nongen */

********************************************************************************
*****  By Cohort Analysis    ***************************************************
********************************************************************************

///////////////////////   2.1 School Attendance    /////////////////////////////

// Creatig birth_year variable
gen birth_year = 2023 - age_est

gen cohort5 = floor(birth_year/5)*5

tostring cohort5, gen(cohort5_str)

gen cohort5_lbl = cohort5_str + "-" + ///
    string(cohort5 + 4)
	
// Histogram of if attended school (those past schooling age) * Weighted
* NO 1% Yes 99%
graph bar (percent) [pw=norm_hh_weight] if age_est >= 18, ///
    over(edu_attend) ///
    over(cohort5, sort(1)) ///
    blabel(bar, format(%9.0f) position(outside))
	
graph bar (mean) edu_attend [pw=norm_design_weight] if age_est >= 18, ///
    over(cohort5, sort(1)) ///
    blabel(bar, format(%9.2f) position(outside))
	
********************************************************************************
*****  Key Findings    *********************************************************
********************************************************************************

// Creatig birth_year variable
gen birth_year = 2023 - age_est

gen cohort10 = floor(birth_year/10)*10

tostring cohort10, gen(cohort10_str)

gen cohort10_lbl = cohort10_str + "s"
	
/* Goal: classify individuals into education systems (K–10 vs K–12) using birth year

Assumptions:
- Students start Grade 1 at ~age 6
- Old system (K–10) ends at ~age 16
- New system (K–12) ends at ~age 18

Approximate graduation year (old system):
- grad_year ≈ birth_year + 16

Key policy timing:
- K–12 introduced: 2012
- K-12 Legally Approved: 2015
- Senior High School (Grades 11–12) begins: 2016

Logic:
- If birth_year + 16 < 2016 → finished before SHS → old system (K–10)
    → birth_year < 2000

- If birth_year ≈ 2000–2001 → in school during transition → mixed exposure

- If birth_year ≥ 2002 → fully exposed to K–12 system

Final classification:
- birth_year ≤ 1999 → K–10 (pre–K–12)
- birth_year 2000–2001 → transition
- birth_year ≥ 2002 → K–12
*/

gen k12_group = .
replace k12_group = 0 if birth_year <= 1999              // K–10 (pre-K–12)
replace k12_group = 2 if inrange(birth_year, 2000, 2001) // transition
replace k12_group = 1 if birth_year >= 2002              // K–12

// Histogram of if attended school (those past schooling age) * Weighted
* NO 2% Yes 98%
graph bar (percent) [pw=norm_design_weight] if age_est >= 18, ///
    over(edu_attend) ///
    blabel(bar, format(%9.0f) position(outside))
	
// Histogram of if attended school (those past schooling age) * Weighted + K12
* No notable change w/k-12
graph bar (mean) edu_attend [pw=norm_design_weight] if age_est >= 18, ///
    over(k12_group) ///
    blabel(bar, format(%9.0f) position(outside))
	
mean edu_attend [pw = norm_design_weight] if age_est >= 18, over(k12_group)

// Highest level of education (those past mandatory schooling age) * Weighted
*  13.63% stopped education after elementary school
svyset [pw=norm_design_weight]
svy: tab edu_completed, percent

// Highest level of education (those past mandatory schooling age) * Weighted + K12
svy: tab edu_completed k12_group if age_est >= 18, column percent

// completed highschool or more (k10)
* 50.6471% 23.66 completed highschool
display 23.66 + 0.3475 + 3.011 + 0.6684 + 1.842 + 3.303 + 2.655 + 2.143 + 1.021 + 0.4918 + 11.16 + 0.3444

// completed highschool or more (k12)
* 61.3343% 36.4% completed highschool
display 36.4 + .7908 + .0348 + .6448 + 12.28 + 6.754 + 3.181 + .466 + .7666 + .0163

// began but did not complete highschool (k10)
* 13.142%
display 3.722 + 5.347 + 4.073

// began but did not complete highschool (k12)
* 27.494
display 3.228 + 3.084 + 3.192 + 10.38 + 7.61

// Attendance by cohorts
svy: proportion edu_completed if age_est >= 18 & cohort10 >= 1930, over(cohort10), column percent

// Dropouts (those of school-age, have attended school in past, but are not now)
*tab dropout: 1,678 () have dropped out
gen dropout = (edu_attend == 1 & edu_current == 0) ///
    if age_est >= 6 & age_est <= 18
	
svy: tab dropout

svy: mean ///
    edu_reasons1_NoMoney ///
    edu_reasons1_Job ///
    edu_reasons1_Marriage ///
    edu_reasons1_Disability ///
    edu_reasons1_Sick ///
    edu_reasons1_HomeObligation ///
    edu_reasons1_NoSchool_Teacher ///
    edu_reasons1_NoTime ///
    edu_reasons1_ParentsDeath ///
    edu_reasons1_ParentsSep ///
    edu_reasons1_Pregnant ///
    if age_est > 6 & age_est < 25
	
// expressed money as barrier to education
* not enrolled bc lack money .1113706
svy: mean edu_reasons1_NoMoney
* never attended bc lack money .1308398
svy: mean edu_reasons2_NoMoney
* absent bc lack money .0795646
svy: mean edu_reasons3_NoMoney

// Mean Costs elementary

foreach var in edu_exp_1 edu_exp_2 edu_exp_3 edu_exp_4 ///
               edu_exp_5 edu_exp_6 edu_exp_7 edu_exp_8 {

    quietly summarize `var' if age_est >= 6 & age_est <= 12, detail
    
    * removes outliers within age 6–12
    local p1  = r(p1)
    local p99 = r(p99)

    quietly summarize `var' if `var' >= `p1' & `var' <= `p99' ///
                              & age_est >= 6 & age_est <= 12
    
    local m_`var' = r(mean)
}

display `m_edu_exp_1' + `m_edu_exp_2' + `m_edu_exp_3' + ///
        `m_edu_exp_4' + `m_edu_exp_5' + `m_edu_exp_6' + ///
        `m_edu_exp_7' + `m_edu_exp_8'
	
// Mean Costs Highschool
foreach var in edu_exp_1 edu_exp_2 edu_exp_3 edu_exp_4 ///
               edu_exp_5 edu_exp_6 edu_exp_7 edu_exp_8 {

    quietly summarize `var' if age_est >= 13 & age_est <= 18, detail
    
    * removes outliers within age 6–12
    local p1  = r(p1)
    local p99 = r(p99)

    quietly summarize `var' if `var' >= `p1' & `var' <= `p99' ///
                              & age_est >= 13 & age_est <= 18
    
    local m_`var' = r(mean)
}

display `m_edu_exp_1' + `m_edu_exp_2' + `m_edu_exp_3' + ///
        `m_edu_exp_4' + `m_edu_exp_5' + `m_edu_exp_6' + ///
        `m_edu_exp_7' + `m_edu_exp_8'
		
// Mean Costs Junior Highschool
foreach var in edu_exp_1 edu_exp_2 edu_exp_3 edu_exp_4 ///
               edu_exp_5 edu_exp_6 edu_exp_7 edu_exp_8 {

    quietly summarize `var' if age_est >= 13 & age_est <= 16, detail
    
    local p1  = r(p1)
    local p99 = r(p99)

    quietly summarize `var' if `var' >= `p1' & `var' <= `p99' ///
                              & age_est >= 13 & age_est <= 16
    
    local m_`var' = r(mean)
}

display `m_edu_exp_1' + `m_edu_exp_2' + `m_edu_exp_3' + ///
        `m_edu_exp_4' + `m_edu_exp_5' + `m_edu_exp_6' + ///
        `m_edu_exp_7' + `m_edu_exp_8'
		
// Mean Costs Senior Highschool
foreach var in edu_exp_1 edu_exp_2 edu_exp_3 edu_exp_4 ///
               edu_exp_5 edu_exp_6 edu_exp_7 edu_exp_8 {

    quietly summarize `var' if age_est >= 17 & age_est <= 18, detail
    
    local p1  = r(p1)
    local p99 = r(p99)

    quietly summarize `var' if `var' >= `p1' & `var' <= `p99' ///
                              & age_est >= 17 & age_est <= 18
    
    local m_`var' = r(mean)
}

display `m_edu_exp_1' + `m_edu_exp_2' + `m_edu_exp_3' + ///
        `m_edu_exp_4' + `m_edu_exp_5' + `m_edu_exp_6' + ///
        `m_edu_exp_7' + `m_edu_exp_8'
	
use "/Users/sarahnorman/Desktop/PSPS Exploration/edu_policy_knowledge.dta"

//////////

tab know_tes_1

*. 68.8% say they wouldn't take it because they think they won't be selected / low chance
mean avail_1_b_complex_process avail_1_b_expect_discontinue avail_1_b_has_funds avail_1_b_inelig_finNeed avail_1_b_low_priority avail_1_b_noInterest_HE avail_1_b_noInterest_elig_insti avail_1_b_oth_needier avail_1_b_other_support

merge m:1 brgy_code fourp_status using "/Users/sarahnorman/Desktop/PSPS Exploration/design_weights.dta", keep (1 3)

//////////

// not aware of child labor laws
svy: tab ch_lab_2a




