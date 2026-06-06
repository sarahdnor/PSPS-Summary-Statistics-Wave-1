********************************************************************************
*                                                                              *
*                    PSPS Analysis: Salient Educational Findings               *
*                                                                              *
*                Summary: This do file displays the code used to 			   *
*		   obtain quantitative findings related to educational variables       *
*                                                                              *
*                                                                              *
*   			    	Date of last change: 04/16/2026                        *
*                                                                              *
*                                                                              *
********************************************************************************

********************************************************************************
*									CONTENTS                                   * 
********************************************************************************
*                                                                              *  
*                                                                              *                                                                    
*      [Dataset]: 1_roster_edu_mig copy.dta   								   *    
*                                 											   *                                											 *   							                                                *   
*      1. Prepare workspace  												   *
*         1.1 setting up working directory 								       *  
*         1.2 creating relevant variables                                      *   
*                                                                              *
*      2. General findings											           * 
*         2.1 School attendance											       * 
*         2.2 Highest level of scholing										   * 
*         2.3 Dropouts											      		   * 
*         2.4 Costs of Education											   * 
* 											 								   * 
*      3. K10 Vs. K12 Findings											       * 
*         3.1 School attendance 											   * 
*         3.2 Highest Level of Schooling 								       * 
*											 								   * 
*	  4. Cohort Findings 													   * 
*         4.1 School attendance   								               * 								   *                                                                              *															 	*                                                                              *      
*     [Dataset]: edu_policy_knowledge.dta									   *   
*                                                                              *
*     1. Prepare workspace  												   *
*         1.1 setting up working directory 								       *  
*                                                                              *
*      2. General findings											           * 
*         2.1 Tertiary Education Subsidy									   *                                                       
*                                                                              *
*                                                                              *
********************************************************************************


********************************************************************************
******************* Dataset: 1_roster_edu_mig copy.dta *************************
********************************************************************************

********************************************************************************
*****  Prepare Workspace   *****************************************************
********************************************************************************

//////////////////   1.1 setting up working directory    ///////////////////////

* loading in dta
use "/Users/sarahnorman/Desktop/PSPS Exploration/1_roster_edu_mig copy.dta", clear
* merging with weights
merge m:1 brgy_code fourp_status using "/Users/sarahnorman/Desktop/PSPS Exploration/design_weights.dta", keep (1 3)

* setting weights
svyset [pw=norm_design_weight]

//////////////////   1.2 creating relevant variables    ////////////////////////

// Creatig birth_year variable

gen birth_year = 2023 - age_est

// Creatig cohort10 variable

gen cohort10 = floor(birth_year/10)*10

tostring cohort10, gen(cohort10_str)

gen cohort10_lbl = cohort10_str + "s"
	
// Creatig k12_group variable

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

********************************************************************************
*****  General Findings   ******************************************************
********************************************************************************

////////////////////////   2.1 school attendance    ////////////////////////////

// Histogram of if ever attended school (those past schooling age) * Weighted
* NO 2% Yes 98%
graph bar (percent) [pw=norm_design_weight] if age_est >= 18, ///
    over(edu_attend) ///
    blabel(bar, format(%9.0f) position(outside))
	
//////////////////   2.2 Highest Level of Schooling    /////////////////////////

// Highest level of education (those past mandatory schooling age) * Weighted
*  13.63% stopped education after elementary school
svyset [pw=norm_design_weight]
svy: tab edu_completed, percent

//////////////////////////////   2.3 Dropouts    ///////////////////////////////

// Dropouts (those of school-age, have attended school in past, but are not now)
*tab dropout: 1,678 () have dropped out
gen dropout = (edu_attend == 1 & edu_current == 0) ///
    if age_est >= 6 & age_est <= 18
	
svy: tab dropout

// Reasons not currently enrolled
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

///////////////////////   2.4 Costs of Education    ////////////////////////////

// Mean Costs elementary
foreach var in edu_exp_1 edu_exp_2 edu_exp_3 edu_exp_4 ///
               edu_exp_5 edu_exp_6 edu_exp_7 edu_exp_8 {

    quietly summarize `var' [aw=norm_design_weight] if age_est >= 6 & age_est <= 12, detail
    
    * removes outliers within age 6–12
    local p1  = r(p1)
    local p99 = r(p99)

    quietly summarize `var' [aw=norm_design_weight] if `var' >= `p1' & `var' <= `p99' ///
                              & age_est >= 6 & age_est <= 12
    
    local m_`var' = r(mean)
}

display `m_edu_exp_1' + `m_edu_exp_2' + `m_edu_exp_3' + ///
        `m_edu_exp_4' + `m_edu_exp_5' + `m_edu_exp_6' + ///
        `m_edu_exp_7' + `m_edu_exp_8'
	
// Mean Costs Highschool
foreach var in edu_exp_1 edu_exp_2 edu_exp_3 edu_exp_4 ///
               edu_exp_5 edu_exp_6 edu_exp_7 edu_exp_8 {

    quietly summarize `var' [aw=norm_design_weight] if age_est >= 13 & age_est <= 18, detail
    
    * removes outliers within age 6–12
    local p1  = r(p1)
    local p99 = r(p99)

    quietly summarize `var' [aw=norm_design_weight] if `var' >= `p1' & `var' <= `p99' ///
                              & age_est >= 13 & age_est <= 18
    
    local m_`var' = r(mean)
}

display `m_edu_exp_1' + `m_edu_exp_2' + `m_edu_exp_3' + ///
        `m_edu_exp_4' + `m_edu_exp_5' + `m_edu_exp_6' + ///
        `m_edu_exp_7' + `m_edu_exp_8'
		
// Mean Costs Junior Highschool
foreach var in edu_exp_1 edu_exp_2 edu_exp_3 edu_exp_4 ///
               edu_exp_5 edu_exp_6 edu_exp_7 edu_exp_8 {

    quietly summarize `var' [aw=norm_design_weight] if age_est >= 13 & age_est <= 16, detail
    
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

    quietly summarize `var' [aw=norm_design_weight] if age_est >= 17 & age_est <= 18, detail
    
    local p1  = r(p1)
    local p99 = r(p99)

    quietly summarize `var' if `var' >= `p1' & `var' <= `p99' ///
                              & age_est >= 17 & age_est <= 18
    
    local m_`var' = r(mean)
}

display `m_edu_exp_1' + `m_edu_exp_2' + `m_edu_exp_3' + ///
        `m_edu_exp_4' + `m_edu_exp_5' + `m_edu_exp_6' + ///
        `m_edu_exp_7' + `m_edu_exp_8'
	
********************************************************************************
*****  K10 Vs. K12 Findings   **************************************************
********************************************************************************	
	
////////////////////////   3.1 school attendance    ////////////////////////////
	
// Histogram of if attended school (those past schooling age) * Weighted + K12
* No notable change w/k-12
graph bar (mean) edu_attend [pw=norm_design_weight] if age_est >= 18, ///
    over(k12_group) ///
    blabel(bar, format(%9.0f) position(outside))
	
mean edu_attend [pw = norm_design_weight] if age_est >= 18, over(k12_group)

//////////////////   3.2 Highest Level of Schooling    /////////////////////////
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

********************************************************************************
*****  Cohort Findings   *******************************************************
********************************************************************************	

////////////////////////   3.1 school attendance    ////////////////////////////

// Attendance by cohorts
svy: proportion edu_completed if age_est >= 18 & cohort10 >= 1930, over(cohort10), column percent

********************************************************************************
******************* Dataset: edu_policy_knowledge.dta **************************
********************************************************************************

//////////////////   1.1 setting up working directory    ///////////////////////

* loading in dta
use "/Users/sarahnorman/Desktop/PSPS Exploration/edu_policy_knowledge.dta", clear

* merging with weights
merge m:1 brgy_code fourp_status using "/Users/sarahnorman/Desktop/PSPS Exploration/design_weights.dta", keep (1 3)

* setting weights
svyset [pw=norm_design_weight]

//////////////////   2.1 tertiary education subdiy    //////////////////////////

// awareness of subsidy
svy: tab know_tes_1

// why no take up
*. 68.8% say they wouldn't take it because they think they won't be selected / low chance
svy: mean avail_1_b_complex_process avail_1_b_expect_discontinue ///
	avail_1_b_has_funds avail_1_b_inelig_finNeed avail_1_b_low_priority ///
	avail_1_b_noInterest_HE avail_1_b_noInterest_elig_insti ///
	avail_1_b_oth_needier avail_1_b_other_support

svy: count if fourp_status == 1 & edu_current == 0
*19,219
