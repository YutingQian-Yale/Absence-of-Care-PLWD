/***********************************************************************
                Disability, Dementia and LTC (PData)
************************************************************************

    *Title: Absence of Care Project
    *Aim of This Do-file: Preparing Data
    *Authors: Zhuoer Lin, Yuting Qian
    *Date Edited: Sept 2024

***********************************************************************/

clear all
set more off
set maxvar 100000

//* install the following package if not yet
//   ssc install grc1leg2
//   ssc install moremata
//   ssc install valuesof
//   ssc install distinct 
//   net install dm79, from("http://www.stata.com/stb/stb56")
//   net install asdoc, from("http://fintechprofessor.com")
//   net install gr0070, from("http://www.stata-journal.com/software/sj17-3/")  
//   net install scheme-modern, from("https://raw.githubusercontent.com/mdroste/stata-scheme-modern/master/")
set scheme modern

*-----------------------------------------------------------------------
* Note: may need to redifine the directory with your own
*-----------------------------------------------------------------------
* if already defined in "Master_Do_File_Run_ALL.do", no need to define again
//cd "$master_directory" 

* if not, need to redefine the master directory in this do file
//global master_directory "/Users/zlin01/Library/Mobile Documents/com~apple~CloudDocs/Documents/Aging Data/HRS Sister Studies" 
//cd "$master_directory" 
. pwd


*-----------------------------------------------------------------------
*                         Define file locations
*-----------------------------------------------------------------------

*** define output folders ***
global root              "Dementia Disability and LTC"
global data              "${root}/data"
global rawdata           "${root}/data/rawdata"
global code              "${root}/code"
global temp              "${root}/temp"
global result            "${root}/result"
global figure            "$result/figure"
global table             "$result/table"

// Create these folders if they do not exist
cap mkdir "$root"
cap mkdir "$data"
cap mkdir "$rawdata"
cap mkdir "$code"
cap mkdir "$result"
cap mkdir "$table"
cap mkdir "$temp"
cap mkdir "$figure"


cap mkdir "$figure/main"
cap mkdir "$figure/robustness"
cap mkdir "$figure/descriptive"
cap mkdir "$figure/supplementary"

cap mkdir "$table/main"
cap mkdir "$table/robustness"
cap mkdir "$table/descriptive"
cap mkdir "$table/supplementary"

cap mkdir "$temp/robustness"
cap mkdir "$temp/supplementary"


*** define data files *** 

* HRS 
global RandHRS           "$rawdata/HRS/randhrs1992_2018v2/randhrs1992_2018v2.dta"
global H_HRS             "$rawdata/HRS/Harmonized HRS/H_HRS_c.dta"

* ELSA 
global H_ELSA            "$rawdata/ELSA/Harmonized ELSA/H_ELSA_g2.dta"

* SHARE
global H_SHARE           "$rawdata/SHARE/Harmonized SHARE/H_SHARE_e3.dta"

* CHARLS
global H_CHARLS          "$rawdata/CHARLS/Harmonized CHARLS/H_CHARLS_D/H_CHARLS_D_Data.dta"


*-----------------------------------------------------------------------
*                         Preparing Data (US)
*-----------------------------------------------------------------------
* US 
* load Rand harmonized HRS Data
use  hhidpn hhid pn r*wtresp r*iwendy r*nhmliv ///
     ragender raedyrs /// gender education 
     r*agey_m /// age 
     r*mstat /// marital status
     h*child /// number of living children
     r*tr20 r*ser7 r*bwc20 r*dy r*mo r*yr r*dw /// cognition
     r*adl6a r*adl5a r*iadl5a /// ADL/IADL
     r*walkra r*dressa r*batha r*eata r*beda r*toilta /// Any ADL difficulties
     r*walkrh r*dressh r*bathh r*eath r*bedh r*toilth /// ADL help 
     r*mealsa r*shopa r*phonea r*medsa r*moneya /// Any IADL difficulties
     h*hhres ///
     using "$RandHRS", clear

drop r*emstat
     
drop hhid pn 
drop re*

* Reshape wide data to long data
reshape long r@wtresp r@iwendy r@nhmliv ///
             r@agey_m /// age 
             r@mstat /// marital status
             h@child /// number of living children
             r@tr20 r@ser7 r@bwc20 r@dy r@mo r@yr r@dw /// cognition
             r@adl6a r@adl5a r@iadl5a /// ADL/IADL
             r@walkra r@dressa r@batha r@eata r@beda r@toilta /// Any ADL difficulties
             r@walkrh r@dressh r@bathh r@eath r@bedh r@toilth /// ADL help 
             r@mealsa r@shopa r@phonea r@medsa r@moneya /// Any IADL difficulties
             h@hhres ///
             , i(hhidpn) j(wave)
             
drop if wave==1 | wave==2     

* Create year indicator
gen year = 1996 + (wave-3)*2
rename riwendy riwy

save "$temp/w_H_data", replace


*-----------------------------------------------------------------------
* Merge cleaned measures from USC Harmonized Data
use  hhidpn raeducl ///
     r*mealhlp r*shophlp r*phonehlp r*medhlp r*moneyhlp /// IADL help item
     r*racany r*racaany r*rascare r*raccare r*rarcare r*rafcare r*rafaany r*rapfcare r*raufcare r*ahlppdn /// ADL help
     r*ricany r*ricaany r*riscare r*riccare r*rircare r*rifcare r*rifaany r*ripfcare r*riufcare r*ihlppdn /// IADL help
     r*rcany r*rcaany r*rscare r*rccare r*rrcare r*rfcare r*rfaany r*rpfcare r*rufcare r*hlppdn /// ADL/IADL help
     r*lideal r*lexcl r*lstsf r*limptt r*lsatsc r*lideal3 r*lexcl3 r*lstsf3 r*limptt3 r*lsatsc3 /// satisfaction with life scale
     using "$H_HRS", clear
     


reshape long r@mealhlp r@shophlp r@phonehlp r@medhlp r@moneyhlp /// IADL help item
             r@racany r@racaany r@rascare r@raccare r@rarcare r@rafcare r@rafaany r@rapfcare r@raufcare r@ahlppdn /// ADL help
             r@ricany r@ricaany r@riscare r@riccare r@rircare r@rifcare r@rifaany r@ripfcare r@riufcare r@ihlppdn /// IADL help
             r@rcany r@rcaany r@rscare r@rccare r@rrcare r@rfcare r@rfaany r@rpfcare r@rufcare r@hlppdn /// ADL/IADL help
             r@lideal r@lexcl r@lstsf r@limptt r@lsatsc r@lideal3 r@lexcl3 r@lstsf3 r@limptt3 r@lsatsc3 ///
             , i(hhidpn) j(wave)
             
drop if wave==1 | wave==2     

gen year = 1996 + (wave-3)*2
             
save "$temp/w_Harmonized_data", replace

*-----------------------------------------------------------------------
* Merge all cleaned datasets 
use "$rawdata/HRS/LangaWeir2020v2/HRSCog1995_2020_LangaW_Long", clear

keep hhidpn year cogtot27_imp cogfunction dlrc_imp imrc_imp ser7_imp

merge 1:1 hhidpn year using "$temp/w_H_data"

keep if _merge==3
drop _merge



merge 1:1 hhidpn year using "$temp/w_Harmonized_data"

keep if _merge==3
drop _merge

*-----------------------------------------------------------------------
* Only keep those age 50 and older 
rename ragey_m ragey
keep if ragey>=50

* Only keep community-dwelling sample (i.e., drop sample living in nursing home)
keep if rnhmliv ==0 

* Only keep waves 2012-2018 
keep if inrange(year, 2012, 2018)

*-----------------------------------------------------------------------
//* Clean and Construct Variables *//

* Gender 
gen rafemale = ragender - 1                  

* Educatin level
recode raeduc (1=1)(2/3=2)(4/5=3), gen(raeduc_l3) test 
label var raeduc_l3 "Education Level (3 levels)"    
label def raeduc_l3_lbl 1 "Less than high school" ///
                        2 "High school diploma" ///
                        3 "Some college or above"
label val raeduc_l3 raeduc_l3_lbl    

* Marital Status 
recode rmstat (1 3=1)(2=2)(4/6=3)(7=4)(8=5), gen(rmstat_l5) test 
label var rmstat_l5 "Marital Status (5 levels)"    
label def rmstat_l5_lbl 1 "Married or partnered" ///
                        2 "Married, spouse absent" ///
                        3 "Separated/Divorced" ///
                        4 "Widowed" ///
                        5 "Never married" 
label val rmstat_l5 rmstat_l5_lbl    

gen rmstat01 = cond(rmstat_l5==1, 1, 0) if rmstat_l5!=.


*-----------------------------------------------------------------------
* Construct LTC variables for ADL/IADL
rename riadl5a riadlza

recode radl5a (0=0)(1/5=1), gen(radl5a_01) test
recode radl6a (0=0)(1/6=1), gen(radl6a_01) test 
recode riadlza (0=0)(1/5=1), gen(riadlza_01) test 

gen radliadl= radl6a + riadlza  
egen radliadl_01 = rowmax(radl6a_01 riadlza_01)

label var radl5a_01 "Any ADL difficulties (0/1)"
label var radl6a_01 "Any ADL difficulties (0/1)"
label var riadlza_01 "Any IADL difficulties (0/1)"
label var radliadl_01 "Any ADL/IADL difficulties (0/1)"


* Any ADL/IADL help
foreach var of varlist rrcany rrcaany rrfaany{
    replace `var'= .x if radliadl_01==0
}   
foreach var of varlist rracany rracaany rrafaany{
    replace `var'= .x if radl6a_01==0
}
foreach var of varlist rricany rricaany rrifaany{
    replace `var'= .x if riadlza_01==0
}  

* Absence of Any ADL/IADL help (reverse coded)
global varlist0 rrcany rrcaany rrfaany /// informal/formal ADL/IADL help
                rracany rracaany rrafaany /// informal/formal ADL help
                rricany rricaany rrifaany //  informal/formal IADL help
                
foreach var in $varlist0{
    recode `var' (1=0) (0=1), gen(`var'_rev) test
}   

        
*-----------------------------------------------------------------------        
* Cognition and Dementia Assessment (Langa-Weir 27-score)
clonevar rcognition = cogtot27_imp
gen demented = cond(rcognition<=6 & radliadl_01==1, 1, 0) if !mi(rcognition)

* Ever Had Dementia
bys hhidpn: egen ever_demented = max(demented) if !mi(ragey)

* If Demented At Baseline Wave 
bys hhidpn (year): egen base_year = min(cond(demented!=., year, . )) 
bys hhidpn (year): egen demented_base = min(cond(year==base_year, demented, . ))

* Flag Obs from the first occurance of dementia and after
bys hhidpn (year): egen baseD_year = min(cond(demented==1, year, . )) 
bys hhidpn (year): gen demented_baseD = cond(year>=baseD_year, 1, 0) 



*-----------------------------------------------------------------------
* Cognition and Dementia Assessment (25-score, 1.5 SD by Edu Levels)
xtile raedu_l3=raedyrs, n(3) 
gen rcognition_25 = dlrc_imp + imrc_imp + ser7_imp

* Dementia Status
gen demented_25=.
forvalues i = 1/3{
   sum rcognition_25 if wave!=. & raedu_l3==`i', detail 
   replace demented_25 = cond(rcognition_25 < `r(mean)'- 1.5*`r(sd)' & radliadl_01==1, 1, 0) ///
        if !mi(rcognition_25) & wave!=. & raedu_l3==`i'
}
    
* Ever Had Dementia        
bys hhidpn: egen ever_demented_25 = max(demented_25) if !mi(ragey)                    


* If Demented At Baseline Wave 
bys hhidpn (year): egen base_year_25 = min(cond(demented_25!=., year, . )) 
bys hhidpn (year): egen demented_base_25 = min(cond(year==base_year_25, demented_25, . )) 

* Flag Obs from the first occurance of dementia and after
bys hhidpn (year): egen baseD_year_25 = min(cond(demented_25==1, year, . )) 
bys hhidpn (year): gen demented_baseD_25 = cond(year>=baseD_year_25, 1, 0) 

*-----------------------------------------------------------------------            
* Save Cleaned Dataset

save "$data/HRS_Data_L", replace 


 
*-----------------------------------------------------------------------
*                         Preparing Data (England)
*-----------------------------------------------------------------------
* England

* Clean Vars of informal/formal LTC from ELSA rawdata 
global ELSA_rawdata "$rawdata/ELSA/ELSA Wave 0-9 1998-2019/rawdata"

forvalues wv=6/9{
    //local wv =6
    local dataname = cond(`wv'==6, "wave_6_elsa_data_v2", ///
                        cond(`wv'==7, "wave_7_elsa_data", ///
                            cond(`wv'==8, "wave_8_elsa_data_eul_v2", ///
                                                "wave_9_elsa_data_eul_v1")))
                                                
    use "$ELSA_rawdata/`dataname'", clear 
    rename *, lower
    merge 1:1 idauniq using "$H_ELSA", ///
        keepusing(r`wv'dressa r`wv'walkra r`wv'batha r`wv'eata r`wv'beda r`wv'toilta ///
                  r`wv'shopa r`wv'medsa r`wv'housewka r`wv'moneya ///
                  r`wv'rcany r`wv'racany r`wv'ricany)

    gen noprobadl = .
    replace noprobadl = 1 if r`wv'dressa==0 | r`wv'walkra==0 | r`wv'batha==0 | r`wv'eata==0 | r`wv'beda==0 | r`wv'toilta==0
    replace noprobadl = 0 if r`wv'dressa==1 | r`wv'walkra==1 | r`wv'batha==1 | r`wv'eata==1 | r`wv'beda==1 | r`wv'toilta==1

    gen noprobiadl = .
    replace noprobiadl = 1 if r`wv'shopa==0 | r`wv'medsa==0 | r`wv'housewka==0 | r`wv'moneya==0
    replace noprobiadl = 0 if r`wv'shopa==1 | r`wv'medsa==1 | r`wv'housewka==1 | r`wv'moneya==1

    
    * any informal care for ADL/IADL 
    gen r`wv'rcaany = .
    replace r`wv'rcaany = 0 if r`wv'rcany==0 //no help at all for ADLs 
    replace r`wv'rcaany = 0 if (cahinno1==1 | cahinno2 ==1 | cahinno3 == 1 | ///
                                cahinno4==1 | cahinno5 ==1 | cahinno6 == 1) & !mi(r`wv'rcany)
    replace r`wv'rcaany = 1 if (cahinno1==0 | cahinno2 ==0 | cahinno3 == 0 | ///
                                cahinno4==0 | cahinno5 ==0 | cahinno6 == 0) & !mi(r`wv'rcany)
    replace r`wv'rcaany = .n if askinst==1
    replace r`wv'rcaany = .x if noprobadl==1 & noprobiadl==1
    
    * any formal care for ADL/IADL
    gen r`wv'rfaany = .
    replace r`wv'rfaany = 0 if r`wv'rcany==0 //no help at all for ADLs 
    replace r`wv'rfaany = 0 if (cahfmno1==1 | cahfmno2 ==1 | cahfmno3 == 1 | ///
                                cahfmno4==1 | cahfmno5 ==1 | cahfmno6 == 1) & !mi(r`wv'rcany)
    replace r`wv'rfaany = 1 if (cahfmno1==0 | cahfmno2 ==0 | cahfmno3 == 0 | ///
                                cahfmno4==0 | cahfmno5 ==0 | cahfmno6 == 0) & !mi(r`wv'rcany)
    replace r`wv'rfaany = .n if askinst==1
    replace r`wv'rfaany = .x if noprobadl==1 & noprobiadl==1
    
    
    * any informal care for ADLs 
    gen r`wv'racaany = .
    replace r`wv'racaany = 0 if r`wv'racany==0 //no help at all for ADLs 
    replace r`wv'racaany = 0 if (cahinno1==1 | cahinno2 ==1 | cahinno3 == 1) & !mi(r`wv'racany)
    replace r`wv'racaany = 1 if (cahinno1==0 | cahinno2 ==0 | cahinno3 == 0) & !mi(r`wv'racany)
    replace r`wv'racaany = .n if askinst==1
    replace r`wv'racaany = .x if noprobadl==1

    * any formal care for ADLs 
    gen r`wv'rafaany = .
    replace r`wv'rafaany = 0 if r`wv'racany==0 //no help at all for ADLs 
    replace r`wv'rafaany = 0 if (cahfmno1==1 | cahfmno2 ==1 | cahfmno3 == 1) & !mi(r`wv'racany)
    replace r`wv'rafaany = 1 if (cahfmno1==0 | cahfmno2 ==0 | cahfmno3 == 0) & !mi(r`wv'racany)
    replace r`wv'rafaany = .n if askinst==1
    replace r`wv'rafaany = .x if noprobadl==1
                           

    * any informal care for IADLs 
    gen r`wv'ricaany = .
    replace r`wv'ricaany = 0 if r`wv'ricany==0 //no help at all for IADLs 
    replace r`wv'ricaany = 0 if (cahinno4==1 | cahinno5 ==1 | cahinno6 == 1) & !mi(r`wv'ricany)
    replace r`wv'ricaany = 1 if (cahinno4==0 | cahinno5 ==0 | cahinno6 == 0) & !mi(r`wv'ricany)
    replace r`wv'ricaany = .n if askinst==1
    replace r`wv'ricaany = .x if noprobiadl==1

    * any formal care for IADLs 
    gen r`wv'rifaany = .
    replace r`wv'rifaany = 0 if r`wv'ricany==0 //no help at all for IADLs 
    replace r`wv'rifaany = 0 if (cahfmno4==1 | cahfmno5 ==1 | cahfmno6 == 1) & !mi(r`wv'ricany)
    replace r`wv'rifaany = 1 if (cahfmno4==0 | cahfmno5 ==0 | cahfmno6 == 0) & !mi(r`wv'ricany)
    replace r`wv'rifaany = .n if askinst==1
    replace r`wv'rifaany = .x if noprobiadl==1
                           
    keep idauniq r`wv'rcaany r`wv'rfaany r`wv'racaany r`wv'rafaany r`wv'ricaany r`wv'rifaany   
    
    save "$temp/elsa_`wv'_data", replace
}

*-----------------------------------------------------------------------    
* Clean Vars from Harmonized ELSA    
use idauniq r*cwtresp r*lwtresp r*iwindy r*nhmliv r*agey r*mstat ///
    ragender raeduc_e raedyrs_e raeducl raracem ///
    r*tr20 r*ser7 r*bwc20 r*orient ///
    r*child /// 
    r*adla r*adltot_e r*iadlza ///    
    r*walkra r*beda r*batha r*dressa r*eata r*toilta ///
    r*racany r*walkhlp r*dresshlp r*bathehlp r*eathlp r*bedhlp r*toilethlp ///
    r*ricany r*hswkhlp r*shophlp r*medhlp r*moneyhlp ///
    r*rcany ///
    h*hhres ///
    using "$H_ELSA", clear

forvalues wv=6/9{ 
    merge 1:1 idauniq using "$temp/elsa_`wv'_data", nogen
}           
* reshape wide data to long data
reshape long r@cwtresp r@lwtresp r@iwindy r@nhmliv r@agey r@mstat ///
             r@tr20 r@ser7 r@bwc20 r@orient ///
             r@child /// 
             r@adla r@adltot_e r@iadlza ///    
             r@walkra r@beda r@batha r@dressa r@eata r@toilta ///
             r@racany r@walkhlp r@dresshlp r@bathehlp r@eathlp r@bedhlp r@toilethlp ///
             r@ricany r@hswkhlp r@shophlp r@medhlp r@moneyhlp ///
             r@rcany r@rcaany r@rfaany ///
             r@racaany r@rafaany r@ricaany r@rifaany ///
             h@hhres ///
             , i(idauniq) j(wave)

gen year = 2002 + (wave-1)*2

rename riwindy riwy 
             
*-----------------------------------------------------------------------
* Only keep those age 50 and older 
keep if ragey>=50

* Only keep community-dwelling sample (i.e., drop sample living in nursing home)
keep if rnhmliv ==0 

* Only keep waves 2012-2018 
keep if inrange(year, 2012, 2018)
    
*-----------------------------------------------------------------------    
//* Clean and Construct Variables *//  

* Gender
gen rafemale = ragender - 1 

* Education   
rename raedyrs_e raedyrs
  
* Marital Status  
recode rmstat (1 3=1 "1.Married/Partnered")(4/8=0 "0.Non-married"), gen(rmstat01) test

* Number of Children
rename rchild hchild

*-----------------------------------------------------------------------
* Construct LTC variables for ADL/IADL help
rename radla radl5a 
rename radltot_e radl6a

recode radl5a (0=0)(1/5=1), gen(radl5a_01) test
recode radl6a (0=0)(1/6=1), gen(radl6a_01) test 
recode riadlza (0=0)(1/5=1), gen(riadlza_01) test 

gen radliadl= radl6a + riadlza  
egen radliadl_01 = rowmax(radl6a_01 riadlza_01)

label var radl5a_01 "Any ADL difficulties (0/1)"
label var radl6a_01 "Any ADL difficulties (0/1)"
label var riadlza_01 "Any IADL difficulties (0/1)"
label var radliadl_01 "Any ADL/IADL difficulties (0/1)"


* Any ADL/IADL help
foreach var of varlist rrcany rrcaany rrfaany{
    replace `var'= .x if radliadl_01==0
}   
foreach var of varlist rracany rracaany rrafaany{
    replace `var'= .x if radl6a_01==0
}
foreach var of varlist rricany rricaany rrifaany{
    replace `var'= .x if riadlza_01==0
}  

* Absence of Any ADL/IADL help (reverse coded)
global varlist0 rrcany rrcaany rrfaany /// informal/formal ADL/IADL help
                rracany rracaany rrafaany /// informal/formal ADL help
                rricany rricaany rrifaany //  informal/formal IADL help
                
foreach var in $varlist0{
    recode `var' (1=0) (0=1), gen(`var'_rev) test
}   

*-----------------------------------------------------------------------
* Cognition and Dementia Assessment (25-score, 1.5 SD by Edu Levels)
gen rcognition= rtr20 + rser7 
            
xtile raedu_l3=raedyrs, n(3) 

* Dementia Status
gen demented=.
forvalues i = 1/3{
   sum rcognition if wave!=. & raedu_l3==`i', detail 
   replace demented = cond(rcognition < `r(mean)'- 1.5*`r(sd)' & radliadl_01==1, 1, 0) ///
        if !mi(rcognition) & wave!=. & raedu_l3==`i'
}
  
* Ever Demented  
bys idauniq: egen ever_demented = max(demented) if !mi(ragey)

* If Demented At Baseline Wave 
bys idauniq (year): egen base_year = min(cond(demented!=., year, . )) 
bys idauniq (year): egen demented_base = min(cond(year==base_year, demented, . )) 

* Flag Obs from the first occurance of dementia and after
bys idauniq (year): egen baseD_year = min(cond(demented==1, year, . )) 
bys idauniq (year): gen demented_baseD = cond(year>=baseD_year, 1, 0) 
     
clonevar ever_demented_25 = ever_demented   
clonevar base_year_25 = base_year 
clonevar demented_base_25 = demented_base
clonevar demented_baseD_25 = demented_baseD

*----------------------------------------------------------------------- 
* Save Cleaned Dataset           
              
save "$data/ELSA_Data_L", replace        
        
 
 


*-----------------------------------------------------------------------
*                         Preparing Data (Europe)
*----------------------------------------------------------------------- 
* Europe

* Clean and Construct informal/formal care for ADL/ADL from raw data
forvalues wv = 5/7{
    local ver = cond(`wv'==7, 1, 0)
    
    local datafile "$rawdata/SHARE/sharew`wv'_rel7-1-`ver'_ALL_datasets_stata"
    
    use "`datafile'/sharew`wv'_rel7-1-`ver'_sp",clear
    merge 1:1 mergeid using "`datafile'/sharew`wv'_rel7-1-`ver'_cv_r", ///
        keepusing(hhsize) 
    keep if _merge==3
    drop _merge

    merge 1:1 mergeid using "`datafile'/sharew`wv'_rel7-1-`ver'_hc", ///
        keepusing(hc127d1 hc127d2 hc127d3 hc127d4 hc127dno hc029)
    keep if _merge==3
    drop _merge
       
    merge 1:1 mergeid using "$H_SHARE", ///
        keepusing(r`wv'adltota_s r`wv'iadlzaa inw`wv' r`wv'agey r`wv'adltot_s) 
     
    keep if _merge==3
    drop _merge
    
    * Any informal ADL/IADL Care 
    gen rout = .
    replace rout = 0 if sp002_ == 5
    replace rout = 1 if sp002_ == 1
    replace rout = .x if r`wv'adltota_s != 1 & r`wv'iadlzaa != 1

    gen rin = .
    replace rin = 0 if sp020_ == 5 | hhsize == 1
    replace rin = 1 if sp020_ == 1
    replace rin = .x if r`wv'adltota_s != 1 & r`wv'iadlzaa != 1

    gen r`wv'rcaany = .
    replace r`wv'rcaany = 0 if rout == 0 & rin ==0 
    replace r`wv'rcaany = 1 if rout == 1 | rin == 1
    replace r`wv'rcaany = . if mi(rout) | mi(rin)
    label variable r`wv'rcaany "r`wv'rcaany:w`wv' R recv any informal care"


    * Any formal ADL/IADL Care 
    gen rhfm  = .
    replace rhfm =.d  if hc127d1==-1 & hc127d2==-1 & hc127d3==-1 & hc127d4==-1
    replace rhfm =.r  if hc127d1==-2 & hc127d2==-2 & hc127d3==-2 & hc127d4==-2
    replace rhfm =.n if hc029==3
    replace rhfm =0 if !mi(hc127dno)
    replace rhfm =1 if hc127d1==1 | hc127d2==1 | hc127d3==1 | hc127d4==1
    replace rhfm = . if r`wv'adltota_s != 1 & r`wv'iadlzaa != 1 // no difficulties
    lab var rhfm "rhfm_adl:w`wv' R formal ADL care, pr 12 mos"  
    rename rhfm r`wv'rfaany 
    
    * Any informal or formal ADL care 
    gen r`wv'rcany = 0 if r`wv'rcaany ==0 & r`wv'rfaany==0 
    replace r`wv'rcany = 1 if r`wv'rcaany ==1 | r`wv'rfaany==1
    replace r`wv'rcany = . if mi(r`wv'rcaany) | mi(r`wv'rfaany)
    
    keep mergeid r`wv'rcany r`wv'rcaany r`wv'rfaany
    order mergeid r`wv'rcany r`wv'rcaany r`wv'rfaany
    
    save "$temp/SHARE_aiadl_w`wv'", replace   
}    


* ADL and IADL seperately (wave 5, only formal)
forvalues wv = 5/5{
    local ver = cond(`wv'==7, 1, 0)
    
    local datafile "$rawdata/SHARE/sharew`wv'_rel7-1-`ver'_ALL_datasets_stata"
    
    use "`datafile'/sharew`wv'_rel7-1-`ver'_sp",clear
    merge 1:1 mergeid using "`datafile'/sharew`wv'_rel7-1-`ver'_cv_r", ///
        keepusing(hhsize) 
    keep if _merge==3
    drop _merge

    merge 1:1 mergeid using "`datafile'/sharew`wv'_rel7-1-`ver'_hc", ///
        keepusing(hc127d1 hc127d2 hc127d3 hc127d4 hc127dno hc029)
    keep if _merge==3
    drop _merge
       
    merge 1:1 mergeid using "$H_SHARE", ///
        keepusing(r`wv'adltota_s r`wv'iadlzaa inw`wv' r`wv'agey r`wv'adltot_s) 
     
    keep if _merge==3
    drop _merge
    
    * ADL
    * Any formal ADL Care 
    gen rhfm_adl = .
    replace rhfm_adl=.d  if hc127d1==-1 & hc127d2==-1 & hc127d3==-1 & hc127d4==-1
    replace rhfm_adl=.r  if hc127d1==-2 & hc127d2==-2 & hc127d3==-2 & hc127d4==-2
    replace rhfm_adl=.n if hc029==3
    replace rhfm_adl=0 if !mi(hc127dno)
    replace rhfm_adl=1 if hc127d1==1
    replace rhfm_adl = . if r`wv'adltota_s!=1 // no difficulties
    lab var rhfm_adl "rhfm_adl:w`wv' R formal ADL care, pr 12 mos"  
    rename rhfm_adl r`wv'rafaany 
   

    * IADL 
    * Any formal IADL Care
    gen rhfm_iadl = .
    replace rhfm_iadl=.d  if hc127d1==-1 & hc127d2==-1 & hc127d3==-1 & hc127d4==-1
    replace rhfm_iadl=.r  if hc127d1==-2 & hc127d2==-2 & hc127d3==-2 & hc127d4==-2
    replace rhfm_iadl=.n if hc029==3
    replace rhfm_iadl=0 if !mi(hc127dno)
    replace rhfm_iadl=1 if hc127d2==1 | hc127d3==1 | hc127d4==1
    replace rhfm_iadl = . if r`wv'iadlzaa!=1 // no difficulties  
    rename rhfm_iadl r`wv'rifaany

    
    save "$temp/SHARE_ADLIADL_w`wv'", replace       
}    
* ADL and IADL seperately (wave 6-7, informal and formal)
forvalues wv = 6/7{
    //local wv = 6
    local ver = cond(`wv'==7, 1, 0)
    
    local datafile "$rawdata/SHARE/sharew`wv'_rel7-1-`ver'_ALL_datasets_stata"
    
    use "`datafile'/sharew`wv'_rel7-1-`ver'_sp",clear
    merge 1:1 mergeid using "`datafile'/sharew`wv'_rel7-1-`ver'_cv_r", ///
        keepusing(hhsize) 
    keep if _merge==3
    drop _merge

    merge 1:1 mergeid using "`datafile'/sharew`wv'_rel7-1-`ver'_hc", ///
        keepusing(hc127d1 hc127d2 hc127d3 hc127d4 hc127dno hc029)
    keep if _merge==3
    drop _merge
       
    merge 1:1 mergeid using "$H_SHARE", ///
        keepusing(r`wv'adltota_s r`wv'iadlzaa inw`wv' r`wv'agey r`wv'adltot_s) 
     
    keep if _merge==3
    drop _merge
    
    * ADL
    * Informal ADL care from people outside of the households (excluding professionals)
    gen routc_adl = .
    replace routc_adl = 0 if sp002_ == 5 | (sp002_ == 1 & !((sp004d1_1 == 1 & inrange(sp003_1,1,35)) | ///
                                                           (sp004d1_2 == 1 & inrange(sp003_2,1,35)) | ///
                                                           (sp004d1_3 == 1 & inrange(sp003_3,1,35))))
    replace routc_adl = 1 if sp002_ == 1 & ((sp004d1_1 == 1 & inrange(sp003_1,1,35)) | ///
                                           (sp004d1_2 == 1 & inrange(sp003_2,1,35)) | ///
                                           (sp004d1_3 == 1 & inrange(sp003_3,1,35)))
    replace routc_adl = .x if r`wv'adltota_s!=1 //no difficulties


    * Informal ADL care from people inside the households (excluding professionals)
    egen sp021dinc = rowmax(sp021d1 sp021d2 sp021d3 sp021d4 sp021d5 sp021d6 ///
                            sp021d7 sp021d8 sp021d9 sp021d10 sp021d11 ///
                            sp021d20 sp021d21 sp021d22 sp021d23 sp021d24 ///
                            sp021d25 sp021d26 sp021d27 sp021d28 sp021d29 ///
                            sp021d30 sp021d31 sp021d32 sp021d35)
    gen rinc_adl = .
    replace rinc_adl = 0 if sp020_ == 5 | hhsize == 1 | (sp020_ == 1 & sp021dinc!=1)
    replace rinc_adl = 1 if sp020_ == 1 & sp021dinc==1
    replace rinc_adl = . if r`wv'adltota_s!=1 // no difficulties

    * Any Informal ADL Care 
    gen r`wv'racaany = 0 if routc_adl == 0 & rinc_adl ==0 
    replace r`wv'racaany = 1 if routc_adl == 1 | rinc_adl ==1 
    replace r`wv'racaany = . if mi(routc_adl) | mi(rinc_adl)
    
    * Any formal ADL Care 
    gen rhfm_adl = .
    replace rhfm_adl=.d  if hc127d1==-1 & hc127d2==-1 & hc127d3==-1 & hc127d4==-1
    replace rhfm_adl=.r  if hc127d1==-2 & hc127d2==-2 & hc127d3==-2 & hc127d4==-2
    replace rhfm_adl=.n if hc029==3
    replace rhfm_adl=0 if !mi(hc127dno)
    replace rhfm_adl=1 if hc127d1==1
    replace rhfm_adl = . if r`wv'adltota_s!=1 // no difficulties
    lab var rhfm_adl "rhfm_adl:w`wv' R formal ADL care, pr 12 mos"  
    rename rhfm_adl r`wv'rafaany 
    
    * Any informal or formal ADL care 
    gen r`wv'racany = 0 if r`wv'racaany ==0 & r`wv'rafaany==0 
    replace r`wv'racany = 1 if r`wv'racaany ==1 | r`wv'rafaany==1
    replace r`wv'racany = . if mi(r`wv'racaany) | mi(r`wv'rafaany)
    
    * IADL 
    * Informal IADL care from people outside of the households (excluding professionals)
    

    gen routc_iadl = .
    replace routc_iadl = 0 if sp002_ == 5 | (sp002_ == 1 & !((sp004d2_1 == 1 & inrange(sp003_1,1,35)) | ///
                                                             (sp004d2_2 == 1 & inrange(sp003_2,1,35)) | ///
                                                             (sp004d2_3 == 1 & inrange(sp003_3,1,35)) | ///
                                                             (sp004d3_1 == 1 & inrange(sp003_1,1,35)) | ///
                                                             (sp004d3_2 == 1 & inrange(sp003_2,1,35)) | ///
                                                             (sp004d3_3 == 1 & inrange(sp003_3,1,35))))
    replace routc_iadl = 1 if sp002_ == 1 & ((sp004d2_1 == 1 & inrange(sp003_1,1,35)) | ///
                                             (sp004d2_2 == 1 & inrange(sp003_2,1,35)) | ///
                                             (sp004d2_3 == 1 & inrange(sp003_3,1,35)) | ///
                                             (sp004d3_1 == 1 & inrange(sp003_1,1,35)) | ///
                                             (sp004d3_2 == 1 & inrange(sp003_2,1,35)) | ///
                                             (sp004d3_3 == 1 & inrange(sp003_3,1,35)))
    replace routc_iadl = .x if r`wv'iadlzaa !=1

    gen r`wv'ricaany = 0 if routc_iadl == 0
    replace r`wv'ricaany = 1 if routc_iadl == 1 

    * IADL 
    * Any formal IADL Care
    gen rhfm_iadl = .
    replace rhfm_iadl=.d  if hc127d1==-1 & hc127d2==-1 & hc127d3==-1 & hc127d4==-1
    replace rhfm_iadl=.r  if hc127d1==-2 & hc127d2==-2 & hc127d3==-2 & hc127d4==-2
    replace rhfm_iadl=.n if hc029==3
    replace rhfm_iadl=0 if !mi(hc127dno)
    replace rhfm_iadl=1 if hc127d2==1 | hc127d3==1 | hc127d4==1
    replace rhfm_iadl = . if r`wv'iadlzaa!=1 // no difficulties  
    rename rhfm_iadl r`wv'rifaany
    
    * Any informal or formal IADL care 
    gen r`wv'ricany = 0 if r`wv'ricaany ==0 & r`wv'rifaany==0 
    replace r`wv'ricany = 1 if r`wv'ricaany ==1 | r`wv'rifaany==1
    replace r`wv'ricany = . if mi(r`wv'ricaany) | mi(r`wv'rifaany)
    
    keep mergeid r`wv'racany r`wv'racaany r`wv'rafaany /// informal/formal ADL help
                 r`wv'ricany r`wv'ricaany r`wv'rifaany //  informal/formal IADL help
    order mergeid r`wv'racany r`wv'racaany r`wv'rafaany /// informal/formal ADL help
                  r`wv'ricany r`wv'ricaany r`wv'rifaany //  informal/formal IADL help
    

    save "$temp/SHARE_ADLIADL_w`wv'", replace       
}    

use "$temp/SHARE_aiadl_w5", clear 
merge 1:1 mergeid using "$temp/SHARE_ADLIADL_w5", nogen
forvalues wv = 6/7{
    merge 1:1 mergeid using "$temp/SHARE_aiadl_w`wv'", nogen
    merge 1:1 mergeid using "$temp/SHARE_ADLIADL_w`wv'", nogen
}
tempfile SHARE_ADLIADL_Help
save "$temp/SHARE_ADLIADL_Help", replace


*----------------------------------------------------------------------- 
* Clean vars from Harmonized SHARE
use mergeid r*wtresp r*iwy r*nhmliv inw* ///
    ragender raedyrs raeducl raedisced ///
    country isocountry ///
    r*tr20 r*ser7 r*orient ///
    r*agey ///
    r*mstat ///
    h*child ///
    r*eurod ///
    r*adla r*adltot_s r*iadlza ///    
    r*walkra r*beda r*batha r*dressa r*eata r*toilta ///
    h*rcaany h*rcasrc h*rscare h*rccare h*rrcare h*rfcare ///
    h*racaany h*racasrc h*rascare h*raccare h*rarcare h*rafcare ///
    h*ricaany h*riscare h*riccare h*rircare h*rifcare ///
    hh*hhres ///
    using "$H_SHARE", clear
        
merge 1:1 mergeid using "$temp/SHARE_ADLIADL_Help", nogen
  
* reshape wide data to long data
reshape long r@wtresp r@iwy r@nhmliv inw@ ///
             r@tr20 r@ser7 r@orient ///
             r@agey ///
             r@mstat ///
             h@child ///
             r@eurod ///
             r@adla r@adltot_s r@iadlza ///    
             r@walkra r@beda r@batha r@dressa r@eata r@toilta ///
             h@rcaany h@rcasrc h@rscare h@rccare h@rrcare h@rfcare ///
             h@racaany h@racasrc h@rascare h@raccare h@rarcare h@rafcare ///
             h@ricaany h@riscare h@riccare h@rircare h@rifcare ///
             r@rcany r@rcaany r@rfaany /// informal/formal ADL/IADL help
             r@racany r@racaany r@rafaany /// informal/formal ADL help
             r@ricany r@ricaany r@rifaany ///  informal/formal IADL help             
             hh@hhres ///
             , i(mergeid) j(wave)

rename hhhhres hhhres

drop if wave==3 //life history survey
gen year = 2004 if wave==1 
replace year = 2007 if wave==2
replace year = 2011 if wave==4
replace year = 2013 if wave==5
replace year = 2015 if wave==6
replace year = 2017 if wave==7

*-----------------------------------------------------------------------
* Only keep those age 50 and older 
keep if ragey>=50

* Only keep community-dwelling sample (i.e., drop sample living in nursing home)
keep if rnhmliv ==0 

* Only keep waves 2013-2017          
keep if inrange(year, 2013, 2017)


*----------------------------------------------------------------------- 
//* Clean and Construct Variables *//

* Gender
gen rafemale = ragender - 1 

  
* Marital Status
recode rmstat (1 3=1 "1.Married/Partnered")(4/8=0 "0.Non-married"), gen(rmstat01) test

    
*-----------------------------------------------------------------------     
* Construct LTC variables for ADL/IADL
rename radla radl5a 
rename radltot_s radl6a

recode radl5a (0=0)(1/5=1), gen(radl5a_01) test
recode radl6a (0=0)(1/6=1), gen(radl6a_01) test 
recode riadlza (0=0)(1/5=1), gen(riadlza_01) test 

gen radliadl= radl6a + riadlza  
egen radliadl_01 = rowmax(radl6a_01 riadlza_01)

label var radl5a_01 "Any ADL difficulties (0/1)"
label var radl6a_01 "Any ADL difficulties (0/1)"
label var riadlza_01 "Any IADL difficulties (0/1)"
label var radliadl_01 "Any ADL/IADL difficulties (0/1)"


* Any ADL/IADL help
foreach var of varlist rrcany rrcaany rrfaany{
    replace `var'= .x if radliadl_01==0
}   
foreach var of varlist rracany rracaany rrafaany{
    replace `var'= .x if radl6a_01==0
}
foreach var of varlist rricany rricaany rrifaany{
    replace `var'= .x if riadlza_01==0
}  

* Absence of Any ADL/IADL help (reverse coded)
global varlist0 rrcany rrcaany rrfaany /// informal/formal ADL/IADL help
                rracany rracaany rrafaany /// informal/formal ADL help
                rricany rricaany rrifaany //  informal/formal IADL help
                
foreach var in $varlist0{
    recode `var' (1=0) (0=1), gen(`var'_rev) test
}   


*-----------------------------------------------------------------------               
* Cognition and dementia assessment (25 score, 1.5 SD by Edu Levels)
gen rcognition= rtr20 + rser7 
            
xtile raedu_l3=raedyrs, n(3) 

* Dementia Status
gen demented=.
forvalues i = 1/3{
   sum rcognition if wave!=. & raedu_l3==`i', detail 
   replace demented = cond(rcognition < `r(mean)'- 1.5*`r(sd)' & radliadl_01==1, 1, 0) ///
        if !mi(rcognition) & wave!=. & raedu_l3==`i'
}
  
* Ever Demented  
bys mergeid: egen ever_demented = max(demented) if !mi(ragey)

* If Demented At Baseline Wave 
bys mergeid (year): egen base_year = min(cond(demented!=., year, . )) 
bys mergeid (year): egen demented_base = min(cond(year==base_year, demented, . ))
 
* Flag from the first wave of dementia and onward 
bys mergeid (year): egen baseD_year = min(cond(demented==1, year, . )) 
bys mergeid (year): gen demented_baseD = cond(year>=baseD_year, 1, 0) 
     
clonevar ever_demented_25 = ever_demented   
clonevar base_year_25 = base_year 
clonevar demented_base_25 = demented_base
clonevar demented_baseD_25 = demented_baseD


*-----------------------------------------------------------------------     
* Save Cleaned Dataset

save "$data/SHARE_Data_L", replace        
 
 
 
*-----------------------------------------------------------------------
*                         Preparing Data (China)
*-----------------------------------------------------------------------
* China  

* Clean Vars from Harmonized CHARLS       
use ID householdID communityID ///
    ragender raeduc_c raeducl r*agey r*mstat r*wtresp r*iwy r*nhmliv ///
    r*tr20 r*ser7 r*draw r*orient ///
    h*child ///
    h*hhres /// 
    r*adlab_c r*adlfive r*iadlza ///
    r*dressa r*batha r*eata r*beda r*toilta r*urina ///
    r*housewka r*mealsa r*shopa r*phonea r*medsa r*moneya ///
    r*racany r*dresshlp r*bathehlp r*eathlp r*bedhlp r*toilethlp ///
    r*ricany r*hswkhlp r*mealhlp r*shophlp r*phonehlp r*medhlp r*moneyhlp ///
    r*rcany r*rcaany r*rscare r*rccare r*rrcare r*rfcare r*rfaany r*rpfcare r*rufcare /// ADL/IADL help
    r*racaany r*rascare r*raccare r*rarcare r*rafcare r*rafaany /// ADL help
    r*ricaany r*riscare r*riccare r*rircare r*rifcare r*rifaany /// IADL help 
    using "$H_CHARLS", clear


* Education levels to years of education    
recode raeduc_c (1/3=0) (4=6) (5=9) (6/7=12) (8=15) (9=16) (10=19) (11=22), ///
    gen (raedyrs) test 
    
* reshape wide data to long data    
reshape long r@agey r@mstat r@wtresp r@iwy r@nhmliv ///
             r@tr20 r@ser7 r@draw r@orient ///
             h@child ///
             h@hhres /// 
             r@adlab_c r@adlfive r@iadlza ///
             r@dressa r@batha r@eata r@beda r@toilta r@urina ///
             r@housewka r@mealsa r@shopa r@phonea r@medsa r@moneya ///
             r@racany r@dresshlp r@bathehlp r@eathlp r@bedhlp r@toilethlp ///
             r@ricany r@hswkhlp r@mealhlp r@shophlp r@phonehlp r@medhlp r@moneyhlp ///
             r@rcany r@rcaany r@rscare r@rccare r@rrcare r@rfcare r@rfaany r@rpfcare r@rufcare /// ADL/IADL help
             r@racaany r@rascare r@raccare r@rarcare r@rafcare r@rafaany /// ADL help
             r@ricaany r@riscare r@riccare r@rircare r@rifcare r@rifaany /// IADL help 
             , i(ID) j(wave)

label var wave ""             
label val wave 

gen year = 2011 if wave==1
replace year = 2013 if wave==2 
replace year = 2015 if wave==3
replace year = 2018 if wave==4 

*-----------------------------------------------------------------------
* Only keep those age 50 and older 
keep if ragey>=50

* Only keep community-dwelling sample (i.e., drop sample living in nursing home)
keep if rnhmliv ==0 

* Only keep waves 2013-2018 
keep if inrange(year, 2013, 2018)

*----------------------------------------------------------------------- 
//* Clean and Construct Variables *//

* Gender
gen rafemale = ragender - 1 

* Marital Status    
recode rmstat (1/3=1 "1.Married")(4/8=0 "0.Non-married"), gen(rmstat01) test
                       
        
*-----------------------------------------------------------------------                 
* LTC Variables for ADL/IADL help 
recode radlfive (0=0)(1/5=1), gen(radlfive_01) test 
recode riadlza (0=0)(1/5=1), gen(riadlza_01) test
egen radliadl_01 = rowmax(radlfive_01 riadlza_01) // no walk across room measure
gen radliadl= radlfive + riadlza  // no walk across room measure

label var radlfive_01 "Any ADL difficulties (0/1)"
label var riadlza_01 "Any IADL difficulties (0/1)"
label var radliadl_01 "Any ADL/IADL difficulties (0/1)"


* Any ADL/IADL help

* ADL
foreach var of varlist rracany rracaany rrafaany{
    replace `var'= .x if radlfive_01==0
}

* IADL
// i.e., redefine excluding help with household chores (only asked in CHARLS)
egen anyhelp_iadl = rowmax(rmealhlp rshophlp rphonehlp rmedhlp rmoneyhlp)
foreach var of varlist rricany rricaany rrifaany{
    replace `var' = 0 if anyhelp_iadl==0 & !mi(`var') 
    replace `var'= .x if riadlza_01==0
} 

* ADL/IADL 
// i.e., redefine excluding help with household chores (only asked in CHARLS)
egen anyhelp_adliadl = rowmax(rdresshlp rbathehlp reathlp rbedhlp rtoilethlp ///
                              rmealhlp rshophlp rphonehlp rmedhlp rmoneyhlp)
foreach var of varlist rrcany rrcaany rrfaany{
    replace `var'= 0 if anyhelp_adliadl == 0 & !mi(`var')
    replace `var'= .x if radliadl_01==0
}   



* Absence of Any ADL/IADL help (reverse coded)
global varlist0 rrcany rrcaany rrfaany /// informal/formal ADL/IADL help
                rracany rracaany rrafaany /// informal/formal ADL help
                rricany rricaany rrifaany //  informal/formal IADL help
                
foreach var in $varlist0{
    recode `var' (1=0) (0=1), gen(`var'_rev) test
}                             
  
*-----------------------------------------------------------------------       
* Cognition and Dementia Assessment (25-score, 1.5 SD by Edu Levels)
gen rcognition= rtr20 + rser7
                
xtile raedu_l3=raedyrs, n(3) 
    
gen demented=.
forvalues i = 1/3{
   sum rcognition if wave!=. & raedu_l3==`i', detail 
   replace demented = cond(rcognition < `r(mean)'- 1.5*`r(sd)' & radliadl_01==1, 1, 0) ///
            if !mi(rcognition) & wave!=. & raedu_l3==`i'
}
        

* Ever Demented  
bys ID: egen ever_demented = max(demented) if !mi(ragey)

* If Demented At Baseline Wave 
bys ID (year): egen base_year = min(cond(demented!=., year, . )) 
bys ID (year): egen demented_base = min(cond(year==base_year, demented, . ))
 
* Flag Obs from the first occurance of dementia and after
bys ID (year): egen baseD_year = min(cond(demented==1, year, . )) 
bys ID (year): gen demented_baseD = cond(year>=baseD_year, 1, 0) 
     
clonevar ever_demented_25 = ever_demented   
clonevar base_year_25 = base_year 
clonevar demented_base_25 = demented_base
clonevar demented_baseD_25 = demented_baseD


*-----------------------------------------------------------------------             
* Save Cleaned Dataset
   
save "$data/CHARLS_Data_L", replace      
        



        

