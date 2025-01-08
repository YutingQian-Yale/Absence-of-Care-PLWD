/***********************************************************************
                Disability, Dementia and LTC (Master Files)
************************************************************************

    *Title: Absence of Care Project
    *Aim of This Do-file: Master Files (Run All Do Files)
    *Authors: Zhuoer Lin, Yuting Qian
    *Date Edited: Dec 2024

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
* Important: need to redifine the directory with your own 
*-----------------------------------------------------------------------
global master_directory "/Users/zlin01/Dropbox/WorldBankReport/Research Manuscript - Dementia, ADL and LTC" 
cd "$master_directory" 

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
*                         Replicate All Analyses
*-----------------------------------------------------------------------

* Step 1: Preparing Data 
do "$code/Main/Global_Dementia_LTC_PData.do"

* Step 2: Main Analysis - Absence of Care Trend (Fig.1) 
do "$code/Main/Global_Dementia_LTC_Trend.do"

* Step 3: Main Analysis - Disparities (Fig.2 & Fig.3)
do "$code/Main/Global_Dementia_LTC_Disparities_Edu.do" // Fig.2
do "$code/Main/Global_Dementia_LTC_Disparities_LiveAlone.do" // Fig.3

* Step 4: Main Analysis - Descriptive Analysis (for statistics shown in Table 1 and Extended Data Fig.1) 
do "$code/Descriptive/Global_Dementia_LTC_Descriptive.do" 

* Step 5: Additional Analysis - Disability Trend (Extended Data Fig.2)
do "$code/Descriptive/Global_Dementia_LTC_Disability.do"

* Step 6: Sensitivity Analyses (Extended Data Figs.3 and 4)
do "$code/Robustness/Global_Dementia_LTC_Robust_Cog25.do" // (Extended Data Fig.3)
do "$code/Robustness/Global_Dementia_LTC_Robust_Demented.do" // (Extended Data Fig.4)

* Step 7: Additional Analysis - Absence of Care for PLWD with More Severe Limitations (Extended Data Fig.5)
do "$code/Robustness/Global_Dementia_LTC_Robust_MoreDisabled.do" 

* Step 8: Supplementary Analysis among Non-Dementia Sample
do "$code/Supplementary/Global_Dementia_LTC_Descriptive_NonDemented.do" // (Supplementary Table S7)
do "$code/Supplementary/Global_Dementia_LTC_Disability_NonDemented.do" // (Extended Data Fig.6)
do "$code/Supplementary/Global_Dementia_LTC_Trend_NonDemented.do" // (Extended Data Fig.7)

