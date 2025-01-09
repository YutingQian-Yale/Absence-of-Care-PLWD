/***********************************************************************
                Disability, Dementia and LTC (Descriptive)
************************************************************************

    *Title: Absence of Care Project
    *Aim of This Do-file: Descriptive Analysis
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
*                   Descriptive Statistics (Table 1)
*-----------------------------------------------------------------------
cls
global cty_list ///
       HRS ELSA SHARE CHARLS 


foreach cty in $cty_list{
    
    use "$data/`cty'_Data_L", clear 
    
    keep if year>=2012 & ever_demented==1 & radliadl_01==1

    tab raeducl, gen(raeducl_)

    recode hhhres (1=1)(2/100=0), gen(rlalone_01) test
        
        
    mat summary_m = (0,0,0,0)

    * Socioeconomic status 
    global varlist0 ragey rafemale rlalone_01 raeducl_1 raeducl_2 raeducl_3             
    local i = 1
    foreach var in $varlist0{
        sum `var'
        scalar m = `r(mean)'
        scalar sd = `r(sd)'
        scalar N = `r(N)'
        if `i'==1 {
           scalar n1 = .   
        }
        else {
           sum `var' if `var'==1 
           scalar n1 = `r(N)'
        }
        mat summary_m = (summary_m\m, sd, n1, N)  
        local i = `i' + 1 
    }


    * Extent of ADL/IADL functional limitations (rawdata)
    if "`cty'"=="CHARLS" {
        cap drop radliadl 
        gen radliadl = radlab_c + riadlza
        rename radlab_c radltotal 
    } 
    else{
        rename radl6a radltotal
    }
    global varlist1 radliadl radltotal riadlza
    local i = 1
    foreach var in $varlist1{
        sum `var'  
        scalar m = `r(mean)'
        scalar sd = `r(sd)'
        scalar N = `r(N)'
        scalar n1 = .   
        mat summary_m = (summary_m\m, sd, n1, N)  
        local i = `i' + 1 
    }

    * ADL/IADL help (rawdata)
    global varlist2 rrcany_rev rrcaany_rev rrfaany_rev /// informal/formal ADL/IADL help
                    rracany_rev rracaany_rev rrafaany_rev /// informal/formal ADL help
                    rricany_rev rricaany_rev rrifaany_rev //  informal/formal IADL help
    local i = 1
    foreach var in $varlist2{
        
        sum `var' 
        scalar m = `r(mean)'
        scalar sd = `r(sd)'
        scalar N = `r(N)'
        
        sum `var' if `var'==1 
        scalar n1 = `r(N)' 
        
        mat summary_m = (summary_m\m, sd, n1, N)  
        local i = `i' + 1 
    }
    
    mat rownames ///
        summary_m = first $varlist0 $varlist1 $varlist2 
    mat colnames ///    
        summary_m = m sd n1 N
    mat list summary_m 
    
    clear 
    svmat2 summary_m, names(col) rnames(varname) full  
    drop if _n==1 
    order varname 
    gen data = "`cty'"
    gen varid = _n
    
    tempfile `cty'_sum
    save "``cty'_sum'", replace 
}                    


clear 
foreach cty in $cty_list{
    local use_append = cond("`cty'"=="HRS", "use", "append using")
    `use_append' "``cty'_sum'"
}    

* Generate stats in good formats
gen stats = string(round(m, 0.1))+" (" + string(round(sd, 0.1)) + ")" if n1==.
replace stats = string(n1) + " (" + string(round(m*100, 0.1)) + ")" if n1!=.
keep varid varname data stats N 
order varid varname data stats N 

* drop vars for SHARE that contains informal IADL care
global varlist_s rrcany_rev rrcaany_rev rricany_rev rricaany_rev
foreach var in $varlist_s {
    replace stats = "" if data == "SHARE" & varname == "`var'"
    replace N = . if data == "SHARE" & varname == "`var'"
}

* Reshape long data to wide data     
gen dataid = 1 if data == "HRS"
replace dataid = 2 if data == "ELSA"
replace dataid = 3 if data == "SHARE"
replace dataid = 4 if data == "CHARLS"

drop data
reshape wide stats N, ///
        i(varid) j(dataid)
order varid varname      

forvalues i = 1/4{
    local cty = cond(`i'==1, "HRS", cond(`i'==2, "ELSA", cond(`i'==3, "SHARE", "CHARLS")))
    rename stats`i' stats_`cty'
    rename N`i' N_`cty'
}
drop varid
asdoc list _all, save($table/descriptive/table1.rtf) replace    
        
*-----------------------------------------------------------------------
*                   Flow Chart (Exteded Data Fig.1)
*-----------------------------------------------------------------------

* note: no estimates saved (only for the flow chart)
cls
global cty_list ///
       HRS ELSA SHARE CHARLS 
       
foreach cty in $cty_list{
    local id = cond("`cty'"=="HRS", "hhidpn", ///
                cond("`cty'"=="ELSA", "idauniq", ///
                  cond("`cty'"=="SHARE", "mergeid", "ID")))
    use "$data/`cty'_Data_L", clear 
    di ""
    di "`cty'"
	
    keep if year>=2012 & ragey>=50 & radliadl_01==1
	
    distinct `id' 

    distinct `id' if ever_demented==.
	
	distinct `id' if ever_demented==0
	
	distinct `id' if ever_demented==1

}    
       
	   
*-----------------------------------------------------------------------
*             Variable Missingness (Supplementary Table 5)
*-----------------------------------------------------------------------
    
cls 
global cty_list ///
       HRS ELSA SHARE CHARLS 

foreach cty in $cty_list{
    //local cty SHARE
    use "$data/`cty'_Data_L", clear

    keep if year>=2012 & ragey>=50 & ever_demented==1 & radliadl_01==1 

    * Extent of ADL/IADL functional limitations (rawdata) 
    if "`cty'"=="CHARLS" {
        cap drop radliadl 
        gen radliadl = radlab_c + riadlza
        rename radlab_c radltotal 
        rename radlfive_01 radl_01
    } 
    else{
        rename radl6a radltotal
        rename radl6a_01 radl_01
    }
      
    tab raeducl, gen(raeducl_)

    recode hhhres (1=1)(2/100=0), gen(rlalone_01) test


    mat miss_`cty' =(0,0,0) 


    * Socioeconomic status + ADL/IADL
    global varlist0 ragey rafemale rlalone_01 raeducl_1 raeducl_2 raeducl_3 ///
                    radliadl radltotal riadlza
        
    foreach var in $varlist0{
            
        qui sum radliadl_01
        scalar N = `r(N)'
        
        qui sum `var'
        scalar n2 = `r(N)'
        
        scalar n1 = N-n2
        
        di N 
        di n1 
        di n2
        
        mat miss_`cty' = (miss_`cty'\n1, `=n1/N*100', N)  
    }

    global varlist1 rrcany_rev rrcaany_rev rrfaany_rev /// informal/formal ADL/IADL help
                    rracany_rev rracaany_rev rrafaany_rev /// informal/formal ADL help
                    rricany_rev rricaany_rev rrifaany_rev //  informal/formal IADL help
                    
    local i = 1                
    foreach var in $varlist1{
        
        local varname = cond(inrange(`i', 1, 3), "radliadl_01", ///
                            cond(inrange(`i', 4, 6), "radl_01", "riadlza_01")) 
                           
        qui sum wave if !mi(`var'), detail
        //di `"`varname'"'                
        qui sum `varname' if `varname'==1 & inrange(wave, `r(min)', `r(max)')
        
        scalar N = `r(N)'
        
        qui sum `var' if `varname'==1
        scalar n2 = `r(N)'
        
        scalar n1 = N-n2
        
        di N 
        di n1 
        di n2
        
        mat miss_`cty' = (miss_`cty'\n1, n2, N) 
        
        local i = `i' + 1 
    }

    mat rownames ///
        miss_`cty' = first $varlist0 $varlist1

    mat colnames ///    
        miss_`cty' = M_`cty' NM_`cty' N_`cty'
        
}

    

mat summary_miss = (miss_HRS, miss_ELSA, miss_SHARE, miss_CHARLS)
        
mat list summary_miss   

/* Redefine sample size for "rracany_rev" and "rracaany_rev", 
   as only waves 6 & 7 data were used for these two outcomes */
   
local cty SHARE
use "$data/`cty'_Data_L", clear
keep if year>=2012 & ragey>=50 & ever_demented==1 & radliadl_01==1 
rename radl6a_01 radl_01
sum radl_01 if radl_01==1 & ((inw3lh==1 & wave==7) | wave==6)
scalar N_SHARE_ADL = `r(N)' 
 
clear 
svmat2 summary_miss, names(col) rnames(varname) full  
drop if _n==1 
order varname 
gen varid = _n


* drop vars for SHARE that contains informal IADL care
global varlist_s rrcany_rev rrcaany_rev rricany_rev rricaany_rev
foreach var in $varlist_s {
    replace M_SHARE = . if varname == "`var'"
    replace NM_SHARE = . if varname == "`var'"
    replace N_SHARE = . if varname == "`var'"
}
 
* Reassign sample size for "rracany_rev" and "rracaany_rev"
   
replace N_SHARE = `=N_SHARE_ADL' if varname == "rracany_rev" | varname == "rracaany_rev"
replace M_SHARE = N_SHARE - NM_SHARE if varname == "rracany_rev" | varname == "rracaany_rev"


foreach i in $cty_list{
    gen MP_`i' = M_`i' /N_`i' *100
    gen Missing_`i'= strofreal(M_`i',"%9.0f") + ///
                            " (" + strofreal(MP_`i', "%9.1f") + "%)" ///
                            if MP_`i' > 1 
    replace Missing_`i' = strofreal(M_`i',"%9.0f") + ///
                            " (" + strofreal(MP_`i', "%9.2f") + "%)" ///
                            if MP_`i' < 1   
    order Missing_`i' N_`i', last                        
}


 
keep varname Missing_* N_*
asdoc list _all, save($table/descriptive/table_S5.rtf) replace    
       


      


