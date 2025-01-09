/***********************************************************************
             Disability, Dementia and LTC (Disabiltiy Trend)
************************************************************************

    *Title: Absence of Care Project
    *Aim of This Do-file: Disabiltiy Trend (Dementia Sample)
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
*                  Overall Trend in Disability (eFig.2)
*-----------------------------------------------------------------------
forvalues i = 1/4{
    
    local cty = cond(`i'==1, "HRS", cond(`i'==2, "ELSA", cond(`i'==3, "SHARE", "CHARLS")))
    local ind_id = cond(`i'==1, "hhidpn", cond(`i'==2, "idauniq", cond(`i'==3, "mergeid", "ID")))
    local weight = cond(inlist(`i', 1, 3, 4), "rwtresp", "rcwtresp")
    local pweight = cond(inlist(`i', 1, 3, 4), "[pw=rwtresp]", "[pw=rcwtresp]")
    local aweight = cond(inlist(`i', 1, 3, 4), "[aw=rwtresp]", "[aw=rcwtresp]")
    use "$data/`cty'_Data_L", clear 
         
    rename ragey* ragey 
    

    if `i'==4 {
        cap drop radliadl 
        gen radliadl = radlab_c + riadlza
        
        rename radlab_c radltotal 
        //rename radlfive radltotal
    } 
    else{
        rename radl6a radltotal
    }
    
    keep if ragey>=50 & ever_demented==1 & radliadl_01==1
    
    global varlist0 radliadl radltotal riadlza

    * Generate numerical id
    egen newid = group(`ind_id')
 
        
    sum year 
    scalar year_min_all = `r(min)'
    scalar year_max_all = `r(max)'
    
    distinct year
    scalar num_year = `r(ndistinct)'
        
    mat aapc_`cty' = (0,0,0,0,0,0,0,0,0,0,0)  
        
    foreach var in $varlist0{
        //local var radliadl
        sum year if !mi(`var') 
        scalar year_min = `r(min)'
        scalar year_max = `r(max)'
        scalar year_gap = `r(max)' - `r(min)'
        
        distinct year if !mi(`var') 
        scalar num_nonmis_year = `r(ndistinct)'
        
        * Obtain the terminal-year weight for longitudinal data 
        cap drop last_year weight_last 
        bys newid (year): egen last_year = max(cond(!mi(`var') & !mi(`weight'), year, . )) 
        bys newid (year): egen weight_last = max(cond(year==last_year, `weight', . )) 

        * GEE        
        xtset newid year   
        xtgee `var' i.year ragey ragender [pw=weight_last] ///
              , family(gaussian) link(identity) corr(exchangeable) vce(robust) 
         

        * Predictive margins at each year
        margins year
        scalar `var'_`cty'_N = r(N)
        
        matselrc r(table) margins_`var', r(b se ll ul) 
        mat margins_`var' = margins_`var''

        * AAPC
        xtgee `var' year ragey ragender [pw=weight_last] ///
              , family(gaussian) link(identity) corr(exchangeable) vce(robust) 
        
        scalar b = r(table)["b", "year"]
        scalar b_se = r(table)["se", "year"]
        scalar b_ll = r(table)["ll", "year"]
        scalar b_ul = r(table)["ul", "year"]
        scalar rho = e(R)[1,2]
        
        scalar aapc = b
        scalar aapc_ll = b_ll
        scalar aapc_ul = b_ul
        
        * Significance (P-value)
        scalar z_stats = r(table)["z", "year"]
        scalar pvalue = r(table)["pvalue", "year"]
        
        mat aapc_`cty' = (aapc_`cty'\aapc, aapc_ll, aapc_ul, z_stats, pvalue, rho, e(g_avg), e(g_min), e(g_max), e(N_g), e(N))
             
        
        mat colnames ///
            margins_`var' = `var'_m `var'_se `var'_ll `var'_ul 
    }


    foreach var in $varlist0{
        clear
        svmat2 margins_`var', names(col) rnames(x) full
        rename x year
        replace year = substr(year, 1, 4)
        order year 
        gen data = "`cty'"
        order data year 
        save "$temp/margins_`cty'_`var'", replace 
    }
    
    clear
    local j = 1
    foreach var in $varlist0{
        local use_merge = cond(`j'==1, "use", "merge 1:1 year using")
        `use_merge' "$temp/margins_`cty'_`var'"
        gen `var'_N = `=`var'_`cty'_N'
        cap drop _merge
        local j = `j' + 1 
    }
    save "$temp/margins_`cty'", replace      
    
    
    
    matrix colnames ///
           aapc_`cty' = aapc aapc_ll aapc_ul z_stats pvalue rho g_avg g_min g_max N_g N 
    matrix rownames ///
           aapc_`cty' = row1 $varlist0
 
    clear
    svmat2 aapc_`cty', names(col) rnames(x) full
    drop if _n==1
    gen data = "`cty'"
    order x data
    gen x_id = _n
    gen cty_id = `i'

    
    save "$temp/aapc_`cty'_data", replace 
}

*-----------------------------------------------------------------------
* Save AAPC for Disability - Extent  (Ever Demented with any ADLs/IADLs)

use "$temp/aapc_HRS_data", clear 
append using "$temp/aapc_ELSA_data"
append using "$temp/aapc_SHARE_data"
append using "$temp/aapc_CHARLS_data"


gen AAPC = string(aapc, "%9.2f") if pvalue >=0.05
replace  AAPC = string(aapc, "%9.2f")+"*" if pvalue <0.05
replace  AAPC = string(aapc, "%9.2f")+"**" if pvalue <0.01
replace  AAPC = string(aapc, "%9.2f")+"***" if pvalue <0.001

gen sig = "" if pvalue >=0.05
replace sig = "*" if pvalue <0.05
replace sig = "**" if pvalue <0.01
replace sig = "***" if pvalue <0.001


global cty_list HRS ELSA SHARE CHARLS

foreach var in $varlist0{
    foreach cty in $cty_list{
        valuesof AAPC if data == "`cty'" & x == "`var'"
        scalar `var'_AAPC_`cty' = r(values)
        
        valuesof sig if data == "`cty'" & x == "`var'"
        scalar `var'_sig_`cty' = r(values)

    }
}

gen data_id = cond(data=="HRS", 1, cond(data=="ELSA", 2, cond(data=="SHARE", 3, 4)))


gen varname = ""
gen panel = ""
local i = 1
gen var_id = .
foreach var in $varlist0{
    local ADL_IADL = cond(inrange(`i',1,1), "ADL/IADL", ///
                        cond(inrange(`i',2,2), "ADL", "IADL"))
    replace varname = "`ADL_IADL' limitations" if x =="`var'"
    
    replace var_id = `i' if x == "`var'"
    
    replace panel = cond(`i'==1, "Panel a", cond(`i'==2, "Panel b", "Panel c")) if x =="`var'"
                        
    local i = `i' + 1 
}
format aapc %9.2f

sort var_id data_id
                         
keep panel varname data aapc aapc_ll aapc_ul z_stats pvalue N 
order panel varname data aapc aapc_ll aapc_ul z_stats pvalue N

rename aapc aac
rename aapc_ll aac_ll
rename aapc_ul aac_ul 
                              
export excel using "$table/descriptive/efig2.xlsx", replace firstrow(variables) keepcellfmt sheet("efig2_AAC")


*-----------------------------------------------------------------------
* Save Estimates (95% CI) for Disability - Extent (Ever Demented with any ADLs/IADLs)

use "$temp/margins_HRS", clear 
append using "$temp/margins_ELSA"
append using "$temp/margins_SHARE"
append using "$temp/margins_CHARLS"


* calculate summary statistis
foreach var in $varlist0{
    sum `var'_m
}

destring year, replace


* save estimates for disability
local i = 1
foreach var in $varlist0{
    local ADL_IADL = cond(inrange(`i',1,1), "ADL/IADL", ///
                        cond(inrange(`i',2,2), "ADL", "IADL"))
    label var `var'_m "`ADL_IADL' limitations (Estimate)"
    label var `var'_ll "`ADL_IADL' limitations (95%CI ll)"
    label var `var'_ul "`ADL_IADL' limitations (95%CI ul)"
    label var `var'_N  "`ADL_IADL' limitations (N)"
    cap drop `var'_se
    local i = `i' + 1 
}

save "$temp/efig2_estimates", replace

* reshape to long form and export to excel
gen data_id = cond(data=="HRS", 1, cond(data=="ELSA", 2, cond(data=="SHARE", 3, 4)))
egen year_id = group(year)
reshape long @m @ll @ul @N ///
        ,i(data year) j(x) string
        
local i = 1
gen var_id = .
gen varname = ""
gen panel = ""
replace x = substr(x, 1, length(x) - 1)
foreach var in $varlist0{
    local ADL_IADL = cond(inrange(`i',1,1), "ADL/IADL", ///
                        cond(inrange(`i',2,2), "ADL", "IADL"))
    replace varname = "`ADL_IADL' limitations" if x =="`var'"
    
    replace panel = cond(`i'==1, "Panel a", cond(`i'==2, "Panel b", "Panel c")) if x =="`var'"
                                     
    replace var_id = `i' if x == "`var'"
    local i = `i' + 1 
}        
order panel varname data year
sort var_id data_id year_id     
drop var_id data_id year_id x

rename m estimates_by_year    
rename ll ll_95CI
rename ul ul_95CI    
                                
export excel using "$table/descriptive/efig2.xlsx", sheetreplace firstrow(varlabels) keepcellfmt sheet("efig2_ADLIADL")


*-----------------------------------------------------------------------
* Plot eFig.2 

use "$temp/efig2_estimates", clear

gen year_v2 = year
replace year_v2 = year - 0.05 if data == "HRS" 
replace year_v2 = year + 0.05 if data == "ELSA" 
replace year_v2 = year + 0.1 if data == "CHARLS" & year == 2018


local i = 1
foreach var in $varlist0{
    local ADL_IADL = cond(inrange(`i',1,1), "ADL/IADL", ///
                        cond(inrange(`i',2,2), "ADL", "IADL"))  
    local ylab = cond(inrange(`i',1,1), "ylab(0(1)5)", ///
                        cond(inrange(`i',2,2), "ylab(0(1)3)", "ylab(0(1)3)"))      
     
    local PanelN = cond(`i'==1, "a", cond(`i'==2, "b", "c"))
    
    //local var rracaany                  
    tw line `var'_m year_v2 if data=="HRS",lc("5 113 176") lp(solid) || ///
       line `var'_m year_v2 if data=="ELSA",lc("146 197 222") lp(solid) || ///  
       line `var'_m year_v2 if data=="SHARE",lc(gray) lp(solid) || /// 
       line `var'_m year_v2 if data=="CHARLS",lc(maroon) lp(solid) || /// 
       sc `var'_m year_v2 if data=="HRS",mc("5 113 176") ms(circle) msize(*0.6)|| ///
       sc `var'_m year_v2 if data=="ELSA",mc("146 197 222") ms(diamond) msize(*0.6) || ///
       sc `var'_m year_v2 if data=="SHARE",mc(gray) ms(square) lp(solid) msize(*0.6) || /// 
       sc `var'_m year_v2 if data=="CHARLS", mc(maroon) ms(triangle) msize(*0.6) || ///
       rcap `var'_ul `var'_ll year_v2 if data=="HRS",lc("5 113 176") lp(solid) msize(*0.3) || ///
       rcap `var'_ul `var'_ll  year_v2 if data=="ELSA",lc("146 197 222") lp(solid) msize(*0.3) || ///  
       rcap `var'_ul `var'_ll  year_v2 if data=="SHARE",lc(gray) lp(solid) msize(*0.3) || /// 
       rcap `var'_ul `var'_ll  year_v2 if data=="CHARLS",lc(maroon) lp(solid) msize(*0.3) || /// 
       , legend(order(5 "HRS" 6 "ELSA" 7 "SHARE" 8 "CHARLS") ///
                pos(6) row(1) size(*1) colgap(*1) keygap(*1) region(lcolor(gray)) bmargin(t-0)) ///
        title("{bf:`PanelN'}", pos(11) color(black) size(*1.05) margin(b+0.5 t-1 l-10)) ///  
        subtitle("`ADL_IADL' Limitations", pos(12) color(black) size(*0.9) margin(b+1 t-4.5 r+0)) ///
        note("{bf:AAC}" ///
             "HRS: `=`var'_AAPC_HRS'" ///
             "ELSA: `=`var'_AAPC_ELSA'" ///
             "SHARE: `=`var'_AAPC_SHARE'" ///
             "CHARLS: `=`var'_AAPC_CHARLS'", ///
             ring(0) pos(5) linegap(2) size(*0.7) j(center) box lcolor(white) margin(r=1 t=1 l=1 b=1) ///
             bmargin(r+0 b+1)) ///
        xtitle("Year", size(*0.8)) ///
        ytitle("Number of Limitations", margin(r+0 l+0.5) size(*0.8)) /// ysc(range(0 `=`r(max)'+5'))
        xlab(2012(2)2018, labsize(*0.9)) xticks(2012(1)2018) xsc(range(2011 2019)) ///
        `ylab' ///
        scheme(modern) /// 
        saving("$temp/`var'", replace)            
    local i = `i' + 1         
}


grc1leg2 ///
   "$temp/radliadl" "$temp/radltotal" "$temp/riadlza" ///
   ,col(3) graphr(margin(zero)) xsize(*1.3) ysize(*0.8) iscale(*1.55) imargin(l=1.5 r=1.5 t=1 b=0) ///
   lsubtsize(*1.2) lmsize(*1.8) labsize(*1.2) 
   
gr export "$figure/descriptive/efig2.png", replace width(5000)   
gr export "$figure/descriptive/efig2.pdf", replace as(pdf) 





