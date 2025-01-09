/***********************************************************************
                Disability, Dementia and LTC (Rosbutsness)
************************************************************************

    *Title: Absence of Care Project
    *Aim of This Do-file: Robustness Check (More Disabled)
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
*       Overall Trend in Absence of Care (eFig.5: More Disabled)
*-----------------------------------------------------------------------

forvalues i = 1/4{
    
    local cty = cond(`i'==1, "HRS", cond(`i'==2, "ELSA", cond(`i'==3, "SHARE", "CHARLS")))
    local ind_id = cond(`i'==1, "hhidpn", cond(`i'==2, "idauniq", cond(`i'==3, "mergeid", "ID")))
    local weight = cond(inlist(`i', 1, 3, 4), "rwtresp", "rcwtresp")
    local pweight = cond(inlist(`i', 1, 3, 4), "[pw=rwtresp]", "[pw=rcwtresp]")
    local aweight = cond(inlist(`i', 1, 3, 4), "[aw=rwtresp]", "[aw=rcwtresp]")
    use "$data/`cty'_Data_L", clear 
         
    cap drop rrcany_e
    global varlist0 rrcany_rev rrcaany_rev rrfaany_rev /// informal/formal ADL/IADL help
                    rracany_rev rracaany_rev rrafaany_rev /// informal/formal ADL help
                    rricany_rev rricaany_rev rrifaany_rev //  informal/formal IADL help

    rename ragey* ragey 
    
    if `i'==4 {
        rename radlfive radltotal
    } 
    else{
        rename radl6a radltotal
    }
   

    keep if ragey>=50 & ever_demented==1 & radliadl_01==1
    
    * Generate numerical id
    egen newid = group(`ind_id')

        
    sum year 
    scalar year_min_all = `r(min)'
    scalar year_max_all = `r(max)'
    
    distinct year
    scalar num_year = `r(ndistinct)'
        
    mat aapc_`cty' = (0,0,0,0,0,0,0,0,0,0,0)    
    
    local j = 1
    foreach var in $varlist0{
        
        local if_cond = cond(inrange(`j',1,3), "if radltotal>=2 | riadlza>=2", ///
                            cond(inrange(`j',4,6), "if radltotal>=2", "if riadlza>=2"))
        di "`if_cond'"                    
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
        
        if `=num_nonmis_year'<=1 { // <=1 wave of observation (i.e.,China; e.g., rracaany) GEE no longer applied
            mat aapc_`cty' = (aapc_`cty'\., ., ., ., ., ., ., ., ., .,.)
            
            if "`var'"== "rrafaany_rev" | "`var'"== "rrifaany_rev"{ 
                // gender cannot be included as it predict perfectly about these outcomes (e.g., China ~100% formal care absence)
                logit `var' year ragey c.radltotal c.riadlza `pweight' `if_cond', vce(robust) nocons
            }
            else{
                logit `var' year ragey ragender c.radltotal c.riadlza `pweight' `if_cond', vce(robust) nocons
            }
                
            margins
            scalar `var'_`cty'_N = r(N)
            
            matselrc r(table) margins_`var', r(b se ll ul) 
            mat margins_`var' = margins_`var''
            
            local rownm_yr = cond(year_max!=., year_max, year_max_all)
            mat rownames margins_`var' = `rownm_yr'
            
        }
        
        else{
            * GEE        
            xtset newid year   
            xtgee `var' i.year ragey ragender c.radltotal c.riadlza [pw=weight_last] `if_cond' ///
                  , family(binomial) link(logit) corr(exchangeable) vce(robust) 
            
            * Predictive margins at each year
            margins year
            scalar `var'_`cty'_N = r(N)
            
            matselrc r(table) margins_`var', r(b se ll ul) 
            mat margins_`var' = margins_`var''

            
            * AAPC
            xtgee `var' year ragey ragender c.radltotal c.riadlza [pw=weight_last] `if_cond' ///
                  , family(binomial) link(logit) corr(exchangeable) vce(robust) 
            
            scalar b = r(table)["b", "year"]
            scalar b_se = r(table)["se", "year"]
            scalar b_ll = r(table)["ll", "year"]
            scalar b_ul = r(table)["ul", "year"]
            scalar rho = e(R)[1,2]
            
            scalar aapc = (exp(b) - 1) * 100
            scalar aapc_ll = (exp(b_ll) - 1) * 100
            scalar aapc_ul = (exp(b_ul) - 1) * 100
            
            * Significance (P-value)
            scalar z_stats = r(table)["z", "year"]
            scalar pvalue = r(table)["pvalue", "year"]
            
            mat aapc_`cty' = (aapc_`cty'\aapc, aapc_ll, aapc_ul, z_stats, pvalue, rho, e(g_avg), e(g_min), e(g_max), e(N_g), e(N))
            
        }
        mat colnames ///
            margins_`var' = `var'_m `var'_se `var'_ll `var'_ul 
            
        local j = `j' + 1   
    }


    foreach var in $varlist0{
        
        clear
        svmat2 margins_`var', names(col) rnames(x) full
        rename x year
        replace year = substr(year, 1, 4)
        order year 
        gen data = "`cty'"
        order data year 
        save "$temp/robustness/margins_`cty'_`var'", replace 
    }
    
    clear
    local j = 1
    foreach var in $varlist0{
        local use_merge = cond(`j'==1, "use", "merge 1:1 year using")
        `use_merge' "$temp/robustness/margins_`cty'_`var'"
        gen `var'_N = `=`var'_`cty'_N'
        cap drop _merge
        local j = `j' + 1 
    }
    save "$temp/robustness/margins_`cty'", replace      
    
    
    
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

    
    save "$temp/robustness/aapc_`cty'_data", replace 
}

*-----------------------------------------------------------------------
* Save AAPC for Absence of Care

use "$temp/robustness/aapc_HRS_data", clear 
append using "$temp/robustness/aapc_ELSA_data"
append using "$temp/robustness/aapc_SHARE_data"
append using "$temp/robustness/aapc_CHARLS_data"


gen AAPC = string(aapc, "%9.1f") if pvalue >=0.05
replace  AAPC = string(aapc, "%9.1f")+"*" if pvalue <0.05
replace  AAPC = string(aapc, "%9.1f")+"**" if pvalue <0.01
replace  AAPC = string(aapc, "%9.1f")+"***" if pvalue <0.001

global varlist_s rrcany_rev rrcaany_rev rricany_rev rricaany_rev
foreach var in $varlist_s {
    replace AAPC = "" if data == "SHARE" & x == "`var'"
    replace AAPC = "" if data == "SHARE" & x == "`var'"
}
replace AAPC = "NA" if AAPC == ""
replace AAPC = "NA" if pvalue==.

replace aapc = . if AAPC == "NA"
replace aapc_ll = . if AAPC == "NA"
replace aapc_ul = . if AAPC == "NA"
replace z_stats = . if AAPC == "NA"
replace pvalue = . if AAPC == "NA"
replace rho = . if AAPC == "NA"
replace g_avg = . if AAPC == "NA"
replace g_min = . if AAPC == "NA"
replace g_max = . if AAPC == "NA"
replace N = . if AAPC == "NA"
replace N_g = . if AAPC == "NA"
foreach var in $varlist0{
    di "`var'"
    valuesof AAPC if data == "HRS" & x == "`var'"
    scalar `var'_AAPC_HRS = r(values)
    
    valuesof AAPC if data == "ELSA" & x == "`var'"
    scalar `var'_AAPC_ELSA = r(values)
    
    valuesof AAPC if data == "SHARE" & x == "`var'"
    scalar `var'_AAPC_SHARE = r(values)
    
    valuesof AAPC if data == "CHARLS" & x == "`var'"
    scalar `var'_AAPC_CHARLS = r(values)
}


gen data_id = cond(data=="HRS", 1, cond(data=="ELSA", 2, cond(data=="SHARE", 3, 4)))


gen varname = ""
gen panel = ""
local i = 1
gen var_id = .
foreach var in $varlist0{
    local ADL_IADL = cond(inrange(`i',1,3), "ADL/IADL", ///
                        cond(inrange(`i',4,6), "ADL", "IADL"))
    local informal_formal = cond(inlist(`i',1,4,7), "", ///
                                cond(inlist(`i',2,5,8), "informal ", "formal "))
    replace varname = "No `informal_formal'care for `ADL_IADL'" if x =="`var'"
    
    replace var_id = `i' if x == "`var'"
    
    replace panel = cond(`i'==1, "Panel a", ///
                        cond(`i'==2, "Panel b", ///
                          cond(`i'==3, "Panel c", ///
                            cond(`i'==4, "Panel d", ///
                              cond(`i'==5, "Panel e", ///
                                cond(`i'==6, "Panel f", ///
                                   cond(`i'==7, "Panel g", ///
                                     cond(`i'==8, "Panel h", "Panel i")))))))) if x =="`var'"
    local i = `i' + 1 
}
format aapc %9.1f

sort var_id data_id
                         
keep panel varname data aapc aapc_ll aapc_ul z_stats pvalue rho N 
order panel varname data aapc aapc_ll aapc_ul z_stats pvalue rho N 

                             
export excel using "$table/robustness/efig5.xlsx", replace firstrow(variables) keepcellfmt sheet("efig5_AAPC")


*-----------------------------------------------------------------------
* Save Point Estimates (95% CI) by Year for Absence of Care

use "$temp/robustness/margins_HRS", clear 
append using "$temp/robustness/margins_ELSA"
append using "$temp/robustness/margins_SHARE"
append using "$temp/robustness/margins_CHARLS"

foreach var in $varlist0{
    replace `var'_m = `var'_m * 100
    replace `var'_se = `var'_se * 100
    replace `var'_ll = `var'_ll * 100
    replace `var'_ul = `var'_ul * 100
    replace `var'_N =. if `var'_m==.
}

global varlist_s rrcany_rev rrcaany_rev rricany_rev rricaany_rev
foreach var in $varlist_s {
    replace `var'_m = . if data == "SHARE" 
    replace `var'_se = . if data == "SHARE"  
    replace `var'_ll = . if data == "SHARE" 
    replace `var'_ul = . if data == "SHARE" 
    replace `var'_N = . if data == "SHARE"
}

* calculate summary statistis
foreach var in $varlist0{
    sum `var'_m
}

destring year, replace

* save estimates to dta for absence of care
local i = 1
foreach var in $varlist0{
    local ADL_IADL = cond(inrange(`i',1,3), "ADL/IADL", ///
                        cond(inrange(`i',4,6), "ADL", "IADL"))
    local informal_formal = cond(inlist(`i',1,4,7), "", ///
                                cond(inlist(`i',2,5,8), "informal ", "formal "))
    label var `var'_m "No `informal_formal'care for `ADL_IADL' (Estimate)"
    label var `var'_ll "No `informal_formal'care for `ADL_IADL' (95%CI ll)"
    label var `var'_ul "No `informal_formal'care for `ADL_IADL' (95%CI ul)"
    label var `var'_N "No `informal_formal'care for `ADL_IADL' (N)"
    cap drop `var'_se
    local i = `i' + 1 
}

save "$temp/robustness/efig5_estimates", replace

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
    local ADL_IADL = cond(inrange(`i',1,3), "ADL/IADL", ///
                        cond(inrange(`i',4,6), "ADL", "IADL"))
    local informal_formal = cond(inlist(`i',1,4,7), "", ///
                                cond(inlist(`i',2,5,8), "informal ", "formal "))
    replace varname = "No `informal_formal'care for `ADL_IADL'" if x =="`var'"
    
    replace panel = cond(`i'==1, "Panel a", ///
                        cond(`i'==2, "Panel b", ///
                          cond(`i'==3, "Panel c", ///
                            cond(`i'==4, "Panel d", ///
                              cond(`i'==5, "Panel e", ///
                                cond(`i'==6, "Panel f", ///
                                   cond(`i'==7, "Panel g", ///
                                     cond(`i'==8, "Panel h", "Panel i")))))))) if x =="`var'"
                                     
    replace var_id = `i' if x == "`var'"
    local i = `i' + 1 
}        
order panel varname data year
sort var_id data_id year_id     
drop var_id data_id year_id x

rename m estimates_by_year    
rename ll ll_95CI
rename ul ul_95CI    


export excel using "$table/robustness/efig5.xlsx", sheetreplace firstrow(variables) keepcellfmt sheet("efig5_AbCare")

*-----------------------------------------------------------------------
* Plot eFig.5

use "$temp/robustness/efig5_estimates", clear

gen year_v2 = year
replace year_v2 = year - 0.05 if data == "HRS" 
replace year_v2 = year + 0.05 if data == "ELSA" 
replace year_v2 = year + 0.1 if data == "CHARLS" & year == 2018


local i = 1
foreach var in $varlist0{
    local ADL_IADL = cond(inrange(`i',1,3), "ADL/IADL", ///
                        cond(inrange(`i',4,6), "ADL", "IADL"))
    local informal_formal = cond(inlist(`i',1,4,7), "", ///
                                cond(inlist(`i',2,5,8), "informal ", "formal "))     
    local fxsz = cond(inlist(`i',1,4,7), "fxsize(60)", "")      
    local PanelN = cond(`i'==1, "a", ///
                    cond(`i'==2, "b", ///
                      cond(`i'==3, "c", ///
                        cond(`i'==4, "d", ///
                          cond(`i'==5, "e", ///
                            cond(`i'==6, "f", ///
                               cond(`i'==7, "g", ///
                                 cond(`i'==8, "h", "i"))))))))
                                 
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
        title("{bf:`PanelN'}", pos(11) color(black) size(*1.05) margin(b+0.5 t-1 l-11)) ///    
        subtitle("No `informal_formal'care for `ADL_IADL'", pos(12) color(black) size(*0.9) margin(b+1 t-4.5 r+0)) ///
        note("{bf:AAPC}" ///
             "HRS: `=`var'_AAPC_HRS'" ///
             "ELSA: `=`var'_AAPC_ELSA'" ///
             "SHARE: `=`var'_AAPC_SHARE'" ///
             "CHARLS: `=`var'_AAPC_CHARLS'", ///
             ring(0) pos(3) linegap(2) size(*0.75) j(center) box lcolor(white) margin(l=0.1 r=0.1)) ///
        xtitle("Year", size(*0.8)) ///
        ytitle("Percent (%)", margin(r-2 l+0.5) size(*0.8)) /// ysc(range(0 `=`r(max)'+5'))
        xlab(2012(2)2018, labsize(*0.9)) xticks(2012(1)2018) xsc(range(2011 2020)) ///
        ylab(0(20)100, labsize(*0.9)) ///
        scheme(modern) /// 
        saving("$temp/robustness/`var'", replace)            
    local i = `i' + 1         
}


grc1leg2 ///
   "$temp/robustness/rrcany_rev" "$temp/robustness/rrcaany_rev" "$temp/robustness/rrfaany_rev" ///
   "$temp/robustness/rracany_rev" "$temp/robustness/rracaany_rev" "$temp/robustness/rrafaany_rev" ///
   "$temp/robustness/rricany_rev" "$temp/robustness/rricaany_rev" "$temp/robustness/rrifaany_rev", ///
   col(3) graphr(margin(r=1 l=0 t=1 b=0.5)) ysize(*1.4) xsize(*1.5) imargin(l=1 r=0 t=0 b=0) ///
   lmsize(*1.5) labsize(*0.8)
gr export "$figure/robustness/efig5.png", replace width(5000)   
gr export "$figure/robustness/efig5.pdf", replace as(pdf)



