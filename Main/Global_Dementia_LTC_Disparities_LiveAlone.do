/***********************************************************************
                Disability, Dementia and LTC (Disparities)
************************************************************************

    *Title: Absence of Care Project
    *Aim of This Do-file: Disparities Analysis (Living Alone)
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
*         Absence of Care by Living Arrangement (Fig.3)
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

    * Define living alone vs not 
    cap drop rlalone_01 
    recode hhhres (1=1)(2/100=0), gen(rlalone_01) test
    
                  
    * Generate numerical id
    egen newid = group(`ind_id')
    

    
    foreach var in $varlist0{
        
        distinct year if !mi(`var') 
        scalar num_nonmis_year = `r(ndistinct)'
        
        * Obtain the terminal-year weight for longitudinal data 
        cap drop last_year weight_last 
        bys newid (year): egen last_year = max(cond(!mi(`var') & !mi(`weight'), year, . )) 
        bys newid (year): egen weight_last = max(cond(year==last_year, `weight', . )) 
        
        
        if `=num_nonmis_year'<=1 { // <=1 wave of observation (e.g., rracaany for China) GEE no longer applied
            
            if "`var'"== "rrafaany_rev" | "`var'"== "rrifaany_rev"{ 
                // gender cannot be included as it predict perfectly about these outcomes (e.g., China ~100% formal care absence)
                logit `var' i.rlalone_01 year ragey c.radltotal c.riadlza `pweight', vce(robust) nocons
                
            }
            else{
                logit `var' i.rlalone_01 year ragey ragender c.radltotal c.riadlza `pweight', vce(robust) nocons
                
            }
            
            scalar `var'_`cty'_N = e(N)
        }
        
        else{
            * GEE        
            xtset newid year   
            xtgee `var' i.rlalone_01 i.year ragey ragender c.radltotal c.riadlza [pw=weight_last] /// 
                  , family(binomial) link(logit) corr(exchangeable) vce(robust) 

            scalar `var'_`cty'_N = e(N)      
             
        }
        
        matselrc r(table) rlalone_dif_`var', r(b ll ul z pvalue) c(2)
        mat rlalone_dif_`var' = rlalone_dif_`var''
        
        
        * Predictive margins for living alone or not
        margins rlalone_01              
        matselrc r(table) rlalone_01_`var', r(b se ll ul) 
        mat rlalone_01_`var' = rlalone_01_`var''  
        
        mat colnames ///
            rlalone_dif_`var' = `var'_b `var'_ll_b `var'_ul_b `var'_z_b `var'_pvalue
            
        mat colnames ///
            rlalone_01_`var' = `var'_m `var'_se `var'_ll `var'_ul

            
    }

    foreach var in $varlist0{
        //local var test
        //local cty "HRS"
        clear
        svmat2 rlalone_01_`var', names(col) rnames(x) full
        gen rlalone_01 = 0  if x == "0bn.rlalone_01"
        replace rlalone_01 = 1 if x == "1.rlalone_01"
        gen data = "`cty'"
        order data x rlalone_01 
        save "$temp/rlalone_01_`cty'_`var'", replace 
    }
    
    clear
    local j = 1
    foreach var in $varlist0{
        local use_merge = cond(`j'==1, "use", "merge 1:1 rlalone_01 using")
        `use_merge' "$temp/rlalone_01_`cty'_`var'"
        cap drop _merge
        local j = `j' + 1 
    }
    save "$temp/rlalone_01_`cty'", replace      
    
    
    foreach var in $varlist0{
        //local var test
        //local cty "HRS"
        clear
        svmat2 rlalone_dif_`var', names(col) rnames(x) full
        gen data = "`cty'"
        drop x
        order data 
        save "$temp/rlalone_dif_`cty'_`var'", replace 
    }
    
    clear
    local j = 1
    foreach var in $varlist0{
        local use_merge = cond(`j'==1, "use", "merge 1:1 data using")
        `use_merge' "$temp/rlalone_dif_`cty'_`var'"
        cap drop _merge
        gen `var'_N = `=`var'_`cty'_N'
        local j = `j' + 1 
    }
    
    save "$temp/rlalone_dif_`cty'", replace       
    
    use "$temp/rlalone_01_`cty'", clear 
    merge m:1 data using "$temp/rlalone_dif_`cty'", nogen
    
    save "$temp/rlalone_`cty'", replace 
}

*-----------------------------------------------------------------------
* Save Estimates Stratified by Living Alone or Not

use "$temp/rlalone_HRS", clear 
append using "$temp/rlalone_ELSA"
append using "$temp/rlalone_SHARE"
append using "$temp/rlalone_CHARLS"

 
foreach var in $varlist0{
    replace `var'_m = `var'_m * 100
    replace `var'_se = `var'_se * 100
    replace `var'_ll = `var'_ll * 100
    replace `var'_ul = `var'_ul * 100
    
    replace `var'_b = exp(`var'_b) 
    replace `var'_ll_b = exp(`var'_ll_b) 
    replace `var'_ul_b = exp(`var'_ul_b) 
}

global varlist_s rrcany_rev rrcaany_rev rricany_rev rricaany_rev
foreach var in $varlist_s {
    replace `var'_m = . if data == "SHARE" 
    replace `var'_se = . if data == "SHARE"  
    replace `var'_ll = . if data == "SHARE" 
    replace `var'_ul = . if data == "SHARE" 
    
    replace `var'_b = . if data == "SHARE"
    replace `var'_ll_b = . if data == "SHARE" 
    replace `var'_ul_b = . if data == "SHARE" 
    replace `var'_z_b = . if data == "SHARE" 
    replace `var'_pvalue = . if data == "SHARE" 
    replace `var'_N = . if data == "SHARE" 
}

drop *_se*

save "$temp/fig3_estimates", replace


* reshape to long form and export to excel
gen data_id = cond(data=="HRS", 1, cond(data=="ELSA", 2, cond(data=="SHARE", 3, 4)))
rename x rlalone
reshape long @m @ll @ul @b @ll_b @ul_b @z_b @pvalue @N ///
        ,i(data rlalone) j(x) string

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
        
order panel varname data 
sort var_id    data_id rlalone_01
drop rlalone x

reshape wide m ll ul ///
        ,i(data varname) j(rlalone_01) 
        
order panel varname    data     
sort var_id data_id 
drop var_id data_id 

rename m0 m_alone0
rename ll0 ll_alone0 
rename ul0 ul_alone0 

rename m1 m_alone1 
rename ll1 ll_alone1  
rename ul1 ul_alone1  

rename b OR
rename ll_b OR_ll
rename ul_b OR_ul
rename z_b z_stats

gen m_dif = m_alone1 - m_alone0

order panel varname data ///
      m_alone1 ll_alone1 ul_alone1 m_alone0 ll_alone0 ul_alone0 m_dif ///
      OR OR_ll OR_ul z_stats pvalue N

export excel using "$table/main/fig3.xlsx", replace firstrow(variables) keepcellfmt


*-----------------------------------------------------------------------
* Plot Fig.3

use "$temp/fig3_estimates", clear
cap drop *dif

cap drop data_n id
gen data_n = 1 if data=="HRS"
replace data_n = 2 if data=="ELSA"
replace data_n = 3 if data=="SHARE"
replace data_n = 4 if data=="CHARLS"

gen id = data_n*3 - 2 if rlalone_01==1
replace id = data_n*3 - 1 if rlalone_01==0


global cty_list HRS ELSA SHARE CHARLS

foreach var in $varlist0{
    foreach cty in $cty_list{
        sum `var'_pvalue if data=="`cty'"
        
        if `r(N)'==0{ // no data (e.g.,SHARE)
           local `var'_p_`cty' = ""
           local `var'_max_`cty' = 5
        }
        else{
           local `var'_p_`cty' = cond(`r(mean)'<0.001, "***", ///
                                    cond(`r(mean)'<0.01, "**", ///
                                        cond(`r(mean)'<0.05, "*", ""))) 
           sum `var'_ul if data=="`cty'"
           local `var'_max_`cty' = `r(max)' + 5                             
        }
    } 
}


local i = 1
foreach var in $varlist0{
    local ADL_IADL = cond(inrange(`i',1,3), "ADL/IADL", ///
                        cond(inrange(`i',4,6), "ADL", "IADL"))
    local informal_formal = cond(inlist(`i',1,4,7), "", ///
                                cond(inlist(`i',2,5,8), "informal ", "formal ")) 
                                
    local PanelN = cond(`i'==1, "a", ///
                    cond(`i'==2, "b", ///
                      cond(`i'==3, "c", ///
                        cond(`i'==4, "d", ///
                          cond(`i'==5, "e", ///
                            cond(`i'==6, "f", ///
                               cond(`i'==7, "g", ///
                                 cond(`i'==8, "h", "i"))))))))                            
    /*
    cap drop `var'_m_0 
    gen `var'_m_0 = 0  
    local `var'_m_EU = cond(inlist(`i',1,2,7,8), "`var'_m_0", "`var'_m") 
    local `var'_p_EU = cond(inlist(`i',1,2,7,8), "", "``var'_p_SHARE'")    */ 
    
    local rcap_condition= cond(inlist(`i',1,2,7,8), "SHARE","")
                         
    tw (bar `var'_m id if data=="HRS" & rlalone_01==1, color("5 113 176") fcolor(white)) ///
       (bar `var'_m id if data=="ELSA" & rlalone_01==1, color("146 197 222")  fcolor(white)) ///
       (bar `var'_m id if data=="SHARE" & rlalone_01==1, color(gray) fcolor(white)) ///
       (bar `var'_m id if data=="CHARLS" & rlalone_01==1, color(maroon) fcolor(white)) ///
       (bar `var'_m id if data=="HRS" & rlalone_01==0, color("5 113 176")) ///
       (bar `var'_m id if data=="ELSA" & rlalone_01==0, color("146 197 222")) ///
       (bar `var'_m id if data=="SHARE" & rlalone_01==0, color(gray)) ///
       (bar `var'_m id if data=="CHARLS" & rlalone_01==0, color(maroon)) ///
       (rcap `var'_ul `var'_ll id if data != "`rcap_condition'"), ///
       text(``var'_max_HRS' 1 "``var'_p_HRS'") ///
       text(``var'_max_ELSA' 4 "``var'_p_ELSA'") ///
       text(``var'_max_SHARE' 7 "``var'_p_SHARE'") ///
       text(``var'_max_CHARLS' 10 "``var'_p_CHARLS'") ///
       title("{bf:`PanelN'}", pos(11) color(black) size(*1.05) margin(b+0.5 t-1 l-12)) ///         
       subtitle("No `informal_formal'care for `ADL_IADL'", pos(12) color(black) size(*0.9) margin(b+1 t-4.5 r+0)) ///
       legend(order(1 "HRS" 2 "ELSA" 3 "SHARE" 4 "CHARLS" ///
                    5 "HRS" 6 "ELSA" 7 "SHARE" 8 "CHARLS") ///
              subtitle( "Living Alone" "Not Living Alone", size(*.65) pos(9) linegap(1.3)) ///       
              pos(6) row(2) size(*0.8) colgap(*0.5) keygap(*0.7) region(lcolor(gray)) bmargin(0.1 0.1 0.1 0.1)) ///
       xlabel(none) xtitle("") ///
       ytitle("Percent (%)", margin(r-2 l+0.5) size(*0.9)) ///
       ylab(0(20)100) scheme(modern)  ///  
       saving("$temp/`var'", replace)              
    local i = `i' + 1         
}


grc1leg2 ///
   "$temp/rrcany_rev" "$temp/rrcaany_rev" "$temp/rrfaany_rev" ///
   "$temp/rracany_rev" "$temp/rracaany_rev" "$temp/rrafaany_rev" ///
   "$temp/rricany_rev" "$temp/rricaany_rev" "$temp/rrifaany_rev", ///
   col(3) graphr(margin(r=0 l=0 t=-3 b=1)) ysize(*1.2) xsize(*1.1) imargin(l=2 r=2 t=3 b=3) ///
   lsubtsize(*1.05) symysize(*1.2) symxsize(*1.2) labsize(*0.95) iscale(*0.95)
   
gr export "$figure/main/fig3.png", replace width(5000)   
gr export "$figure/main/fig3.pdf", replace as(pdf)

