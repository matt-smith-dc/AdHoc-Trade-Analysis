
* Change to relevant directory
cd ""

version 17.0
set more off
clear all

capture log close
log using "Results\ProjectLog.smcl", replace


**** Step 1. Initial processing of Trade Data
* Combine trade data files
import delimited "Project Data\1997 - 2005 Imports to US.csv"
save "Intermediate DTA Files\1997 - 2005 Imports to US.dta", replace

clear
import delimited "Project Data\2006 - 2008 Imports to US.csv"
save "Intermediate DTA Files\2006 - 2008 Imports to US.dta", replace

clear
import delimited "Project Data\2009 - 2019 Imports to US.csv"
save "Intermediate DTA Files\2009 - 2019 Imports to US.dta", replace
clear

append using "Intermediate DTA Files\1997 - 2005 Imports to US.dta" "Intermediate DTA Files\2006 - 2008 Imports to US.dta"
append using "Intermediate DTA Files\2009 - 2019 Imports to US.dta"

*Remove unnecessary fields and rename fields for matching
keep refyear reporterdesc partnerdesc cmdcode cmddesc primaryvalue
rename cmdcode sitc
rename refyear year	

*Reorganize data to have one row per year, industry
by year sitc, sort : egen float vietnam_import_value = total(primaryvalue) if partnerdesc =="Viet Nam"
by year sitc, sort : egen float world_import_value = total(primaryvalue) if partnerdesc =="World"

by year sitc, sort : egen float vietnam_imports = max(vietnam_import_value)
by year sitc, sort : egen float world_imports = max(world_import_value)

keep year reporterdesc sitc cmddesc vietnam_imports world_imports 
duplicates drop

*Final processing of trade data
*Drop year 2019, which won't be included in manufacturing data
keep if year <= 2018
save "Intermediate DTA Files\US Imports.dta", replace

gen viet_imp_bill = vietnam_imports /1000000000
graph bar (sum) viet_imp_bill, over(year, label(angle(vertical))) ytitle(`"Total Vietnamese Imports (Billions)"') title(`"Vietnamese Imports by Year"') note(`"Data from UN Comtrade"')
graph export "Results\Annual Vietnamese Imports.png", as(png) name("Graph") replace
drop viet_imp_bill

**** Step 2. Initial processing of Manufacturing Data
clear all
import excel "Project Data\nberces5818v1_s1987.xlsx", sheet("Data") firstrow
tostring sic, replace
drop if year < 1997

*Only save variables used in analysis
keep sic year prodh prodw cap
save "Intermediate DTA Files\NBER-CES Manufacturing.dta", replace

**** Step 3. Process Crosswalk
clear all
import delimited "Project Data\sitc_sic_conversion.txt", varnames(1) stringcols(_all) clear

*Remove unnecessary records
duplicates drop
drop if sic==""

*Remove sitc mappings with multiple sic codes
by sitc, sort : egen float sitc_count = count(sic)
drop if sitc_count > 1
drop sitc_count
save "Intermediate DTA Files\SITC SIC Crosswalk.dta", replace

**** Step 4. Combine Crosswalk and Manufacturing Data
*Filter to records with manufacturing and crosswalk values
joinby sic using "Intermediate DTA Files\NBER-CES Manufacturing.dta", unmatched(both) _merge(_merge)
keep if _merge==3
drop _merge
save "Intermediate DTA Files\Manufacturing Data with SITC Code.dta", replace

**** Step 5. Combine Trade and Manufacturing Data
*Filter to records with manufacturing and trade data values
clear all
use "Intermediate DTA Files\US Imports.dta"
joinby sitc year using "Intermediate DTA Files\Manufacturing Data with SITC Code.dta", unmatched(both) _merge(_merge)
keep if _merge==3
drop _merge

*Drop records missing capital stock
drop if cap == .
sort sitc year

**** Step 6. Create final columns

*Create exposure field
gen vietnam_exposure = vietnam_imports / world_imports
replace vietnam_exposure = 0 if vietnam_exposure==.

*Set treatment to industries with any Vietnamese imports in 2000
gen treatyr = 1 if year==2000 & vietnam_imports != .
by sitc, sort : egen float treat = max(treatyr)
replace treat = 0 if treat == .
drop treatyr

*Set WTO treatment to industries with any Vietnamese imports in 2006
gen treatyrwto = 1 if year==2006 & vietnam_imports != .
by sitc, sort : egen float treatwto = max(treatyrwto)
replace treatwto = 0 if treatwto == .
drop treatyrwto

*Identify world imports in 2000, used to filter main analysis
gen imports2000 = world_imports if year == 2000
by sitc, sort : egen float imp_2000 = max(imports2000)
drop imports2000

*Identify world imports in 2006, used to filter WTO analysis
gen imports2006 = world_imports if year == 2006
by sitc, sort : egen float imp_2006 = max(imports2006)
drop imports2006

*Additional columns used in regression analyses
gen post = year >= 2002
gen postwto = year >= 2007
gen lncap = ln(cap)
gen hrwage = prodw/prodh
gen sector = substr(sitc,1,1)
gen post_treat = post*treat
gen post_treat_wto = postwto*treatwto
tab sector, generate(manuf_)

save "Intermediate DTA Files\Combined Data.dta", replace
**********************************************************************************************************


preserve

*Main BTA analysis only uses data up to 2006
*It excludes industries with no total imports in the year 2000
drop if year >= 2007
drop if imp_2000 == .

asdoc sum year hrwage lncap treat post manuf_*

*Regression results for hourly wages
eststo: quietly reg hrwage post treat post_treat, vce(cluster sector)
eststo: quietly reg hrwage post treat post_treat lncap, vce(cluster sector)
eststo: quietly reg hrwage post treat post_treat lncap i.manuf_*, vce(cluster sector)
esttab, se star(* 0.10 ** 0.05 *** 0.01) indicate("Industry FE = *.manuf_*") drop(_cons)
eststo clear


*egen treat_mean_hrwage = mean(hrwage), by(treat year)
*separate treat_mean_hrwage, by(treat)

*twoway (line treat_mean_hrwage0 year if treat==0, sort) (line treat_mean_hrwage1 year, sort), ytitle(`"Hourly Wage"') xtitle(`"Year"') xline(2002) title(`"Treatment and Control Group Production Wages"') note(`"Reference line at year of treatment, 2002"') legend(order(1 "Control Group" 2 "Treatment Group"))

*graph export "Results\Wage Trends by Treatment.png", as(png) name("Graph") replace

*areg hrwage b1999.year##i.treat lncap, absorb(sector) vce(cluster sector)
eststo: quietly reg hrwage b1999.year##i.treat lncap i.manuf*, vce(cluster sector)
esttab , star(* 0.10 ** 0.05 *** 0.01) drop(_cons *.manuf_* lncap *.year 0.treat  1.treat *#0.treat) varwidth(30)
eststo clear

***************************************************************************

***WTO Policy Analysis
restore

*Main BTA analysis only uses data starting 2003
*It excludes industries with no total imports in the year 2006
drop if imp_2006 == .
drop if year < 2003

asdoc sum year hrwage lncap treatwto post manuf_*

*Regression results for hourly wages
eststo: quietly reg hrwage postwto treatwto post_treat_wto, vce(cluster sector)
eststo: quietly reg hrwage postwto treatwto post_treat_wto lncap, vce(cluster sector)
eststo: quietly reg hrwage postwto treatwto post_treat_wto lncap i.manuf_*, vce(cluster sector)
esttab, se star(* 0.10 ** 0.05 *** 0.01) indicate("Industry FE = *.manuf_*") drop(_cons)
eststo clear


eststo: quietly reg hrwage b2005.year##i.treatwto lncap i.manuf*, vce(cluster sector)
esttab , se star(* 0.10 ** 0.05) drop(_cons *.manuf_* lncap *.year 0.treatwto  1.treatwto *#0.treatwto) varwidth(30)


*areg hrwage b2005.year##i.treatwto lncap, absorb(sector) vce(cluster sector)

log close
