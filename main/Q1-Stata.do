cd C:\Users\R\Desktop\Stata
clear
**Export the result of regression to word
*ssc install asdoc
sysuse auto, clear

**********************
insheet using "Data.csv", clear
*move defaulterflag to the first column
order defaulterflag
*remove defaultertype,agebin,mthincthbin,saldtbin,tenorbin  
foreach i in defaultertype agebin mthincthbin saldtbin tenorbin{ 
	drop `i'
	}
	
*remove region
drop df_metadataregionap1-df_metadataregionvellore

*remove contract
drop df_metadatacontractstatusclosed- df_metadatacontractstatusseized


*Model1: logistic 
asdoc logit defaulterflag age-df_metadatabranchvizag
asdoc margins, dydx(*)
*Model1:logistic-robust
asdoc logit defaulterflag age-df_metadatabranchvizag,vce(robust)
asdoc margins, dydx(*)

*Model3: interaction: Income& education, Age & education, Gender& profession
gen mthincth_qualhsc = mthincth*qualhsc
gen mthincth_qual_pg = mthincth*qual_pg
gen age_qualhsc = age*qualhsc
gen age_qual_pg = age*qual_pg
gen sexcode_profbus=sexcode*profbus

asdoc logit defaulterflag age-sexcode_profbus
asdoc margins, dydx(*)
*Model3:logistic-robust
asdoc logit defaulterflag age-sexcode_profbus,vce(robust)
asdoc margins, dydx(*)



