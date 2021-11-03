cd C:\Users\R\Desktop\Stata
clear
**Export the result of regression to word
*ssc install asdoc
sysuse auto, clear

**********************
insheet using "Data.csv", clear

*move defaultertype to the first column
order defaultertype

*remove defaultertype,agebin,mthincthbin,saldtbin,tenorbin  
foreach i in defaulterflag agebin mthincthbin saldtbin tenorbin{ 
	drop `i'
	}
*remove region
drop df_metadataregionap1-df_metadataregionvellore

*remove contract
drop df_metadatacontractstatusclosed- df_metadatacontractstatusseized

*Model4: interaction: Income& education, Age & education, Gender& profession
gen mthincth_qualhsc = mthincth*qualhsc
gen mthincth_qual_pg = mthincth*qual_pg
gen age_qualhsc = age*qualhsc
gen age_qual_pg = age*qual_pg
gen sexcode_profbus=sexcode*profbus

asdoc ologit defaultertype age-sexcode_profbus
asdoc margins, dydx(*)
*Model4:logistic-robust
asdoc ologit defaultertype age-sexcode_profbus,vce(robust)
asdoc margins, dydx(*)
