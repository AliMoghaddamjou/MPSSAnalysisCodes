cd "C:\Users\Ali\OneDrive\Projects\SYGEN-NASCIS\Feb2022"

use "Febv1.0.dta"



drop if noexam_exclude1 == 1
//drop if injury_region2 == 3

//cervical only
//keep if  injury_region1 == 1





gen injury_regionCT = .
replace injury_regionCT  = 1 if  injury_region2 == 1

gen injury_regionCTL2 = .
replace injury_regionCTL2 = 1 if  injury_region2 == 1 |  injury_region2 == 2


gen UEMFU = miss_uem_52 
replace UEMFU =  miss_uem_26 if miss_uem_52== .

gen UEMDiff12m=  UEMFU - miss_uem_0

ttest UEMDiff12m, by(steroid) 




gen LEMFU = miss_lem_52 
replace LEMFU =  miss_lem_26 if miss_lem_52== .

gen LEMDiff12m=  LEMFU - miss_lem_0

ttest LEMDiff12m, by(steroid) 



gen TOMFU= miss_mtot_52
replace TOMFU =  miss_mtot_26 if miss_mtot_52== .

gen TOMDiff12m=  TOMFU - miss_lem_0


gen ASIAFU = miss_asia_52
replace ASIAFU = miss_asia_26 if miss_asia_52 == .


gen ASIADiff12m = ASIAFU - miss_asia_0


gen ASIADiffI1 = .
replace  ASIADiffI1 = 1 if ASIADiff12m >=1 & ASIADiff12m!=.
replace ASIADiffI1 = 0 if ASIADiff12m<1

gen ASIADiffI2 = .
replace ASIADiffI2 = 1 if ASIADiff12m >=2 & ASIADiff12m!=.
replace ASIADiffI2 = 0 if ASIADiff12m<1



gen UEMFU6m = miss_uem_26 
replace UEMFU6m =  miss_uem_52 if miss_uem_26== .
gen UEMDiff6m=  UEMFU6m - miss_uem_0

gen LEMFU6m = miss_lem_26
replace LEMFU6m =  miss_lem_52 if miss_lem_26== .
gen LEMDiff6m=  LEMFU6m - miss_lem_0



gen TOMFU6m= miss_mtot_26
replace TOMFU6m =  miss_mtot_52 if miss_mtot_26== .

gen TOMDiff6m=  TOMFU6m - miss_lem_0




gen ASIAFU6m = miss_asia_26
replace ASIAFU6m = miss_asia_52 if miss_asia_26 == .


gen ASIADiff6m = ASIAFU6m - miss_asia_0


gen ASIADiffI16m = .
replace  ASIADiffI16m = 1 if ASIADiff6m >=1 & ASIADiff6m!=.
replace ASIADiffI16m = 0 if ASIADiff6m<1

gen ASIADiffI26m = .
replace ASIADiffI26m = 1 if ASIADiff6m >=2 & ASIADiff6m!=.
replace ASIADiffI26m = 0 if ASIADiff6m<1



	
	
	// Sygen Drug
	
	gen sygendrug = 0
	replace sygendrug = 1 if drug == 4
	replace sygendrug = . if drug == . 
	
	gen naloxonedrug= 0
		replace naloxonedrug = 1 if drug == 2
	replace naloxonedrug = . if drug == . 
	
. label define SteroidGive 1 "Steroid" 2 "No Steroid"

. label values steroid SteroidGive


//Steroids



		

. save "C:\Users\Ali\OneDrive\Projects\SYGEN-NASCIS\Feb2022\mergeddata.dta", replace
 export delimited using "C:\Users\Ali\OneDrive\Projects\SYGEN-NASCIS\Feb2022\mergeddata.csv", replace
export delimited using "C:\Users\Ali\OneDrive\Projects\SYGEN-NASCIS\Feb2022\MPSS\mergeddata.csv", replace	
export delimited using "C:\Users\Ali\OneDrive\Projects\SYGEN-NASCIS\Feb2022\MPSSASIA\MPSSASIA\mergeddata.csv", replace
export delimited using "C:\Users\Ali\OneDrive\Projects\SYGEN-NASCIS\Feb2022\Baysean\FullDataBaysean\mergeddata.csv", replace


