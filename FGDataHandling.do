import delimited "C:\Users\Ali\OneDrive\Projects\SYGEN-NASCIS\Feb2022\FGdata.csv", encoding(UTF-8) 




gen injury_regionCT = .
replace injury_regionCT  = 1 if  injury_region2 == "Cervical & T"

gen injury_regionCTL2 = .
replace injury_regionCTL2 = 1 if  injury_region2 != ""

gen UEMFU = miss_uem_26or52


gen UEMDiff6m=  UEMFU - miss_uem_0

gen LEMFU = miss_lem_26or52


gen LEMDiff6m=  LEMFU - miss_lem_0




gen TOMFU= miss_mtot_26
replace TOMFU =  miss_mtot_52 if miss_mtot_26== .

gen TOMDiff6m=  TOMFU - miss_lem_0


gen ASIAFU = miss_asia_26
replace ASIAFU = miss_asia_52 if miss_asia_26 == .


gen ASIADiff6m = ASIAFU - miss_asia_0


gen ASIADiffI1 = .
replace  ASIADiffI1 = 1 if ASIADiff6m >=1 & ASIADiff6m!=.
replace ASIADiffI1 = 0 if ASIADiff6m<1

gen ASIADiffI2 = .
replace ASIADiffI2 = 1 if ASIADiff6m >=2 & ASIADiff6m!=.
replace ASIADiffI2 = 0 if ASIADiff6m<1



replace steroid = "Steroid" if steroid == "Yes"
replace steroid = "No Steroid" if steroid == "No"


 save "C:\Users\Ali\OneDrive\Projects\SYGEN-NASCIS\Feb2022\FGData.dta" , replace
export delimited using "C:\Users\Ali\OneDrive\Projects\SYGEN-NASCIS\Feb2022\FGDataApp\fgdata.csv", replace
export delimited using "C:\Users\Ali\OneDrive\Projects\SYGEN-NASCIS\Feb2022\FGDataApp\MPSSContinous\fgdata.csv", replace
export delimited using "C:\Users\Ali\OneDrive\Projects\SYGEN-NASCIS\Feb2022\FGDataApp\ASIA-FGData\fgdata.csv", replace