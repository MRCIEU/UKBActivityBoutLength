

local dataDir : env PROJECT_DATA
local resDir : env RES_DIR

insheet using "`dataDir'/data-for-confounder-table-43777.csv", clear comma names



log using "`resDir'/confounder-assoc.log", replace



*** convert formats from string to numeric

summ


replace ed1college = "" if ed1college == "NA"
destring ed1college, replace
replace ed2alevels = "" if ed2alevels == "NA"
destring ed2alevels, replace
replace ed3gcse = "" if ed3gcse == "NA"
destring ed3gcse, replace
replace ed4cse = "" if ed4cse == "NA"
destring ed4cse, replace
replace ed5nvq = "" if ed5nvq == "NA"
destring ed5nvq, replace
replace ed6other_profes = "" if ed6other_profes == "NA"
destring ed6other_profes, replace
replace ednone = "" if ednone == "NA"
destring ednone, replace

replace ethwhite = "" if ethwhite == "NA"
destring ethwhite, replace
replace ethblack = "" if ethblack == "NA"
destring ethblack, replace
replace ethasian = "" if ethasian == "NA"
destring ethasian, replace
replace ethother = "" if ethother == "NA"
destring ethother, replace

replace townsend ="" if townsend == "NA"
destring townsend, replace

replace winter = "" if winter == "NA"
destring winter, replace
replace summer = "" if summer == "NA"
destring summer, replace
replace spring = "" if spring == "NA"
destring spring, replace
replace autumn = "" if autumn == "NA"
destring autumn, replace

* generate binary variable denoting whether participant has died
gen deathoccur  = .
replace deathoccur = 0 if datedeath0 =="NA"
replace deathoccur = 1 if datedeath0 !="NA" & datedeath0>"2015-12-30"



replace smokestatus = "" if smokestatus == "NA"
destring smokestatus, replace
replace smokestatus = 1 if smokestatus == 2
replace smokestatus = . if smokestatus == -3

replace income = "" if income == "NA"
destring income, replace
replace income = . if income == -1
replace	income = . if income ==	-3





replace bmi = "" if bmi == "NA"
destring bmi, replace



*** age

summ age if insample == 0
summ age if insample == 1
logistic insample age


*** sex

tab sex if insample == 0
tab sex if insample == 1

* 0 female, 1 male
logistic insample sex


*** ethnicity

tab ethwhite if insample == 0   
tab ethblack if insample == 0
tab ethasian if insample == 0
tab ethother if insample == 0
tab ethwhite if insample == 1
tab ethblack if insample == 1
tab ethasian if insample == 1
tab ethother if insample == 1

logistic insample ethblack ethasian ethother



*** townsend deprivation index

summ townsend if insample == 0
summ townsend if insample == 1

logistic insample townsend


*** smoking status

tab smokestatus if insample == 0
tab smokestatus	if insample == 1

logistic insample smokestatus

*** income

tab income if insample == 0
tab income if insample == 1
*summ income if insample == 0
*summ income if insample == 1
logistic insample income


***  disease confounders


tab prev_firstocc_circ if insample == 0
tab prev_firstocc_circ if insample ==1

logistic insample prev_firstocc_circ


tab prev_firstocc_resp if insample == 0
tab prev_firstocc_resp if insample ==1

logistic insample prev_firstocc_resp

tab prev_firstocc_canc if insample == 0 
tab prev_firstocc_canc if insample == 1

logistic insample prev_firstocc_canc



*** bmi

summ bmi if insample == 0
summ bmi if insample == 1

logistic insample bmi


*** education

tab ed1college if insample == 0
tab ed2alevels if insample == 0
tab ed3gcse if insample == 0
tab ed4cse if insample == 0
tab ed5nvq if insample == 0
tab ed6other_profes if insample == 0
tab ednone if insample == 0
tab ed1college if insample == 1
tab ed2alevels if insample == 1
tab ed3gcse if insample == 1
tab ed4cse if insample == 1
tab ed5nvq if insample == 1
tab ed6other_profes if insample == 1
tab ednone if insample == 1

logistic insample ed1college ed2alevels ed3gcse ed4cse ed5nvq ed6other_profes



*** seasons

tab winter if insample == 0
tab autumn if insample == 0
tab spring if insample == 0
tab summer if insample == 0

tab winter if insample == 1
tab autumn if insample == 1
tab spring if insample == 1
tab summer if insample == 1

logistic insample autumn spring summer



*** death occurred

tab deathoccur if insample == 0
tab deathoccur if insample == 1

logistic insample deathoccur



***

log close
