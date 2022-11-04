

*******************************************************************************
***	This file contains code to produce the Tables and Figures
***	found in Ericson, Keith. "Consumer Inertia and Firm Pricing in
***	 the Medicare Part D Prescription Drug Insurance Exchange"
***
***	Requires: Data_main.dta, Data_subsidyinfo.dta
***	Produces: Table#.xml (#=2-4) and Figure#.gph (#=1,5)
***	Table 1 in the text is copied from descriptive statistics
***	(this location is noted in this do file)
***
***	Structure: 
***		PART 1: Prepare program and supplemental data
***		PART 2: Prepare main data (create derivative variables used later) 
***		PART 3a: Prepare Tables & Figures
***		PART 3b: Do additional data manipulation for RD results
***		PART 3c: Produce regression discontinuity (RD) Tables & Figures
***	File is most easily viewed using a text editor with indent-based folding
***
*** Updates: Revised for ECON 771 Assignment 3 by MW 10/30/22
***
*******************************************************************************

cd "C:\Users\mwetze2\OneDrive - Emory University\PhD Coursework\2022b_Fall\ECON 771\Assignments\Assignment 3"

set matsize 400

net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace

ssc install estout

***We will make frequent use of this plugin to produce clear tables
ssc install outreg2
		
***PART 1: Prepare program and supplemental data
	***Define a useful program
		capture program drop defineSample
		program define defineSample
			capture drop inLastSampletemp inLastSample
			gen inLastSampletemp =1 if e(sample)
				replace inLastSampletemp = 0 if inLastSampletemp!=1
			egen inLastSample = max(inLastSampletemp),by(uniqueID)
		end
	***Prepare some supplemental data
		use "Raw_Data\Data_subsidyinfo.dta", replace
			reshape long s, i(PDPregion) j(year)
			sort PDPregion year
			
		save Raw_Data\theSubsidies, replace
	


***PART 2: Prepare main data (create derivative variables used later)

	use Raw_Data\Data_main.dta
	
	***The var "isin" will tell me whether that plan existed in 200x
		gen isin = 1
	
	***Now, create information about companies
		***Create a numeric version of the text string; useful for stata
		egen firmID = group(orgParentCode)
		egen uniqueIDNum = group(uniqueID)
		
			***Create variables that show when a firm began offering a plan
				egen firstYrExist_F0 = min(year),by(firmID)
				label var firstYrExist_F0 "The first year this plan's company offered a plan"
						
				gen thisPlansExist_F0 = 1 if year >firstYrExist_F0 
				replace thisPlansExist_F0 =0 if thisPlansExist_F0 !=1
				label var thisPlansExist_F0  "=1 if company previously offerred a plan in any state,=0 if else"
				
				***Want to know the first year that company offered a plan in that state.
				egen firstYrExistState_F0 = min(year),by(firmID state)
				label var firstYrExistState_F0 "The first year this plan's company offered a plan, firm def 0"
					
				gen thisPlansExistState_F0 = 1 if year >firstYrExistState_F0 
				replace thisPlansExistState_F0 =0 if thisPlansExistState_F0 !=1
				label var thisPlansExistState_F0  "=1 if company (def 0) previously offerred a plan in any state,=0 if else"
		
	***Generate descriptive cohort variables
		egen minYear = min(year),by(uniqueID)
		
		foreach yr in 2006 2007 2008 2009 2010{
			gen cohort`yr' = 1 if minYear == `yr'
		}
	
		gen cohort = .
		foreach x in 2006 2007 2008 2009 2010{
			replace cohort = `x' if cohort`x'==1
		}
		
		replace cohort2006=0 if cohort2006 !=1
		replace cohort2007=0 if cohort2007 !=1
		replace cohort2008=0 if cohort2008 !=1
		replace cohort2009=0 if cohort2009 !=1
		replace cohort2010=0 if cohort2010 !=1
	
	
		***Create the year of plan's existence
		gen yearOfPlan = year-cohort+1
		replace yearOfPlan = . if yearOfPlan<1 
	
		tab year, generate(Dyear)
		tab yearOfPlan, generate(DOfPlan)
		rename DOfPlan1 _DOfPlan1
		rename Dyear1 _Dyear1
		egen maxYear = max(year),by(uniqueID)
	
		***Indicator for if a plan is basic every year until YR
		gen isBasic = 1 if benefit=="B"
		replace isBasic = 0 if benefit!="B"
		egen minIsBasic = min(isBasic),by(uniqueID)
	
		foreach yr in 2007 2008 2009 2010{
			gen isBasic`yr'= isBasic
			replace isBasic`yr'=1 if year >`yr'
			egen minIsBasic`yr' = min(isBasic`yr'),by(uniqueID)
			drop isBasic`yr'
		} 
	
	***Handle Premium
		label var premium "Monthly Premium"
		gen lnPremium = ln(premium)
	
	***Create Variables Describing plan detail
		***Various types of basic plans
			gen DS = 1 if btypedetail =="DS"
			replace DS = 0 if btypedetail !="DS"
			gen AE = 1 if btypedetail =="AE"
			replace AE = 0 if btypedetail !="AE"
		
		foreach y in 2006 2007 2008 2009 2010{ 
			gen AE`y' = 1 if year == `y' & btypedetail =="AE"
			replace AE`y'= 0 if AE`y'!=1
			gen DS`y' = 1 if year == `y' & btypedetail =="DS"
			replace DS`y'= 0 if DS`y'!=1
		}	
	
		***Create the interaction between BA and deductible amount
		***AE and DS always have the same deductible (varies by year). 
		****But BA may have varying deductibles
		
		***Generate Deductible Groups
			gen cDeduct0 = 1 if deductible ==0
			gen cDeduct1_50 = 1 if deductible >0 & deductible<=50
			gen cDeduct51_100 = 1 if deductible >50 & deductible<=100
			gen cDeduct101_150 = 1 if deductible >100 & deductible<=150
			gen cDeduct151_200 = 1 if deductible >150 & deductible<=200
			gen cDeduct201_250 = 1 if deductible >200 & deductible<=250
			gen cDeduct251_300 = 1 if deductible >250 & deductible<=300
			gen cDeduct301_ = 1 if deductible >300 & deductible !=.
		
			foreach x in cDeduct0  cDeduct1_50   cDeduct51_100  cDeduct101_150 cDeduct151_200 cDeduct201_250 cDeduct251_300 cDeduct301_{
				replace `x' = 0 if `x' ==.
			}
			
			foreach x in cDeduct0  cDeduct1_50   cDeduct51_100  cDeduct101_150 cDeduct151_200 cDeduct201_250 cDeduct251_300 cDeduct301_{
				foreach y in 2006 2007 2008 2009 2010{ 
					gen `x'`y' = `x' if year == `y'
					replace `x'`y'= 0 if  year != `y'
				}
			}
			assert cDeduct0  + cDeduct1_50 +   cDeduct51_100   + cDeduct101_150  + cDeduct151_200 +  cDeduct201_250 + cDeduct251_300+cDeduct301_==1 if deductible !=.
		
			***And focus only on the BA plans
			foreach x in cDeduct0  cDeduct1_50   cDeduct51_100  cDeduct101_150 cDeduct151_200 cDeduct201_250 cDeduct251_300 cDeduct301_{
				foreach y in 2006 2007 2008 2009 2010{ 
					gen BA`x'`y' = `x' if year == `y' & btypedetail=="BA"
					replace BA`x'`y'= 0 if  BA`x'`y'==.
				}
			}
	
			
	****For lagged data analysis, create lagged plan type/deductible groups
		xtset uniqueIDNum year, yearly
		sort uniqueIDNum year
		***Stata lags can't handle string variables lagged. Must do manually
			gen L4btypedetail=btypedetail[_n-4] if uniqueID==uniqueID[_n-4] & year == 2010 
			gen L3btypedetail=btypedetail[_n-3] if uniqueID==uniqueID[_n-3] & (year == 2010 | year == 2009) 
			gen L2btypedetail=btypedetail[_n-2] if uniqueID==uniqueID[_n-2] & (year == 2010 | year == 2009 | year ==2008)
			gen L1btypedetail=btypedetail[_n-1] if uniqueID==uniqueID[_n-1] & (year == 2010 | year == 2009 | year ==2008 | year==2007)
			gen L0btypedetail=btypedetail
			
			gen L4benefit=benefit[_n-4] if uniqueID==uniqueID[_n-4] & year == 2010 
			gen L3benefit=benefit[_n-3] if uniqueID==uniqueID[_n-3] & (year == 2010 | year == 2009) 
			gen L2benefit=benefit[_n-2] if uniqueID==uniqueID[_n-2] & (year == 2010 | year == 2009 | year ==2008)
			gen L1benefit=benefit[_n-1] if uniqueID==uniqueID[_n-1] & (year == 2010 | year == 2009 | year ==2008 | year==2007)
			gen L0benefit=benefit
	
		
		foreach L in 0 1 2 3 4{
			***For basic types only
			gen L`L'DS = 1 if L`L'btypedetail =="DS"
			replace L`L'DS = 0 if L`L'btypedetail !="DS"
			gen L`L'AE = 1 if L`L'btypedetail =="AE"
			replace L`L'AE = 0 if L`L'btypedetail !="AE"
		
			****BUT BA may have varying deductibles
			gen  L`L'BA_0 = 1 if  L`L'.deductible ==0 & L`L'btypedetail =="BA"
			gen  L`L'BA_1_99 = 1 if L`L'.deductible >0 & L`L'.deductible<100 & L`L'btypedetail =="BA"
			gen  L`L'BA_100 = 1 if L`L'.deductible==100 & L`L'btypedetail =="BA"
			gen  L`L'BA_101_99 = 1 if L`L'.deductible >100 & L`L'.deductible<200 & L`L'btypedetail =="BA"
			gen  L`L'BA_200_49 = 1 if L`L'.deductible >=200 & L`L'.deductible<250 & L`L'btypedetail =="BA"
			gen  L`L'BA_250Up = 1 if L`L'.deductible >=250 & L`L'.deductible!=. & L`L'btypedetail =="BA"
	
			foreach x in L`L'BA_0 L`L'BA_1_99 L`L'BA_100 L`L'BA_101_99 L`L'BA_200_49 L`L'BA_250Up{
				replace `x' = 0 if `x' !=1
			}
		
			***Make sure everyone is in a category
			egen x = rowtotal(L`L'DS-L`L'BA_250Up)
			assert x==1 if L`L'benefit =="B" & L`L'btypedetail!=""
			drop x
		}
	
	***Work with Enrollment Data
		egen double stateYrEnroll =sum(enrollment),by(state year)
		gen share = enrollment/stateYrEnroll
		gen double lnS = log(share)
		
		gen enrollmentNonLIS = enrollment-enrollmentLIS
		egen double stateYrEnrollNonLIS =sum(enrollmentNonLIS),by(state year)
		gen shareNonLIS = enrollmentNonLIS/stateYrEnrollNonLIS
		gen double lnSNonLIS = log(shareNonLIS)
		
		***When LIS enrollment is missing b/c <10, it is imputed to be 5
		gen enrollmentNonLISimpute = enrollment-enrollmentLISimpute
		egen double stateYrEnrollNonLISimpute =sum(enrollmentNonLISimpute),by(state year)
		
		gen shareNonLISimpute = enrollmentNonLISimpute/stateYrEnrollNonLISimpute
		gen double lnS_std = log(shareNonLISimpute)
		


*****PART 3A: Produce Paper's Figures and Tables
	
	***Table 1: These commands can be used to produce the Descriptive Statistics  
		gen EBene =1 if benefit == "E"
		replace EBene =0 if benefit == "B"
	
		tabstat premium if yearOfPlan ==1,by(cohort) stats(mean sd N)
		tabstat deductible if yearOfPlan ==1,by(cohort) stats(mean sd)	
		tabstat EBene if yearOfPlan ==1,by(cohort) stats(mean sd)	
	
		
		egen tagfirmIDAll = tag(firmID)
		tab tagfirmIDAll
		
		egen tagfirmID = tag(cohort firmID)
		tab tagfirmID cohort
		
		tabstat thisPlansExist_F0 if yearOfPlan==1,by(cohort) stats(mean sd N)
		tabstat thisPlansExistState_F0 if yearOfPlan==1,by(cohort) stats(mean sd N)
		
		tab contractId if cohort ==2007
		tab contractId if cohort ==2008
		tab contractId if cohort ==2009
		tab contractId if cohort ==2010
		
		* Save the data to produce a formatted table in SAS
		save "Out_Data\PreppedData.dta"
		
	
                                                                                                                                          


*****PART 3B: Prepare for Regression Discontinuity Results
	sort PDPregion year
	merge PDPregion year using "Raw_Data\theSubsidies"
	assert _merge==3
	drop _merge
	rename s LISsubsidy

	gen LISPremium =  premium - LISsubsidy
	***Not all proposed plans are actually such
	gen proposedBenchmarkPlan = 1 if LISPremium <= 0
	replace proposedBenchmarkPlan = 0 if  proposedBenchmarkPlan != 1
		
	sum LISPremium, detail
	sum LISPremium if LIS==0, detail
	sum LISPremium if LIS==1, detail
	****SOME MISCATEGORIZATION 
	generate ProblemObs =1 if LISPremium < 0 & LIS == 0
	replace ProblemObs =2 if LISPremium > 0 & LIS == 1
	tab ProblemObs
	***Why positive premiums when LIS == 1? DEMINIMUM PROVISION: note that this problem only occurs in 2007+
		sum LISPremium if ProblemObs==2, detail 
		tab ProblemObs year
	***Why Negative premiums when seemingly not eligible? 
		sum LISPremium if ProblemObs==1, detail 
		tab benefit if  ProblemObs==1
		tab btypedetail if  ProblemObs==1
	****FIX: Not eligible for LIS if benefit == E
		replace LISPremium = . if benefit == "E"
		replace proposedBenchmarkPlan =.  if benefit == "E" 
		
	***Polynomials
		gen LISPremiumSq = LISPremium*LISPremium
		gen LISPremiumCub= LISPremium*LISPremium*LISPremium
		gen LISPremiumQuart= LISPremium*LISPremium*LISPremium*LISPremium
		***Interacted with Status
		gen LISPremiumSq_IS = LISPremium*LISPremium*LIS
		gen LISPremiumCub_IS= LISPremium*LISPremium*LISPremium*LIS
		gen LISPremiumQuart_IS= LISPremium*LISPremium*LISPremium*LISPremium*LIS
		
		gen premiumSq= premium*premium
		gen premiumCub= premium*premium*premium
		gen premiumQuart =premium*premium*premium*premium

	***Splitting by either side of the benchmark
		gen LISPremiumNeg =LISPremium if LISPremium<=0
			replace LISPremiumNeg = 0 if LISPremium>0
		gen LISPremiumPos =LISPremium if LISPremium>=0
			replace LISPremiumPos = 0 if LISPremium<0
		foreach x in Neg Pos{
			gen LISPremium`x'Sq = LISPremium`x'*LISPremium`x'
			gen LISPremium`x'Cub = LISPremium`x'*LISPremium`x'*LISPremium`x'
			gen LISPremium`x'Quart = LISPremium`x'*LISPremium`x'*LISPremium`x'*LISPremium`x'
		}	
	***Lags
	xtset uniqueIDNum year, yearly
	foreach x in 1 2 3 4{
		gen L`x'LISPremium = L`x'.LISPremium
		gen L`x'LISPremiumNeg = L`x'.LISPremiumNeg
		gen L`x'LISPremiumPos = L`x'.LISPremiumPos
		
		gen  L`x'LISPremiumNegSq =  L`x'.LISPremiumNegSq
		gen  L`x'LISPremiumNegCub = L`x'.LISPremiumNegCub
		gen  L`x'LISPremiumNegQuart= L`x'.LISPremiumNegQuart

		gen  L`x'LISPremiumPosSq =  L`x'.LISPremiumPosSq
		gen  L`x'LISPremiumPosCub = L`x'.LISPremiumPosCub
		gen  L`x'LISPremiumPosQuart= L`x'.LISPremiumPosQuart
			}
			
	
	*** MW Edit: Create a variable for LISPremium in 2006 that doesn't depend on years since 2006
		gen LISPremium_temp06 = LISPremium if year==2006
		egen LISPremium_06 = max(LISPremium_temp06), by (uniqueIDNum)
		
		gen LISPremiumN_temp06 = LISPremiumNeg if year==2006
		egen LISPremiumNeg_06 = max(LISPremiumN_temp06), by (uniqueIDNum)
		
		gen LISPremiumP_temp06 = LISPremiumPos if year==2006
		egen LISPremiumPos_06 = max(LISPremiumP_temp06), by (uniqueIDNum)
		
		drop LISPremium_temp06 LISPremiumN_temp06 LISPremiumP_temp06
	
	***Variable for attrition (could be merged with another plan)
		foreach x in 2006 2007 2008 2009 {
			gen attritBy`x' =1 if maxYear <=`x'
			replace attritBy`x' =0  if maxYear >`x'
		}
	
	***Do past indicators for LIS status
		gen alwaysLIS07 = 1 if LIS ==1 & L1.LIS==1 & year ==2007
			replace alwaysLIS07 = 0 if alwaysLIS07 !=1
		gen alwaysLIS08 = 1 if LIS ==1 & L1.LIS==1 & L2.LIS==1 & year ==2008
			replace alwaysLIS08 = 0 if alwaysLIS08 !=1
		gen alwaysLIS09 = 1 if LIS ==1 & L1.LIS==1 & L2.LIS==1 & L3.LIS==1 & year ==2009
			replace alwaysLIS09 = 0 if alwaysLIS09 !=1
		gen alwaysLIS10 = 1 if LIS ==1 & L1.LIS==1 & L2.LIS==1 & L3.LIS==1 & L4.LIS==1 & year ==2010
			replace alwaysLIS10 = 0 if alwaysLIS10 !=1              
	***Never LIS again (after 2006)
		gen neverLIS07 = 1 if LIS ==0 & year ==2007
			replace neverLIS07 = 0 if neverLIS07 !=1
		gen neverLIS08 = 1 if LIS ==0 & L1.LIS==0 & year ==2008
			replace neverLIS08 = 0 if neverLIS08 !=1
		gen neverLIS09 = 1 if LIS ==0 & L1.LIS==0 & L2.LIS==0 & year ==2009
			replace neverLIS09 = 0 if neverLIS09 !=1
		gen neverLIS10 = 1 if LIS ==0 & L1.LIS==0 & L2.LIS==0 & L3.LIS==0 & year ==2010
			replace neverLIS10 = 0 if neverLIS10 !=1              
		
	***CREATE RD Windows		
		******Now, split sample above and below the benchmark subsidy: +/- $10
			sum LISPremium
			***Approx One standard deviation
			gen RDwindow = 1 if LISPremium >= -10 & LISPremium <= 10
			replace RDwindow = 0 if RDwindow != 1  	
			
			gen belowBench = 1 if LISPremium <=0 & RDwindow == 1
			replace belowBench = 0 if LISPremium >0 & RDwindow == 1
			
			***Cleanest experiment: Look at plans above/below the benchmark in 2006
			gen belowBench2006Temp = 1 if belowBench==1 & year ==2006
				replace belowBench2006Temp =0 if belowBench2006Temp!=1
			gen RDwindow2006Temp = 1 if RDwindow==1 & year ==2006
				replace RDwindow2006Temp =0 if RDwindow2006Temp!=1
			gen LISsubsidy2006Temp = LISsubsidy if year == 2006
				replace LISsubsidy2006Temp = 0 if year != 2006
		
			egen belowBench2006=max(belowBench2006Temp), by(uniqueID)
			egen RDwindow2006=max(RDwindow2006Temp), by(uniqueID)
			egen LISsubsidy2006=max(LISsubsidy2006Temp), by(uniqueID)
		
		***Now, take a tighter RD: $4 (optimal for lnS) 	
			local x = 2
			***Create tighter windows
			gen RDwindow`x' = 1 if LISPremium >= -4 & LISPremium <= 4
			replace RDwindow`x' = 0 if RDwindow`x' != 1  	
			
			gen belowBench`x' = 1 if LISPremium <=0 & RDwindow`x' == 1
			replace belowBench`x' = 0 if LISPremium >0 & RDwindow`x' == 1
			
			***Cleanest experiment: Look at plans above/below the benchmark in 2006
			gen belowBench2006Temp`x' = 1 if belowBench`x'==1 & year ==2006
				replace belowBench2006Temp`x' =0 if belowBench2006Temp`x'!=1
			gen RDwindow2006Temp`x' = 1 if RDwindow`x'==1 & year ==2006
				replace RDwindow2006Temp`x' =0 if RDwindow2006Temp`x'!=1
		
			egen belowBench2006`x'=max(belowBench2006Temp`x'), by(uniqueID)
			egen RDwindow2006`x'=max(RDwindow2006Temp`x'), by(uniqueID)
		

			
	***Now, just another variable for largest window (for parallelism)
		gen RDwindow20061 =RDwindow2006

	***Now, examine interactions:
	gen bench0607 = 1 if belowBench2006==1 & LIS ==1 & year ==2007
		replace bench0607  = 0 if bench0607==.  
	gen bench06Not07 = 1 if belowBench2006==1 & LIS ==0 & year ==2007
		replace bench06Not07  = 0 if bench06Not07==.
	gen benchNot06Yes07 = 1 if belowBench2006==0 & LIS ==1 & year ==2007
		replace benchNot06Yes07 = 0 if benchNot06Yes07==.

		foreach yr in 2007 2008 2009 2010{
			gen bench06`yr'= 1 if belowBench2006==1 & LIS ==1 & year ==`yr'
				replace bench06`yr'  = 0 if bench06`yr'==.  
			gen bench06Not`yr' = 1 if belowBench2006==1 & LIS ==0 & year ==`yr'
				replace bench06Not`yr'  = 0 if bench06Not`yr'==.
			gen benchNot06Yes`yr' = 1 if belowBench2006==0 & LIS ==1 & year ==`yr'
				replace benchNot06Yes`yr' = 0 if benchNot06Yes`yr'==.
		}



*****PART 3C: Creates RD Tables and Figures
	* Set up
		label variable belowBench2006 "Below benchmark, 2006"
		label variable LISPremiumNeg_06 "Premium less subsidy, 2006 Below benchmark"
		label variable LISPremiumPos_06 "Premium less subsidy, 2006 Above benchmark"	
		
	***Creates Table 3
		capture estimates drop *
		***Set bandwidth (the x is actually a naming convention and not the bandwidth)
			local x = 2
			*** Local linear
			xi: reg lnS belowBench2006 LISPremiumNeg_06 LISPremiumPos_06 if year ==2010 & RDwindow2006`x'==1 ,cluster(firmID)
			estimates store LinRD`x'iS10                                                                     
			xi: reg lnS belowBench2006 LISPremiumNeg_06 LISPremiumPos_06 if year ==2009 & RDwindow2006`x'==1 ,cluster(firmID)
			estimates store LinRD`x'iS09                                                                     
			xi: reg lnS belowBench2006 LISPremiumNeg_06 LISPremiumPos_06 if year ==2008 & RDwindow2006`x'==1 ,cluster(firmID)
			estimates store LinRD`x'iS08                                                                     
			xi: reg lnS belowBench2006 LISPremiumNeg_06 LISPremiumPos_06 if year ==2007 & RDwindow2006`x'==1 ,cluster(firmID)
			estimates store LinRD`x'iS07
			xi: reg lnS belowBench2006 LISPremiumNeg_06 LISPremiumPos_06 if year ==2006 & RDwindow2006`x'==1 ,cluster(firmID)
			estimates store LinRD`x'iS06
			
		esttab LinRD`x'iS06 LinRD`x'iS07 LinRD`x'iS08 LinRD`x'iS09 LinRD`x'iS10  using Analysis_output/Table3-PanelA.rtf, replace            compress label nonotes  se                        ///
	   title("Table 3: Effect of LIS Benchmark Status in 2006 on Plan Enrollment") ///
	   mtitles("2006" "2007" "2008" "2009" "2010") ///
	   star(* 0.1 ** 0.05 *** 0.01) obslast                 /// 
	   addnotes("*** 1% ** 5% * 10%" "" "")
	   	   
		
		*** With polynomials
			xi: reg lnS belowBench2006 L4.LISPremiumNeg L4.LISPremiumPos L4.LISPremiumNegSq L4.LISPremiumPosSq if year ==2010 & RDwindow2006`x'==1 ,cluster(firmID)
			estimates store iS10OptRD`x'                                                                                                                                                   
			xi: reg lnS belowBench2006 L3.LISPremiumNeg L3.LISPremiumPos L3.LISPremiumNegSq L3.LISPremiumPosSq if year ==2009 & RDwindow2006`x'==1 ,cluster(firmID)
			estimates store iS09OptRD`x'                                                                                                                                                   
			xi: reg lnS belowBench2006 L2.LISPremiumNeg L2.LISPremiumPos L2.LISPremiumNegSq L2.LISPremiumPosSq if year ==2008 & RDwindow2006`x'==1 ,cluster(firmID)
			estimates store iS08OptRD`x'                                                                                                                                                   
			xi: reg lnS belowBench2006 L1.LISPremiumNeg L1.LISPremiumPos L1.LISPremiumNegSq L1.LISPremiumPosSq  if year ==2007 & RDwindow2006`x'==1 ,cluster(firmID)
			estimates store iS07OptRD`x'
			xi: reg lnS belowBench2006 LISPremiumNeg LISPremiumPos LISPremiumNegSq LISPremiumPosSq if year ==2006 & RDwindow2006`x'==1 ,cluster(firmID)
			estimates store iS06OptRD`x'				
                                                                                    

		
esttab iS06OptRD`x' iS07OptRD`x' iS08OptRD`x' iS09OptRD`x' iS10OptRD`x'  using Analysis_output/Table3-PanelB.rtf, replace compress label nonotes  se   title("Table 3 Panel B: Effect of LIS Benchmark Status in 2006 on Plan Enrollment - Panel B") ///
	   mtitles("2006" "2007" "2008" "2009" "2010") ///
	   star(* 0.1 ** 0.05 *** 0.01) obslast                 /// 
	   addnotes("*** 1% ** 5% * 10%" "" "")

	
		capture estimates drop *
	
	***Figure 3: RD-Enrollment
		***Algorithm to compute N bins with range +/- h
			local nBinOver2 = 20
			local h = 10
			local step = `h'/((`nBinOver2'))
			dis "StepSize `step'"
			capture drop theBinAl theBinAlTemp 
			gen  theBinAlTemp= .
			***Only Does this in 2006, then expands to other years
			***Handles Negative Numbers
			forval x = 1/`nBinOver2'{
				replace theBinAlTemp = -`step'*`x' if LISPremium >= (-`step'*(`x')) & LISPremium <(-`step'*(`x'-1)) & year == 2006 
				dis "`x'"
			}
			***Does Positive Numbers
			forval x = 1/`nBinOver2'{
				replace theBinAlTemp = `step'*`x' if LISPremium >= (`step'*(`x')) & LISPremium <(`step'*(`x'+1))  & year == 2006
				dis "`x'"
			}
			egen theBinAl = max(theBinAlTemp),by(uniqueID)
	
				
			local year =2006
			local P = ""
	
			***Select RD Window for scatter 
			local ScatWin =1
			***Select RD Window for local linear regression
			local RegWin  =2
			***Select RD Window for Polynomial regression
			local PolyWin  =1
			
			capture drop lnSHat lnSHatAlt lnSHatAltPoly
			***Regress to get Mean in Each Bin, for scatter
			xi: reg lnS i.theBinAl if year ==`year' & RDwindow2006`ScatWin'==1 & `P'benefit =="B",cluster(firmID)
			predict lnSHat if year ==`year'
			
			***Regress to get local linear line
			xi: reg lnS belowBench2006 `P'LISPremiumNeg `P'LISPremiumPos if year ==`year' & RDwindow2006`RegWin'==1 & `P'benefit =="B",cluster(firmID)
			predict lnSHatAlt if year ==`year'
	
			***Regress to get line: from quartic
			xi: reg lnS belowBench2006 `P'LISPremiumNeg `P'LISPremiumPos `P'LISPremiumNegSq `P'LISPremiumPosSq `P'LISPremiumNegCub `P'LISPremiumPosCub `P'LISPremiumNegQuart `P'LISPremiumPosQuart if year ==`year' & RDwindow2006`PolyWin'==1 & `P'benefit =="B",cluster(firmID)
			predict lnSHatAltPoly if year ==`year'
		
			local ytitle = "Log Enrollment Share, 2006"		
			twoway (scatter lnSHat theBinAl if year ==`year' & RDwindow2006`ScatWin'==1 & `P'benefit =="B") (line lnSHatAlt `P'LISPremium if year ==`year' & RDwindow2006`RegWin'==1 & `P'benefit =="B",sort lpattern(dash) lcolor(gray)) (line lnSHatAltPoly `P'LISPremium if year ==`year' & RDwindow2006`PolyWin'==1 & `P'benefit =="B",sort lpattern(solid) lcolor(black)), legend(order(2 3) label(2 "Local Linear") label(3 "Quartic Polynomial")) xtitle("Monthly Premium - LIS Subsidy, 2006") ytitle("`ytitle'") 
			graph export Analysis_output/Figure3.png, replace
			
	*** Question 3, change partitions for Figure 3
			***Algorithm to compute N bins with range +/- h
			local nBinOver2 = 10
			local h = 10
			
			local step = `h'/((`nBinOver2'))
			dis "StepSize `step'"
			capture drop theBinAl theBinAlTemp 
			gen  theBinAlTemp= .
			***Only Does this in 2006, then expands to other years
			***Handles Negative Numbers
			forval x = 1/`nBinOver2'{
				replace theBinAlTemp = -`step'*`x' if LISPremium >= (-`step'*(`x')) & LISPremium <(-`step'*(`x'-1)) & year == 2006 
				dis "`x'"
			}
			***Does Positive Numbers
			forval x = 1/`nBinOver2'{
				replace theBinAlTemp = `step'*`x' if LISPremium >= (`step'*(`x')) & LISPremium <(`step'*(`x'+1))  & year == 2006
				dis "`x'"
			}
			egen theBinAl = max(theBinAlTemp),by(uniqueID)
	
				
			local year =2006
			local P = ""
	
			***Select RD Window for scatter 
			local ScatWin =1
			***Select RD Window for local linear regression
			local RegWin  =2
			***Select RD Window for Polynomial regression
			local PolyWin  =1
			
			capture drop lnSHat 
			***Regress to get Mean in Each Bin, for scatter
			xi: reg lnS i.theBinAl if year ==`year' & RDwindow2006`ScatWin'==1 & `P'benefit =="B",cluster(firmID)
			predict lnSHat if year ==`year'
		
			local ytitle = "Log Enrollment Share, 2006"	
			twoway (scatter lnSHat theBinAl if year ==`year' & RDwindow2006`ScatWin'==1 & `P'benefit =="B") (line lnSHatAlt `P'LISPremium if year ==`year' & RDwindow2006`RegWin'==1 & `P'benefit =="B",sort lpattern(dash) lcolor(gray)) (line lnSHatAltPoly `P'LISPremium if year ==`year' & RDwindow2006`PolyWin'==1 & `P'benefit =="B",sort lpattern(solid) lcolor(black)), legend(order(2 3) label(2 "Local Linear") label(3 "Quartic Polynomial")) xtitle("Monthly Premium - LIS Subsidy, 2006") ytitle("`ytitle'") title("Figure 3 with J=`nBinOver2'")
			graph export Analysis_output/Q3_J10.png, replace
			
	*** Question 3, change partitions for Figure 3
			***Algorithm to compute N bins with range +/- h
			local nBinOver2 = 30
			local h = 10
			local step = `h'/((`nBinOver2'))
			dis "StepSize `step'"
			capture drop theBinAl theBinAlTemp 
			gen  theBinAlTemp= .
			***Only Does this in 2006, then expands to other years
			***Handles Negative Numbers
			forval x = 1/`nBinOver2'{
				replace theBinAlTemp = -`step'*`x' if LISPremium >= (-`step'*(`x')) & LISPremium <(-`step'*(`x'-1)) & year == 2006 
				dis "`x'"
			}
			***Does Positive Numbers
			forval x = 1/`nBinOver2'{
				replace theBinAlTemp = `step'*`x' if LISPremium >= (`step'*(`x')) & LISPremium <(`step'*(`x'+1))  & year == 2006
				dis "`x'"
			}
			egen theBinAl = max(theBinAlTemp),by(uniqueID)
	
				
			local year =2006
			local P = ""
	
			***Select RD Window for scatter 
			local ScatWin =1
			***Select RD Window for local linear regression
			local RegWin  =2
			***Select RD Window for Polynomial regression
			local PolyWin  =1
			
			capture drop lnSHat 
			***Regress to get Mean in Each Bin, for scatter
			xi: reg lnS i.theBinAl if year ==`year' & RDwindow2006`ScatWin'==1 & `P'benefit =="B",cluster(firmID)
			predict lnSHat if year ==`year'
			
			
			local ytitle = "Log Enrollment Share, 2006"		
			twoway (scatter lnSHat theBinAl if year ==`year' & RDwindow2006`ScatWin'==1 & `P'benefit =="B") (line lnSHatAlt `P'LISPremium if year ==`year' & RDwindow2006`RegWin'==1 & `P'benefit =="B",sort lpattern(dash) lcolor(gray)) (line lnSHatAltPoly `P'LISPremium if year ==`year' & RDwindow2006`PolyWin'==1 & `P'benefit =="B",sort lpattern(solid) lcolor(black)), legend(order(2 3) label(2 "Local Linear") label(3 "Quartic Polynomial")) xtitle("Monthly Premium - LIS Subsidy, 2006") ytitle("`ytitle'")  title("Figure 3 with J=`nBinOver2'")
			graph export Analysis_output/Q3_J30.png, replace			
		
	
			* Save the data
		save "Out_Data\PreppedData.dta", replace
		
***Question 4, optimal bins
	 rdplot lnS LISPremium if  year == 2006 & benefit =="B" & -10 <= LISPremium & LISPremium <= 10, h(10) binselect(es) graph_options(title("IMSE Evenly Spaced Bins"))
		graph export Analysis_output/Q4_Optimal.png, replace		
			
	*** Question 5, density check
	rddensity LISPremium if year == 2006, plot
	rddensity LISPremium if year == 2006, h(4) 
	rddensity LISPremium if year == 2008, plot
	rddensity LISPremium if year == 2010, plot
	
	
***Question 7, Optimal bandwidth
	
* Uniform kernel, to better mimic the Ericson estimates
rdrobust lnS LISPremium if year == 2006, kernel(uni) p(1) masspoints(off) covs(LISPremiumNeg_06 LISPremiumPos_06)
eststo db_2006

rdrobust lnS LISPremium if year == 2007, kernel(uni) p(1) masspoints(off) covs(LISPremiumNeg_06 LISPremiumPos_06)
eststo db_2007

rdrobust lnS LISPremium_06 if year == 2008, kernel(uni) p(1) masspoints(off) covs(LISPremiumNeg_06 LISPremiumPos_06)
eststo db_2008

rdrobust lnS LISPremium_06 if year == 2009, kernel(uni) p(1) masspoints(off) covs(LISPremiumNeg_06 LISPremiumPos_06)
eststo db_2009

rdrobust lnS LISPremium_06 if year == 2010, kernel(uni) p(1) masspoints(off) covs(LISPremiumNeg_06 LISPremiumPos_06)
eststo db_2010


esttab db_2006 db_2007 db_2008 db_2009 db_2010 using Analysis_output/Q7.rtf, replace compress label nonotes  se  ///
		title("Re-estimation of Effect of LIS Benchmark Status in 2006 on Plan Enrollment") ///
		mtitles("2006" "2007" "2008" "2009" "2010") ///
	   star(* 0.1 ** 0.05 *** 0.01) obslast                 /// 
	   addnotes("*** 1% ** 5% * 10%" "" "")

	   
***Question 8: IV
*Prep variables
gen LastYr_Below = 1 if L1LISPremium <= 0
replace LastYr_Below =0 if !missing(L1LISPremium) & L1LISPremium > 0 

xtset uniqueIDNum year, yearly
gen LastYr_Market = l1.lnS

label variable Dyear2 "Year of plan existence, Second"
label variable Dyear3 "Year of plan existence, Third"
label variable Dyear4 "Year of plan existence, Fourth"
label variable Dyear5 "Year of plan existence, Fifth"
label variable LastYr_Market "Market share in previous year"


* Run reg
local ifCondit =`"if  benefit == "B""'
local yearDedInt i.btypedetail*i.year BAcDeduct*
* Instrument info from simpler model
xtivreg lnPremium Dyear* DOfPlan* (LastYr_Market=LastYr_Below) `ifCondit', vce(cluster firmID) first small

xi: xtivreg lnPremium DOfPlan* i.state*i.year (LastYr_Market=LastYr_Below) `ifCondit', vce(cluster firmID)
eststo iv1
xi: xtivreg lnPremium Dyear* MAPlan  `yearDedInt'  i.state*i.year (LastYr_Market=LastYr_Below) `ifCondit', vce(cluster firmID)
eststo iv2
local yearDedInt i.btypedetail*i.year BAcDeduct*
xi: xtivreg lnPremium Dyear* `yearDedInt'  i.state*i.year (LastYr_Market=LastYr_Below) `ifCondit', vce(cluster firmID) fe
eststo iv3


	
log close

