


clear 
cd "C:\Users\braul\OneDrive - Duke University\Papers\Mobility_indig\data"

use "d_l_ses.dta" , clear 


	** Define paths 

	replace path="" if path=="00NA" | path=="01NA" | path=="0NA0" | path=="11NA" | path=="10NA" 
		gen pathn = path 
		replace pathn = "1" if path=="000"
		replace pathn = "2" if path=="011"
		replace pathn = "3" if path=="001"
		replace pathn = "4" if path=="100"
		replace pathn = "5" if path=="110"
		replace pathn = "6" if path=="111"
		replace pathn = "7" if path=="101"
		replace pathn = "8" if path=="010"
		destring pathn, replace
		
		gen path_patterns = "" 
		replace path_patterns = "Never" if path=="000"
		replace path_patterns = "Twice" if path=="011"
		replace path_patterns = "Once" if path=="001"
		replace path_patterns = "Once" if path=="100"
		replace path_patterns = "Twice" if path=="110"
		replace path_patterns = "Always" if path=="111"
		replace path_patterns = "Twice" if path=="101"
		replace path_patterns = "Once" if path=="010"
	
		
		
		
		gen center = region==2
		gen north = region==3
			
			
** Listwise deletion 		 
					
	egen missing=rmiss(path ind m_zpca perind21 anesc edad female region)
		drop if missing!=0 
	
	
	

	
			
** Standardize variables 

	summ perind21 [aw=fac]
	gen mean=r(mean)
	gen sd=r(sd)
	gen perind21std=(perind21-mean)/sd
	drop mean sd
	
	summ m_zpca [aw=fac]
	gen mean=r(mean)
	gen sd=r(sd)
	gen ses_std=(m_zpca-mean)/sd
	drop mean sd	
		
		
	* Table 1
	summ edadw1 female ses_std  anesc speak perind21std if wave==3 [aw=fac]
	
	* Table2
	tab path_patterns [aw=fac]
	
	
	keep if path!="000"
	
	
	* Analyses 
	
	* Table3 
	
	mixed ind  i.speak c.perind21std ses_std  difses m_anesc difed  i.wave   c.edad##c.edad female  i.region  [pw=awt]  || id:
	est store lm1
	esttab lm1 
	
	coefplot, keep(1.speak perind21std ses_std difses m_anesc difed) coeflabels(1.speak = "Indigenous Language Speaker" perind21std = "Ethnic Density (Std.)" ses_std = "Average Economic Standing (Std.)" difses = "Individual Changes in SES" m_anesc = "Average Years of Education" difed = "Increase in Years of Education") xline(0)
	
	
	* Figure 1
	qui mixed ind c.perind21std##i.speak i.wave  difses m_anesc difed ses_std c.edad##c.edad female i.region  [pw=awt]  || id:
	est store lm2
	margins, at(perind21std=(-1.5 (1) 1.5) speak=(0 1)) atmeans 
	marginsplot 
	
	* Figure 2 
	qui mixed ind c.ses_std##i.speak i.wave difses m_anesc difed c.perind21std c.edad##c.edad female  i.region [pw=awt] || id:
	est store lm3
	margins, at(ses_std=(-2 (1) 1) speak=(0 1)) atmeans 
	marginsplot 
	
	
	
	* Export results 
	
	esttab lm*  using "results_models.csv", se replace   b(%5.3f)
	
	
