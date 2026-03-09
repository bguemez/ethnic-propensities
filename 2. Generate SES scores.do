	
clear 
cd "C:\Users\braul\OneDrive - Duke University\Papers\Mobility_indig\data"

clear 
import delimited "seswave_1.csv", varnames(1) 


 replace ah03b="" if ah03b=="NA"
 replace ah03d="" if ah03d=="NA"
 replace ah03f="" if ah03f=="NA"
 replace ah03g="" if ah03g=="NA"
replace ah03h="" if ah03h=="NA"
replace  cvo06_1 ="" if  cvo06_1 =="NA"
replace  cvo05_1="" if  cvo05_1=="NA"

replace  cv16="" if  cv16=="NA"
 
 destring ah03b ah03c ah03d ah03e ah03f ah03g ah03h cvo06_1  cvo07_1  cvo05_1 cvo04 cv08_1  cv16 cv07 , replace 

polychoricpca ah03b ah03c ah03d ah03e ah03f ah03g ah03h cvo06_1  cvo07_1  cvo05_1 cvo04 cv08_1  cv16 cv07 [pw=factor_b2], score(zpca) nscore(2)


rename zpca1 zpca

			summ zpca [aw=fac]
			local mean=r(mean)
			local sd=r(sd)
			replace zpca = (zpca-`mean')/`sd'

kdensity zpca [aw=fac]



			
			keep zpca folio 
			
			
			
		
save "wave1_ses.dta", replace 


** wave2
clear 
import delimited "seswave_2.csv", varnames(1) stringcols(2) clear 

 replace ah03b="" if ah03b=="NA"
 replace ah03c="" if ah03c=="NA"
 replace ah03d="" if ah03d=="NA"
 replace ah03e="" if ah03e=="NA"
 replace ah03f="" if ah03f=="NA"
 replace ah03g="" if ah03g=="NA"
replace ah03h="" if ah03h=="NA"
replace  cvo06_1 ="" if  cvo06_1 =="NA"
replace  cvo07_1 ="" if  cvo07_1 =="NA"
replace  cv08_1 ="" if  cv08_1=="NA"
replace  cvo05_1="" if  cvo05_1=="NA"
replace  cvo04 ="" if  cvo04 =="NA"
replace  cv16="" if  cv16=="NA"
replace  cv07="" if  cv07=="NA"


destring ah03b ah03c ah03d ah03e ah03f ah03g ah03h cvo06_1  cvo07_1  cvo05_1 cvo04 cv08_1  cv16 cv07 , replace 

summ
	
polychoricpca ah03b ah03c ah03d ah03e ah03f ah03g ah03h cvo06_1  cvo07_1  cvo05_1  cvo04 cv08_1  cv16 cv07 [pw=fac_2l] , score(zpca) nscore(2)



rename zpca1 zpca

			summ zpca [aw=fac]
			local mean=r(mean)
			local sd=r(sd)
			replace zpca = (zpca-`mean')/`sd'

			keep zpca folio foliow1 
kdensity zpca

save "wave2_ses.dta", replace 


** wave3

clear 
import delimited "seswave_3.csv", varnames(1) 


 replace ah03b="" if ah03b=="NA"
 replace ah03c="" if ah03c=="NA"
 replace ah03d="" if ah03d=="NA"
  replace ah03d1="" if ah03d1=="NA"
 replace ah03e="" if ah03e=="NA"
 replace ah03f="" if ah03f=="NA"
 replace ah03g="" if ah03g=="NA"
replace ah03h="" if ah03h=="NA"
replace  cvo06_1 ="" if  cvo06_1 =="NA"
replace  cvo07_1 ="" if  cvo07_1 =="NA"
replace  cv08_1 ="" if  cv08_1=="NA"
replace  cvo05_1="" if  cvo05_1=="NA"
replace  cvo04 ="" if  cvo04 =="NA"
replace  cv16="" if  cv16=="NA"
replace  cv07="" if  cv07=="NA"


destring ah03b ah03c ah03d ah03d1  ah03e ah03f ah03g ah03h cvo06_1  cvo07_1  cvo05_1 cvo04 cv08_1  cv16 cv07 , replace 


replace ah03d=1 if ah03d1==1 

polychoricpca ah03b ah03c ah03d ah03e ah03f ah03g ah03h cvo06_1  cvo07_1  cvo05_1  cvo04 cv08_1  cv16 cv07 [pw=fac_2l] , score(zpca) nscore(2)



rename zpca1 zpca

			summ zpca [aw=fac]
			local mean=r(mean)
			local sd=r(sd)
			replace zpca = (zpca-`mean')/`sd'

			kdensity zpca
			
			
keep zpca folio 
			
save "wave3_ses.dta", replace 


