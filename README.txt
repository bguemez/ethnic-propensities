This is the replication package for “Ethnic Fluidity as Classification Propensities. Culture, Socioeconomic Status, and Changes in Indigenous Self-Identification in Mexico.” The paper has four general steps, each corresponding to a file in the “code” folder. All the data used for the analysis is stored in the “data” folder.
 
Step 1. Extract variables to create the SES index 
The file “1. Export data for SES scores.R” Extracts all the variables used for the SES index for each of the waves of the longitudinal data set. This file generates three datasets (seswave_1.csv, seswave_2.csv, seswave_3.csv) that serve as input for the next step.

Step 2. Compute the SES index.
The file “2. Generate SES scores.do” contains Stata instructions to build the SES index using the polychoricpca command. This generates three datasets (wave1_ses.dta, wave2_ses.dta, wave3_ses.dta) that serve as input for the next step.

Step 3. Merge datasets
The file “3. Wrangling.R”  merges all the datasets used in the analyses. This code generates the main dataset of the analysis: "d_l_ses.dta" 

Step 4. Analysis
The file “4. Analyses.do” uses the final dataset to run the descriptive statistics, the final statistical models, and the Figures used in the paper. The final model tables are stored in “results_models.csv” in the “data” folder. 

Please contact me (Braulio.guemez@duke.edu) if you have any questions about this package. 


