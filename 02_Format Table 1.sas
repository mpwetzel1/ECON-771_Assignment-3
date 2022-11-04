

%let root = C:\Users\mwetze2\OneDrive - Emory University\PhD Coursework\2022b_Fall\ECON 771\Assignments\Assignment 3;
%include "&root.\Code\Table1nDone Macro_Custom.sas";
libname out "&root.\Out_Data";


proc import datafile = "&root.\Out_Data\PreppedData.dta" 
	out = prepped replace
	dbms = dta;
run;


/* Create Table 1 Output */

/* Limit to plan's first year on the market */
data planinit;
	set prepped;
	where yearOfPlan = 1;
run;


%Table1nDone(
	DATASET=planinit, /* Input data set */
	DRIVER=&root.\Code\Table1nDone Driver_v1.xlsx , /* File path and name for Excel driver file - no quotes*/ 
	OVERALL=N, /* Request overall summary statistics */
	BYCLASS=Y, /* Request statistic by class variable */
	EXCLUDEMISS=Y, /* Exclude observations with a missing BYCLASS value from the overall calculations */
	OUTLIB= OUT, /* Name of library to save out SAS data sets to */
	OUTPATH= &root.\Analysis_output, /* File pathway for RTF output, no quotes */
	FNAME= Table1,  /* File name for RTF output */
	CLASSVAR= Cohort, /* CAN ONLY TAKE ONE CLASS VARIABLE */
	ODSGRAPHS=N, /* Create or suppress ODS graphics (Y/N) */
	ROUNDTO=0.01, /* Decimal place to round results to */ 
	DISPLAY_PVAL=N, /* Display p-values in RTF report */
	DISPLAY_METRIC=N, /* Display metric names in RTF report */
	DISPLAY_N =N, /* Display variable N's in RTF report */
	CLEARTEMP = N /* Delete temp files created by macro */
);
