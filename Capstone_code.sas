/* Generated Code (IMPORT) */
/* Source File: SAS.csv */
/* Source Path: /home/u49791046 */
/* Code generated on: 11/6/21, 6:16 PM */

%if %sysfunc(exist(MIS540.'Capstone Data'n)) %then %do;
proc sql;
    drop table MIS540.'Capstone Data'n;
run;
%end;


FILENAME REFFILE '/home/u49791046/SAS.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=MIS540.'Capstone Data'n;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=MIS540.'Capstone Data'n; RUN;


%web_open_table(MIS540.'Capstone Data'n);


ods noproctitle;
ods graphics / imagemap=on;

proc glmselect data=MIS543.CAPSTONE_DATA 
		outdesign(addinputvars)=Work.reg_design;
	model pat_zip_index=zip_code pharmacy_index department_index medication_index 
		zip_code*zip_code pharmacy_index*pharmacy_index 
		department_index*department_index medication_index*medication_index 
		zip_code*zip_code*zip_code pharmacy_index*pharmacy_index*pharmacy_index 
		department_index*department_index*department_index 
		medication_index*medication_index*medication_index / showpvalues 
		selection=none;
run;

proc reg data=Work.reg_design alpha=0.05 plots(only)=(diagnostics residuals 
		observedbypredicted);
	ods select DiagnosticsPanel ResidualPlot ObservedByPredicted;
	model pat_zip_index=&_GLSMOD /;
	run;
quit;

proc delete data=Work.reg_design;
run;

ods graphics / reset width=6.4in height=4.8in imagemap;

proc sgplot data=MIS543.CAPSTONE_DATA;
	scatter x=pat_zip_index y=department_index /;
	xaxis grid;
	yaxis grid;
run;

ods graphics / reset;

ods noproctitle;

proc stdize data=MIS543.CAPSTONE_DATA out=Work._std_ method=range;
	var department_index medication_index pat_zip_index;
run;

proc fastclus data=Work._std_ maxclusters=100;
	var department_index medication_index pat_zip_index;
run;

proc delete data=Work._std_;
run;

ods noproctitle;
ods graphics / imagemap=on;

proc glmselect data=MIS543.CAPSTONE_DATA plots=(criterionpanel) seed=72;
	partition fraction(validate=0.59 test=0.4);
	model pat_zip_index=medication_index zip_code pharmacy_index department_index 
		pharm_zip_index / selection=stepwise
(select=sbc) hierarchy=single;
run;

ods noproctitle;
ods graphics / imagemap=on;

proc reg data=MIS543.CAPSTONE_DATA alpha=0.05 plots(only)=(diagnostics 
		residuals observedbypredicted);
	model pat_zip_index=department_index pharmacy_index medication_index /;
	run;
quit;

