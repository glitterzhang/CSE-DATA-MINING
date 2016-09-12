PROC IMPORT OUT= WORK.wine
            DATAFILE = 'C:\SAS\MyRawData\wine.csv' 
			DBMS = CSV 
			REPLACE;
	     	GETNAMES = YES;
			DATAROW = 2; 
RUN;

PROC SGscatter DATA = wine;
TITLE "Times for Men's 1500 Meter Run";
	matrix  fx_acidity vol_acidity citric_acid resid_sugar chlorides free_sulf_d tot_sulf_d density pH sulph alcohol/GROUP= class;
	
RUN;

PROC TABULATE
Data =wine;
CLASS  class;
VAR fx_acidity vol_acidity citric_acid resid_sugar chlorides free_sulf_d tot_sulf_d density pH sulph alcohol;
TITLE ' Statistic data for wine';
TABLE (fx_acidity vol_acidity citric_acid resid_sugar chlorides free_sulf_d tot_sulf_d density pH sulph alcohol)*(MIN MAX MEAN STDDEV )*class;
RUN;
