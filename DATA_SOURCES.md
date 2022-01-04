# Data Source
All data is derived from is from the official data transmission from Ramboll
Environ to CBEP in 2017, at the completion of Ramboll Environ's analysis of
Casco Bay Sediment chemistry data.

Ramboll produced a report for CBEP on their analysis of these data.  That report 
provides much more detail.  

>  Ramboll Environ US Corporation.  2017.  Casco BAy Sediment Assessment 
   1992-2011. Casco Bay, Maine. Report prepared for the Casco Bay Estuary 
   Partnership, poortland, Maine.  Available at
   https://www.cascobayestuary.org/publication/casco-bay-sediment-assessment-1991-2011-casco-bay-maine/

All data was derived from an Access database prepared by Ramboll Environ.
The full database, called “Casco Bay Sediment Data 1991_2001.accdb” and dated
February 1, 2017, is available from CBEP upon request.

The metadata in the source data is not entirely clear, but it appears (from
examining the Access database) that sums of contaminant groups omit non-detects,
effectively equating non-detects to zero. That is inconsistent with how we
handled non-detects in several other toxics data sets, where we have been using
maximum likelihood estimators of expected values.

### Number of Chemical Constituents in "Totals"  
Analytic Total    | Number of Compounds
------------------|------------------------
CDD/CDF (total)*  |  17
PAHs (total)      |  13
PAHs (High MW)    |   6
PAHs (Low MW)     |   7
PCBs (Total)      |  88
Pesticides (total)|  20
Chlordane (total) |   3
DDT+DDE+DDD (sum) |   6

* A combination of dioxins and furans.


## Information on Reference Levels of Contaminants in Sediments
The file **"SQuiRTs.xlsx"** contains ERL and ERM values taken from the Ramboll
Environ Access database. In particular it contains ERL (Effects Range Low) and
ERM (Effects Range Medium) values for selected parameters.  These values were,
in turn, derived from NOAA's SQuiRT tables.  Additional details on the SQuiRT
tables are available in the archives containing data on analysis of toxic
compounds in Portland Harbor sediments, available at
https://github.com/CBEP-SoCB-Details/PortlandHarborToxics.

