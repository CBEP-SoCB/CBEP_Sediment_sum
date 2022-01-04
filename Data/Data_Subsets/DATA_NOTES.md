# Data Sources
All data was derived from an Access database prepared by Ramboll Environ.
The full database, called “Casco Bay Sediment Data 1991_2001.accdb” and dated
February 1, 2017, is available from CBEP upon request.

It is worth pointing out that these data were collected during multiple studies,
conducted over a period of 20 years.  Many methods are inconsistent from 
sample year to sample year.  W e note those concerns in the data analysis 
scripts. For full details, turn to the Ramboll report.

## Data Subset Files
### `butyltins.csv`
### `dioxins.csv`
### `metals.csv`
### `pahs.csv`
### `pcbs.csv`
### `pesticides.csv`

Each of these files represents a subset of the data extracted from the source 
data.  They each have the same general format, as follows:

Column Name  | Contents                                                      
-------------|--------------------------------------------------------
Region       | One of five regions of the Bay, as a string.  See map.
Location     | Location code.  Samples with the same location code were collected within about 30 meters of the nominal location.
Substation   | Substation code. Samples with the same substation code were collected within about 100 meters of the nominal substation.
Sample_ID    | Unique ID combines Station and Year information
Sample_Year  | Year of sample collection
Replicate    | Code indicating replicate samples. 0 = FALSE, -1 = TRUE
Parameter    | Name of the analyte
CASRN        | CAS number of analyte
Result       | Numerical value.  NA indicates missing data, usually due to non-detects.
MDL          | Method detection limit
RL           | Reporting limit
Units        | Units used for reporting results.
Det_Flag     | Flag indicating whether hte chemical was detected or not. o = FALSE, 1 = TRUE
Qualifier    | Data qualifiers.  Short, usually one or two letter codes.  "ND" means non-detect.
QA Qualifier | Additional data qualifiers, also usually shoret codes.
Reportable_Result | Always "Y" for these data.
Era          | Was this sample from the 1990s, 2000s, or 2010s?
ERL          | Effects Range Low threshold (FROM SQuiRT tables)
ERM          | Effects Range Low threshold (FROM SQuiRT tables)
Units_SQuiRT | Units used for reporting the sQuiRTs, sometimes does not match measurement units
LVL          | are observations below ERL, above ERL but below ERM, or above ERM?

## `sums_totals.csv`
This file follows the same file format, but contains data on sums and totals
of groups of related chemicals. These sums and totals are often more convenient
to report and interpret than the raw compound by compound data.

The metadata in the source is not entirely clear, but it appears (from
examining the Access database) that sums of contaminant groups omit non-detects,
effectively equating non-detects to zero. That is inconsistent with how we
handled non-detects in several other toxics data sets, where we have been using
maximum likelihood estimators of expected values.  In practice, that makes very 
little difference in interpretation of these results, as sums are, in most 
cases dominated by a small number of compounds with more than *de minimus* 
concentrations.

### Number of Chemical Constituents in "Totals"  
Analytic Total    | Number of Compounds
------------------|---------------------
CDD/CDF (total)*  |  17
PAHs (total)      |  13
PAHs (High MW)    |   6
PAHs (Low MW)     |   7
PCBs (Total)      |  88
Pesticides (total)|  20
Chlordane (total) |   3
DDT+DDE+DDD (sum) |   6

* A combination of dioxins and furans.

### `SumsGroups.xlsx`
Shows which compounds were included in each Total.  It is included here as
metadata for those who need to understand which compounds were included in the
different sums and totals.

## `physical.csv`
Although included in the data subsets folder, this file does not follow
the same data format.  The file includes the following columns:

Column Name  | Contents                                                      
-------------|--------------------------------------------------------
Region       | One of five regions of the Bay, as a string.  See map.
Location     | Location code.  Samples with the same location code were collected within about 30 meters of the nominal location.
Substation   | Substation code. Samples with the same substation code were collected within about 100 meters of the nominal substation.
Sample_ID    | Unique ID combines Station and Year information
Sample_Year  | Year of sample collection
Replicate    | Code indicating replicate samples. 0 = FALSE, -1 = TRUE
Units        | Units used for reporting results.
Era          | Was this sample from the 1990s, 2000s, or 2010s?
Parameter    | Name of the analyte
Result       | Numerical value.  NA indicates missing data, usually due to non-detects.

Reporting of sediment grain size followed different conventions over the years.
Some years report only "Sand" and "Silt-Clay" fractions. others break down
sediment composition into more categories. Users are encouraged to look
carefully at how the sediment grain size data is reported before drawing any
strong conclusions.
