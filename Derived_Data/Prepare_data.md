Prepare CBEP Historical Sediment Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
October 13, 2020

-   [Introduction](#introduction)
    -   [Sample Locations](#sample-locations)
-   [Load Libraries](#load-libraries)
-   [Load Data](#load-data)
    -   [Folder References](#folder-references)
    -   [Complete Data](#complete-data)
        -   [Add the “Era” variable](#add-the-era-variable)
    -   [SQuiRTs Data](#squirts-data)
        -   [SQuiRT Units](#squirt-units)
    -   [Sums and Totals](#sums-and-totals)
    -   [Subdivide Data BY Parameter
        Group](#subdivide-data-by-parameter-group)
-   [Remove Extraneous Totals](#remove-extraneous-totals)
    -   [PAH Data](#pah-data)
    -   [Physical Data](#physical-data)
        -   [Remove “Percent Total Grain
            Size”](#remove-percent-total-grain-size)
        -   [Remove Extra “Organic Carbon (total)”
            Data](#remove-extra-organic-carbon-total-data)
        -   [Selectively Remove “Percent Silt and
            Clay”](#selectively-remove-percent-silt-and-clay)
        -   [Selectively Remove “Percent
            Sand”](#selectively-remove-percent-sand)
        -   [Recalculate Sums](#recalculate-sums)
        -   [Simplify Data](#simplify-data)
-   [Save Results](#save-results)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

Casco Bay Estuary Partnership collected sediment samples in 1991, 1994,
2000, 2001, 2002, 2010, and 2011 to look at concentrations of toxic
contaminants in Casco Bay surface Sediments. These studies were
complemented by data collected by under the auspices of EPA’s the
National Coastal Assessment (NCA) and National Coastal Condition
Assessment (NCCA).

Chemicals studied included metals, polycyclic aromatic hydrocarbons
(PAHs), polychlorinated biphenyls (PCBs), organochlorine pesticides,
dioxins and furans, and organotins. These contaminants are all
persistent in the marine environment.

## Sample Locations

The original (1991) sampling locations were selected by an expert group
by placing dots on a map of Casco Bay to get sample locations
distributed evenly in areas known to not have rocky substrate (and
where, therefore, soft sediment samples could be collected). Originally,
each location was given a location code, of the form “XX\#\#”, where
“XX” is a two letter region code, and “\#\#” is a two digit sequence
number.

An effort was made in subsequent years to sample the same locations, but
for a variety of reasons, samples were not always collected in quite the
same locations. In the Early 2010s, CBEP staff realized sample locations
were recorded inconsistently, and made a concerted effort to identify
where each sample had actually been collected, often by going back to
paper field data sheets associated with sampling events.

A large fraction, but not all, observations were collected within a
hundred meters or so of nominal original (1991) sample locations. We
grouped sample locations into Locations (clusters of samples within
about 100m of the original sampling locations), assigning new Location
Codes as needed. Within Locations we assigned Substations (within the
resolution of the GPS equipment used at the time, typically \~ 5 to 10
meters, which is smaller than expected drift during sample collection.).

To simplify data analysis and visualization, samples are “binned” into
five major subregions of the Bay, as follows

(descriptions from 2017 Ramboll Environ report to CBEP):

> Inner Bay: The Inner Bay includes the western most part of Casco Bay,
> encompassing the Fore River and Presumpscot River estuaries. The
> cities of Portland and South Portland and the towns of Falmouth and
> Yarmouth are located within its drainage.

> Outer Bay: The Outer Bay includes the large open water area to the
> south of the other regions. It represents the area that connects Casco
> Bay to the rest of the Gulf of Maine.

> West Bay: West Bay extends from Yarmouth on the west to Orrs and
> Bailey Islands on the east. It includes Maquoit and Middle Bays,
> Harpswell Sound, and the communities of Freeport, Harpswell, and parts
> of southern Brunswick. The Royal and Harraseeket Rivers discharge into
> West Bay.

> East Bay: East Bay includes the inland portions of Casco Bay bordered
> by Orrs and Bailey Islands on the west and Phippsburg on the east. It
> includes Quahog Bay and the New Meadows River in southern Brunswick
> and Phippsburg.

> Cape Small: Cape Small is the easternmost region of the bay. It
> includes the southern end of the Phippsburg peninsula to Small Point.
> The mouth of the Lower Kennebec River flows into the Gulf of Maine to
> the east of Phippsburg. While it is not part of Casco Bay, coastal
> circulation patterns indicate that the discharge from the Lower
> Kennebec (which includes flows from the Kennebec and Androscoggin
> Rivers) is entrained into the bay in the Cape Small area (Janzen et
> al. 2005).

# Load Libraries

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.5

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.0     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.0.5

    ## Warning: package 'tidyr' was built under R version 4.0.5

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## Warning: package 'forcats' was built under R version 4.0.5

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 4.0.5

``` r
# library(GGally)

library(CBEPgraphics)
load_cbep_fonts()

library(LCensMeans)
```

# Load Data

## Folder References

``` r
sibfldnm <- 'Original_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
fn <- "Casco_Bay_Sediment_Data_Query.xlsx"
```

## Complete Data

``` r
sedtox_data <- read_excel(file.path(sibling,fn),
    col_types = c("text", "text", "text", 
        "text", "numeric", "numeric", "text", 
        "numeric", "date", "numeric", "text", 
        "text", "text", "text", "text", "numeric", 
        "numeric", "numeric", "text", "text", 
        "numeric", "text", "text", "text", 
        "text"))
```

### Add the “Era” variable

For presentation, we will generally want to replace Year by decade, as
was done in the Ramboll Report. We call the decade the sample “Era”.

``` r
sedtox_data <- sedtox_data %>%
  mutate(Era = (Sample_Year-1990) %/% 10)%>%
  mutate(Era = factor(Era, labels = c('1990s', '2000s', '2010s')))
```

## SQuiRTs Data

Ramboll Environ’s Access database contains a table derived from NOAA’s
standard reference providing multiple screening levels for many toxic
compounds, known as “SQuiRT” Tables. Ramboll’s table contains ERL
(Effects Range Low) and ERM (Effects Range Medium) values for selected
parameters.

Additional details on the SQuiRT tables are available in the archives
containing data on analysis of toxic compounds in Portland Harbor
sediments, available at <https://github.com/ccb60/PortlandHarborToxics>
.

``` r
fn <- "SQuiRTs.xlsx"
squirts_data <- read_excel(file.path(sibling,fn), 
    col_types = c("skip", "text", "text", 
        "text", "numeric", "numeric", "text", 
        "numeric", "numeric", "text", "skip"))
```

### SQuiRT Units

We need to ensure that we are using similar units for analysis and
comparison to the SQuiRTs. Ramboll’s SQuiRT Table contains two different
sets of values, with different units.

The NOAA SQuiRTs express all values in parts per billion (ppb). That
does not match the units Ramboll used to report concentrations of toxic
contaminants. Ramboll converted units, and reported the SQuiRTs in
derived units. The derived units are documented in a data column called
“Units\_fin”.

In general, metals screening levels are expressed in µg/g dry, while
organic contaminants are expressed in ng/g dry. The former correspond
approximately to parts per million, the latter to parts per billion.

## Sums and Totals

Ramboll Environ prepared a Query in the Access Database they that
calculated major sums of significant groups of contaminants. We can load
just the sums data from the Excel Spreadsheet exported from that Query.

``` r
fn <- "Sums_Query.xlsx"

sums_data <- read_excel(file.path(sibling,fn), 
    col_types = c("text", "text", "text", 
        "text", "numeric", "numeric", "text", 
        "numeric", "date", "numeric", "text", 
        "text", "text", "text", "text", "numeric", 
        "numeric", "numeric", "text", "text", 
        "numeric", "numeric", "numeric", 
        "text", "text")) %>%
  mutate(Era = (Sample_Year-1990) %/% 10)%>%
  mutate(Era = factor(Era, labels = c('1990s', '2000s', '2010s'))) %>%
  select(-Project, -Lat_N, -Long_W, -Sample_Date, -Sample_Type,
           -Matrix, -Parameter_Group, -Detect, -Method) %>%
  left_join(squirts_data, by = c('Parameter' = 'Param_Norm')) %>%
  select(-Analyte, -CAS, -ERL_org, -ERM_org, -Units_org) %>%
  rename(ERL =ERL_fin,
         ERM = ERM_fin,
         Units_SQuiRT = Units_fin) %>%
  # The following may be premature, as I have not confirmed legitimacy
  # and have not addressed non-detects.
  mutate(LVL = factor(ifelse(is.na(ERL), 'NA',
                             ifelse(Result <= ERL, 'Below ERL',
                                   ifelse(Result <= ERM,
                                         'Between ERL and ERM','Above ERM'))),
                      
                      levels = c('Below ERL',
                                 'Between ERL and ERM',
                                 'Above ERM',
                                 'NA')))
```

**The metadata is not entirely clear, but it appears that these sums
omit non-detects, effectively equating non-detects to zero. That is
inconsistent with how we have handled non-detects in other toxics data
sets, where we have been using maximum likelihood estimators of expected
values.**

We could recalculate totals using the category definitions in
“SumGroups.xlsx”, but for now, for consistency with the original report,
we use the sums as calculated by Ramboll.

Most sums are expressed in ng/g dry weight (\~ ppb). The Sum of Dioxins
and Furans is expressed in ng/kg, or pg/g (\~ppt). There are no SQuiRTs
for Dioxins and Furans. Instead, Ramboll Environ expressed them in “TEQ”
– Toxic equivalents. Toxic equivalents provide a way to estimate the
cumulative toxic effect of a mixture of related chemicals by weighting
each compound by its relative toxic effect, compared to some reference
compound.

Ramboll followed conventional methods for calculating the TEQs. Each
compound is assigned a Toxicity Equivalency Factor, relative to the
toxic effect of a specific dioxin, 2,3,7,8-tetrachlorodibenzo-p-dioxin,
or TCDD. The TEQ is calculated as the weighted sum of observed
concentrations, weighted by TEFs.

Specific TEFs used are listed in the Access database, in a table called
“PCDD\_TEFs”. We do not copy the table here, as values are widely
standardized. The table includes TEFS for mammals, birds, and fish, but
the TEQ values in the Sums data are keyed to mammalian toxicity.

## Subdivide Data BY Parameter Group

We separate the data by Parameter Group into smaller, more manageable
data sets. While we are at it, we to strip out Totals and Sums, add any
relevant screening values from the SQUIRTS, strip out unnecessary data
columns, and generally clean up after ourselves.

This is a repetitive enough process to justify writing a simple function
to automate it. This ensures we handle all subfiles in a similar way.

``` r
(sum_names    <- unique(sums_data$Parameter))
```

    ## [1] "CDD/CDF (total)"        "Butyltin (mono+di+tri)" "PAHs (High MW)"        
    ## [4] "PAHs (Low MW)"          "PAHs (total)"           "PCBs (total)"          
    ## [7] "Chlordane (total)"      "DDT+DDE+DDD (sum)"      "Pesticides (total)"

``` r
extract_simplify <- function(.source, .selected) {
  df <- .source %>%
    filter(Parameter_Group ==.selected) %>%
    filter(! Parameter %in% sum_names) %>%
    select(-Project, -Lat_N, -Long_W, -Sample_Date, -Sample_Type,
           -Matrix, -Parameter_Group, -Detect, -Method) %>%
    left_join(squirts_data, by = c('Parameter' = 'Param_Norm')) %>%
    select(-Analyte, -CAS, -ERL_org, -ERM_org, -Units_org) %>%
    rename(ERL =ERL_fin,
           ERM = ERM_fin,
           Units_SQuiRT = Units_fin) %>%
    mutate(LVL = factor(ifelse(is.na(ERL), 'NA',
                             ifelse(Result <= ERL, 'Below ERL',
                                   ifelse(Result <= ERM,
                                         'Between ERL and ERM','Above ERM'))),
                      
                      levels = c('Below ERL',
                                 'Between ERL and ERM',
                                 'Above ERM',
                                 'NA')))
}
```

``` r
diox_data   <- extract_simplify(sedtox_data,  'CDDF')
metals_data <- extract_simplify(sedtox_data,  'Inorganic')
tbt_data    <- extract_simplify(sedtox_data,  'Organotin')
pah_data    <- extract_simplify(sedtox_data,  'PAH')
pcb_data    <- extract_simplify(sedtox_data,  'PCB')
pest_data   <- extract_simplify(sedtox_data,  'Pesticide')
phys_data   <- extract_simplify(sedtox_data,  'Physical')
```

# Remove Extraneous Totals

In reviewing the data we noted that the data included some historical
totals, based on different sets of constituents. Ramboll removed most of
those historic totals, and recalculated them in the Access database.

A number of records included in the data include the annotation
“(total)” in the Parameter Name. In reviewing the data, we concluded
that most were ANALYTIC totals, not mathematical SUMS of other
observations. These “totals” represent mixtures of closely related
compounds not well separated by analytic methods (e.g., isomers of
complex PCBs).

The only remaining totals we found were in the PAHs data. Over the
years, different subsets of PAHs have been quantified, as analytic
methods changed. As a consequence, “totals” have included different
numbers of constituents. Further, to make “totals” fully comparable to
benchmarks or other studies, well-defined sets of compounds need to be
summed. The results are a number of “totals” that were used in
historical analyses, but those are not of value here.

## PAH Data

Three historic PAH “Totals” were included in the data. For analysis or
reanalysis of raw data, we do not want to include them, so we remove
them here.

``` r
extras <- c('PAHs (High MW 13)', 'PAHs (Low MW 9)', 'PAHs (total 22)')
pah_data <- pah_data %>%
  filter(! Parameter %in% extras)
```

## Physical Data

Several parameters included here are sums calculated before the data was
submitted to Ramboll. They are not clearly labeled as calculated sums,
so in the following code we selectively remove the calculated values and
add (recalculated) totals that are clearly labeled as sums.

-   **Percent Total Grain Size** was a QA/QC check testing whether the
    sum of all grain size data sums to one (or close to it).  
-   **Percent Silt and Clay** is (almost always) a sum of separate silt
    and clay values. However, seven 2000 samples, one 2002 sample and
    seven 2010 samples only have “Percent Sand” and “Percent Silt and
    Clay” values.  
-   Sand was subdivided into more precise sand fractions only in 2010
    and 2011. **Percent Sand** in those two years was USUALLY a SUM of
    the other sand fractions , except for seven samples from the NCCA
    from 2010.

Note that “Percent Gravel” was inconsistently reported, making it
unclear whether all other grain size fractions are comparable, at least
for the handful of sites with high gravel.

These sums need to be removed and recalculated, without removing the
data associated with samples where we have no finer-scale data.

### Remove “Percent Total Grain Size”

Percent Total Grain size appears to have been an inconsistently
calculated data QA/QC check. We remove it as uninformative and
potentially confusing.

``` r
extras <- 'Percent Total Grain Size'
phys_data <- phys_data %>%
  filter(! Parameter %in% extras)
```

### Remove Extra “Organic Carbon (total)” Data

Organic Carbon is in the data set (where it is available at all) in two
different sets of units. We retain only the version that is in percent,
for the simplicity of making sure that all the units in this data set
are the same.

``` r
phys_data <- phys_data %>%
  filter(!( Parameter == 'Organic Carbon (total)' & Result > 200))
```

### Selectively Remove “Percent Silt and Clay”

“Percent Silt and Clay” is sometimes a sum and sometimes an original
measurement. We selectively remove “Percent Silt and Clay” values that
are sums of other values. Not that we sometimes have “Percent Silt” and
“Percent Clay” values in the data that equal zero, even when no actual
data was collected. Somewhere in prior processing, what should have been
missing data appears to have been replaced with zeros.

``` r
phys_data <- phys_data %>%
  group_by(Sample_ID, Replicate) %>%
  
  # We use mutate because it tags all records in the group with the test result
  mutate(silt = sum((grepl('Silt', Parameter) & ! grepl('Clay', Parameter)) * 
                      Result),
         clay = sum((grepl('Clay', Parameter) & ! grepl('Silt', Parameter)) *
                      Result),
         # The test checks if we have silt or clay observations.
         test = ! (silt == 0 & clay == 0)) %>%
  ungroup(Sample_ID, Replicate) %>%
  
  # And we now selectively remove 'Percent Silt and Clay' values associated with
  # silt or clay observations (which are by definition, sums)
  filter( ! (Parameter == 'Percent Silt and Clay' & test)) %>%
  select(-c(silt, clay, test))
```

And the same thing with the Percent Sand values. Some are sums of
original data, some are not. Here we can filter based on Sample\_Year
and Sample\_ID, which is a bit simpler that what we needed to do for
“Percent Silt and Clay”.

### Selectively Remove “Percent Sand”

``` r
phys_data <- phys_data %>%
  filter(Sample_Year < 2009  |
         grepl("NCCA", Sample_ID)  |
          ! Parameter == 'Percent Sand')
```

### Recalculate Sums

One sample, ‘CBEP2010-SW08’ has two sets of values for the grain size
analysis, apparently a laboratory replicate. That causes problems when
we pivot the data to wider form. The data does not specify which values
are associated with each other, so the best we can do is average the
values.

TWO samples lack any grain size data, so the calculated values, which
return as zeros, should really be missing values.

``` r
# First, we select the OC rows in the database
phys_data <- phys_data %>%
  pivot_wider(names_from = Parameter, values_from = Result, values_fn = mean) %>%
  rowwise() %>%
  mutate(`Percent Sand (Calculated)` =
              sum(c_across(contains("Sand")), na.rm = TRUE),
         `Percent Silt and Clay (Calculated)` = 
              sum(c_across(matches("Silt|Clay")), na.rm = TRUE)
            ) %>%
  ungroup() %>%
  mutate(`Percent Sand (Calculated)` =
           replace(`Percent Sand (Calculated)`,0, NA),
         `Percent Silt and Clay (Calculated)` =
           replace(`Percent Silt and Clay (Calculated)`,0, NA) ) %>%
  pivot_longer(`Percent Sand`:`Percent Silt and Clay (Calculated)`,
               names_to = 'Parameter', values_to = 'Result',
               values_drop_na = TRUE)
```

### Simplify Data

Many of the remaining data columns have little meaning in the context of
the physical data parameters, such as grain size, so we strip them out
here.

``` r
phys_data <- phys_data %>%
  select (-CASRN, -MDL, -RL, -Det_Flag, -Qualifier, -`QA Qualifier`,
            -Reportable_Result, -ERL, -ERM, -LVL, -Units_SQuiRT)
```

# Save Results

``` r
dir.create(file.path(getwd(), 'Data_Subsets'), showWarnings = FALSE)

write_csv(diox_data, 'Data_Subsets/dioxins.csv')
write_csv(metals_data, 'Data_Subsets/metals.csv')
write_csv(tbt_data, 'Data_Subsets/butyltins.csv')
write_csv(pah_data, 'Data_Subsets/pahs.csv')
write_csv(pcb_data, 'Data_Subsets/pcbs.csv')
write_csv(pest_data, 'Data_Subsets/pesticides.csv')
write_csv(phys_data, 'Data_Subsets/physical.csv')

write_csv(sums_data, 'Data_Subsets/sums_totals.csv')
```
