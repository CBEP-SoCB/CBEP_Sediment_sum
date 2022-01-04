Graphics Showing Patterns for Metals
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
Revised October 19, 2020

-   [Introduction](#introduction)
-   [Load Libraries](#load-libraries)
-   [Load Data](#load-data)
    -   [Folder References](#folder-references)
    -   [Metals Data](#metals-data)
        -   [Units](#units)
        -   [Change Factor Levels](#change-factor-levels)
-   [Delete Unused Parameters](#delete-unused-parameters)
-   [Metals by Region](#metals-by-region)
-   [Metals by Metal](#metals-by-metal)
-   [Trend Graphics](#trend-graphics)
    -   [Initial Draft](#initial-draft)
    -   [Add Statistically Significant
        Trendlines](#add-statistically-significant-trendlines)
    -   [Calculate Regression line
        Points](#calculate-regression-line-points)
    -   [Final Version](#final-version)

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

# Load Libraries

``` r
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.7
#> v tidyr   1.1.4     v stringr 1.4.0
#> v readr   2.1.1     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(readxl)
library(knitr)

# library(GGally)

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())

library(LCensMeans)
```

# Load Data

## Folder References

``` r
sibfldnm <- 'Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
niecefldnm <- 'Data_Subsets'
niece <- file.path(sibling,niecefldnm)
fn <- "metals.csv"

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

## Metals Data

``` r
metals_data <- read_csv(file.path(niece,fn),
                      col_types = cols(.default = col_character(),
                                        Sample_Year = col_double(),
                                        Replicate = col_integer(),
                                        CASRN = col_skip(),
                                        Result = col_double(),
                                        MDL = col_skip(),
                                        RL = col_skip(),
                                        Det_Flag = col_skip(),
                                        Qualifier = col_skip(),
                                        `QA Qualifier` = col_skip(),
                                        Reportable_Result = col_skip(),
                                        ERL = col_double(),
                                        ERM = col_double() )
                      ) %>%
  mutate(Replicate = Replicate == -1) %>%
  mutate(Parameter = if_else(Parameter == "Chromium (total)",
                             "Chromium",
                             Parameter))
```

### Units

Ramboll Standardized units in the Access database, so, concentrations of
metals are expressed in *μ**g*/*g* dry weight (\~ ppm).

### Change Factor Levels

``` r
metals_data <- metals_data %>%
  mutate(LVL = factor(LVL, levels = c('Below ERL','Between ERL and ERM',
                                     'Above ERM'))) %>%
  mutate(Region = factor(Region, levels = c("Inner Bay",
                                            "West Bay",
                                            "East Bay",
                                            "Outer Bay",
                                            "Cape Small")))
```

# Delete Unused Parameters

To simplify presentation for State of Casco Bay, we will not report all
metals. we remove unused data here.

For more careful analysis, we may want to compare concentrations of some
metals to concentrations of iron and aluminum, which are largely
naturally occurring, and reflect mineral sediment deposition. We retain
aluminum and iron for now, for reusability of code, although we are
unlikely to incorporate them in State of Casco Bay graphics.

``` r
metals_data <- metals_data %>%
  filter(Parameter %in% c('Aluminum', 'Iron') | ! is.na(LVL)) %>%
  mutate(Parameter = factor(Parameter))
```

Now, we remove Aluminum and Iron

``` r
  metals_data <- metals_data %>%
  filter(! Parameter %in% c('Aluminum', 'Iron')) %>%
  mutate(Parameter = factor(Parameter))
```

# Metals by Region

This is much easier to interpret if the metals are ordered by metal
concentrations.

``` r
metals_data %>%
  mutate(Parameter = fct_reorder(Parameter,Result)) %>%
  ggplot(aes(x = Parameter, y = Result)) +
  geom_point(aes(color = LVL), size = 2, alpha = 0.5) +
  
  scale_y_log10(labels=scales::label_comma(accuracy = 0.1)) +
  scale_color_manual(name = '', values = cbep_colors(), na.value = "firebrick",
                     labels = c('Below ERL','Between ERL and ERM',
                                     'Above ERM', "No Reference Defined")) +
  facet_wrap(~Region, ncol = 5) +
  theme_cbep(base_size = 12) +
  ylab('Concentration (ppm)') +
  xlab('') +
  theme(axis.text.x = element_text(size = 8, angle = 90, 
                                   vjust = 0.25, hjust = 1)) +
  theme(legend.position = 'bottom',
        panel.border = element_rect(fill = NA, size = 0.25))
```

<img src="Graphics_Metals_files/figure-gfm/metals_by_Region-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/metals_five_regions.pdf', device = cairo_pdf, width = 7, height = 5)
```

That’s pretty, but it makes it hard to compare Regions, which is the
main point.

# Metals by Metal

``` r
metals_data %>%
 #mutate(Parameter = fct_reorder(Parameter,Result)) %>%
  ggplot(aes(x = Region, y = Result)) +
  geom_jitter(aes(color = LVL), width = 0.075, size = 1, alpha = 0.4) +
  scale_y_log10(labels=scales::label_comma(accuracy = 0.1)) +
  scale_color_manual(name = '', values = cbep_colors(), na.value = "firebrick",
                     labels = c('Below ERL','Between ERL and ERM',
                                     'Above ERM', "No Reference Defined")) +
  facet_wrap(~Parameter, ncol = 3, scales = 'free_y', strip.position = 'top') +
  theme_cbep(base_size = 10) +
  ylab('Concentration (ppm)') +
  xlab('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1)) +
  theme(legend.position = 'bottom',
        panel.border = element_rect(fill = NA, size = 0.25)) +
  theme(strip.placement = 'inside')
```

<img src="Graphics_Metals_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r
 ggsave('figures/metals_parameters.pdf', device = cairo_pdf, width = 6, height = 5)
```

# Trend Graphics

## Initial Draft

``` r
plt <- metals_data %>%
  ggplot(aes(x = Sample_Year, y = Result)) +
  geom_point(aes(color = LVL), size = 2, alpha = 0.5) +
 
  
  scale_y_log10(labels=scales::label_comma(accuracy = 0.1)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010)) +
  scale_color_manual(name = '', values = cbep_colors(), na.value = "firebrick",
                     labels = c('Below ERL','Between ERL and ERM',
                                     'Above ERM', "No Reference Defined")) +
  
  facet_wrap(~Parameter, ncol = 3, scales = 'free_y', strip.position = 'top') +

  theme_cbep(base_size = 10) +

  ylab('Concentration (ppm)') +
  xlab('') +

  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1)) +
  theme(legend.position = 'bottom',
        panel.border = element_rect(fill = NA, size = 0.25))

plt+
   geom_smooth(method = 'lm', se = FALSE, color = cbep_colors()[3],
              lwd = 0.5, lty = 2)
#> `geom_smooth()` using formula 'y ~ x'
```

<img src="Graphics_Metals_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

## Add Statistically Significant Trendlines

We should NOT show trend lines for metals with no significant trend

We can build a dataframe to control where prediction lines are drawn as
follows. We run linear models, extract coefficients (or predictions),
calculate predictions, and plot.

## Calculate Regression line Points

``` r
  slope_and_intercept <- metals_data %>%
  # SELECT STATISTICALLY SIGNIFICANT TRENDS ONLY
  # (See Metals_Trend_Analysis.Rmd)
  filter(Parameter %in% c("Cadmium", "Chromium", "Lead",
                          "Nickel",  "Zinc",     "Silver")) %>%
  # Calculate linear models
  group_by(Parameter) %>%
  nest() %>%
  mutate(mod = lapply(data,
                      function(df) lm(log(Result) ~ Sample_Year, data = df))) %>%
  # Extract coefficients
  mutate(intercept  = unlist(lapply(mod, function(X) coefficients(X)[1])),
         slope  = unlist(lapply(mod, function(X) coefficients(X)[2]))) %>%
  # Clean up
  select(-mod, - data) %>%
  unnest(c(slope, intercept)) %>%
  ungroup(Parameter)
```

``` r
predicts <- slope_and_intercept %>%
  # add upper and lower limits for regression lines.  Since these are linear
  # models, we don't need intermediate x values.
  mutate(lwr = 1990, upr = 2011) %>%
  # Pivot Longer
  pivot_longer(c(lwr,upr), names_to = "end", values_to = "Year") %>%
  # calculate predicted values
  mutate(predict = exp(intercept + slope * Year))
```

## Final Version

``` r
plt + geom_line(aes(Year, predict), data = predicts, lwd = 0.5, lty = 2)
```

<img src="Graphics_Metals_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/metals_trends.pdf',
       device = cairo_pdf, width = 7, height = 5)
```
