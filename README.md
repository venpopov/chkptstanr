
# chkptstanr <img src="man/figures/logo.png" align="right" width = 250 />

[![CRAN
Version](http://www.r-pkg.org/badges/version/chkptstanr)](https://cran.r-project.org/package=chkptstanr)
[![Downloads](https://cranlogs.r-pkg.org/badges/chkptstanr)](https://cran.r-project.org/package=chkptstanr)

The goal of **chkptstanr** is to fit Bayesian models in Stan with
checkpointing, that is, the ability to stop the MCMC sampler at will,
and then pick right back up where the MCMC sampler left off. Custom Stan
model can be fitted, or the popular package brms can be used to generate
the Stan code. This package is fully compatible with the `R` packages
**brms**, **posterior**, **cmdstanr**, and **bayesplot**.

## Installation

You can install the development version of **chkptstanr** like so:

``` r
# install.packages("chkptstanr")
```

These packages are needed.

## Packages

``` r
library(chkptstanr)
library(brms)
library(lme4)
```

## Data

The illustrative data are bundled with `R` package **lme4**.

``` r
data("VerbAgg")

# 20 subjects
dat_sub <- subset(VerbAgg, id %in% 1:20)

# numeric outcome
dat_sub$y <- ifelse(dat_sub$r2 == "Y", 1, 0)
```

## Rasch Model

To demonstrate how to use **chkptstanr**, we fit a Rasch model with I
fixed item effects and random person effects. This can be done with
familiar **lme4** style syntax, i.e.,

``` r
# brmsformula object
m1 <- bf(y ~ 0 + item + (1|id), family = binomial())
```

### Storage

The additional overhead is to create a folder that will store the
checkpoints, i.e.,

``` r
path <- create_folder(folder_name = "chkpt_folder_m1")
```

### Model Fitting

The primary use of **chkptstanr** is to sample from the posterior
distribution, while having the option of starting and stopping the
sampler at will.

To make this clear, we stopped the following after 2 checkpoints.

``` r
fit_m1 <- chkpt_brms(formula = m1, 
                     data = dat_sub,
                     path  = path,
                     iter_warmup = 1000,
                     iter_sampling = 1000,
                     iter_per_chkpt = 250)


#> Compiling Stan program...
#> Initial Warmup (Typical Set)
#> Chkpt: 1 / 8; Iteration: 250 / 2000 (warmup)
#> Chkpt: 2 / 8; Iteration: 500 / 2000 (warmup)
```

To start at the next checkpoint, rerun the same code.

``` r
fit_m1 <- chkpt_brms(formula = m1, 
                     data = dat_sub,
                     path  = path,
                     iter_warmup = 1000,
                     iter_sampling = 1000,
                     iter_per_chkpt = 250)

#> Sampling next checkpoint
#> Chkpt: 3 / 8; Iteration: 750 / 2000 (warmup)
#> Chkpt: 4 / 8; Iteration: 1000 / 2000 (warmup)
#> Chkpt: 5 / 8; Iteration: 1250 / 2000 (sample)
#> Chkpt: 6 / 8; Iteration: 1500 / 2000 (sample)
#> Chkpt: 7 / 8; Iteration: 1750 / 2000 (sample)
#> Chkpt: 8 / 8; Iteration: 2000 / 2000 (sample)
#> Checkpointing complete
```

### Summary

A key advantage of `chkpt_brms` is that it returns a `brmsfit` object,
as seen when printing the summary output.

``` r
fit_m1

#>  Family: binomial 
#>   Links: mu = logit 
#> Formula: y ~ 0 + item + (1 | id) 
#>    Data: data (Number of observations: 480) 
#>   Draws: 2 chains, each with iter = 1000; warmup = 0; thin = 1;
#>          total post-warmup draws = 2000

#> Group-Level Effects: 
#> ~id (Number of levels: 20) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     1.68      0.36     1.07     2.46 1.00      683      742

#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> itemS1WantCurse     1.53      0.78     0.07     3.09 1.00      883     1034
#> itemS1WantScold    -0.38      0.64    -1.61     0.86 1.00      743      888
#> itemS1WantShout     0.49      0.64    -0.69     1.77 1.00      742     1084
#> itemS2WantCurse     2.02      0.80     0.51     3.61 1.00      837      939
#> itemS2WantScold    -0.65      0.64    -1.97     0.58 1.00      854     1001
#> itemS2WantShout     0.51      0.67    -0.80     1.81 1.00      696      931
#> itemS3WantCurse     1.16      0.72    -0.24     2.61 1.00      764     1169
#> itemS3WantScold    -1.98      0.76    -3.54    -0.59 1.00      799     1122
#> itemS3WantShout    -0.08      0.64    -1.33     1.21 1.00      725     1109
#> itemS4wantCurse    -0.09      0.65    -1.41     1.12 1.00      736     1007
#> itemS4WantScold    -3.07      0.93    -5.07    -1.44 1.00     1065     1131
#> itemS4WantShout    -3.07      0.93    -5.01    -1.41 1.00     1095     1250
#> itemS1DoCurse       2.57      0.95     0.78     4.55 1.00     1125      988
#> itemS1DoScold       0.51      0.67    -0.80     1.85 1.00      738     1085
#> itemS1DoShout       0.21      0.64    -0.97     1.47 1.00      753     1241
#> itemS2DoCurse       1.97      0.80     0.49     3.61 1.00      828     1088
#> itemS2DoScold      -0.09      0.64    -1.35     1.18 1.00      759      991
#> itemS2DoShout      -1.24      0.67    -2.58     0.08 1.00      778      903
#> itemS3DoCurse       0.20      0.66    -1.16     1.50 1.00      657     1091
#> itemS3DoScold      -2.44      0.82    -4.22    -0.98 1.00      976     1012
#> itemS3DoShout      -3.07      0.97    -5.13    -1.36 1.00      999     1186
#> itemS4DoCurse       0.20      0.65    -1.03     1.48 1.00      730     1307
#> itemS4DoScold      -0.37      0.63    -1.66     0.88 1.00      709     1064
#> itemS4DoShout      -2.44      0.81    -4.14    -0.97 1.00      799     1077

#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```
