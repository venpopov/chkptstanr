
# chkptstanr <img src="man/figures/logo.png" align="right" width = 250 />

# chkptstanr <!-- badges: start -->

[![R-CMD-check](https://github.com/venpopov/chkptstanr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/venpopov/chkptstanr/actions/workflows/R-CMD-check.yaml)
[![CRAN
Version](http://www.r-pkg.org/badges/version/chkptstanr)](https://cran.r-project.org/package=chkptstanr)
[![Downloads](https://cranlogs.r-pkg.org/badges/chkptstanr)](https://cran.r-project.org/package=chkptstanr)
<!-- badges: end -->

The goal of **chkptstanr** is to fit Bayesian models in Stan with
checkpointing, that is, the ability to stop the MCMC sampler at will,
and then pick right back up where the MCMC sampler left off. Custom Stan
model can be fitted, or the popular package brms can be used to generate
the Stan code. This package is fully compatible with the `R` packages
**brms**, **posterior**, **cmdstanr**, and **bayesplot**.

## About this fork

The original package was developed by [Donald R.
Williams](https://github.com/donaldRwilliams/chkptstanr). However, the
package has not been updated in 2 years, despite breaking issues. Donald
has kindly agreed for me to take over the package maintenance and
development.

## Installation

The current CRAN version (0.1.1) has several bugs, and until the next
release, you can install the development version from GitHub:

``` r
remotes::install_github("venpopov/chkptstanr")
```

These packages are needed.

## Packages

``` r
library(chkptstanr)
library(brms)
library(lme4)
```

## How to use chkptstanr

The primary use of **chkptstanr** is to sample from the posterior
distribution, while having the option of starting and stopping the
sampler at will. Currently you have two options:

- Manually abort the sampling process at any point
- Predetermine the stopping point by specifying the number of iterations
  after which to stop the sampler via the `stop_after` argument

### Example 1: Manual abort

To make this clear, we will run the main example from the `brms`
(webpage)\[<https://paul-buerkner.github.io/brms/#how-to-use-brms>\].
The only additional arguments here are `path`, the location of the
folder in which to store intermediate samples, and `iter_per_chkpt`, the
number of iterations between each checkpoint. After the model sampled
for 1200 iterations, we *manually* abort it:

``` r
fit1 <- chkpt_brms(count ~ zAge + zBase * Trt + (1|patient),
                   data = epilepsy, 
                   family = poisson(),
                   iter_per_chkpt = 200,
                   path = 'checkpoints/epilepsy')


#> Compiling Stan program...
#> Initial Warmup (Typical Set)
#> Chkpt: 1 / 10; Iteration: 200 / 2000 (warmup)
#> Chkpt: 2 / 10; Iteration: 400 / 2000 (warmup)
#> Chkpt: 3 / 10; Iteration: 600 / 2000 (warmup)
#> Chkpt: 4 / 10; Iteration: 800 / 2000 (warmup)
#> Chkpt: 5 / 10; Iteration: 1000 / 2000 (warmup)
#> Chkpt: 6 / 10; Iteration: 1200 / 2000 (sample)
#> Sampling aborted. You can examine the results or continue sampling by rerunning the same code.
```

If the sampler is passed the warmup stage, it returns a `brmsfit`
object, so you can examine the results:

``` r
summary(fit1)
```

     Family: poisson 
      Links: mu = log 
    Formula: count ~ zAge + zBase * Trt + (1 | patient) 
       Data: data (Number of observations: 236) 
      Draws: 4 chains, each with iter = 1200; warmup = 1000; thin = 1;
             total post-warmup draws = 800

    Multilevel Hyperparameters:
    ~patient (Number of levels: 59) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.57      0.07     0.45     0.72 1.02      209      236

    Regression Coefficients:
               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept      1.75      0.11     1.52     1.98 1.02      206      371
    zAge           0.10      0.08    -0.07     0.25 1.02      240      443
    zBase          0.70      0.11     0.47     0.95 1.00      220      370
    Trt1          -0.24      0.16    -0.56     0.07 1.02      199      239
    zBase:Trt1     0.05      0.15    -0.26     0.35 1.00      243      350

    Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

We see that the model has not converged, and we can continue sampling by
rerunning the same code.

``` r
fit1 <- chkpt_brms(count ~ zAge + zBase * Trt + (1|patient),
                   data = epilepsy, 
                   family = poisson(),
                   iter_per_chkpt = 200,
                   path = 'checkpoints/epilepsy')

#> Model executable is up to date!
#> Chkpt: 7 / 10; Iteration: 1400 / 2000 (sample)
#> Chkpt: 8 / 10; Iteration: 1600 / 2000 (sample)
#> Chkpt: 9 / 10; Iteration: 1800 / 2000 (sample)
#> Chkpt: 10 / 10; Iteration: 2000 / 2000 (sample)
#> Checkpointing complete
```

And examine the final results:

``` r
summary(fit1)
```

    Family: poisson 
      Links: mu = log 
    Formula: count ~ zAge + zBase * Trt + (1 | patient) 
       Data: data (Number of observations: 236) 
      Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
             total post-warmup draws = 4000

    Multilevel Hyperparameters:
    ~patient (Number of levels: 59) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.58      0.07     0.45     0.73 1.00      956     1696

    Regression Coefficients:
               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept      1.77      0.12     1.53     2.00 1.00      887     1575
    zAge           0.10      0.09    -0.07     0.26 1.00      871     1256
    zBase          0.70      0.12     0.47     0.94 1.00      986     1675
    Trt1          -0.26      0.16    -0.60     0.05 1.01      987     1170
    zBase:Trt1     0.05      0.16    -0.26     0.37 1.00     1075     1824

### Predetermine stopping point

In addition to manually aborting the run, we can predetermine the
stopping point by specifying the number of iterations after which to
stop the sampler via the `stop_after` argument.

``` r
fit1 <- chkpt_brms(count ~ zAge + zBase * Trt + (1|patient),
                   data = epilepsy, 
                   family = poisson(),
                   iter_per_chkpt = 200,
                   stop_after = 1400,
                   path = 'checkpoints/epilepsy')


#> Compiling Stan program...
#> Initial Warmup (Typical Set)
#> Chkpt: 1 / 10; Iteration: 200 / 2000 (warmup)
#> Chkpt: 2 / 10; Iteration: 400 / 2000 (warmup)
#> Chkpt: 3 / 10; Iteration: 600 / 2000 (warmup)
#> Chkpt: 4 / 10; Iteration: 800 / 2000 (warmup)
#> Chkpt: 5 / 10; Iteration: 1000 / 2000 (warmup)
#> Chkpt: 6 / 10; Iteration: 1200 / 2000 (sample)
#> Chkpt: 7 / 10; Iteration: 1400 / 2000 (sample)
#> Sampling aborted. You can examine the results or continue sampling by rerunning the same code.
```

### Reset sampling

If we want to reset the sampling, we can use the `reset` argument, as
long as we have not changed any of the key arguments. For example, we
can reset the sampling and start from scratch, but we cannot change the
formula, data, or family (but we can change “stop_after”)

``` r
fit1 <- chkpt_brms(count ~ zAge + zBase * Trt + (1|patient),
                   data = epilepsy, 
                   family = poisson(),
                   iter_per_chkpt = 200,
                   path = 'checkpoints/epilepsy',
                   stop_after = 1600,
                   reset = TRUE)
```

If we try to change the formula, data, or family, we will get an error:

``` r
fit1 <- chkpt_brms(count ~ 1 + (1|patient),
                   data = epilepsy, 
                   family = poisson(),
                   iter_per_chkpt = 200,
                   path = 'checkpoints/epilepsy',
                   stop_after = 1600,
                   reset = TRUE)
```

    Error: Important arguments have been changed. Please completely reset the checkpointing via reset_checkpoints(path, recompile = TRUE).
    Interupted before or during warmup. No samples available.

This is because we cannot use the existing compiled model. We need to
reset the checkpoints and recompile the model:

``` r
reset_checkpoints('checkpoints/epilepsy', recompile = TRUE)
```
