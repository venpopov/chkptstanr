
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
      Draws: 4 chains, each with iter = 200; warmup = 0; thin = 1;
             total post-warmup draws = 800

    Multilevel Hyperparameters:
    ~patient (Number of levels: 59) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.57      0.07     0.45     0.71 1.02       92      103

    Regression Coefficients:
               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept      1.75      0.11     1.54     1.97 1.04      112      158
    zAge           0.10      0.08    -0.06     0.23 1.01      142      283
    zBase          0.71      0.11     0.50     0.95 1.00      149      190
    Trt1          -0.23      0.16    -0.56     0.06 1.03      114      129
    zBase:Trt1     0.04      0.15    -0.26     0.35 1.00      140      189

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
      Draws: 4 chains, each with iter = 1000; warmup = 0; thin = 1;
             total post-warmup draws = 8000

    Multilevel Hyperparameters:
    ~patient (Number of levels: 59) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.58      0.07     0.46     0.73 1.01      441      680

    Regression Coefficients:
               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept      1.76      0.12     1.53     1.99 1.00      524      796
    zAge           0.09      0.09    -0.07     0.26 1.00      664      981
    zBase          0.71      0.12     0.47     0.94 1.00      643      895
    Trt1          -0.26      0.17    -0.58     0.06 1.00      428      726
    zBase:Trt1     0.05      0.16    -0.27     0.37 1.00      689     1013

    Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

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
