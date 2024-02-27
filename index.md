# Checkpoint MCMC Sampling with Stan 

Fit Bayesian models in **Stan** (Carpenter et al. 2017) with checkpointing, that is, 
the ability to stop the HMC sampler at will, and then pick right back up where the 
sampler left off. Custom **Stan** models can be fitted, or the popular package 
**brms** (Bürkner 2017) can be used to generate the **Stan** code. This package 
is fully compatible with the `R` packages 
[**brms**](http://paul-buerkner.github.io/brms/), [**posterior**](https://mc-stan.org/posterior/), 
[**cmdstanr**](https://mc-stan.org/cmdstanr/), 
and [**bayesplot**](https://mc-stan.org/bayesplot/).

There are a variety of use cases for **chkptstanr**, 
including (but not limited to) the following:

* The primary motivation for developing  **chkptstanr** is to
  reduce the cost of fitting models with **Stan** when using, say, AWS,
  and in particular by taking advantage of so-called *spot instances*. 
  These instances are "a cost-effective choice if you can be flexible about 
  when your applications run and if your applications can be 
  *interrupted* [emphasis added]" 
  [AWS website]((https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-spot-instances.html)).
  
  **chkptstanr** thus allows for taking advantage of spot instances by 
  enabling "interruptions" during model fitting. This can reduce the cost
  by 90 %.
  
* **Stan** allows for fitting complex models. This often entails 
  iteratively improving the model to ensure that the MCMC algorithm
  has converged. Typically this requires waiting until the model has 
  *finished sampling*, and then assessing MCMC diagnostics (e.g., R-hat).
  
  **chkptstanr** can be used to make iterative model building more  
  efficient, e.g., by having the ability to pause sampling and examine the model 
  (e.g., convergence diagnostics), and then deciding to stop sampling or to continue on.
  

* Computationally intensive models can sometimes take several days to 
  finish up. When using a personal computer, this can take up all 
  the computing resources.
  
  **chkptstanr** can be used with scheduling, such that the model is fitted 
  during certain windows (e.g., at night, weekends, etc.)

* Those familiar with Bayesian methods will know all too well that a model can take 
  longer than expected. This can be problematic when there is another task
  that needs to be completed, because one is faced with
  waiting it out or stopping the model (and loosing all of the progress).
  
  **chkptstanr** makes it so that models can be conveniently stopped 
  if need be, while not loosing any of the progress.



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

## References

Bürkner P (2017). “brms: An R package for Bayesian multilevel models using Stan.” 
Journal of statistical software, 80, 1–28.

Carpenter B, Gelman A, Hoffman MD, Lee D, Goodrich B, Betancourt M, Brubaker M, 
Guo J, Li P, Riddell A (2017). “Stan: A probabilistic programming language.” Journal of statistical software, 76(1).
