# Checkpoint MCMC Sampling with Stan

Fit Bayesian models in **Stan** with checkpointing, that is, the ability to stop 
the HMC sampler at will, and then pick right back up where the sampler left off. 
Custom **Stan** models.  Custom **Stan** models
can be fitted, or the popular package **brms**
can be used to generate the **Stan** code. This package is fully compatible with the 
*R* packages **brms**, **posterior**, **cmdstanr**, and **bayesplot**

There are a variety of use cases for **chkptstanr**, 
including (but not limited to) the following:

* The primary motivation for developing  \strong{chkptstanr} is to
  reduce the cost of fitting models with \strong{Stan} when using, say, AWS,
  and in particular by taking advantage of so-called *spot instances*. 
  These instances are "a cost-effective choice if you can be flexible about 
  when your applications run and if your applications can be 
  *interrupted* [emphasis added]".
