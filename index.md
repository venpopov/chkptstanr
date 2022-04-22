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
