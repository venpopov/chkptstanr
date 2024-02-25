# chkptstanr 0.1.3

### New maintainer

* With the permission of the original creator, Donald R. Williams, Ven Popov becomes the new maintainer of the package. The development will continue at [venpopov/chkptstanr](https:://github.com/venpopov/chkptstanr). 

### Major bug fixes
* Fix major bug `"stan_code_path" not found` when resuming sampling, preventing the core functionality of the package ([original issue #8](https://github.com/donaldRwilliams/chkptstanr/issues/8)]
* `chkpt_brms()` now works with any `brm()` arguments, including custom families, data2, etc ([original issue #15](https://github.com/donaldRwilliams/chkptstanr/issues/15))

### New features

* a new argument `stop_after` to `chkpt_brms()` and `chkpt_stan()` allows you to predetermine a fixed point to stop the sampling after a certain number of iterations, e.g. `stop_after = 1000` will stop the sampling after 1000 iterations. ([original issue #4](https://github.com/donaldRwilliams/chkptstanr/issues/4))
* `chkpt_brms()` now returns a `brmsfit` object if post-warmup sampling is stopped for any reason, either programmatically via stop_after, because of an error, or due to a manual abort by the user. The `brmsfit` object will contain samples until the last successful checkpoint. You can resume sampling from the last checkpoint by rerunning the same code. (#4)
* a new argument `reset` to chkpt_brms() and chkpt_stan() allows you to reset the checkpointing process and start from the beginning without recompiling the model. Setting `reset = TRUE` will delete the existing checkpoints but keep the stan model code and binary. This is also available via the new function `reset_checkpoints(path)`, which achieves the same.
* `create_folder()` is deprecated. Please provide the folder name or full path to the argument path directly to `chkpt_brms()` and a folder to store the checkpoints will be created automatically. This significantly simplifies the workflow. 
* `create_folder()` and the path argument to `chkpt_brms()` and `chkpt_stan()` no longer give an error if a folder already exist, allowing a reusable programmatic workflow
* `create_folder()` and the path argument to `chkpt_brms()` and `chkpt_stan()` works with nested folder names, e.g. `"output/checkpoints1"`, even if `output/` does not exist

## Minor bug fixes
* Fix an incorrect error message when providing iter_warmup, iter_sampling, or iter_warmup+iter_sampling not divisible by iter_per_chkpt. The error message now correctly states that the number of iterations per checkpoint must be a divisor of the all three quantities.


### Other changes
* Set-up initial automated testing and continuous integration with GitHub Actions to ensure the package is always working as expected

# chkptstanr 0.1.1

### Bug fixes and minor improvements

* Bug fix ([original issue #3](https://github.com/donaldRwilliams/chkptstanr/issues/3)):
"model_thread.exe not found on unbuntu/mac". Added a suggestion to 
check for the operating system.

### chkptstanr 0.1.0

Initial release
