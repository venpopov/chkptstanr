# chkptstanr 0.2.0

This is the first release of the package under the new maintainer, Ven Popov. The package has been significantly updated and improved. The major bugs have been fixed, and new features have been added. The package is now fully compatible with the latest versions of `brms` and `cmdstanr`. The package now works as expected; however, due to issue #10, I do not recommend using it for any production work. This release is meant to capture the current state of the package after the major bugs have been fixed. The next update will correct the adaptation procedure as described in issue #10, but this will require a major rewrite of the package. The current Development Roadmap is available [here](https://github.com/venpopov/chkptstanr/issues/1).

### New maintainer

* With the permission of the original creator, Donald R. Williams, Ven Popov becomes the new maintainer of the package. The development will continue at [venpopov/chkptstanr](https:://github.com/venpopov/chkptstanr). 

### Major bug fixes
* **Resolve error "stan_code_path" not found when resuming sampling**, which completely prevented the core functionality of the package from working ([original issue #8](https://github.com/donaldRwilliams/chkptstanr/issues/8)]
* **Resolve incorrect detection of existing model binaries**, which was causing the package to fail to detect changes to arguments and incorrectly continue to sample (#2).
* **Fix the incorrect combination of checkpoint samples into a single stanfit object**, which was causing problems with post-processing methods (#8)
* **chkpt_brms() now works with any brm() arguments**, including custom families, data2, etc, rather than giving an error ([original issue #15](https://github.com/donaldRwilliams/chkptstanr/issues/15))

### New features

* **Add argument "stop_after" to predetermine a stopping checkpoint**. This allows you to predetermine a fixed point to stop the sampling after a certain number of iterations, e.g. `stop_after = 1000` will stop the sampling after 1000 iterations. ([original issue #4](https://github.com/donaldRwilliams/chkptstanr/issues/4))
* **Add argument `reset` to restart sampling**. This allows you to reset the checkpointing process and start from the beginning without recompiling the model. Setting `reset = TRUE` will delete the existing checkpoints but keep the stan model code and binary. This is also available via the new function `reset_checkpoints(path)`, which achieves the same.
* **Return a brmsfit object when sampling is interrupted**. Instead of having to reconstruct the samples manually, `chkpt_brms()` now returns a `brmsfit` object if post-warmup sampling is stopped for any reason, either programmatically via stop_after, because of an error, or due to a manual abort by the user. The `brmsfit` object will contain samples until the last successful checkpoint. You can resume sampling from the last checkpoint by rerunning the same code. (#4)
* **No longer necessary to manually create a folder for the checkpoints via "create_folder()"** before using `chkpt_brms()` or `chkpt_stan()`. `create_folder()` is deprecated. Please provide the folder name or full path to the argument path directly to `chkpt_brms()` and a folder to store the checkpoints will be created automatically. This significantly simplifies the workflow. 
* **You can now reuse checkpoint folders**. The path argument to `chkpt_brms()` and `chkpt_stan()` no longer give an error if a folder already exist, allowing a reusable programmatic workflow
* **Checkpoint folders can be specified with a nested path**. The path argument to `chkpt_brms()` and `chkpt_stan()` works with nested folder names, e.g. `"output/checkpoints1"`, even if `output/` does not exist
* **You can now use any formula that brm accepts**. Remove an unnecessary check that the formula should be a `brmsformula` object, allowing for more flexibility in the input to `chkpt_brms()` such as `mvbrmsformula` objects or other arguments that `brm()` accepts ([original issue #9](https://github.com/donaldRwilliams/chkptstanr/issues/9#issue-1278744728))

### Minor bug fixes
* **Fix an incorrect error message** when providing iter_warmup, iter_sampling, or iter_warmup+iter_sampling not divisible by iter_per_chkpt. The error message now correctly states that the number of iterations per checkpoint must be a divisor of the all three quantities.


### Other changes
* **Automated testing for package stability**. Set-up initial automated testing and continuous integration with GitHub Actions to ensure the package is always working as expected
* **Change default number of chains from 2 to 4** to be consistent with brms defaults
* **Rename argument "iter_typical" to "iter_adaptation"** to better reflect what this stage is doing. iter_typical is deprecated. In future releases, the adaptation procedure will be rewritten and this argument will be completely removed (see #10)

# chkptstanr 0.1.1

### Bug fixes and minor improvements

* Bug fix ([original issue #3](https://github.com/donaldRwilliams/chkptstanr/issues/3)):
"model_thread.exe not found on unbuntu/mac". Added a suggestion to 
check for the operating system.

### chkptstanr 0.1.0

Initial release
