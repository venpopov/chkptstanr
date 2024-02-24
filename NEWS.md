# chkptstanr 0.1.1+

### New maintainer

* With the permission of the original creator, Donald R. Williams, Ven Popov becomes the new maintainer of the package. The development will continue at [venpopov/chkptstanr](https:://github.com/venpopov/chkptstanr). 

### New features

* chkpt_brms() now works with any brm() arguments, including custom families, data2, etc ([issue #15](https://github.com/donaldRwilliams/chkptstanr/issues/15))
* create_folder() is deprecated. Please provide the folder name or full path to the argument path directly to chkpt_brms() and a folder to store the checkpoints will be created automatically. This significantly simplifies the workflow. 
* create_folder() and the path argument to chkpt_brms()/chkpt_stan() no longer give an error if a folder already exist, allowing a reusable programmatic workflow
* create_folder() and the path argument to chkpt_brms()/chkpt_stan() works with nested folder names, e.g. "output/checkpoints1", even if output/ does not exist

### Major bug fixes
* Fix major bug '"stan_code_path" not found' when resuming sampling, preventing the core functionality of the package ([issue #8](https://github.com/donaldRwilliams/chkptstanr/issues/8)]

### Other changes
* Set-up initial automated testing and continuous integration with GitHub Actions to ensure the package is always working as expected

# chkptstanr 0.1.1

### Bug fixes and minor improvements

* Bug fix ([issue #3](https://github.com/donaldRwilliams/chkptstanr/issues/3)):
"model_thread.exe not found on unbuntu/mac". Added a suggestion to 
check for the operating system.

### chkptstanr 0.1.0

Initial release
