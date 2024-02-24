# chkptstanr 0.1.1+

### New maintainer

* With the permission of the original creator, Donald R. Williams, Ven Popov becomes the new maintainer of the package. The development will continue at [venpopov/chkptstanr](https:://github.com/venpopov/chkptstanr). 

### New features

* chkpt_brms() now works with any brm() arguments, including custom families, data2, etc ([issue #15](https://github.com/donaldRwilliams/chkptstanr/issues/15))
* create_folder() does not give an error if a folder already exist, it just returns the path

### Bug fixes
* Fix major bug "stan_code_path" not found when resuming sampling ([issue #8](https://github.com/donaldRwilliams/chkptstanr/issues/8)]

# chkptstanr 0.1.1

### Bug fixes and minor improvements

* Bug fix ([issue #3](https://github.com/donaldRwilliams/chkptstanr/issues/3)):
"model_thread.exe not found on unbuntu/mac". Added a suggestion to 
check for the operating system.

### chkptstanr 0.1.0

Initial release
