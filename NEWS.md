# bdsmodels 0.3.3

- Updates `collect_predictors()` in preparation to model `overweight-4y`, version `20230905`
- Updates `map_varnames()` and built-in `mapping` variable in preparation to model `overweight-4y`, version `20230905`
- Adds built-in variable `age_map` that defines how to bin ages

# bdsmodels 0.3.2

- Adds a new function `map_varnames()` that transforms the JAMES child data into the CBS model format
- Adds a vignette documenting experiences with `ranger::predict.ranger()`

# bdsmodels 0.3.1

- Refreshes the built-in data `pc4` and `table34` so that their output column have the right type and factor levels
- Removes desired country levels info from `collect_predictors()`

# bdsmodels 0.3.0

- Adds a new function `collect_predictors()` for outcome `overweight-4y`
- Adds a table `pc4` to match 4-digit postal code to `woz` and `urb`
- Updates the vignette with altered definitions for parental country of birth and level of education

# bdsmodels 0.2.0

- Adds vignette that documents the mapping between CBS and JAMES models for outcome `overweight-4y`
- Automates site building with `pkgdown`

# bdsmodels 0.1.0

- Initial commit
