# Support tool for phytotools R package

This function provides an enhancement of the processes developed under
the R package phytotools. Initially, phytotools fits PE and RLC data to
one of a four published PE models, Eilers and Peeters 1988, Jassby and
Platt 1976, Platt, Gallegos and Harrison 1980 or Webb et al. 1974.

## Usage

``` r
fits_pe_rlc_data(
  data_id = NULL,
  data_par,
  data_fqfm,
  fit_methods = "Nelder-Mead",
  normalize = TRUE
)
```

## Arguments

- data_id:

  Optional. By default NULL. Type "character" or "factor" expected.
  Vector of id(s) for a given RLC. If NULL, all data should be
  associated with a single RLC.

- data_par:

  Mandatory. Type "numeric" expected. Vector of PAR data. Units of umol
  m-2 s-1.

- data_fqfm:

  Mandatory. Type "numeric" expected. Vector of Photosynthetic rate or
  PSII quantum efficiency data.

- fit_methods:

  Mandatory. By default "Nelder-Mead". Type "character" or"factor"
  expected. You can use one of several methods among "Marq", "Port",
  "Newton", "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN" or "Pseudo".

- normalize:

  Optional. By default TRUE. Type "boolean" expected. Set to TRUE if you
  want to normalize data.

## Value

Return a list in the R environment.
