#' @title Support tool for phytotools R package
#' @description
#' This function provides an enhancement of the processes developed under the R package phytotools.
#' Initially, phytotools fits PE and RLC data to one of a four published PE models, Eilers and Peeters 1988, Jassby and Platt 1976, Platt, Gallegos and Harrison 1980 or Webb et al. 1974.
#' @param data_id Optional. By default NULL. Type "character" or"factor" expected. Vector of id(s) for a given RLC. If NULL, all data should be associated with a single RLC.
#' @param data_par Mandatory. Type "numeric" expected. Vector of PAR data. Units of umol m-2 s-1.
#' @param data_fqfm Mandatory. Type "numeric" expected. Vector of Photosynthetic rate or PSII quantum efficiency data.
#' @param fit_methods Mandatory. By default "Nelder-Mead". Type "character" or"factor" expected.
#' @param normalize Optional. By default TRUE. Type "boolean" expected. Set to TRUE if you want to normalize data.
#' @returns Return a list in the R environment.
#' @export
fits_pe_rlc_data <- function(data_id = NULL,
                             data_par,
                             data_fqfm,
                             fit_methods = "Nelder-Mead",
                             normalize = TRUE) {
  # Global argument(s) check ----
  ## data_id ----
  if (! is.null(x = data_id)) {
    checkmate::assert_multi_class(x = data_id,
                                  classes = c("character",
                                              "factor"),
                                  null.ok = FALSE)
    checkmate::assert_vector(x = data_id,
                             min.len = 1)
  }
  ## data_par ----
  checkmate::assert_numeric(x = data_par,
                            min.len = 1,
                            null.ok = FALSE)
  if (! is.null(x = data_id)) {
    checkmate::assert_vector(x = data_par,
                             len = length(x = data_id))
  }
  ## data_fqfm ----
  checkmate::assert_numeric(x = data_fqfm,
                            min.len = 1,
                            null.ok = FALSE)
  checkmate::assert_vector(x = data_fqfm,
                           len = length(x = data_par))
  ## fit_methods ----
  checkmate::assert_multi_class(x = fit_methods,
                                classes = c("character",
                                            "factor"),
                                null.ok = FALSE)
  checkmate::assert_subset(x = fit_methods,
                           choices = c("Marq",
                                       "Port",
                                       "Newton",
                                       "Nelder-Mead",
                                       "BFGS",
                                       "CG",
                                       "L-BFGS-B",
                                       "SANN",
                                       "Pseudo"))
  ## normalize ----
  checkmate::assert_flag(x = normalize)
  # Process ----

}
