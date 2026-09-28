#' @title Support tool for phytotools R package
#' @description
#' This function provides an enhancement of the processes developed under the R package phytotools.
#' Initially, phytotools fits PE and RLC data to one of a four published PE models, Eilers and Peeters 1988, Jassby and Platt 1976, Platt, Gallegos and Harrison 1980 or Webb et al. 1974.
#' @param data_id Optional. By default NULL. Type "character" or "factor" expected. Vector of id(s) for a given RLC. If NULL, all data should be associated with a single RLC.
#' @param data_par Mandatory. Type "numeric" expected. Vector of PAR data. Units of umol m-2 s-1.
#' @param data_fqfm Mandatory. Type "numeric" expected. Vector of Photosynthetic rate or PSII quantum efficiency data.
#' @param fit_methods Mandatory. By default "Nelder-Mead". Type "character" or"factor" expected. You can use one of several methods among "Marq", "Port", "Newton", "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN" or "Pseudo".
#' @param normalize Optional. By default TRUE. Type "boolean" expected. Set to TRUE if you want to normalize data.
#' @returns Return a list in the R environment.
#' @export
#' @importFrom rlang .data
fits_pe_rlc_data <- function(data_id = NULL,
                             data_par,
                             data_fqfm,
                             fit_methods = "Nelder-Mead",
                             normalize = TRUE) {
  # Checks dependancies availability for phytotools packages
  if (! requireNamespace("phytotools",
                        quietly = TRUE)) {
    stop("The \"phytotools\" package is mandatory for this function.\n",
         "Install it along with its dependencies through:\n",
         "  install.packages(\"FME\")\n",
         "  remotes::install_github(\"https://github.com/cran/insol\")\n",
         "  remotes::install_github(\"https://github.com/cran/phytotools\")",
         call. = FALSE)
    }
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
  # Log setup ----
  log_storage <- log_storage()
  logger::log_appender(logger::appender_console,
                       index = 1)
  logger::log_appender(log_appender_tibble(name_log_tibble = "log_storage"),
                       index = 2)
  # Process ----
  global_data <- tibble::tibble("id" = data_id,
                                "par" = data_par,
                                "fqfm" = data_fqfm)
  models <- list()
  for (current_id in unique(x = global_data$id)) {
    logger::log_info("Parameters estimations for {current_id} id")
    current_data_id <- dplyr::filter(.data = global_data,
                                     .data$id == current_id)
    current_data_id_models <- list()
    for (current_method in fit_methods) {
      current_models <- list()
      withCallingHandlers({
        current_models <- append(x = current_models,
                                 values = list(phytotools::fitEP(x = current_data_id$PAR,
                                                                 y = current_data_id$FqFm,
                                                                 normalize = normalize,
                                                                 fitmethod = current_method)))
      }, warning = function(w) {
        logger::log_warn(conditionMessage(w),
                         "Check data for EP model and fit method {current_method}.")
        invokeRestart("muffleWarning")
      })
      withCallingHandlers({
        current_models <- append(x = current_models,
                                 values = list(phytotools::fitJP(x = current_data_id$PAR,
                                                                 y = current_data_id$FqFm,
                                                                 normalize = normalize,
                                                                 fitmethod = current_method)))
      }, warning = function(w) {
        logger::log_warn(conditionMessage(w),
                         "Check data for JP model and fit method {current_method}.")
        invokeRestart("muffleWarning")
      })
      withCallingHandlers({
        current_models <- append(x = current_models,
                                 values = list(phytotools::fitPGH(x = current_data_id$PAR,
                                                                  y = current_data_id$FqFm,
                                                                  normalize = normalize,
                                                                  fitmethod = current_method)))
      }, warning = function(w) {
        logger::log_warn(conditionMessage(w),
                         "Check data for PGH model and fit method {current_method}.")
        invokeRestart("muffleWarning")
      })
      withCallingHandlers({
        current_models <- append(x = current_models,
                                 values = list(phytotools::fitWebb(x = current_data_id$PAR,
                                                                   y = current_data_id$FqFm,
                                                                   normalize = normalize,
                                                                   fitmethod = current_method)))
      }, warning = function(w) {
        logger::log_warn(conditionMessage(w),
                         " Check data for Webb model and fit method {current_method}.")
        invokeRestart("muffleWarning")
      })
      names(current_models) <- paste(unlist(x = lapply(X = current_models,
                                                       FUN = function(x) x$model)),
                                     "fitmethod",
                                     current_method,
                                     sep = "_")
      current_data_id_models <- append(x = current_data_id_models,
                                       values = current_models)
    }
    models <- append(x = models,
                     values = list(list("data" = current_data_id,
                                        "models_estimations" = current_data_id_models)))
    names(x = models)[length(x = models)] <- current_id
    logger::log_info("Parameters estimations successful for {current_id} id")
  }
  models <- append(x = models,
                   values = list("log" = log_storage),
                   after = 0)
}
