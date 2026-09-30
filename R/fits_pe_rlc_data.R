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
  ## Models estimations ----
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
                                 values = list(phytotools::fitEP(x = current_data_id$par,
                                                                 y = current_data_id$fqfm,
                                                                 normalize = normalize,
                                                                 fitmethod = current_method)))
      }, warning = function(w) {
        logger::log_warn(conditionMessage(w),
                         "Check data for EP model and fit method {current_method}.")
        invokeRestart("muffleWarning")
      })
      withCallingHandlers({
        current_models <- append(x = current_models,
                                 values = list(phytotools::fitJP(x = current_data_id$par,
                                                                 y = current_data_id$fqfm,
                                                                 normalize = normalize,
                                                                 fitmethod = current_method)))
      }, warning = function(w) {
        logger::log_warn(conditionMessage(w),
                         "Check data for JP model and fit method {current_method}.")
        invokeRestart("muffleWarning")
      })
      withCallingHandlers({
        current_models <- append(x = current_models,
                                 values = list(phytotools::fitPGH(x = current_data_id$par,
                                                                  y = current_data_id$fqfm,
                                                                  normalize = normalize,
                                                                  fitmethod = current_method)))
      }, warning = function(w) {
        logger::log_warn(conditionMessage(w),
                         "Check data for PGH model and fit method {current_method}.")
        invokeRestart("muffleWarning")
      })
      withCallingHandlers({
        current_models <- append(x = current_models,
                                 values = list(phytotools::fitWebb(x = current_data_id$par,
                                                                   y = current_data_id$fqfm,
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
    ## Graphical displays ----
    formulas <- c("EP" = quote(expr = E / ((1 / (alpha[1] * eopt[1]^2)) * E^2 + (1 / ps[1] - 2 / (alpha[1] * eopt[1])) * E + (1 / alpha[1]))),
                  "JP" = quote(expr = alpha[1] * ek[1] * tanh(x = E / ek[1])),
                  "PGH" = quote(expr = ps[1] * (1 - exp(x = -1 * alpha[1] * E / ps[1])) * exp(x = -1 * beta[1] * E / ps[1])),
                  "Webb" = quote(expr = alpha[1] * ek[1] * (1 - exp(x = -E / ek[1]))))
    for (current_model_id in seq_len(length.out = length(x = current_data_id_models))) {
      current_model <- current_data_id_models[[current_model_id]]
      current_ggplot <- ggplot2::ggplot(data = current_data_id,
                                        ggplot2::aes(x = .data$par,
                                                     y = .data$fqfm)) +
        ggplot2::geom_point() +
        ggplot2::scale_x_continuous(limits = c(0, 1500)) +
        ggplot2::scale_y_continuous(limits = c(0, 150)) +
        ggplot2::labs(x = "PAR",
                      y = "Photosynthetic rate",
                      title = names(current_models[current_model_id]))
      current_formula <- formulas[[current_model$model]]
      E <- seq(0,
               1500,
               by = 1)
      current_pr_model_estimate <- eval(expr = current_formula,
                                        envir = current_model)
      current_data_estimate <- tibble::tibble("par" = E,
                                              "fqfm" = current_pr_model_estimate)
      current_ggplot <- current_ggplot + ggplot2::geom_line(data = current_data_estimate,
                                                            ggplot2::aes(x = .data$par,
                                                                         y = .data$fqfm),
                                                            color = "red")
      current_data_id_models[[current_model_id]]$graphic <- current_ggplot
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
