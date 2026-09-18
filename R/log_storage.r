#' @title Log tibble initialisation
#' @description
#' This function initialises a tibble with a specific format for log incrementation.
#' @returns Return a tibble in the R environment.
#' @export
log_storage <- function() {
  log_storage <- tibble::tibble("timestamp" = as.POSIXct(character()),
                                "level" = character(),
                                "message" = character())
  return(log_storage)
}
