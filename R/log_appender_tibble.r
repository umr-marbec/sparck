#' @title Personalised log record appender function
#' @description
#' This function is a personalised log record appender used according the function log_appender of the R package logger.
#' @param name_log_tibble Optional. Type "character" expected. Name of the tibble which will contain log information. Use the function log_storage to generate an empty tibble with a validated template.
#' @returns Return a tibble in the R environment.
#' @export
log_appender_tibble <- function(name_log_tibble = "log_storage") {
  target_environment <- parent.frame()
  # Global argument(s) check ----
  checkmate::assert_character(x = name_log_tibble,
                              len = 1)
  checkmate::assert_true(x = exists(x = name_log_tibble,
                                    envir = target_environment,
                                    inherits = FALSE))
  log_tibble <- get(x = name_log_tibble,
                    envir = target_environment,
                    inherits = FALSE)
  checkmate::assert_names(x = names(x = log_tibble),
                          must.include = c("timestamp",
                                           "level",
                                           "message"))
  log_tibble_expected_types <- c("timestamp" = "POSIXct",
                                 "level" = "character",
                                 "message" = "character")
  for (colname in names(x = log_tibble_expected_types)) {
    checkmate::assert_class(x = log_tibble[[colname]],
                            classes = log_tibble_expected_types[[colname]],
                            .var.name = colname)
  }
  # Function to appender ----
  function(lines) {
    log_tibble <- get(x = name_log_tibble,
                      envir = target_environment,
                      inherits = FALSE)
    for (log_line in lines) {
      log_level <- stringr::str_extract(string = log_line,
                                        pattern = ".*(?= \\[)")
      log_timestamp <- as.POSIXct(stringr::str_extract(string = log_line,
                                                       pattern = "(?<=\\[).*(?=\\])"))
      log_message <- stringr::str_extract(string = log_line,
                                          pattern = "(?<=\\] ).*")
      log_tibble <- tibble::add_row(.data = log_tibble,
                                    timestamp = log_timestamp,
                                    level = log_level,
                                    message = log_message)
    }
    assign(x = name_log_tibble,
           value = log_tibble,
           envir = target_environment)
  }
}
