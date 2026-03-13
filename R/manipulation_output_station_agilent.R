#' @title Raw data manipulation from TapeStation Agilent 4150
#' @description
#' This function transform raw data created from the tape station Agilent 4150, an automated electrophoresis platform.
#' This machine is used to perform microelectrophoresis of DNA or RNA nucleic acids. It provides data on the size of DNA or RNA fragments and quantities.
#' @param raw_data_path Mandatory. Class "character" is expected. Path of the csv raw date file.
#' @param output_path Optional. Class "character" is expected. Be default NULL. Directory path  directory for csv output extraction.
#' @returns Return a tibble in the R environment and optionally create a csv output ("output_path" argument).
#' @export
#' @examples
#' \dontrun{
#' manipulation_raw_data_tape_station_agilent(raw_date_path = "path_of_my_raw_data.csv",
#'                                            output_path = "path_of_my_output_directory")
#' }
manipulation_raw_data_tape_station_agilent <- function(raw_data_path,
                                                       output_path = NULL) {
  # 1 - Global argument check ----
  if (missing(x = raw_data_path)
      || ! inherits(x = raw_data_path,
                    what = "character")
      || length(x = raw_data_path) != 1
      || is.na(x = stringr::str_extract(string = raw_data_path,
                                        pattern = "\\.csv$"))) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, invalid \"raw_data_path\" argument.")
  }
  if (! is.null(x = output_path)
      && (! inherits(x = output_path,
                     what = "character")
          || length(x = output_path) != 1)) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, invalid \"output_path\" argument.")
  }
  # 2 - Global process ----
  message(format(x = Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          " - Start raw data import and manipulations")
  raw_data <- readr::read_delim(file = raw_data_path,
                                delim = ",",
                                col_types = "ccciiidddc") %>%
    dplyr::rename(file_name = "FileName",
                  well_id = "WellId",
                  sample_description = "Sample Description",
                  from_bp = "From [bp]",
                  to_bp = "To [bp]",
                  average_size_bp = "Average Size [bp]",
                  "conc_ng_\u00b5l" = "Conc. [ng/\u00b5l]",
                  region_molarity_nmol_l = "Region Molarity [nmol/l]",
                  pourcentage_of_total = "% of Total",
                  region_comment = "Region Comment")
  final_data <- raw_data
  message(format(x = Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          " - Successful raw data import and manipulations")
  # 3 - Export ----
  if (! is.null(x = output_path)) {
    final_output_path <- file.path(output_path,
                                   paste0(format(x = Sys.time(),
                                                 "%Y%m%d_%H%M%S"),
                                          "_tape_station_agilent_data_improved"))
    dir.create(path = final_output_path)
    readr::write_csv(x = final_data,
                     file = file.path(final_output_path,
                                      "tape_station_agilent_data_improved.csv"))
    message(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Successful data export in the output directory \"",
            final_output_path,
            "\"")
  }
  return(final_data)
}
