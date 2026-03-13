# Raw data manipulation from TapeStation Agilent 4150

This function transform raw data created from the tape station Agilent
4150, an automated electrophoresis platform. This machine is used to
perform microelectrophoresis of DNA or RNA nucleic acids. It provides
data on the size of DNA or RNA fragments and quantities.

## Usage

``` r
manipulation_raw_data_tape_station_agilent(raw_data_path, output_path = NULL)
```

## Arguments

- raw_data_path:

  Mandatory. Class "character" is expected. Path of the csv raw date
  file.

- output_path:

  Optional. Class "character" is expected. Be default NULL. Directory
  path directory for csv output extraction.

## Value

Return a tibble in the R environment and optionally create a csv output
("output_path" argument).

## Examples

``` r
if (FALSE) { # \dontrun{
manipulation_raw_data_tape_station_agilent(raw_date_path = "path_of_my_raw_data.csv",
                                           output_path = "path_of_my_output_directory")
} # }
```
