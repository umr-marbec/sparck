# Personalised log record appender function

This function is a personalised log record appender used according the
function log_appender of the R package logger.

## Usage

``` r
log_appender_tibble(name_log_tibble = "log_storage")
```

## Arguments

- name_log_tibble:

  Optional. Type "character" expected. Name of the tibble which will
  contain log information. Use the function log_storage to generate an
  empty tibble with a validated template.

## Value

Return a tibble in the R environment.
