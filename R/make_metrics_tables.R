#' Make a metrics table for Grattan
#' @name make_metrics_tables
#' @param data The data you want to convert into a metrics table.
#' Note that it should be in the format of a metrics table.
#'
#' @import dplyr
#'
#' @export


## quiets concerns of R CMD check re undefined vars
globalVariables(c("Australia", "metrics", "."))


make_metrics_tables <- function(data) {

  # Remove rows that don't have an Australia value (ie gap rows)
  data <- dplyr::filter(data, !is.na(Australia))

  # Run on all valid rows in the data
  lines <- data %>%
    purrr::map(1:nrow(.), get_row, data = .) %>%
    unlist(lines)

  readr::write_lines(lines, "metrics_table.tex")
  message(paste0("Yay! The table has been produced and exported as metrics_table.tex",
                 "\n",
                 "(Make sure to give it a good once-over, though)"))


  # Make chapter metrics tables with the same data
  make_chapter_metrics_tables(data, useLines = lines)

}


