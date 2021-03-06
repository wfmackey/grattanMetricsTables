#' Make a metrics table for Grattan
#' @name make_metrics_tables
#' @param data The data you want to convert into a metrics table.
#' Note that it should be in the format of a metrics table.
#'
#' @import dplyr
#' @importFrom forcats fct_rev
#'
#' @export


## quiets concerns of R CMD check re undefined vars
globalVariables(c("Australia", "metrics", ".", "country", "value"))


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


  # Export metrics table data for use in Shiny App
  export <- data %>%
    do(zoo::na.locf(.)) %>%
    rename(chapter = 1, metric = 2) %>%
    gather(key = "country", value = "value", -1, -2) %>%
    mutate(country = forcats::fct_rev(factor(country)),
           num = as.numeric(value)) %>%
    rename(char = value) %>%
    mutate(isAus = country == "Australia")

  readr::write_rds(export, "data/ob_data.Rds")


  # Export Excel sheet
    # Generate table
    chart_data_export <-
    data %>%
      do(zoo::na.locf(.)) %>%
      # Drop 'higher number' column
      select(-starts_with("Higher"))

    # Set save path
    chart_data_path <- "~/Dropbox (Grattan Institute)/Grattan Report - Commonwealth Orange Book 2019/Final PDF"
    if (dir.exists(chart_data_path)) {
        message("Saving chart data Excel file to Grattan Report - Commonwealth Orange Book 2019/Final PDF")
        chart_data_export_path <- "~/Dropbox (Grattan Institute)/Grattan Report - Commonwealth Orange Book 2019/Final PDF/metrics_chart_data.xlsx"
    } else {
      message("Saving chart data Excel file to data")
      chart_data_export_path <- "data/metrics_chart_data.xlsx"
    }


    # Create and export xlsx worksheet
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "metrics_chart_data")
    openxlsx::writeData(wb, "metrics_chart_data", chart_data_export, rowNames = FALSE)
    openxlsx::saveWorkbook(wb, chart_data_export_path, overwrite = TRUE)


}




