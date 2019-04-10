#' Make a individual chapter's metrics tables
#' @name make_chapter_metrics_tables
#' @param data The data you want to convert into a metrics table.
#' Note that this should be downloaded from the Google Doc (see 'get_metrics_data()')
#' @param useLines The vector of bulk lines from the main metrics table.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import zoo
#'

utils::globalVariables(c(".", "metric", "chapter", "lines", "separate",
                         "create_chapter_metrics", "val", "var"))


make_chapter_metrics_tables <- function(data,
                                        useLines) {

# Clean data
data <- data %>%
  rename(chapter = 1,
         metric = 2) %>%
  filter(!is.na(Australia)) %>%
  do(na.locf(.))


# Get chapter list
chapters <- data %>%
  select(chapter)


# Get metric names list
metric_names <- data %>%
  mutate(metric_names = gsub("([^\\(]*)\\(([^\\)]*)\\)", "\\1", metric)) %>%
  mutate(metric_names = gsub("\\%", "\\\\%", metric_names),
         metric_names = gsub("\\$", "\\\\$", metric_names)) %>%
  select(metric_names)

metric_units <- data %>%
  mutate(metric_units = gsub("(.*?)\\((.*)\\)", "\\2", metric)) %>%
  mutate(metric_units = gsub("\\%", "\\\\%", metric_units),
         metric_units = gsub("\\$", "\\\\$", metric_units)) %>%
  select(metric_units)

header <- bind_cols(chapters, metric_names, metric_units)


# Create chart bulk
newdata <- tibble(lines = useLines) %>%
    separate(lines, names(metrics), "\\&") %>%
    select(-1) %>%
    rename(metric = 1) %>%
  # Add chapter names
  bind_cols(chapters, .) %>%
  # Remove \\whitespace from last column
  mutate_all(function(x) gsub("\\\\\\\\  ?\\\\[a-z]*id", "", x))

# Remove 'reverse' column if exists
columns <- ncol(newdata)
if (grepl("(H|h)igh", names(newdata)[columns])) newdata <- newdata[1:(columns-1)]



create_chapter_metrics <- function(useChapter) {

  # Retrieve a chapter's metric names and labels
  chap_header <- header %>%
    filter(chapter == useChapter) %>%
    select(-1)

  # Determine column spacing
    # Total width available to metrics (after countries) is 20.76em
    number_of_metrics <- nrow(chap_header)
    em <- 20.76 / number_of_metrics

  # Custom-define column width if Economic development [quick fix :/]
  if(useChapter == "Economic development") em <- c(4.19, 5.19, 6.19, 5.19)
  if(useChapter == "Regional development") em <- c(10.38, 8.38)

  # Add column spacing/formatting to header
  chap_header <- chap_header %>%
    mutate(metric_names = str_c("\\multicolumn{1}{p{", em, "em}}{\\textbf{", metric_names, "}}"),
           metric_units = str_c("\\parbox[b]{", em, "em}{\\raggedleft \\textit{", metric_units, "}}"))

  names_line <- chap_header %>% pull(metric_names) %>% str_c(collapse = "   &   ") %>% str_c("&   ", ., " \\\\")
  units_line <- chap_header %>% pull(metric_units) %>% str_c(collapse = "   &   ") %>% str_c("&   ", ., " \\\\")


  # Transpose a chapter's bulk
  transpose_bulk <- function(metric) {
  a <-
  newdata %>%
    filter(chapter == useChapter,
           !is.na(Australia)) %>%
    select(-1)


  return(t(a[metric,])[-1,])
  }

  countries <- names(transpose_bulk(1))

  chap <- cbind(countries, transpose_bulk(1))

  if (number_of_metrics >= 2) chap <- cbind(chap, transpose_bulk(2))
  if (number_of_metrics >= 3) chap <- cbind(chap, transpose_bulk(3))
  if (number_of_metrics >= 4) chap <- cbind(chap, transpose_bulk(4))
  if (number_of_metrics >= 5) chap <- cbind(chap, transpose_bulk(5))
  if (number_of_metrics >= 6) chap <- cbind(chap, transpose_bulk(6))


  create_lines <- function(x) {
    as.character(chap[x,]) %>%
     str_c(collapse = "   &   ") %>%
       str_c(., " \\\\")
  }

  lines <- purrr::map(1:nrow(chap),
                      create_lines) %>%
           unlist()


  # Crate table environment preamble
  chapter_lc <- tolower(useChapter)

  Rs <- str_c(rep("r", number_of_metrics), collapse = "")
  tabular_line <- str_c("\\begin{tabular}{l", Rs, "}", collapse = "")

  chap_tex_name <- gsub(" ","-", tolower(useChapter))
  caption_line <- str_c("\\caption{International scorecard for ", tolower(useChapter), "}\\label{tbl:key-performance-metrics-for-", chap_tex_name, "}",
                        collapse = "")

  table_preamble <- c("\\bgroup \\def\\arraystretch{1.75}",
                      "\\begin{table}[htbp]",
                      "\\centering",
                      caption_line,
                      "",
                      tabular_line)

  # And add header lines
  lines <- c("%% Created by grattanMetricsTables. Make all data edits in the Google Doc.",
             "",
             "",
             table_preamble,
             "",
             names_line,
             "",
             units_line,
             "",
             lines,
             "",
             "\\end{tabular}\\notes{See \\Chapref{chap:technical-appendix} for notes and sources.}",
             "\\end{table}"
             )

  if(!dir.exists("chapter_tables")) dir.create("chapter_tables")
  file_name <- str_c("chapter_tables/tbl-", chap_tex_name, ".tex")
  readr::write_lines(lines, file_name)

  return(print(str_c("Writing ", useChapter, " metrics table to ", file_name)))

}


purrr::map(chapters %>% pull() %>% unique(), create_chapter_metrics)


}
