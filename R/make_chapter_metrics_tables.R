
library(googlesheets4)
library(tidyverse)
library(zoo)

metrics <- read_sheet("1p2oz-9s29IN4M7paosSEG24A3GwVn3uskXP0-3npd4Q",
                      sheet = "Metric",
                      range = "A1:L42",
                      col_types = "c")


lines <- make_metrics_table(metrics)

# Clean data
data <- metrics %>%
filter(!is.na(Australia))

# Get chapter list
chapters <- metrics %>%
  select(chapter = 1) %>%
  do(na.locf(.))

# Get metric names list
metric_names <- metrics %>%
  select(metric = 2) %>%
  mutate(metric = gsub("(.*?)\\(([^\\)]*)\\)", ""))




newdata <-
  tibble(lines = lines) %>%
    separate(lines, names(metrics), "\\&") %>%
    select(-1) %>%
    rename(metric = 1) %>%
  # Add chapter names
  bind_cols(chapters, .) %>%
  # Remove \\whitespace from last column
  mutate_all(function(x) gsub("\\\\\\\\  ?\\\\[a-z]*id", "", x))




create_chapter_metrics <- function(newData, useChapter) {

  # Transpose a chapter's data
  chap <- newData %>%
    filter(chapter == useChapter) %>%
    select(-1) %>%
    gather(var, val, 2:ncol(.)) %>%
    spread(metric, val)


  create_lines <- function(x) {
    as.character(chap[x,]) %>% str_c(collapse = "   &   ") %>% str_c(., " \\\\")
  }

  lines <- purrr::map(1:nrow(chap),
                      create_lines) %>%
           unlist()

  return(lines)

}

create_chapter_metrics(newdata, "Economic development")


lines

