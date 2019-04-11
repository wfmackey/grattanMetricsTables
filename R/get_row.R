#' Work out a particular row of a metrics table.
#' @name get_row
#' @param data The data that contains the row to convert.
#' @param row The row of the table to be analysed.
#' @param skip Should a particular row be skipped? If so, enter the number or vector of numbers.
#'
#' @import dplyr
#'
#' @export


get_row <- function(row, data, skip = 0) {

  # Take a row from the dataset
  origRow <- data %>%
    select(-1, -2) %>%
    slice(row) %>%
    unlist(., use.names=FALSE)

  # Make a copy to mangle
  a <- origRow

  # Test if character row using perm_chars
  pos_chars <- c("Yes", "High", "Good")
  neg_chars <- c("No", "Low", "Bad")
  perm_chars <- c(pos_chars, neg_chars)

  if (sum(origRow %in% perm_chars) > 1) {
    char_row <- TRUE
  } else {
    char_row <- FALSE
    suppressWarnings(
      if (class(a) == "character") {
        a <- gsub(",", "", a)
        a <- as.numeric(a)
      }
    )
  }


  # Set reverse option:
  reverse <- FALSE

  # Is there a 'reverse' column? (ie one containing 'higher number'?)
  columns <- ncol(data)

  if (grepl("(H|h)igh", names(data)[columns])) {

    rev_char <- as.character(data[row, columns])
    if(grepl("(W|w)ors", rev_char)) reverse <- TRUE

    # Then drop the reverse column if it exists:
    a <- a[1:(length(a)-1)]

  }

  # The end of the line will be "\\ \whitemid" unless it is the last in a chapter's metrics
  endLine <- "\\\\  \\whitemid"

  # Get the chapter name
  chap <- as.character(data[row, 1])

  if (!is.na(data[row+1, 1])) endLine <- "\\\\  \\blackmid"

  # If the chapter is NA, set to empty and move on
  if ( is.na(chap)) {
    chap <- "                                        & " } else {
      # If the chapter is not NA:
      # Count how many metrics the chapter has
      if (!is.na(data[row+1, 1])) {
        chaprows <- 1} else if (!is.na(data[row+2, 1])) {
          chaprows <- 2} else if (!is.na(data[row+3, 1])) {
            chaprows <- 3} else if (!is.na(data[row+4, 1])) {
              chaprows <- 4} else if (!is.na(data[row+5, 1])) {
                chaprows <- 5} else if (!is.na(data[row+6, 1])) {
                  chaprows <- 6} else {chaprows <- 0}



      chap <- paste0("\\multirow{", chaprows, "}{=}{\\textbf{", chap, "}}  & ")


    }

  # Get the metric name
  metric <- as.character(data[row, 2])
  # Fix up latex escape characters
  metric <- gsub("(%|\\$)", "\\\\\\1", metric, perl = T)

  # Set colours for the row

  # First, set colour codes:
  # (initially reversed to make "a" mean the highest number)
  codes <- rev(letters[1:11])

  # And reverse if required
  if (reverse) codes <- rev(codes)



  # If numeric, get median and sd of the row
  if (!char_row) {
    med <- stats::median(a, na.rm = T)
    sd <- stats::sd(a, na.rm = T)
  }

  set_colours <- function(x) {

    # If a character row:
    if (char_row) {
      col <- case_when(
        a[x] %in% pos_chars ~ "b",
        a[x] %in% neg_chars ~ "j",
        a[x] == "--" ~ "z",
        TRUE ~ "f"
        )

      # Write cell
      col <- str_c("  & \\q", col, "  " , origRow[x], "  ")

      # Return and send message
      print(paste0(origRow[x], " is ", col))
      return(col)

    }

    if (!char_row) {

      if (!is.na(a[x])) {
        col <- codes[1]
        if(a[x] >= med - 1.5*sd) col <- codes[2]
        if(a[x] >= med - 1.0*sd) col <- codes[3]
        if(a[x] >= med - 0.5*sd) col <- codes[4]
        if(a[x] >= med - 0.3*sd) col <- codes[5]
        if(a[x] >= med)          col <- codes[6]
        if(a[x] >= med + 0.3*sd) col <- codes[7]
        if(a[x] >= med + 0.5*sd) col <- codes[8]
        if(a[x] >= med + 1.0*sd) col <- codes[9]
        if(a[x] >= med + 1.5*sd) col <- codes[10]
        if(a[x] >= med + 2.0*sd) col <- codes[11]

        # Write cell
        col <- str_c("  & \\q", col, "  " , origRow[x], "  ")

      } else {

        # Write cell
        col <- "  &  \\qz  "
      }

      # Return and send message
    print(paste0(a[x], " is ", col))
    return(col)
    }
  }

  # Run on row
  line <- purrr::map(1:length(a), set_colours)

  # Collapse into single vector
  line <- paste0(line, collapse = "")

  # Add end of line \\
  line <- paste0(line, endLine)

  # Add chapter name and metrics rows
  line <- paste0(chap, metric, line, collapse = "")


  return(line)

}
