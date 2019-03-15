# grattanMetricsTables
A package to automatically generate Grattan's metrics tables. Below is how to use it.

------

First, install the `grattanMetricsTables` package:

```r
devtools::install_github("wfmackey/grattanMetricsTables")
```

Then load the `grattanMetricsTables` package and the `googlesheets4` package (to connect to the Google Sheets version of the metrics table).

```r
library(grattanMetricsTables)
library(googlesheets4)
```

The first of two steps is to read the Google Sheets metrics table. You'll have to guide `R` through this process the first time to give it access to the sheet:

```r
# Read and manipulate metrics data
# You will need to allow the googlesheets API access to your account
metrics <- read_sheet("1p2oz-9s29IN4M7paosSEG24A3GwVn3uskXP0-3npd4Q", 
                      sheet = "Metric", 
                      range = "A1:L42",
                      col_types = "c")
```

Once the sheet is loaded and assigned to an object, relay that object to the `make_metrics_table` function:

```r
# Generate the metrics tables:
make_metrics_table(metrics) 
```

This will produce a `metrics_table.tex` file that contains the 'innards' of the table (i.e. the metrics rows of the table) that can be copied-and-pasted into the table you have built.

# Reversing colour order
The function will look for the word 'higher' in the final column. If it is found, it will designate that column as a reversal instructions where:

"Higher number is..."
* "better": highest numbers will be yellow; lowest numbers will be red.
* "worse":  **reversed** lowest numbers will be yellow; highest numbers will be red.

