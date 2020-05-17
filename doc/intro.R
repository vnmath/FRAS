## ---- echo = FALSE, include = FALSE-------------------------------------------
library(FRAS)
library(dplyr)
library(maps)

## ----fars_read_example--------------------------------------------------------
filename <- system.file("extdata/accident_2014.csv.bz2", package = "FRAS")
fars_read(filename)

## ----fars_summarize_years_example---------------------------------------------
setwd(system.file("extdata", package = "FRAS"))
fars_summarize_years(2013:2015)

## ----fars_map_state_example---------------------------------------------------
setwd(system.file("extdata", package = "FRAS"))
fars_map_state(10, 2013)

