#' Loads a .csv file in the dataset
#'
#' @description
#' The function reads a .csv file named by \code{filename} and returns a data frame.
#' If the file does not exist, there is an error message.
#'
#' @param filename Path to the .csv file (string)
#'
#' @return The function returns a data frame from the read .csv file.
#' If the file does not exist, there is an error message.
#'
#' @examples
#' \dontrun{
#' accident_2013 <- fars_read("./data/accident_2013.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' @title make_filename
#'
#' @description
#' The function creates a filename for a .csv.bz2 file based on the \code{year}
#'
#' @param year An integer represents for a year of a data set.
#'
#' @return Returns a string in a format "accident_<year>.csv.bz2" as a file name
#' If the input is not integer, there is an error message.
#'
#' @examples
#' \dontrun{
#' makefilename(2013)
#' }
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' @title fars_read_years
#' @description
#' Reads files corresponding to the vector of years input into data frames
#' There is an error message if the input invalid.
#'
#' @param years A vector of numbers representing years need to be read
#' @return This function returns a data frame about the number of accidents
#' with months in rows, years in columns. Returns a invalid year warning if
#' every input year that does not exist in the datasets.
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' }
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}
#' @title fars_summarize_years
#' @description
#' The function summarizes the number of accidents by months based on input \code{years}.
#'
#' @param years A vector of integers for years
#' @return This function returns a data frame abot the number of accidents
#' with months in rows, years in columns.
#' Returns a warning if input years that does not exist in the datasets.
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2015)
#' }
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' @title fars_map_state
#' @description
#' This function plots the number of accidents in an input year on an input US state map.
#' This function returns an error message if either the \code{state.num} or
#' \code{year} do not exist in the dataset
#'
#' @param state.num A number representing a state in the dataset
#' @param year A number representing the year in the dataset
#' @return This function returns a plot of the number of accidents
#' for based on the \code{state.num} with \code{year} inputs.
#' Returns an error message if the state or year do not exist in the dataset.
#' @examples
#' \dontrun{
#' fars_map_state('10',2013)
#' }
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
