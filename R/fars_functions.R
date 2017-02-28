

#' Read csv file !!!!
#' Read csv file
#'
#' Takes file name and read to memory
#' @param  filename  A string of file name
#' @details If file does not exist, it will stop with an error message.
#' @return data.frame from csv file contents
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Make file name
#'
#' Takes year to compose file name
#' @param  year   year number
#' @details year should be four digits, although not mandatory
#' @return string of file name
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Assemble data from multiple year
#'
#' Takes year vector to commpose file names and read the files and put month/year to data frame.
#' @param  years   multiple year in a vector
#' @details year should be four digits, although not mandatory, files must be exisiting. 
#' @return data frame of month/year
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

#' Summarize by year, spread to months
#'
#' Takes year vector then read the files of these years and count by year to the level of month.
#' @param  years   multiple year in a vector
#' @details year should be four digits, although not mandatory, files must be exisiting. 
#' @return summary data frame by year, with each month on columns
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Show incident locations in a 2D plot
#'
#' Takes state number and year, then read and filter from file, and show in a 2D plot.
#' @param  state.num  state numnber
#' @param  year       year number
#' @details year should be four digits; state number must exist in file. if invalid state number, it will stop with error.
#' @return map of the state, with incident locations.
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



