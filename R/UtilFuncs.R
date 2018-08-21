#' Convert time stamp to POSIX
#'
#' This function returns the associated \code{POSIXct} time
#' to the time stamp integer in a block header.
#' 
#' @param x \code{integer}, the block header time stamp
#'
#' @return An object of class \code{POSIXct, POSIXt}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Block_timestamp}
#' @name int2date
#' @rdname int2date
#' @examples
#' ts <- 1532954868
#' int2date(ts)
#' @export
int2date <- function(x){
    x <- abs(as.integer(x))
    ans <- as.POSIXct(x, tz = "GMT", origin = "1970-01-01")
    format(ans, "%Y-%m-%d %H:%M:%S")
}
#' Convert date/time to integer
#'
#' This function returns the associated \code{integer} time
#' for a given date/time object (coercible as \code{POSIXct} object.
#' 
#' @param x \code{POSIXct}, date/time object.
#' @return \code{integer}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name date2int
#' @rdname date2int
#' @examples
#' d <- "2017-03-15"
#' date2int(d)
#' @export
date2int <- function(x){
    x <- as.POSIXct(x, tz = "GMT", origin = "1970-01-01")
    as.integer(x)
}
#' Integer representation of a day-begin 
#'
#' This function returns the associated \code{integer} time
#' for the start of a specific day (\emph{i.e.}, \code{00:00:00} time).
#' 
#' @param x \code{POSIXct}, date/time object.
#' @return \code{integer}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name intMinDay
#' @rdname intMinDay
#' @examples
#' d1 <- "2017-03-15"
#' d1 <- intMinDay(d1)
#' d2 <- "2017-03-15 00:00:00"
#' d2 <- intMinDay(d2)
#' identical(d1,d2)
#' @export
intMinDay <- function(x){
    d <- as.Date(x)
    s <- as.POSIXct(paste0(d, "00:00:00"),
                    tz = "GMT",
                    origin = "1970-01-01")
    date2int(s)
}
#' Integer representation of a day-end 
#'
#' This function returns the associated \code{integer} time
#' for the end of a specific day (\emph{i.e.}, \code{23:59:59} time).
#' 
#' @param x \code{POSIXct}, date/time object.
#' @return \code{integer}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name intMaxDay
#' @rdname intMaxDay
#' @examples
#' d1 <- "2017-03-15"
#' d1 <- intMaxDay(d1)
#' d2 <- "2017-03-15 23:59:59"
#' d2 <- intMaxDay(d2)
#' identical(d1,d2)
#' @export
intMaxDay <- function(x){
    d <- as.Date(x)
    e <- as.POSIXct(paste0(d, "23:59:59"),
                    tz = "GMT",
                    origin = "1970-01-01")
    date2int(e)
}
#' Integer range within a day 
#'
#' This function returns the associated \code{integer} times
#' for the start and end of a specific day.
#' 
#' @param x \code{POSIXct}, date/time object.
#'
#' @return \code{integer}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name intRangeDay
#' @rdname intRangeDay
#' @examples
#' d1 <- "2017-03-15"
#' intRangeDay(d1)
#' intMinDay(d1)
#' intMaxDay(d1)
#' @export
intRangeDay <- function(x){
    d <- as.Date(x)
    c(first = intMinDay(d), last = intMaxDay(d))
}
#' Integer range between two dates 
#'
#' This function returns the associated \code{integer} times
#' for the start of date \code{d1} and the end of date \code{d2}.
#' 
#' @param d1 \code{POSIXct}, date/time object.
#' @param d2 \code{POSIXct}, date/time object.
#'
#' @return \code{integer}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name intRangePeriod
#' @rdname intRangePeriod
#' @examples
#' d1 <- "2017-03-15"
#' d2 <- "2017-04-15"
#' intRangePeriod(d1, d2)
#' intMinDay(d1)
#' intMaxDay(d2)
#' @export
intRangePeriod <- function(d1, d2){
    d1 <- as.Date(d1)
    d2 <- as.Date(d2)
    if (d1 <= d2){
        ans <- c(first = intMinDay(d1), last = intMaxDay(d2))
    } else {
        ans <- c(first = intMinDay(d2), last = intMaxDay(d1))
    }
    ans
}
