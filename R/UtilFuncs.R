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
    ans <- as.POSIXct(x,
                      tz = "GMT",
                      origin = "1970-01-01 00:00:00",
                      format = "%Y-%m-%d %H:%M:%S")
    ans
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
#' Retrieve TX Ids in block
#'
#' This function retrieves the transaction IDs in a block.
#'
#' @param con \code{CONRPC}, configuration object.
#' @param height \code{integer}, the block's height.
#' @param excoinbase \code{logical}, whether coinbase transaction
#' should be excluded (default is \code{TRUE}).
#'
#' @return \code{character}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name txids
#' @rdname txids
#' @export
txids <- function(con, height, excoinbase = TRUE){
    height <- as.integer(abs(height))
    bc <- unlist(slot(getblockcount(con),
                      "result"))
    if (height > bc) {
        stop("'height' exceeds max height in local chain.\n")
    }
    h <- slot(getblockhash(con, height),
              "result")
    b <- slot(getblock(con, h),
              "result")
    ans <- unlist(b[["tx"]])
    if (excoinbase){
        ans <- ans[-1]
    }
    if (length(ans) < 1) {
        warning("No transactions in block.\n")
    }
    ans
}
#' Retrieving the input transaction IDs
#'
#' This function returns the transaction IDs of the inputs for
#' a given transaction.
#'
#' @param con \code{CONRPC}, configuration object.
#' @param txid \code{character}, the id of the transaction.
#'
#' @return \code{data.frame}, the transaction ID(s) and
#' the position(s) of the previous UTXO(s).
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name txinids
#' @rdname txinids
#' @export
txinids <- function(con, txid){
    txraw <- slot(getrawtransaction(con, txid),
                  "result")
    txdec <- slot(decoderawtransaction(con, txraw),
                  "result")
    vin <- txdec[["vin"]]
    txinids <- unlist(lapply(vin, function(x) x[["txid"]]))
    txinpos <- unlist(lapply(vin, function(x) x[["vout"]]))  + 1
    ans <- data.frame(txinids, txinpos,
                      stringsAsFactors = FALSE)
    ans
}
#' Retrieving values of UTXOs
#'
#' This function returns the values of UTXO(s) in a transaction.
#'
#' @param con \code{CONRPC}, configuration object.
#' @param txid \code{character}, the id of the transaction.
#'
#' @return \code{numeric}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name utxovalue
#' @rdname utxovalue
#' @export
utxovalue <- function(con, txid){
    txraw <- slot(getrawtransaction(con, txid),
                  "result")
    txdec <- slot(decoderawtransaction(con, txraw),
                  "result")
    vout <- txdec[["vout"]]
    ans <- unlist(
        lapply(vout, function(x) x[["value"]])
    )
    ans
}
#' Retrieving types of UTXOs
#'
#' This function returns the types of the UTXO(s) in a transaction.
#'
#' @param con \code{CONRPC}, configuration object.
#' @param txid \code{character}, the id of the transaction.
#'
#' @return \code{character}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name utxotype
#' @rdname utxotype
#' @export
utxotype <- function(con, txid){
    txraw <- slot(getrawtransaction(con, txid),
                  "result")
    txdec <- slot(decoderawtransaction(con, txraw),
                  "result")
    vout <- txdec[["vout"]]
    ans <- unlist(
        lapply(vout, function(x) x[["type"]])
    )
    ans
}
#' Age of UTXOs
#'
#' This function returns a \code{difftime} object measuring the elapsed time(s)
#' between the UTXO(s) in a transaction and its input(s) (previous UTXO(s)).
#'
#' @param con \code{CONRPC}, configuration object.
#' @param txid \code{character}, the id of the transaction.
#' @param units \code{character}, the time difference units;
#' passed to \code{difftime()}.
#'
#' @return \code{difftime}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name utxoage
#' @rdname utxoage
#' @export
utxoage <- function(con, txid,
                    units = c("auto", "secs", "mins",
                              "hours", "days", "weeks")){
    txraw <- slot(getrawtransaction(con, txid, verbose = TRUE),
                  "result")
    txotime <- int2date(txraw[["time"]])
    txins <- unlist(lapply(txraw[["vin"]],
                           function(x) x[["txid"]])
                    )
    n <- length(txins)
    txitime <- integer(n)
    for (i in 1:n){
        txraw <- slot(getrawtransaction(con, txins[i], verbose = TRUE),
                      "result")
        txitime[i] <- txraw[["time"]]
    }
    ans <- difftime(txotime, int2date(txitime), units = units)
    list("TimeUtxo" = txraw[["time"]],
         "AgeInput" = ans)
}
#' Compute fee of a transaction
#'
#' This function returns the implicit fee of a transaction,
#' by computing the difference between the sum of its inputs
#' and the sum of its outputs.
#'
#' @param con \code{CONRPC}, configuration object.
#' @param txid \code{character}, the id of the transaction.
#'
#' @return \code{numeric}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name txfee
#' @rdname txfee
#' @export
txfee <- function(con, txid){
    valo <- sum(utxovalue(con, txid))
    txin <- txinids(con, txid)
    n <- nrow(txin)
    vali <- 0
    for (i in 1:n){
        val <- utxovalue(con, txin[i, 1])[txin[i, 2]]
        vali <- vali + val
    }
    ans <- vali - valo
    ans
}

#' Compute fee in a block
#'
#' This function returns the fee of the coinbase transaction.
#' Hereby, the mining reward has been deducted.
#' Initially, the mining reward was 50 BTC and is halved every
#' 210,000 blocks.
#'
#' @param con \code{CONRPC}, configuration object.
#' @param height \code{integer}, the height of the block.
#'
#' @return \code{numeric}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name bkfee
#' @rdname bkfee
#' @export
bkfee <- function(con, height){
    height <- as.integer(abs(height))
    cb <- txids(con, height, excoinbase = FALSE)[1]
    mtot <- sum(utxovalue(con, cb))  
    hf <- ceiling(height / 209999) - 1
    hf <- 2 ^ hf
    mrwd <- 50 / hf
    ans <- mtot - mrwd
    ans
}
#' Obtaining statistics of a block
#'
#' This function returns key statistics of a block's content,
#' such as the time, the count of transactions,
#' and summary statistics of the UTXOs.
#'
#' @param con \code{CONRPC}, configuration object.
#' @param height \code{integer}, the block's height.
#' @param excoinbase \code{logical}, whether coinbase transaction
#' should be excluded (default is \code{TRUE}).
#'
#' @return An object of class \code{data.frame}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name blockstats
#' @rdname blockstats
#' @export
blockstats <- function(con, height, excoinbase = TRUE){
    height <- as.integer(abs(height))
    bc <- slot(getblockcount(con),
               "result")
    if (height > bc){
        stop("'height' exceeds max height in local chain.\n")
        }
    h <- slot(getblockhash(con, height),
              "result")
    b <- slot(getblock(con, h),
              "result")
    ## to be filled
    btime <- b[["time"]]
    txids <- unlist(b[["tx"]])
    n <- length(txids)
    if (excoinbase){
        txids <- txids[-1]
    }
    n <- length(txids)
    if (n < 1) {
        warning("No transactions in block.\n")
        umax <- NA
        umin <- NA
        uavg <- NA
        umed <- NA
        usum <- NA
    } else {
        usum <- 0
        uvals <- c()
        for (i in 1:n){
            uval <- utxovalue(con, txids[i])
            usum <- usum + sum(uval)
            uvals <- c(uvals, uval)
        }
        ## Summary statistics of uvals
        umax <- max(uvals)
        umin <- min(uvals)
        uavg <- mean(uvals)
        umed <- stats::median(uvals)
    }
    ans <- data.frame("Height" = height,
                      "Time" = btime,
                      "TxCount" = n,
                      "UtxoMax" = umax,
                      "UtxoMin" = umin,
                      "UtxoMean" = uavg,
                      "UtxoMedian" = umed,
                      "UtxoVolume" = usum
                      )
    ans
}
#' Statistics of a transaction
#'
#' This function returns key statistics/characteristics of
#' a transaction.
#'
#' @param con \code{CONRPC}, configuration object.
#' @param txid \code{character}, the id of the transaction.
#'
#' @return \code{data.frame}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name txstats
#' @rdname txstats
#' @export
txstats <- function(con, txid){
    txraw <- slot(getrawtransaction(con, txid, verbose = TRUE),
                  "result")
    uvals <- utxovalue(con, txid)
    fee <- txfee(con, txid)
    ans <- data.frame("CountTxInIds" = length(txraw[["vin"]]),
                      "CountTxOutIds" = length(txraw[["vout"]]),
                      "SumOfUtxo" = sum(uvals),
                      "Fee" = fee,
                      "Size" = txraw[["size"]],
                      "Time" = txraw[["blocktime"]],
                      "BlockHash" = txraw[["blockhash"]],
                      "Confirmations" = txraw[["confirmations"]],
                      stringsAsFactors = FALSE)
    ans
}
#' Time of a block
#'
#' This function returns the time of a block in GMT.
#'
#' @param con \code{CONRPC}, configuration object.
#' @param height \code{integer}, the height of the block.
#'
#' @return \code{POSIXct}
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name timeofblock
#' @rdname timeofblock
#' @export
timeofblock <- function(con, height){
    height <- as.integer(abs(height))
    h <- slot(getblockhash(con, height),
              "result")
    b <- slot(getblock(con, h),
              "result")
    ans <- int2date(b[["time"]])
    ans
}
#' Block height at time
#'
#' This function returns the block heights closest to
#' a provided date/time (time zone is GMT).
#'
#' @param con \code{CONRPC}, configuration object.
#' @param targetdate \code{POSIXct}, the date/time of closest block heights.
#'
#' @return \code{data.frame}: the heights, the times and
#' the time differences (in minutes) to the provided date/time.
#' @family UtilityFuncs
#' @author Bernhard Pfaff
#' @name blockattime
#' @rdname blockattime
#' @export
blockattime <- function(con, targetdate){
    dt <- as.POSIXct(targetdate, tz = "GMT")
    bh <- slot(getbestblockhash(con),
               "result")
    lastblock <- slot(getblock(con, bh),
                      "result")
    lastheight <- lastblock[["height"]]
    lasttime <- int2date(lastblock[["time"]])
    if (dt > lasttime) {
        stop("Required block time is greater than last block in local chain.\n")
    }
    deltatime <- difftime(lasttime, dt, units = "mins")
    deltaheight <- ceiling(as.numeric(deltatime / 10))
    approxh1 <- lastheight - deltaheight
    ht <- timeofblock(con, approxh1)
    signtimedelta <- searchdir <- sign(as.numeric(dt - ht))
    while (identical(signtimedelta, searchdir)) {
        approxh1 <- approxh1 + searchdir
        ht <- timeofblock(con, approxh1)
        ## big step, if current delta time is more than 100 mins
        curdelta <- as.numeric(abs(
        difftime(dt, ht, units = "mins")))
        if (curdelta > 100){
            approxh1 <- approxh1 + searchdir * floor(curdelta / 10)
            ht <- timeofblock(con, approxh1)
            searchdir <- sign(as.numeric(dt - ht))
        }
        signtimedelta <- sign(as.numeric(dt - ht))
    }
    approxt1 <- timeofblock(con, approxh1)
    approxh2 <- approxh1 - searchdir
    approxt2 <- timeofblock(con, approxh2)
    Times <- c(approxt1, approxt2)
    attr(Times, "tzone") <- "GMT"
    ans <- data.frame("Height" = c(approxh1, approxh2),
                  "Time" = Times,
                  "DateQuery" = dt,
                  "DeltaMinutes" = difftime(dt,
                                          c(approxt1, approxt2),
                                          units = "mins")
                  )
    ans <- ans[order(ans[, 1]), ]
    ans
}
