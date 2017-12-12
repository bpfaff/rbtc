#' RPC-JSON API: getblockchaininfo
#' 
#' Returns an object containing various state info regarding blockchain processing.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getblockchaininfo},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getblockchaininfo
#' @aliases getblockchaininfo 
#' @rdname getblockchaininfo
#' @export
getblockchaininfo <- function(obj){
    rpcpost(obj, "getblockchaininfo")
}
#' RPC-JSON API: getbestblockhash
#' 
#' Returns the hash of the best (tip) block in the longest blockchain.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getbestblockhash},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getbestblockhash
#' @aliases getbestblockhash 
#' @rdname getbestblockhash
#' @export
getbestblockhash <- function(obj){
    rpcpost(obj, "getbestblockhash")
}
#' RPC-JSON API: getblock
#' 
#' Returns information of a block hash. The returned level of details depends on the
#' argument \code{verbosity}.
#'
#' @section: Details
#' If verbosity is 'l0', returns a string that is serialized,
#' hex-encoded data for block 'hash'.
#' If verbosity is 'l1' (the default), returns an object with information about block <hash>.
#' If verbosity is 'l2', returns an object with information about block <hash> and
#' information about each transaction. 
#'
#' @param obj object of class \code{CONRPC}.
#' @param blockhash \code{character}, the block hash.
#' @param verbosity \code{character}, level of returned details. 
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getblock},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getblock
#' @aliases getblock 
#' @rdname getblock
#' @export
getblock <- function(obj, blockhash, verbosity = c("l1", "l0", "l2")){
    bh <- as.character(blockhash)
    verbosity <- match.arg(verbosity)
    verb <- switch(verbosity,
                   l0 = 0L,
                   l1 = 1L,
                   l2 = 2L)
    pl <- list(blockhash = bh, verbosity = verb)
    rpcpost(obj, "getblock", pl)
}
#' RPC-JSON API: getblockcount
#' 
#' Returns the number of blocks in the longest blockchain.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getblockcount},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getblockcount
#' @aliases getblockcount 
#' @rdname getblockcount
#' @export
getblockcount <- function(obj){
    rpcpost(obj, "getblockcount")
}
