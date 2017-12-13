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
#' RPC-JSON API: getblockhash
#' 
#' Returns hash of block in best-block-chain at height provided.
#'
#' @param obj object of class \code{CONRPC}.
#' @param height \code{integer} the height index.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getblockhash},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getblockhash
#' @aliases getblockhash 
#' @rdname getblockhash
#' @export
getblockhash <- function(obj, height){
    h <- as.integer(height)
    rpcpost(obj, "getblockhash", list(height = h))
}
#' RPC-JSON API: getchaintips
#' 
#' Return information about all known tips in the block tree,
#' including the main chain as well as orphaned branches.
#' 
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getchaintips},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getchaintips
#' @aliases getchaintips 
#' @rdname getchaintips
#' @export
getchaintips <- function(obj){
    rpcpost(obj, "getchaintips") 
}
#' RPC-JSON API: getdifficulty
#' 
#' Returns the proof-of-work difficulty as a multiple of the minimum difficulty.
#' 
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getdifficulty},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getdifficulty
#' @aliases getdifficulty 
#' @rdname getdifficulty
#' @export
getdifficulty <- function(obj){
    rpcpost(obj, "getdifficulty")
}
