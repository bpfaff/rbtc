#' RPC-JSON API: getblockchaininfo
#' 
#' Returns an object containing various state info regarding blockchain processing.
#'
#' @param conobj object of class \code{CONRPC}.
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
getblockchaininfo <- function(conobj){
    stopifnot(class(conobj) == "CONRPC")
    ans <- POST(slot(conobj, "url"),
                authenticate(user = slot(conobj, "rpcuse"),
                             password = slot(conobj, "rpcpwd"),
                             type = "basic"),
                body = list(jsonrpc = "1.0",
                            id = "curltest",
                            method = "getblockchaininfo",
                            params = c()),
                encode = "json")
    stop_for_status(ans)
    ans <- content(ans)
    ans
}
#' RPC-JSON API: getbestblockhash
#' 
#' Returns the hash of the best (tip) block in the longest blockchain.
#'
#' @param conobj object of class \code{CONRPC}.
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
getbestblockhash <- function(conobj){
    stopifnot(class(conobj) == "CONRPC")
    ans <- POST(slot(conobj, "url"),
                authenticate(user = slot(conobj, "rpcuse"),
                             password = slot(conobj, "rpcpwd"),
                             type = "basic"),
                body = list(jsonrpc = "1.0",
                            id = "curltest",
                            method = "getbestblockhash",
                            params = c()),
                encode = "json")
    stop_for_status(ans)
    ans <- content(ans)
    ans
}
