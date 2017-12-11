#' RPC-JSON API: getinfo
#'
#' Returning information about bitcoin configuration and settings.
#'
#' WARNING: getinfo is deprecated and will be fully removed in 0.16.
#' Projects should transition to using getblockchaininfo, getnetworkinfo, and
#' getwalletinfo before upgrading to 0.16.
#'
#' @param conobj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Control RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getinfo
#' @aliases getinfo 
#' @rdname getinfo
#' @export
getinfo <- function(conobj){
    stopifnot(class(conobj) == "CONRPC")
    ans <- POST(slot(conobj, "url"),
                authenticate(user = slot(conobj, "rpcuse"),
                             password = slot(conobj, "rpcpwd"),
                             type = "basic"),
                body = list(jsonrpc = "1.0",
                            id = "curltest",
                            method = "getinfo",
                            params = c()),
                encode = "json")
    stop_for_status(ans)
    content(ans)
}
#' RPC-JSON API: Help
#'
#' Returning information about RPC functions.
#'
#' @param conobj object of class \code{CONRPC}.
#' @param rpcname \code{character}, name of RPC function.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Control RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name gethelp
#' @aliases gethelp 
#' @rdname gethelp
#' @export
gethelp <- function(conobj, rpcname = ""){
    stopifnot(class(conobj) == "CONRPC")
    rpcname <- as.character(rpcname)[1]
    ans <- POST(slot(conobj, "url"),
                authenticate(user = slot(conobj, "rpcuse"),
                             password = slot(conobj, "rpcpwd"),
                             type = "basic"),
                body = list(jsonrpc = "1.0",
                            id = "curltest",
                            method = "help",
                            params = list(command = rpcname)),
                encode = "json")
    stop_for_status(ans)
    ans <- content(ans)
    cat(ans$result)
    invisible(ans)
}
#' RPC-JSON API: getwalletinfo
#'
#' Returning information about bitcoin wallet.
#'
#' @param conobj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Control RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getwalletinfo
#' @aliases getwalletinfo 
#' @rdname getwalletinfo
#' @export
getwalletinfo <- function(conobj){
    stopifnot(class(conobj) == "CONRPC")
    ans <- POST(slot(conobj, "url"),
                authenticate(user = slot(conobj, "rpcuse"),
                             password = slot(conobj, "rpcpwd"),
                             type = "basic"),
                body = list(jsonrpc = "1.0",
                            id = "curltest",
                            method = "getwalletinfo",
                            params = c()),
                encode = "json")
    stop_for_status(ans)
    ans <- content(ans)
    ans
}
#' RPC-JSON API: getwalletinfo
#'
#' Returning information about bitcoin wallet.
#'
#' @param conobj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Control RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getwalletinfo
#' @aliases getwalletinfo 
#' @rdname getwalletinfo
#' @export
getwalletinfo <- function(conobj){
    stopifnot(class(conobj) == "CONRPC")
    ans <- POST(slot(conobj, "url"),
                authenticate(user = slot(conobj, "rpcuse"),
                             password = slot(conobj, "rpcpwd"),
                             type = "basic"),
                body = list(jsonrpc = "1.0",
                            id = "curltest",
                            method = "getwalletinfo",
                            params = c()),
                encode = "json")
    stop_for_status(ans)
    ans <- content(ans)
    ans
}
#' RPC-JSON API: getnetworkinfo
#' 
#' Returns an object containing various state info regarding P2P networking.
#'
#' @param conobj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Control RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getnetworkinfo
#' @aliases getnetworkinfo 
#' @rdname getnetworkinfo
#' @export
getnetworkinfo <- function(conobj){
    stopifnot(class(conobj) == "CONRPC")
    ans <- POST(slot(conobj, "url"),
                authenticate(user = slot(conobj, "rpcuse"),
                             password = slot(conobj, "rpcpwd"),
                             type = "basic"),
                body = list(jsonrpc = "1.0",
                            id = "curltest",
                            method = "getnetworkinfo",
                            params = c()),
                encode = "json")
    stop_for_status(ans)
    ans <- content(ans)
    ans
}
#' RPC-JSON API: getblockchaininfo
#' 
#' Returns an object containing various state info regarding blockchain processing.
#'
#' @param conobj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Control RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
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
