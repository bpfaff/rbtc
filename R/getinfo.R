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
#' @seealso conrpc
#' @author Bernhard Pfaff
#' @name getinfo
#' @aliases getinfo 
#' @rdname getinfo
#' @export
getinfo <- function(conobj){
    stopifnot(class(conobj) == "CONRPC")
    ans <- POST("http://127.0.0.1:8332/",
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
