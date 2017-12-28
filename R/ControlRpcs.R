#' RPC-JSON API: getinfo
#'
#' Returning information about bitcoin configuration and settings.
#'
#' WARNING: getinfo is deprecated and will be fully removed in 0.16.
#' Projects should transition to using getblockchaininfo, getnetworkinfo, and
#' getwalletinfo before upgrading to 0.16.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Control RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getinfo},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getinfo
#' @aliases getinfo 
#' @rdname getinfo
#' @export
getinfo <- function(obj){
    rpcpost(obj, "getinfo")
}
#' RPC-JSON API: Help
#'
#' Returning information about RPC functions.
#'
#' @param obj object of class \code{CONRPC}.
#' @param rpc \code{character}, name of RPC function.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Control RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#help},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name gethelp
#' @aliases gethelp 
#' @rdname gethelp
#' @export
gethelp <- function(obj, rpc = ""){
    rpcpost(obj, "help", list(command = rpc))
}
#' RPC-JSON API: getwalletinfo
#'
#' Returning information about bitcoin wallet.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Control RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getwalletinfo},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getwalletinfo
#' @aliases getwalletinfo 
#' @rdname getwalletinfo
#' @export
getwalletinfo <- function(obj){
    rpcpost(obj, "getwalletinfo")
}
#' RPC-JSON API: getnetworkinfo
#' 
#' Returns an object containing various state info regarding P2P networking.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Control RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getnetworkinfo},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getnetworkinfo
#' @aliases getnetworkinfo 
#' @rdname getnetworkinfo
#' @export
getnetworkinfo <- function(obj){
    rpcpost(obj, "getnetworkinfo")
}
