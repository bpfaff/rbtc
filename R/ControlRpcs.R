#' RPC-JSON API: getinfo
#'
#' Returning information about bitcoin configuration and settings.
#'
#' WARNING: getinfo is deprecated and will be fully removed in 0.16.
#' Projects should transition to using getblockchaininfo, getnetworkinfo, and
#' getwalletinfo before upgrading to 0.16.
#'
#' @param con object of class \code{CONRPC}.
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
getinfo <- function(con){
    rpcpost(con, "getinfo")
}
#' RPC-JSON API: Help
#'
#' Returning information about RPC functions.
#'
#' @param con object of class \code{CONRPC}.
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
gethelp <- function(con, rpc = ""){
    rpcpost(con, "help", list(command = rpc))
}
#' RPC-JSON API: getwalletinfo
#'
#' Returning information about bitcoin wallet.
#'
#' @param con object of class \code{CONRPC}.
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
getwalletinfo <- function(con){
    rpcpost(con, "getwalletinfo")
}
