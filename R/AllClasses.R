#' The CONRPC class
#'
#' S4-class for curl connections to RPC-JSON. 
#'
#' The slots \code{rpcuse} and \code{rpcpwd} are required in the call
#' to \code{curl}. Furthermore, the fully qualified path to
#' \code{bitcoin.conf} (slot \code{config}) is required for starting
#' and stopping \code{bitcoind} as daemon. 
#'
#' @seealso conrpc, startbtc, stopbtc
#' @name CONRPC 
#' @rdname CONRPC-class
#' @aliases CONRPC-class
#' @exportClass CONRPC
#' @author Bernhard Pfaff
#'
setClass("CONRPC", representation = list(
                       rpcuse = "character",
                       rpcpwd = "character",
                       config = "character")
         )
