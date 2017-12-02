#' The CONRPC class
#'
#' S4-class for curl connections to RPC-JSON. 
#'
#' The slots \code{rpcuser} and \code{rpcpassowrd} are required in the call
#' to \code{curl}. Furthermore, the fully qualified path to bitcoin.conf is
#' required for starting and stopping bitcoind as daemon.
#'
#' @name CONRPC 
#' @rdname CONRPC
#' @aliases CONRPC-class
#' @exportClass CONRPC
#' @author Bernhard Pfaff
#'
setClass("CONRPC", representation = list(
                       rpcuser = "character",
                       rpcpwd = "character",
                       conf.file = "character")
         )
