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
                       testnet = "logical",
                       url = "character",
                       config = "character")
         )
#' @title S4 Class Union NULL or character
#' 
#' @description
#' S4-class union of \code{NULL} or \code{character}.
#' @export
setClassUnion(name = "NullOrCharacter",
              members = c("NULL", "character"))
#' @title S4 Class Union NULL or integer
#' 
#' @description
#' S4-class union of \code{NULL} or \code{integer}.
#' @export
setClassUnion(name = "NullOrInteger",
              members = c("NULL", "integer"))
