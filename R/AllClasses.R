#' The CONRPC class
#'
#' S4-class for curl connections to RPC-JSON. 
#'
#' The slots \code{rpcuse} and \code{rpcpwd} are required in the call
#' to \code{curl}. Furthermore, the fully qualified path to
#' \code{bitcoin.conf} (slot \code{config}) is required for starting
#' and stopping \code{bitcoind} as daemon. 
#'
#' @family bitcoind functions
#' @name CONRPC-class 
#' @rdname CONRPC-class
#' @export
#' @author Bernhard Pfaff
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
#' @family bitcoind functions
#' @export
setClassUnion(name = "NullOrCharacter",
              members = c("NULL", "character"))
#' @title S4 Class Union NULL or integer
#' 
#' @description
#' S4-class union of \code{NULL} or \code{integer}.
#' @family bitcoind functions
#' @export
setClassUnion(name = "NullOrInteger",
              members = c("NULL", "integer"))
#' @title S4 Class Union character or list
#' 
#' @description
#' S4-class union of \code{character} or \code{list}.
#' @family bitcoind functions
#' @export
setClassUnion(name = "CharacterOrList",
              members = c("character", "list"))

#' The ANSRPC class
#'
#' This class definition is employed to cast the JSON-objects
#' returned by API-calls to bitcoind.
#'
#' @slot rpcname \code{character} the name of the API.
#' @slot result \code{CharacterOrList} the output/result of the API.
#' @slot ecode \code{NullOrInteger} the error code,
#' in case of no error \code{NULL}.
#' @slot emessage \code{NullOrIntegerCharacter} the error message,
#' in case of no error \code{NULL}.
#' @slot id \code{character} identifier to API-call.
#' 
#' @family bitcoind functions
#' @name ANSRPC-class
#' @rdname ANSRPC-class
#' @export
setClass("ANSRPC", representation = list(
                       rpcname = "character",
                       result = "CharacterOrList",
                       ecode = "NullOrInteger",
                       emessage = "NullOrCharacter",
                       id = "character")
         )
