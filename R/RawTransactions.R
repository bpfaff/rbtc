#' RPC-JSON API: getrawtransaction
#'
#' Returns the raw transaction data.
#'
#' @section: Details
#' By default this function only works for mempool transactions.
#' If the -txindex option is enabled, it also works for blockchain transactions.
#' DEPRECATED: for now, it also works for transactions with unspent outputs.
#' If verbose is 'true', returns an object with information about 'txid'.
#' If verbose is 'false' or omitted, returns a string that is serialized,
#' hex-encoded data for 'txid'.
#'
#' @param con object of class \code{CONRPC}.
#' @param txid \code{character}, the transaction id.
#' @param verbose \code{logical}, type of output.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family RawTransactions RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getblock},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getrawtransaction
#' @aliases getrawtransaction
#' @rdname getrawtransaction
#' @export
getrawtransaction <- function(con, txid, verbose = FALSE){
    txid <- as.character(txid)
    verb <- ifelse(verbose, 1L, 0L)
    pl <- list(txid = txid, verbose = verb)
    rpcpost(con, "getrawtransaction", pl)
}
#' RPC-JSON API: decoderawtransaction
#'
#' Return a JSON object representing the serialized,
#' hex-encoded transaction.
#'
#' @param con object of class \code{CONRPC}.
#' @param hexstring \code{character}, the transaction hex string.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family RawTransactions RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getblock},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name decoderawtransaction
#' @aliases decoderawtransaction
#' @rdname decoderawtransaction
#' @export
decoderawtransaction <- function(con, hexstring){
    hexstring <- as.character(hexstring)
    pl <- list(hexstring = hexstring)
    rpcpost(con, "decoderawtransaction", pl)
}
