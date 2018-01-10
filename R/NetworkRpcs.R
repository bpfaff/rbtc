#' RPC-JSON API: getpeerinfo
#' 
#' Returns data about each connected network node as a json array of objects.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Network RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getpeerinfo},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getpeerinfo
#' @aliases getpeerinfo 
#' @rdname getpeerinfo
#' @export
getpeerinfo <- function(obj){
    rpcpost(obj, "getpeerinfo")
}
#' RPC-JSON API: getnetworkinfo
#' 
#' Returns an object containing various state info regarding P2P networking.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Network RPCs
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
#' RPC-JSON API: ping
#' 
#' Requests that a ping be sent to all other nodes, to measure ping time.
#' Results provided in getpeerinfo, pingtime and pingwait fields are
#' decimal seconds. Ping command is handled in queue with all other commands,
#' so it measures processing backlog, not just network ping.
#' 
#' @param obj object of class \code{CONRPC}.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Network RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#ping},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name ping
#' @aliases ping 
#' @rdname ping
#' @export
ping <- function(obj){
    rpcpost(obj, "ping")
}
#' RPC-JSON API: getnettotals
#' 
#' Returns information about network traffic, including bytes in,
#' bytes out, and current time.
#' 
#' @param obj object of class \code{CONRPC}.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Network RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getnettotals},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getnettotals
#' @aliases getnettotals 
#' @rdname getnettotals
#' @export
getnettotals <- function(obj){
    rpcpost(obj, "getnettotals")
}
#' RPC-JSON API: getconnectioncount
#' 
#' Returns the number of connections to other nodes.
#' 
#' @param obj object of class \code{CONRPC}.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Network RPCs
#' @author Bernhard Pfaff
#' @references
#' \url{https://bitcoin.org/en/developer-reference#getconnectioncount},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getconnectioncount
#' @aliases getconnectioncount
#' @rdname getconnectioncount
#' @export
getconnectioncount <- function(obj){
    rpcpost(obj, "getconnectioncount")
}
#' RPC-JSON API: setnetworkactive
#' 
#' Disable/enable all p2p network activity.
#' 
#' @param obj object of class \code{CONRPC}.
#' @param state \code{logical} the network state.
#' 
#' @return A S4-object of class \code{ANSRPC}.
#' @family Network RPCs
#' @author Bernhard Pfaff
#' @references
#' \url{https://bitcoin.org/en/developer-reference#setnetworkactive},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name setnetworkactive
#' @aliases setnetworkactive
#' @rdname setnetworkactive
#' @export
setnetworkactive <- function(obj, state = TRUE){
    pl <- list(state = state)
    rpcpost(obj, "setnetworkactive", pl)
}
