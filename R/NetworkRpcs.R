#' RPC-JSON API: getpeerinfo
#'
#' Returns data about each connected network node as a json array of objects.
#'
#' @param con object of class \code{CONRPC}.
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
getpeerinfo <- function(con){
    rpcpost(con, "getpeerinfo")
}
#' RPC-JSON API: getnetworkinfo
#'
#' Returns an object containing various state info regarding P2P networking.
#'
#' @param con object of class \code{CONRPC}.
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
getnetworkinfo <- function(con){
    rpcpost(con, "getnetworkinfo")
}
#' RPC-JSON API: ping
#'
#' Requests that a ping be sent to all other nodes, to measure ping time.
#' Results provided in getpeerinfo, pingtime and pingwait fields are
#' decimal seconds. Ping command is handled in queue with all other commands,
#' so it measures processing backlog, not just network ping.
#'
#' @param con object of class \code{CONRPC}.
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
ping <- function(con){
    rpcpost(con, "ping")
}
#' RPC-JSON API: getnettotals
#'
#' Returns information about network traffic, including bytes in,
#' bytes out, and current time.
#'
#' @param con object of class \code{CONRPC}.
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
getnettotals <- function(con){
    rpcpost(con, "getnettotals")
}
#' RPC-JSON API: getconnectioncount
#'
#' Returns the number of connections to other nodes.
#'
#' @param con object of class \code{CONRPC}.
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
getconnectioncount <- function(con){
    rpcpost(con, "getconnectioncount")
}
#' RPC-JSON API: setnetworkactive
#'
#' Disable/enable all p2p network activity.
#'
#' @param con object of class \code{CONRPC}.
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
setnetworkactive <- function(con, state = TRUE){
    pl <- list(state = state)
    rpcpost(con, "setnetworkactive", pl)
}
#' RPC-JSON API: listbanned
#'
#' List all banned IPs/Subnets.
#'
#' @param con object of class \code{CONRPC}.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Network RPCs
#' @author Bernhard Pfaff
#' @references
#' \url{https://bitcoin.org/en/developer-reference#listbanned},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name listbanned
#' @aliases listbanned
#' @rdname listbanned
#' @export
listbanned <- function(con){
    rpcpost(con, "listbanned")
}
#' RPC-JSON API: addnode
#'
#' Attempts to add or remove a node from the addnode list.
#' Or try a connection to a node once.
#'
#' @param con object of class \code{CONRPC}.
#' @param node \code{character} the node (see \code{getpeerinfo()} for nodes).
#' @param command \code{character} 'add' to add a node to the list,
#' 'remove' to remove a node from the list, 'onetry' to try a connection
#' to the node once.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Network RPCs
#' @author Bernhard Pfaff
#' @references
#' \url{https://bitcoin.org/en/developer-reference#addnode},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name addnode
#' @aliases addnode
#' @rdname addnode
#' @export
addnode <- function(con, node, command = c("add", "remove", "onetry")){
    node <- as.character(node)
    command <- match.arg(command)
    pl <- list(node = node, command = command)
    rpcpost(con, "addnode", pl)
}
#' RPC-JSON API: clearbanned
#'
#' Clear all banned IPs.
#'
#' @param con object of class \code{CONRPC}.
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Network RPCs
#' @author Bernhard Pfaff
#' @references
#' \url{https://bitcoin.org/en/developer-reference#clearbanned},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name clearbanned
#' @aliases clearbanned
#' @rdname clearbanned
#' @export
clearbanned <- function(con){
    rpcpost(con, "clearbanned")
}
#' RPC-JSON API: getaddednodeinfo
#'
#' Returns information about the given added node,
#' or all added nodes (note that onetry addnodes
#' are not listed here)
#'
#' @param con object of class \code{CONRPC}.
#' @param node \code{character} the node (see \code{getpeerinfo()}
#' for nodes).
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Network RPCs
#' @author Bernhard Pfaff
#' @references
#' \url{https://bitcoin.org/en/developer-reference#getaddednodeinfo},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getaddednodeinfo
#' @aliases getaddednodeinfo
#' @rdname getaddednodeinfo
#' @export
getaddednodeinfo <- function(con, node = NULL){
    if (is.null(node)){
        return(rpcpost(con, "getaddednodeinfo"))
    } else {
        node <- as.character(node)[1]
        pl <- list(node = node)
        return(rpcpost(con, "getaddednodeinfo", pl))
    }
}
#' RPC-JSON API: disconnectnode
#'
#' Immediately disconnects from the specified peer node.
#' Strictly one out of \code{address} and \code{nodeid} can be
#' provided to identify the node.
#'
#' @param con object of class \code{CONRPC}.
#' @param address \code{character} the IP address/port
#' of the node.
#' @param nodeid \code{character} The node ID
#' (see \code{getpeerinfo()} for node IDs).
#'
#' @return A S4-object of class \code{ANSRPC}.
#' @family Network RPCs
#' @author Bernhard Pfaff
#' @references
#' \url{https://bitcoin.org/en/developer-reference#disconnectnode},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name disconnectnode
#' @aliases disconnectnode
#' @rdname disconnectnode
#' @export
disconnectnode <- function(con, address = NULL, nodeid = NULL){
    if (is.null(address) & is.null(nodeid)){
        stop("Either 'address' or 'nodeid' must be provided.\n")
    }
    if (!is.null(address) & !is.null(nodeid)){
        warning("Arguments 'address' and 'nodeid' provided, using 'address'.\n")
        address <- as.character(address)
        pl <- list(address = address)
    }
    if (is.null(address) & !is.null(nodeid)){
        nodeid <- as.character(nodeid)
        pl <- list(nodeid = nodeid)
    }
    if (!is.null(address) & is.null(nodeid)){
        address <- as.character(address)
        pl <- list(address = address)
    }
    rpcpost(con, "disconnectnode", pl)
}
