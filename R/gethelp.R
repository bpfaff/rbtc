#' RPC-JSON API: Help
#'
#' Returning information about RPC functions.
#'
#' @param conobj object of class \code{CONRPC}.
#' @param rpcname \code{character}, name of RPC function.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Control RPCs
#' @author Bernhard Pfaff
#' @name gethelp
#' @aliases gethelp 
#' @rdname gethelp
#' @export
gethelp <- function(conobj, rpcname = ""){
    stopifnot(class(conobj) == "CONRPC")
    rpcname <- as.character(rpcname)[1]
    ans <- POST(slot(conobj, "url"),
                authenticate(user = slot(conobj, "rpcuse"),
                             password = slot(conobj, "rpcpwd"),
                             type = "basic"),
                body = list(jsonrpc = "1.0",
                            id = "curltest",
                            method = "help",
                            params = list(command = rpcname)),
                encode = "json")
    content(ans)
}
