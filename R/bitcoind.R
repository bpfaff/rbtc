#' Extracting Configuration Settings
#'
#' This function extracts information from the configuration
#' file \code{bitcoin.conf} with respect to the options  \code{rpcuser}
#' and \code{rpcpassword}.
#'
#' @param conf.file \code{character}, the fully qualified path.
#'
#' @return An S4-object of class \code{CONRPC}.
#' @seealso CONRPC-class, startbtc, stopbtc
#' @author Bernhard Pfaff
#' @name conrpc
#' @aliases conrpc 
#' @rdname conrpc
#' @export
#' 
conrpc <- function(conf.file){
    con <- file(conf.file, open = "r", blocking = FALSE)
    bconf <- readLines(con)
    close(con)
    bconfl <- strsplit(bconf, split = "=")
    idx <- which(unlist(lapply(bconfl, function(x) x[1] == "rpcuser")))
    if (length(idx) > 0){
        valrpcuse <- bconfl[[idx]][2]
    } else {
        warning("'rpcuser' not set, using 'rbtc'.\n")
        valrpcuse <- "rbtc"
    }
    idx <- which(unlist(lapply(bconfl, function(x) x[1] == "rpcpassword")))
    if (length(idx) > 0){
        valrpcpwd <- bconfl[[idx]][2]
    } else {
        warning("'rpcpassword' not set, creating random string.\n")
        valrpcpwd <- paste0(sample(LETTERS, 12, TRUE), collapse = "")
    }
    new("CONRPC",
        rpcuse = valrpcuse,
        rpcpwd = valrpcpwd,
        config = conf.file)
}
#' Start bitcoind server process
#'
#' This function does start the bitcoind-server process.
#' It should only be called when no suitable RPC-JSON
#' process is running
#'
#' @param confbtc \code{CONRPC} object, returned from \code{conrpc()}.
#'
#' @details The process is started by calling \code{system()}.
#' Hereby, the options: \code{rpcuser}, \code{rpcpassword} and
#' \code{conf} are used in the call to \code{bitcoind}.
#'
#' @seealso conrpc, CONRPC-class, stopbtc
#' @author Bernhard Pfaff
#' @return \code{NULL}
#' @name startbtc
#' @aliases startbtc 
#' @rdname startbtc
#' @export
#' 
startbtc <- function(confbtc){
    stopifnot(class(confbtc) == "CONRPC")
    rpcuse <- slot(confbtc, "rpcuse")
    rpcpwd <- slot(confbtc, "rpcpwd")
    config <- slot(confbtc, "config")
    strcmd <- paste("bitcoind -daemon -rpcuser=", rpcuse,
                    " -rpcpassword=", rpcpwd,
                    " -conf=", config,
                    sep = "")
    system(strcmd)
    NULL
}
#' Stop bitcoind server process
#'
#' This function stops a running bitcoind process.
#' It calls \code{bitcoin-cli stop} \emph{via} the
#' R function \code{system()}.
#'
#' @param confbtc \code{CONRPC} object, returned from \code{conrpc()}.
#' 
#' @return NULL
#' @seealso startbtc, conrpc, CONRPC-class
#' @author Bernhard Pfaff
#' @name stopbtc
#' @aliases stopbtc 
#' @rdname stopbtc
#' @export
#'
stopbtc <- function(confbtc){
    stopifnot(class(confbtc) == "CONRPC")
    rpcuse <- slot(confbtc, "rpcuse")
    rpcpwd <- slot(confbtc, "rpcpwd")
    config <- slot(confbtc, "config")
    strcmd <- paste("bitcoin-cli -rpcuser=", rpcuse,
                    " -rpcpassword=", rpcpwd,
                    " -conf=", config,
                    " stop", sep = "")
    system(strcmd)
    NULL
}
