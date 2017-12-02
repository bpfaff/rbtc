#' Start bitcoind server process
#'
#' This function does start the bitcoind-server process.
#' It should only be called when no suitable RPC-JSON
#' process is running
#'
#' @param rpcuser \code{character}, the name of the RPC-user.
#' @param rpcpassword \code{character}, a given (strong) password.
#' @param conf.file \code{character}, the path and filename to the
#' bitcoind configuration file.
#'
#' @details The default value for \code{conf.file} is \code{NULL}.
#' In this case, the bitcoind process is started with the options:
#' server, rpcuser and rpcpassword. The process is started by calling
#' \code{system2()}.
#'
#' @author Bernhard Pfaff
#' @return \code{NULL}
#' @export
#' 
startbtc <- function(rpcuser = "rbtc", rpcpassword = "$GiveItATry4Me",
                     conf.file = NULL){
    if (is.null(conf.file)){
        strcmd <- paste("bitcoind -daemon -rpcuser=", rpcuser,
                        " -rpcpassword=", rpcpassword, sep = "")
    } else {
        strcmd <- paste("bitcoind -daemon -rpcuserconf=", conf.file,
                        sep = "")
    }
    system(strcmd)
    NULL
}
#' Stop bitcoind server process
#'
#' This function stops a running bitcoind process.
#' It calls \code{bitcoin-cli stop} \emph{via} the
#' R function \code{system2()}.
#'
#' @param rpcuser \code{character}, the name of the RPC-user.
#' @param rpcpassword \code{character}, a given (strong) password.
#' @param conf.file \code{character}, the path and filename to the
#' bitcoind configuration file.
#' 
#' @return NULL
#' @author Bernhard Pfaff
#' @export
#'
stopbtc <- function(rpcuser = "rbtc", rpcpassword = "$GiveItATry4Me",
                     conf.file = NULL){
    if (is.null(conf.file)){
        strcmd <- paste("bitcoin-cli -rpcuser=", rpcuser,
                        " -rpcpassword=", rpcpassword,
                        " stop", sep = "")
    } else {
        strcmd <- paste("bitcoin-cli -rpcuserconf=", conf.file,
                        " stop", sep = "")
    }
    system(strcmd)
    NULL
}
