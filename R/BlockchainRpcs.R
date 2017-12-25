#' RPC-JSON API: getblockchaininfo
#' 
#' Returns an object containing various state info regarding blockchain processing.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getblockchaininfo},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getblockchaininfo
#' @aliases getblockchaininfo 
#' @rdname getblockchaininfo
#' @export
getblockchaininfo <- function(obj){
    rpcpost(obj, "getblockchaininfo")
}
#' RPC-JSON API: getbestblockhash
#' 
#' Returns the hash of the best (tip) block in the longest blockchain.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getbestblockhash},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getbestblockhash
#' @aliases getbestblockhash 
#' @rdname getbestblockhash
#' @export
getbestblockhash <- function(obj){
    rpcpost(obj, "getbestblockhash")
}
#' RPC-JSON API: getblock
#' 
#' Returns information of a block hash. The returned level of details depends on the
#' argument \code{verbosity}.
#'
#' @section: Details
#' If verbosity is 'l0', returns a string that is serialized,
#' hex-encoded data for block 'hash'.
#' If verbosity is 'l1' (the default), returns an object with information about block <hash>.
#' If verbosity is 'l2', returns an object with information about block <hash> and
#' information about each transaction. 
#'
#' @param obj object of class \code{CONRPC}.
#' @param blockhash \code{character}, the block hash.
#' @param verbosity \code{character}, level of returned details. 
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getblock},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getblock
#' @aliases getblock 
#' @rdname getblock
#' @export
getblock <- function(obj, blockhash, verbosity = c("l1", "l0", "l2")){
    bh <- as.character(blockhash)
    verbosity <- match.arg(verbosity)
    verb <- switch(verbosity,
                   l0 = 0L,
                   l1 = 1L,
                   l2 = 2L)
    pl <- list(blockhash = bh, verbosity = verb)
    rpcpost(obj, "getblock", pl)
}
#' RPC-JSON API: getblockcount
#' 
#' Returns the number of blocks in the longest blockchain.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getblockcount},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getblockcount
#' @aliases getblockcount 
#' @rdname getblockcount
#' @export
getblockcount <- function(obj){
    rpcpost(obj, "getblockcount")
}
#' RPC-JSON API: getblockhash
#' 
#' Returns hash of block in best-block-chain at height provided.
#'
#' @param obj object of class \code{CONRPC}.
#' @param height \code{integer} the height index.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getblockhash},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getblockhash
#' @aliases getblockhash 
#' @rdname getblockhash
#' @export
getblockhash <- function(obj, height){
    h <- as.integer(height)
    rpcpost(obj, "getblockhash", list(height = h))
}
#' RPC-JSON API: getchaintips
#' 
#' Return information about all known tips in the block tree,
#' including the main chain as well as orphaned branches.
#' 
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getchaintips},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getchaintips
#' @aliases getchaintips 
#' @rdname getchaintips
#' @export
getchaintips <- function(obj){
    rpcpost(obj, "getchaintips")
}
#' RPC-JSON API: getdifficulty
#' 
#' Returns the proof-of-work difficulty as a multiple of the minimum difficulty.
#' 
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getdifficulty},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getdifficulty
#' @aliases getdifficulty 
#' @rdname getdifficulty
#' @export
getdifficulty <- function(obj){
    rpcpost(obj, "getdifficulty")
}
#' RPC-JSON API: getmempoolinfo
#' 
#' Returns details on the active state of the TX memory pool.
#' 
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getmempoolinfo},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getmempoolinfo
#' @aliases getmempoolinfo 
#' @rdname getmempoolinfo
#' @export
getmempoolinfo <- function(obj){
    rpcpost(obj, "getmempoolinfo")
}
#' RPC-JSON API: gettxoutsetinfo
#' 
#' Returns statistics about the unspent transaction output set.
#' Note this call may take some time.
#'  
#' @param obj object of class \code{CONRPC}.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#gettxoutsetinfo},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name gettxoutsetinfo
#' @aliases gettxoutsetinfo 
#' @rdname gettxoutsetinfo
#' @export
gettxoutsetinfo <- function(obj){
    rpcpost(obj, "gettxoutsetinfo")
}
#' RPC-JSON API: pruneblockchain
#' 
#' Pruning of blockchain. 
#'
#' @param obj object of class \code{CONRPC}.
#' @param height \code{integer} The block height to prune up to.
#'
#' @section Details:
#' May be set to a discrete height, or a unix timestamp to prune blocks whose block time
#' is at least 2 hours older than the provided timestamp.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#pruneblockchain},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name pruneblockchain
#' @aliases pruneblockchain 
#' @rdname pruneblockchain
#' @export
pruneblockchain <- function(obj, height){
    rpcpost(obj, "pruneblockchain",
            list(height = height))
}
#' RPC-JSON API: getblockheader
#' 
#' Returns the block header for a given hash string. 
#'
#' @param obj object of class \code{CONRPC}.
#' @param hash \code{character} the block hash.
#' @param verbose \code{logical} \code{TRUE} for a json object,
#' \code{FALSE} for the hex encoded data.
#'
#' @section Details:
#' If verbose is false, returns a string that is serialized,
#' hex-encoded data for blockheader 'hash'. If verbose is true,
#' returns an Object with information about blockheader <hash>.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getblockheader},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getblockheader
#' @aliases getblockheader 
#' @rdname getblockheader
#' @export
getblockheader <- function(obj, hash, verbose = TRUE){
    hash <- as.character(hash)
    verbose <- as.logical(verbose)
    rpcpost(obj, "getblockheader",
            list(hash, verbose))
}
#' RPC-JSON API: getchaintxstats
#'
#' Compute statistics about the total number and
#' rate of transactions in the chain.
#' 
#' @param obj object of class \code{CONRPC}.
#' @param nblocks \code{integer} optional, size of the window in
#' number of blocks (default: one month).
#' @param blockhash \code{character} optional, the hash of the block
#' that ends the window.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getchaintxstats},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getchaintxstats
#' @aliases getchaintxstats 
#' @rdname getchaintxstats
#' @export
getchaintxstats <- function(obj, nblocks = NULL, blockhash = NULL){
    if (!is.null(nblocks)){
        rpcpost(obj, "getchaintxstats",
                list(nblocks = nblocks))        
    } else if (!is.null(blockhash)){
        p <- as.character(blockhash)
        rpcpost(obj, "getchaintxstats",
                list("blockhash" = blockhash))        
    } else {
        rpcpost(obj, "getchaintxstats")
    }
}
#' RPC-JSON API: preciousblock
#'
#' Treats a block as if it were received before others with the same work.
#' A can override the effect of an earlier one. The effects of preciousblock
#' are not retained across restarts. 
#' 
#' @param obj object of class \code{CONRPC}.
#' @param blockhash \code{character}, the hash of the block to mark as precious.
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#preciousblock},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name preciousblock
#' @aliases preciousblock 
#' @rdname preciousblock
#' @export
preciousblock <- function(obj, blockhash){
    bh <- as.character(blockhash)
    rpcpost(obj, "preciousblock",
            list("blockhash" = bh))
}
#' RPC-JSON API: getmempoolentry
#'
#' Returns mempool data for given transaction.
#' 
#' @param obj object of class \code{CONRPC}.
#' @param txid \code{character}, the transaction id (must be in mempool).
#'
#' @return A coerced \code{list} object from RPC-JSON API.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getmempoolentry},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getmempoolentry
#' @aliases getmempoolentry 
#' @rdname getmempoolentry
#' @export
getmempoolentry <- function(obj, txid){
    txid <- as.character(txid)
    rpcpost(obj, "getmempoolentry",
            list(txid))
}
