#' RPC-JSON API: getblockchaininfo
#' 
#' Returns an object containing various state info regarding blockchain processing.
#'
#' @param obj object of class \code{CONRPC}.
#'
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
        p <- as.integer(nblocks)
        rpcpost(obj, "getchaintxstats",
                list(nblocks = p))
    } else if (!is.null(blockhash)){
        p <- as.character(blockhash)
        rpcpost(obj, "getchaintxstats",
                list(blockhash = p))
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' @return A S4-object of class \code{ANSRPC}.
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
#' RPC-JSON API: getmempoolancestors
#'
#' If txid is in the mempool, returns all in-mempool ancestors.
#' 
#' @param obj object of class \code{CONRPC}.
#' @param txid \code{character}, the transaction id (must be in mempool).
#' @param verbose \code{logical}, \code{TrueTRUE} for a json object,
#' \code{FALSE} for array of transaction ids (default).
#' 
#' @return A S4-object of class \code{ANSRPC}.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getmempoolancestors},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getmempoolancestors
#' @aliases getmempoolancestors
#' @rdname getmempoolancestors
#' @export
getmempoolancestors <- function(obj, txid, verbose = FALSE){
    txid <- as.character(txid)
    rpcpost(obj, "getmempoolancestors",
            list(txid = txid,
                 verbose = verbose))
}
#' RPC-JSON API: getmempooldescendants
#'
#' If txid is in the mempool, returns all in-mempool descendants.
#' 
#' @param obj object of class \code{CONRPC}.
#' @param txid \code{character}, the transaction id (must be in mempool).
#' @param verbose \code{logical}, \code{TrueTRUE} for a json object,
#' \code{FALSE} for array of transaction ids (default).
#' 
#' @return A S4-object of class \code{ANSRPC}.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getmempooldescendants},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getmempooldescendants
#' @aliases getmempooldescendants
#' @rdname getmempooldescendants
#' @export
getmempooldescendants <- function(obj, txid, verbose = FALSE){
    txid <- as.character(txid)
    rpcpost(obj, "getmempooldescendants",
            list(txid = txid,
                 verbose = verbose))
}
#' RPC-JSON API: getrawmempool
#'
#' Returns all transaction ids in memory pool as a json array
#' of string transaction ids.
#' Hint: use getmempoolentry to fetch a specific transaction
#' from the mempool.
#' 
#' @param obj object of class \code{CONRPC}.
#' @param verbose \code{logical}, \code{TRUE} for a json object,
#' \code{FALSE} for array of transaction ids
#' 
#' @return A S4-object of class \code{ANSRPC}.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#getrawmempool},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name getrawmempool
#' @aliases getrawmempool
#' @rdname getrawmempool
#' @export
getrawmempool <- function(obj, verbose = TRUE){
    rpcpost(obj, "getrawmempool",
            list(verbose))
}
#' RPC-JSON API: gettxout
#'
#' Returns details about an unspent transaction output.
#' 
#' @param obj object of class \code{CONRPC}.
#' @param txid \code{charcater} the transaction id.
#' @param n \code{integer} vout number.
#' @param incmempool \code{logical} whether to include the mempool (default \code{TRUE}).
#'
#' @details
#' Note that an unspent output that is spent in the mempool won't appear.
#' 
#' @return A S4-object of class \code{ANSRPC}.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#gettxout},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name gettxout
#' @aliases gettxout
#' @rdname gettxout
#' @export
gettxout <- function(obj, txid, n, incmempool = TRUE){
    txid <- as.character(txid)
    n <- as.integer(n)
    incmempool <- as.logical(incmempool)
    rpcpost(obj, "gettxout",
            list(txid = txid,
                 n = n,
                 include_mempool = incmempool))
}
#' RPC-JSON API: gettxoutproof
#'
#' Returns a hex-encoded proof that "txid" was included in a block.
#' 
#' @param obj object of class \code{CONRPC}.
#' @param txids \code{charcater} a json array of txids to filter.
#' @param blockhash \code{integer} looks for txid in the block with this hash,
#' (optional, default \code{NULL}).
#'
#' @details
#' NOTE: By default this function only works sometimes. This is when there is an
#' unspent output in the utxo for this transaction. To make it always work,
#' you need to maintain a transaction index, using the -txindex command line
#' option or specify the block in which the transaction is included manually
#' (by blockhash).
#' 
#' @return A S4-object of class \code{ANSRPC}.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#gettxoutproof},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name gettxoutproof
#' @aliases gettxoutproof
#' @rdname gettxoutproof
#' @export
gettxoutproof <- function(obj, txids, blockhash = NULL){
    txids <- as.character(txids)
    txids <- matrix(txids, ncol = 1)
    if (is.null(blockhash)){
        ans <- rpcpost(obj, "gettxoutproof",
                       list(txids = as.list(txids)))
    } else {
        bh <- as.integer(blockhash)
        ans <- rpcpost(obj, "gettxoutproof",
                       list(txids = as.list(txids),
                            blockhash = bh))
    }
    ans
}
#' RPC-JSON API: verifychain
#'
#' Verifies blockchain database.
#' 
#' @param obj object of class \code{CONRPC}.
#' @param checklevel \code{integer} (optional, 0-4, default=3),
#' how thorough the block verification is.a json array of txids to filter.
#' @param nblocks \code{integer} (optional, default=6, 0=all),
#' the number of blocks to check.
#' 
#' @return A S4-object of class \code{ANSRPC}.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#verifychain},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name verifychain
#' @aliases verifychain
#' @rdname verifychain
#' @export
verifychain <- function(obj, checklevel = NULL, nblocks =  NULL){
    if (!is.null(checklevel) && is.null(nblocks)){
        ans <- rpcpost(obj, "verifychain",
                       list(checklevel = checklevel))
    }
    if (is.null(checklevel) && !is.null(nblocks)){
        ans <- rpcpost(obj, "verifychain",
                       list(nblocks = nblocks))
    }
    if (!is.null(checklevel) && !is.null(nblocks)){
        ans <- rpcpost(obj, "verifychain",
                       list(checklevel = checklevel,
                            nblocks = nblocks))
    }
    if (is.null(checklevel) && is.null(nblocks)){
        ans <- rpcpost(obj, "verifychain",
                       list())
    }
    ans
}
#' RPC-JSON API: verifytxoutproof
#'
#' Verifies that a proof points to a transaction in a block,
#' returning the transaction it commits to and throwing an
#' RPC error if the block is not in our best chain.
#' 
#' @param obj object of class \code{CONRPC}.
#' @param proof \code{character} the hex-encoded proof generated
#' by gettxoutproof.
#' 
#' @return A S4-object of class \code{ANSRPC}.
#' @family Blockchain RPCs
#' @author Bernhard Pfaff
#' @references \url{https://bitcoin.org/en/developer-reference#verifytxoutproof},
#' \url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
#' @name verifytxoutproof
#' @aliases verifytxoutproof
#' @rdname verifytxoutproof
#' @export
verifytxoutproof <- function(obj, proof){
    proof <- as.character(proof)
    rpcpost(obj, "verifytxoutproof",
            list(proof))
}
