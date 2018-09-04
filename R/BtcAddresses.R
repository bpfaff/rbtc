#' Creation of a private key
#'
#' Returns a random 256-bit private key in hex notation.
#'
#' @return \code{character}.
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Wallet_import_format},\cr
#' \url{https://en.bitcoin.it/wiki/Address}
#' @name createPrivateKey
#' @aliases createPrivateKey
#' @rdname createPrivateKey
#' @examples
#' createPrivateKey()
#' @export
createPrivateKey <- function(){
    warning(paste0("\nFor exemplary purposes, only.\n",
                   "Use at your own risk!\n"))
    s <- "0123456789ABCDEF"
    idx <- sample(1:16, 64, replace = TRUE)
    pk <- sapply(idx, function(x) substr(s, x, x))
    pk <- paste(pk, collapse = "")
    pk
}
#' Create WIF from a private key
#'
#' Returns the corresponding WIF key from a private key
#'
#' @param privkey \code{character}, a private key.
#' @param mainnet \code{logical}, whether the WIF should correspond
#' to the mainnet or testnet.
#'
#' @return \code{character}, the WIF key
#'
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Wallet_import_format},\cr
#' \url{https://en.bitcoin.it/wiki/Address}
#' @name PrivKey2Wif
#' @aliases PrivKey2Wif
#' @rdname PrivKey2Wif
#' @examples
#' pk <- createPrivateKey()
#' PrivKey2Wif(pk)
#' @export
PrivKey2Wif <- function(privkey, mainnet = TRUE){
    warning(paste0("\nFor exemplary purposes, only.\n",
                   "Use at your own risk!\n"))
    if (mainnet){
        pke <- paste0("80", privkey)
    } else {
        pke <- paste0("EF", privkey)
    }
    privkeyext <- decodeHex(pke)
    privkeyexth <- hash256(privkeyext)
    checksum <- privkeyexth[1:4]
    privkeyextcs <- c(privkeyext, checksum)
    base58CheckEncode(privkeyextcs)
}
#' Create private key from WIF
#'
#' Returns the corresponding private key from a WIF key.
#'
#' @param wif \code{character}, a WIF key.
#'
#' @return \code{character}, the corresponding private key.
#'
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Wallet_import_format},\cr
#' \url{https://en.bitcoin.it/wiki/Address}
#' @name Wif2PrivKey
#' @aliases Wif2PrivKey
#' @rdname Wif2PrivKey
#' @examples
#' pk1 <- createPrivateKey()
#' wif <- PrivKey2Wif(pk1)
#' pk2 <- Wif2PrivKey(wif)
#' identical(pk1, pk2)
#' @export
Wif2PrivKey <- function(wif){
    warning(paste0("\nFor exemplary purposes, only.\n",
                   "Use at your own risk!\n"))
    fc <- substr(wif, 1, 1)
    if (!(fc %in% c("5", "9"))){
        msg <- paste0("First character in WIF does neither\n",
                      "correspond to mainnet nor testnet.\n")
        stop(msg)
    }
    if (!identical(nchar(wif), 51L)){
        stop("WIF must have a length of 51 characters.\n")
    }
    bytestring <- base58CheckDecode(wif)
    n <- length(bytestring) - 4L
    bytestringshort <- bytestring[1:n]
    privkey <- bytestringshort[-1]
    toupper(paste0(privkey, collapse = ""))
}
#' Create public key from private key
#'
#' This function creates the 512-bit public key corresponding
#' to a private key.
#'
#' @param privkey \code{character}, the private key.
#' @param mainnet \code{logical}, whether the WIF should correspond
#' to the mainnet or testnet.
#'
#' @return \code{character}, the public key.
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name PrivKey2PubKey
#' @aliases PrivKey2PubKey
#' @rdname PrivKey2PubKey
#' @export
PrivKey2PubKey <- function(privkey, mainnet = TRUE){
    warning(paste0("\nFor exemplary purposes, only.\n",
                   "Use at your own risk!\n"))
    pk <- paste0("0x", privkey, collapse = "")
    pk <- gmp::as.bigz(pk)
    p <- "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"
    b <- "0x0000000000000000000000000000000000000000000000000000000000000007"
    a <- "0x0000000000000000000000000000000000000000000000000000000000000000"
    r <- "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"
    Gx <- "0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
    Gy <- "0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"
    ec <- ecparam(p, a, b)
    g <- ecpoint(ec, gmp::as.bigz(Gx), gmp::as.bigz(Gy), gmp::as.bigz(r))
    ans <- g * pk
    x <- as.character(ans@x, b = 16)
    y <- as.character(ans@y, b = 16)
    toupper(paste0("04", x, y, collapse = ""))
}
#' Create public key hash from 512-bit public key
#'
#' This function returns the associated public key hash
#' from a 512-bit public key by using the \code{hash160()} function.
#'
#' @param pubkey \code{character}, the public key.
#' @param mainnet \code{logical}, whether the WIF should correspond
#' to the mainnet or testnet.
#'
#' @return \code{character}, the hash of a public key
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name PubKey2PubHash
#' @aliases PubKey2PubHash
#' @rdname PubKey2PubHash
#' @export
PubKey2PubHash <- function(pubkey, mainnet = TRUE){
    warning(paste0("\nFor exemplary purposes, only.\n",
                   "Use at your own risk!\n"))
    pubkey160 <- hash160(decodeHex(pubkey))
    checksum <- pubkey160[1:4]
    if (mainnet){
        pubkey160ext <- c(decodeHex("00"), pubkey160)
    } else {
        pubkey160ext <- c(decodeHex("6f"), pubkey160)
    }
    pubkey160extcs <- c(pubkey160ext, checksum)
    toupper(paste0(pubkey160extcs,
                   collapse = "")
            )
}
#' Create BTC address from public key hash
#'
#' This function returns the corresponding BTC address from a
#' hashed public key.
#'
#' @param pubhash \code{character}, the public key hash.
#'
#' @return \code{character}, the BTC address
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name PubHash2BtcAdr
#' @aliases PubHash2BtcAdr
#' @rdname PubHash2BtcAdr
#' @export
PubHash2BtcAdr <- function(pubhash){
    warning(paste0("\nFor exemplary purposes, only.\n",
                   "Use at your own risk!\n"))
    pubhashhex <- decodeHex(pubhash)
    base58CheckEncode(pubhashhex)
}
#' Create BTC addresses
#'
#' This function creates an object of S4-class \code{BTCADR}.
#'
#' @param privkey \code{character}, a private key.
#' @param mainnet \code{logical}, for which net the keys should belong to.
#'
#' @return Object of S4-class \code{BTCADR}
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name createBtcAdr
#' @aliases createBtcAdr
#' @rdname createBtcAdr
#' @export
createBtcAdr <- function (privkey, mainnet = TRUE) {
    warning(paste0("\nFor exemplary purposes, only.\n",
                   "Use at your own risk!\n"))
    pk <- sapply(seq(1, nchar(privkey), by = 2), function(x) substr(privkey,
        x, x + 1))
    if (mainnet) {
        pke <- c("80", pk)
    }
    else {
        pke <- c("ef", pk)
    }
    pke <- as.raw(strtoi(pke, base = 16L))
    h <- hash256(pke)
    checksum <- h[1:4]
    pkecs <- c(pke, checksum)
    wif <- base58CheckEncode(pkecs)
    pubkey <- PrivKey2PubKey(privkey)
    pub160 <- hash160(decodeHex(pubkey))
    if (mainnet) {
        pub160e <- c(decodeHex("00"), pub160)
    }
    else {
        pub160e <- c(decodeHex("6f"), pub160)
    }
    cs <- hash256(pub160e)[1:4]
    pubhash <- c(pub160e, cs)
    btcadr <- base58CheckEncode(pubhash)
    new("BTCADR", privkey = privkey, wif = wif, pubkey = pubkey,
        pubhash = toupper(paste(pubhash, collapse = "")), btcadr = btcadr,
        mainnet = mainnet)
}
#' Decoding of a hex string
#'
#' This function converts a hex string,, whereby the string must not
#' contain the \code{0x} prefix, to a \code{list} object with the associated
#' integers as its elements.
#'
#' @param s \code{character}, the hex string.
#'
#' @return \code{list}
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Wallet_import_format},\cr
#' \url{https://en.bitcoin.it/wiki/Address}
#' @name decodeHex
#' @aliases decodeHex
#' @rdname decodeHex
#' @examples
#' pk <- createPrivateKey()
#' decodeHex(pk)
#' @export
decodeHex <- function(s){
    h <- sapply(seq(1, nchar(s), by = 2), function(x) substr(s, x, x + 1))
    ans <- as.raw(strtoi(h, base = 16L))
    ans
}
#' Concatenate two hex strings
#'
#' This function concatenates two hex strings, provided without the \code{0x} prefix,
#' and returns a \code{list} object of the associated
#' integers.
#'
#' @param hex1 \code{character}, a hex string.
#' @param hex2 \code{character}, a hex string.
#' @return \code{list}
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Wallet_import_format},\cr
#' \url{https://en.bitcoin.it/wiki/Address}
#' @name concatHex
#' @aliases concatHex
#' @rdname concatHex
#' @examples
#' h1 <- "80"
#' h2 <- createPrivateKey()
#' concatHex(h1, h2)
#' @export
concatHex <- function(hex1, hex2){
    h1 <- decodeHex(hex1)
    h2 <- decodeHex(hex2)
    ans <- c(h1, h2)
    ans
}
#' Base 58 binary-to-text-encoding
#'
#' This is a modified binary-to-text encoding used
#' for encoding Bitcoin addresses, aka \emph{Base58Check}.
#' If this is applied to an extended private key with its trailing
#' check sum, then the result is the \emph{Wallet Import Format},
#' (WIF).
#'
#' @param x \code{character}, string in hex format.
#'
#' @return \code{character}, the encoded string.
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Wallet_import_format},\cr
#' \url{https://en.bitcoin.it/wiki/Address},\cr
#' \url{https://en.bitcoin.it/wiki/Base58Check_encoding}
#' @name base58CheckEncode
#' @aliases base58CheckEncode
#' @rdname base58CheckEncode
#' @export
base58CheckEncode <- function(x){
    b58 <- "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    zerohex <- as.raw(strtoi("", base = 16L))
    countzero <- which(x == zerohex)
    xchar <- paste0("0x", paste(x, collapse = ""))
    ben <- gmp::as.bigz(xchar)
    ans <- ""
    while (ben > 0){
        idx <- ben %% 58L ## remainder
        ans <- paste0(substr(b58, idx + 1, idx + 1), ans) ## reverse order
        ben <- ben %/% 58L
    }
    if (length(countzero) > 0){
        ans <- paste(c(rep("1", countzero), ans), collapse = "")
    }
    ans
}
#' Base 58 binary-to-text-decoding
#'
#' This is a modified binary-to-text decoding used
#' for decoding Bitcoin addresses, aka \emph{Base58Check}.
#' If this is applied to a WIF address and the first and last four
#' bytes are dropped, the result is the corresponding private key.
#'
#' @param x \code{character}, string in hex format.
#'
#' @return \code{list}, the decoded elements of the string.
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Wallet_import_format},\cr
#' \url{https://en.bitcoin.it/wiki/Address},\cr
#' \url{https://en.bitcoin.it/wiki/Base58Check_encoding}
#' @name base58CheckDecode
#' @aliases base58CheckDecode
#' @rdname base58CheckDecode
#' @export
base58CheckDecode <- function(x){
    b58 <- "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    b58v <- unlist(strsplit(b58, ""))
    xv <- unlist(strsplit(x, ""))
    ben <- 0L # big endian number
    for (i in 1:nchar(x)){
        ## index starts at 1 not 0
        ben <- gmp::as.bigz(ben * 58L + which(b58v == xv[i]) - 1.0)
    }
    shex <- ""
    while (ben > 0){
        ben %% 256L
        h <- as.integer(ben %% 256L)
        class(h) <- "hexmode"
        h <- format(h, width = 2)
        shex <- paste0(h, shex)
        ben <- gmp::as.bigz(ben %/% 256L)
    }
    decodeHex(shex)
}
#' BTC hash256
#'
#' This function returns the hash by applying the \code{sha256} hashing
#' algorithm twice to a \code{raw} object.
#'
#' @param d \code{raw}, vector.
#'
#' @return \code{character}, the value of \code{d} hashed twice.
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name hash256
#' @aliases hash256
#' @rdname hash256
#' @export
hash256 <- function(d){
    if (!identical(class(d), "raw")){
        msg <- paste0("Object 'd' is not of class raw.",
                      "\n",
                      "Trying to coerce from character.\n")
        warning(msg)
        d <- as.raw(strtoi(d, base = 16L))
    }
    d1 <- openssl::sha256(d)
    d1 <- decodeHex(paste(d1, collapse = ":"))
    d2 <- openssl::sha256(d1)
    d2 <- decodeHex(paste(d2, collapse = ":"))
    d2
}
#' BTC hash160
#'
#' This function returns the hash by applying the \code{sha256} hashing
#' first and then to the resulting hash the \code{ripemd160} algorithm.
#'
#' @param d \code{raw}, vector.
#'
#' @return \code{character}, the value of \code{d} hashed with
#' \code{sha256} and \code{ripemd160}.
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name hash160
#' @aliases hash160
#' @rdname hash160
#' @export
hash160 <- function(d){
    if (!identical(class(d), "raw")){
        msg <- paste0("Object 'd' is not of class raw.",
                      "\n",
                      "Trying to coerce from character.\n")
        warning(msg)
        d <- as.raw(strtoi(d, base = 16L))
    }
    d1 <- openssl::sha256(d)
    d1 <- decodeHex(paste(d1, collapse = ":"))
    d2 <- openssl::ripemd160(d1)
    d2 <- decodeHex(paste(d2, collapse = ":"))
    d2
}
