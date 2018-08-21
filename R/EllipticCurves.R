#' Creating objects of class ECPARAM
#'
#' This function returns an object of S4-class \code{ECPARAM},
#' that does contain the parametrization of an elliptic curve.
#'
#' @param p \code{integer}
#' @param a \code{integer}
#' @param b \code{integer}
#'
#' @return An object of S4-class \code{ECPARAM}
#' @family EllipticCurve
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Secp256k1}
#' @name ecparam 
#' @rdname ecparam
#' @examples
#' p <- "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"
#' b <- "0x0000000000000000000000000000000000000000000000000000000000000007"
#' a <- "0x0000000000000000000000000000000000000000000000000000000000000000"
#' curve256 <- ecparam(p, a, b)
#' curve256
#' @export
ecparam <- function(p, a, b){
    p <- gmp::as.bigz(p)
    a <- gmp::as.bigz(a)
    b <- gmp::as.bigz(b)
    new("ECPARAM", p = p, a = a, b = b)
}
#' @title containsPoint-methods
#'
#' @description Checks whether a point is on a defined ellipctic curve.
#'
#' @param curve an S4-object of class \code{ECPARAM}.
#' @param x an S4-object of class \code{bigz}, the x-coordinate.
#' @param y an S4-object of class \code{bigz}, the y-coordinate.
#'
#' @return \code{logical}
#' 
#' @family EllipticCurve
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Secp256k1}
#' @name containsPoint
#' @aliases containsPoint
#' @docType methods
#' @rdname containsPoint-methods
#' @examples
#' p <- "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"
#' b <- "0x0000000000000000000000000000000000000000000000000000000000000007"
#' a <- "0x0000000000000000000000000000000000000000000000000000000000000000"
#' curve256 <- ecparam(p, a, b)
#' Gx <- "0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
#' Gy <- "0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"
#' containsPoint(curve256, Gx, Gy)
#' @export
setGeneric("containsPoint",
           function(curve, x, y) standardGeneric("containsPoint"))

#' @rdname containsPoint-methods
#' @aliases containsPoint,ECPARAM,bigz,bigz-method
setMethod("containsPoint", signature("ECPARAM", "bigz", "bigz"),
          definition = function(curve, x, y){
              term <- (y * y - (x * x * x + curve@a * x + curve@b)) %% curve@p
              term == 0L
          })
#' @rdname containsPoint-methods
#' @aliases containsPoint,ECPARAM,integer,integer-method
setMethod("containsPoint", signature("ECPARAM", "integer", "integer"),
          definition = function(curve, x, y){
              x <- gmp::as.bigz(x)
              y <- gmp::as.bigz(y)
              term <- (y * y - (x * x * x + curve@a * x + curve@b)) %% curve@p
              term == 0L
          })
#' @rdname containsPoint-methods
#' @aliases containsPoint,ECPARAM,character,character-method
setMethod("containsPoint", signature("ECPARAM", "character", "character"),
          definition = function(curve, x, y){
              x <- gmp::as.bigz(x)
              y <- gmp::as.bigz(y)
              term <- (y * y - (x * x * x + curve@a * x + curve@b)) %% curve@p
              term == 0L
          })
#' Creating objects of class ECPOINT
#'
#' This function returns an object of S4-class \code{ECPOINT},
#' that does represent a point on an elliptic curve.
#'
#' @param ecparam \code{integerECPARAM}
#' @param x x-coordinate, to be coercible to \code{bigz}.
#' @param y y-coordinate, to be coercible to \code{bigz}.
#' @param r the order of the base point.
#'
#' @return An object of S4-class \code{ECPOINT}
#' @family EllipticCurve
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Secp256k1}
#' @name ecpoint
#' @rdname ecpoint
#' @examples
#' p <- "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"
#' b <- "0x0000000000000000000000000000000000000000000000000000000000000007"
#' a <- "0x0000000000000000000000000000000000000000000000000000000000000000"
#' r <- "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"
#' x <- "0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
#' y <- "0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"
#' curve256 <- ecparam(p, a, b)
#' ecp <- ecpoint(curve256, x, y, r)
#' ecp
#' @export
ecpoint <- function(ecparam = NULL, x, y, r = NULL){
    x <- gmp::as.bigz(x)
    y <- gmp::as.bigz(y)
    r <- gmp::as.bigz(r)
    if (!is.null(ecparam)){
        checkpoint <- containsPoint(ecparam, x, y)
    } else {
        checkpoint <- TRUE
    }
    if (!checkpoint){
        stop("Point (x, y) is not on elliptic curve.\n")
    }
    new("ECPOINT", ecparam = ecparam, x = x, y = y, r = r)
}
#' @title Test for empty EC point
#'
#' @description Checks whether an EC point does exist.
#'
#' @param x object
#'
#' @return \code{logical}
#' 
#' @family EllipticCurve
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Secp256k1}
#' @name isNull
#' @aliases isNull
#' @docType methods
#' @rdname isNull-methods
#' @export
setGeneric("isNull", function(x) standardGeneric("isNull"))
#' @rdname isNull-methods
#' @aliases isNull,ECPOINT-method
setMethod("isNull", signature = "ECPOINT", function(x){
    ans <- length(x@x) == 0L & length(x@y) == 0L
    ans
    }
)
#' @title Elliptic curve operators
#'
#' @param x integer
#' @param y integer
#' @param ecp point on elliptic curve
#' @param e1 point on elliptic curve, or integer
#' @param e2 point on elliptic curve, or integer
#' 
#' @description The following operarations for EC points
#' are available:
#' \itemize{
#' \item{doubleUp}{multiplying a point by itself}
#' \item{+}{point addition}
#' \item{leftmostBit}{highest bit value of an integer}
#' \item{AND}{logical and-operator for two integers}
#' \item{*}{multiplication of an integer scalar with an EC point}
#' }
#' @family EllipticCurve
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Secp256k1}
#' @name ecoperators
#' @rdname ecoperators-methods
#' @aliases doubleUp
#' @docType methods
#' @export
setGeneric("doubleUp", function(ecp) standardGeneric("doubleUp"))
#' @rdname ecoperators-methods
#' @aliases doubleUp,ECPOINT-method
setMethod("doubleUp", signature = "ECPOINT", function(ecp){
    if (isNull(ecp)){
        return(ecpoint(NULL, NULL, NULL))
    }
    p <- ecp@ecparam@p
    a <- ecp@ecparam@a
    l <- ( (3 * ecp@x * ecp@x + a) *
          inv.bigz(2 * ecp@y, p)) %% p
    x3 <- (l * l - 2 * ecp@x ) %% p
    y3 <- (l * (ecp@x - x3) - ecp@y) %% p
    ans <- ecpoint(ecp@ecparam, x3, y3)
    ans
})
#' @rdname ecoperators-methods
#' @aliases +,ECPOINT,ECPOINT-method
setMethod("+", signature = c("ECPOINT", "ECPOINT"), function(e1, e2){
    if (isNull(e2)){
        return(e1)
    }
    if (isNull(e1)){
        return(e2)
    }
    if (!identical(e1@ecparam, e2@ecparam)){
        stop("EC parameters of operands not identical.\n")
    }
    p <- e1@ecparam@p
    if (e1@x == e2@x){
        check <- (e1@y + e2@y) %% p
        if (check == 0L){
            return(ecpoint(NULL, NULL, NULL))
        } else {
            return(doubleUp(e1))
        }
    }
    l <- ( (e2@y - e1@y) * inv.bigz(e2@x - e1@x, p) ) %% p
    x3 <- (l * l - e1@x - e2@x) %% p
    y3 <- (l * (e1@x - x3) - e1@y) %% p
    ecpoint(e1@ecparam, x3, y3)
})
#' @rdname ecoperators-methods
#' @aliases leftmostBit
setGeneric("leftmostBit", function(x) standardGeneric("leftmostBit"))
#' @rdname ecoperators-methods
#' @aliases leftmostBit,bigz-method
setMethod("leftmostBit", signature = "bigz", function(x){
    if (x <= 0L){
        stop("Negative 'bigz' integer provided.\n")
    }
    ans <- as.bigz(1L)
    while (ans <= x){
        ans <- 2L * ans
    }
    as.bigz(ans / 2L)
})
#' @rdname ecoperators-methods
#' @aliases AND
setGeneric("AND", function(x, y) standardGeneric("AND"))
#' @rdname ecoperators-methods
#' @aliases AND,bigz,bigz-method
setMethod("AND", signature = c("bigz", "bigz"), function(x, y){
    b1 <- as.character(x, b = 2)
    b1l <- nchar(b1)
    b2 <- as.character(y, b = 2)
    b2l <- nchar(b2)
    n <- min(b1l, b2l)
    b1num <- rev(as.numeric( (unlist(strsplit(b1, "")))))
    b2num <- rev(as.numeric( (unlist(strsplit(b2, "")))))
    ans <- rep(0, n)
    for (i in 1:n){
        ans[i] <- ifelse(b1num[i] + b2num[i] > 1, 1, 0)
    }
    ans <- rev(ans)
    ans <- paste("0b", paste0(ans, collapse = ""), sep = "")
    as.bigz(ans)
})
#' @rdname ecoperators-methods
#' @aliases *,ECPOINT,bigz-method
setMethod("*", signature = c("ECPOINT", "bigz"), function(e1, e2){
    ecp <- e1
    e <- e2
    if (isNull(ecp)){
        return(ecp)
    }
    if (length(ecp@r) > 0){
        e <- e %% ecp@r
    }
    if ( e == 0L){
        return(ecpoint(NULL, NULL, NULL))
    }
    if (e < 0L){
        stop("Negative 'bigz' integer.\n")
    }
    e3 <- 3L * e
    negpoint <- ecpoint(ecp@ecparam, ecp@x, -ecp@y, ecp@r)
    i <- as.bigz(leftmostBit(e3) / 2L)
    ans <- ecp
    while (i > 1){
        ans <- doubleUp(ans)
        if ( (AND(e3, i) != 0L) && (AND(e, i) == 0L) ){
            ans <- ans + ecp
        }
        if ( (AND(e3, i) == 0L) && (AND(e, i) != 0L) ){
            ans <- ans + negpoint
        }
        i <- as.bigz(i / 2)
    }
    ans
})
#' @rdname ecoperators-methods
#' @aliases *,bigz,ECPOINT-method
setMethod("*", signature = c("bigz", "ECPOINT"), function(e1, e2){
    e2 * e1
})
