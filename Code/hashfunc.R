#' Create a SHA-256 Hash with an optional Salt and truncate it.
#'
#' This function takes a character vector and an optional  salt as input, hashes
#' them together using the SHA-256 hashing algorithm, and then truncates the
#' resultant hash to either the length of the original string (if a NULL
#' truncation length is provided), or the specified truncation length (64 max).
#'
#' @name hashfunc
#' @param x The string to be hashed
#' @param salt The salt to be appended to the string before hashing
#' @param trnc The number of characters to truncate the hash to. If NULL, the
#'     length of the first string in the input vector is used.  Since the
#'     SHA-256 hash is 64 hexadecimal digits, the length cannot be greater than
#'     64; trnc = 64 and trnc = NULL (the default) are equivalent when the
#'     input string or string vector has more than 64 characters.
#' @return A truncated SHA-256 hash
#' @export
hashfunc <- function(x, salt = NULL, trnc = NULL) {

    # loading libraries
    if (!require(stringr)) message("Package dependency 'stringr' not installed.")
    if (!require(openssl)) message("Package dependency 'openssl' not installed.")

    # handling the possibility of multiple elements separated by a comma
    cvec <- strsplit(x, ",")[[1]]

    # assembling the concatenated string used for the hash input
    y <- paste(cvec, salt, sep = "")
    # if the truncated length is not supplied, use the length of the input string
    if (is.null(trnc)) t <- nchar(cvec[1])
    else               t <- trnc
    if (t > 64)        t <- 64
    # return the truncated hash
    return(str_trunc(sha256(y), t, ellipsis = ""))
}
