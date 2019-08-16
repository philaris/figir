expand_digits <- function(v) {
  Reduce(c,
    base::lapply(v,
                 function(x)
                   as.integer(substring(x, seq(nchar(x)), seq(nchar(x)))))
  )
}

isin_compute_checksum <- function(s) {
  u8.l <- figi_char_to_utf8(s)
  figi_code.l <- base::lapply(u8.l, FUN = figi_utf8_to_code)
  digi_code.l <- base::lapply(figi_code.l, FUN = expand_digits)
  mul12.l <- base::lapply(digi_code.l,
                          FUN = function (v) { v * rev(rep(c(2L, 1L), length.out = length(v))) })
  figi_sum2.l <- base::lapply(mul12.l, FUN = figi_sum_digits)
  sum_digits.v <- base::vapply(figi_sum2.l, FUN = sum, FUN.VALUE = NA_integer_)
  check_digit.v <- -sum_digits.v %% 10L
  as.character(check_digit.v)
}

isin_has_correct_checksum <- function(s) {
  isin_compute_checksum(substr(s, 1L, 11L)) == substr(s, 12L, 12L)
}

#' Check validity of ISIN
#'
#' Given a character vector, check the validity of ISIN
#' (International Securities Identification Number)
#' for each of its elements.
#'
#' @param s a character vector for whose elements validity of ISIN
#'   (International Securities Identification Number) is checked.
#'
#' @return A logical vector.
#'
#' @examples
#' isin_check('BBG000BLNQ16')
#' isin_check('NRG92C84SB39')
#' isin_check(c('BBG000BLNQ16', 'NRG92C84SB39'))
#'
#' @export
isin_check <- function(s) {
  base::nchar(s) == 12L &
    grepl('^[A-Z]{2}[A-Z0-9]{9}[0-9]$',
          s) &
    isin_has_correct_checksum(s)
}
