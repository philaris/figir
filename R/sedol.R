sedol_compute_checksum <- function(s) {
  u8_l <- figi_char_to_utf8(s)
  figi_code_l <- base::lapply(u8_l, FUN = figi_utf8_to_code)
  mulw_l <- base::lapply(figi_code_l,
                         FUN = function(v) v * c(1L, 3L, 1L, 7L, 3L, 9L))
  sum_digits_v <- base::vapply(mulw_l, FUN = sum, FUN.VALUE = NA_integer_)
  check_digit_v <- (10L - sum_digits_v %% 10L) %% 10L
  as.character(check_digit_v)
}

sedol_has_correct_checksum <- function(s) {
  sedol_compute_checksum(substr(s, 1L, 6L)) == substr(s, 7L, 7L)
}

#' Check validity of SEDOL
#'
#' Given a character vector, check the validity of SEDOL
#' (Stock Exchange Daily Official List)
#' for each of its elements.
#'
#' @param s a character vector for whose elements validity of SEDOL
#'   is checked.
#'
#' @return A logical vector.
#'
#' @examples
#' sedol_check('B014635')
#' sedol_check('0263494')
#' sedol_check(c('B014635', '0263494'))
#' sedol_check(c('B014635', '0263495'))
#'
#' @export
sedol_check <- function(s) {
  !is.na(s) &
  base::nchar(s) == 7L &
    grepl("^[0-9BCDFGHJKLMNPQRSTVWXYZ]{7}$",
          s) &
    sedol_has_correct_checksum(s)
}
