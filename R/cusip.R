mul12 <- function(v) {
  v * rep(c(1L, 2L), length.out = length(v))
}

cusip_compute_checksum <- function(s) {
  u8_l <- figi_char_to_utf8(s)
  figi_code_l <- base::lapply(u8_l, FUN = figi_utf8_to_code)
  mul12_l <- base::lapply(figi_code_l, FUN = mul12)
  digi_code_l <- base::lapply(mul12_l, FUN = expand_digits)
  figi_sum2_l <- base::lapply(digi_code_l, FUN = figi_sum_digits)
  sum_digits_v <- base::vapply(figi_sum2_l, FUN = sum, FUN.VALUE = NA_integer_)
  check_digit_v <- - sum_digits_v %% 10L
  as.character(check_digit_v)
}

cusip_has_correct_checksum <- function(s) {
  ifelse(base::nchar(s) == 9L,
         cusip_compute_checksum(substr(s, 1L, 8L)) == substr(s, 9L, 9L),
         FALSE)
}

#' Check validity of CUSIP
#'
#' Given a character vector, check the validity of CUSIP
#' (Committee on Uniform Security Identification Procedures)
#' for each of its elements.
#'
#' @param s a character vector for whose elements validity of CUSIP
#'   is checked.
#'
#' @return A logical vector.
#'
#' @examples
#' cusip_check("052800109")
#' cusip_check("87162M409")
#' cusip_check("500750104")
#' cusip_check(c("052800109", "87162M409"))
#' cusip_check(c("052800109", "87162M407"))
#'
#' @export
cusip_check <- function(s) {
  !is.na(s) &
    grepl("^[A-Z0-9]{9}$", s) &
    cusip_has_correct_checksum(s)
}
