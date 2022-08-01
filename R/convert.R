#' Convert ISBN-10 and ISBN-13
#'
#' @param x ISBN strings
#' @param .prefix For ISBN-13 prefix. Default is' '978'.
#' @examples
#' isbn_convert13to10("978-4-06-516404-4")
#' isbn_convert13to10(c("978-4-06-516404-4", "9784863542167"))
#'
#' isbn_convert10to13(x = c("412345674X", "4022518286"))
#' @export
#' @rdname convert
isbn_convert13to10 <- function(x) {
  x <-
    stringr::str_remove_all(x, "-")
  x <-
    stringr::str_sub(x, 4, 12)
  cd <-
    isbn10_checkdigit(
      isbn_to_num(
        stringr::str_extract_all(x, "[0-9]", simplify = FALSE)))
  paste0(x, cd)
}

isbn_to_num <- function(x) {
  purrr::map(
    x,
    ~ as.numeric(.x))
}

isbn10_checkdigit <- function(x) {
  purrr::map_chr(
    x,
    function(.x) {
      cd <-
        as.character(11 - (sum(.x * 10:2) %% 11))
      if (cd == "11") {
        cd <- 0L
      } else if (cd == "10") {
        cd <- "X"
      }
      cd
    }
  )
}

isbn_to_str_collapse <- function(x) {
  purrr::map(
    x,
    ~ paste0(
      as.numeric(
        stringr::str_replace_all(.x, "X", "6")),
      collapse = ""))
}

#' @export
#' @rdname convert
isbn_convert10to13 <- function(x, .prefix = "978") {
  x <-
    stringr::str_remove_all(x, "-")
  paste0(.prefix,
         isbn_to_str_collapse(
           stringr::str_extract_all(x,
                                    stringr::boundary("character"),
                                    simplify = FALSE)))
}
