#' Coerce to a funtsibble object
#'
#' @param x Objects to be coerced to a funtsibble (`fun_tbl_ts`).
#' @param key Unquoted variable(s) that uniquely determine time indices. 
#' `c()` for multiple variables. It works with tidy selector
#' (e.g. [dplyr::starts_with()]).
#' @param funkey A subset of keys that define the functions.
#' @param index A bare (or unquoted) variable to specify the time index variable.
#' @param regular Regular time interval (`TRUE`) or irregular (`FALSE`). The
#' interval is determined by the greatest common divisor of index column, if `TRUE`.
#' @param validate `TRUE` suggests to verify that each key or each combination
#' of key variables leads to unique time indices (i.e. a valid tsibble). If you
#' are sure that it's a valid input, specify `FALSE` to skip the checks.
#' @param .drop If `TRUE`, empty key groups are dropped.
#' @param ... Other arguments passed on to individual methods.
#'
#' @details A funtsibble is sorted by any non-functional keys first, then functional keys and index.
#'
#' @return A funtsibble object.
#' @rdname as_funtsibble
#' @seealso \code{\link[tsibble]{as_tsibble}}
#'
#' @export
as_funtsibble <- function(x, key = NULL, funkey, index, regular = TRUE,
                       validate = TRUE, .drop = TRUE, ...) {
  stopifnot(rlang::is_logical(regular, n = 1))
  UseMethod("as_funtsibble")
}

#' @rdname as_funtsibble
#'
#' @examples
#' # coerce demogdata object to funtsibble ----
#' as_funtsibble(demography::fr.mort)
#' @export
as_funtsibble.demogdata <- function(x, ..., validate = TRUE) {
  rates_included <- ("rate" %in% names(x))
  pop_included <- ("pop" %in% names(x))
  # Avoid CRAN error check by declaring variables
  Year <- Age <- AgeGroup <- Exposure <- Group <- Rates <- Mortality <- Fertility <- NULL
  if (rates_included) {
    rates <- NULL
    for (i in seq_along(x$rate)) {
      tmp <- x$rate[[i]] %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          AgeGroup = rownames(x$rate[[i]]),
          Age = x$age
        ) %>%
        tidyr::gather(key = "Year", value = "Rates", -AgeGroup, -Age) %>%
        dplyr::mutate(
          Year = as.numeric(Year),
          Group = names(x$rate)[i]
        )
      rates <- rbind(rates, tmp)
    }
    if (x$type == "mortality") {
      rates <- dplyr::rename(rates, Mortality = Rates)
    } else if (x$type == "fertility") {
      rates <- dplyr::rename(rates, Fertility = Rates)
    } else if (x$type == "migration") {
      rates <- dplyr::rename(rates, NetMigration = Rates)
    } else {
      stop("Unknown type")
    }
  }
  if (pop_included) {
    pop <- NULL
    for (i in seq_along(x$pop)) {
      tmp <- x$pop[[i]] %>%
        as_tibble() %>%
        dplyr::mutate(
          AgeGroup = rownames(x$pop[[i]]),
          Age = x$age
        ) %>%
        tidyr::gather(key = "Year", value = "Exposure", -AgeGroup, -Age) %>%
        dplyr::mutate(
          Year = as.numeric(Year),
          Group = names(x$pop)[i]
        )
      pop <- rbind(pop, tmp)
    }
  }
  if (rates_included & pop_included) {
    output <- dplyr::full_join(rates, pop, by = c("Group", "Year", "AgeGroup", "Age"))
    if ("Mortality" %in% colnames(output) & "Exposure" %in% colnames(output)) {
      output <- output %>% dplyr::mutate(Deaths = Exposure * Mortality)
    } else if ("Fertility" %in% colnames(output) & "Exposure" %in% colnames(output)) {
      output <- output %>% dplyr::mutate(Births = Exposure * Fertility / 1000)
    }
  }
  output <- output %>%
    dplyr::select(Year, AgeGroup, Age, Group, dplyr::everything()) %>%
    dplyr::mutate(
      Age = as.integer(Age),
      Year = as.integer(Year)
    ) %>%
    tsibble::as_tsibble(index = Year, key = c(AgeGroup, Age, Group), validate = validate) %>%
    dplyr::arrange(Group, Year, Age)
    class(output) <- c("fun_tbl_ts", class(output))
    attributes(output)["funkey"] <- "Age"
    return(output)
}

#' @rdname as_funtsibble
#'
#' @examples
#' # coerce fts object to funtsibble ----
#' as_funtsibble(rainbow::ElNino_ERSST_region_1and2)
#' @export
as_funtsibble.fts <- function(x, ..., validate = TRUE) {
  output <- x$y %>%
    tibble::as_tibble() %>%
    dplyr::mutate(..x = x$x) %>%
    tidyr::gather(key = "Year", value = "..yname", -..x) %>%
    dplyr::mutate(.index = rep(x$time, rep(NROW(x$y),NCOL(x$y)))) %>%
    dplyr::arrange(.index, ..x) %>%
    dplyr::select(.index, ..x, ..yname, tidyselect::everything())
  colnames(output)[colnames(output)=="..x"] <- x$xname
  colnames(output)[colnames(output)=="..yname"] <- x$yname
  output <- tsibble::as_tsibble(output, index = .index, key = !!rlang::sym(x$xname), validate = validate)
  class(output) <- c("fun_tbl_ts", class(output))
  attributes(output)["funkey"] <- x$xname
  output
}


#' @rdname as_funtsibble
#'
#' @export
as_funtsibble.tbl_ts <- function(x, funkey, ..., validate = TRUE) {
  class(x) <- c("fun_tbl_ts", class(x))
  attributes(x)["funkey"] <- as.character(substitute(funkey))
  x
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.fun_tbl_ts <- function(x) {
  int_x <- tsibble::interval(x)
  fnt_int <- format(int_x)
  idx <- x[[tsibble::index_var(x)]]
  first <- c("A funtsibble" = paste(dim_tbl_ts(x), brackets(fnt_int)))
  n_keys <- big_mark(n_keys(x))
  key_sum <- c(Key = paste(comma(key_vars(x)), brackets(n_keys)))
  funkeys <- attributes(x)$funkey
  key_sum <- c(key_sum, "Functional Key" = comma(funkeys))
  c(first, key_sum)
}

## Following functions imported from tsibble

dim_tbl_ts <- function (x) {
    dim_x <- dim(x)
    format_dim <- purrr::map_chr(dim_x, big_mark)
    paste(format_dim, collapse = " x ")
}
big_mark <- function (x, ...) {
    mark <- if (identical(getOption("OutDec"), ",")) 
        "."
    else ","
    ret <- formatC(x, big.mark = mark, format = "d", ...)
    ret[is.na(x)] <- "??"
    ret
}
brackets <- function (x) {
    paste0("[", x, "]")
}
comma <- function (...) {
    paste(..., collapse = ", ")
}
