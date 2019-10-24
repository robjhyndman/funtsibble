#' Coerce to a funtsibble object
#'
#' @param x Objects to be coerced to a funtsibble (`fun_tbl_ts`).
#' @param validate `TRUE` suggests to verify that each key or each combination
#' of key variables leads to unique time indices (i.e. a valid tsibble). If you
#' are sure that it's a valid input, specify `FALSE` to skip the checks.
#' @param ... Other arguments passed on to individual methods.
#'
#' @return A funtsibble object.
#' @rdname as_funtsibble
#' @seealso \code{\link[tsibble]{as_tsibble}}
#'
#' @export
as_funtsibble <- function(x, key = NULL, index, regular = TRUE,
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
  output
}


#' @rdname as_funtsibble
#'
#' @export
as_funtsibble.tsibble <- function(x, ..., validate = TRUE) {
  class(x) <- c("fun_tbl_ts", class(x))
  x
}
