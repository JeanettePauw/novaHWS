#' @title tenpointsummary
#' @description convenience function to summarise data inside dplyr pipeline
#' @param x Numeric
#' @return Tibble

# Voorbeeld dfTraffic %>% group_by(main_place, road_type) %>% summarise(across(where(is.numeric), ~tenpointsummary(.) %>% nest()))

#' tenpointsummary
#'
#' @param x
#'
#' @return tibble
#' @export

tenpointsummary <- function(x){
  tibble(
    n = length(x),
    NAs = length(x) - length(na.omit(x)),
    q25 = quantile(x, probs = 0.25, na.rm= TRUE),
    median = quantile(x, probs = 0.5, na.rm= TRUE),
    #CI.Mean.L = t.test(x)$"conf.int"[[1]],
    mean  = mean(x, na.rm = TRUE),
    #CI.Mean.U = t.test(x)$"conf.int"[[2]],
    sd = sd(x, na.rm = TRUE),
    q75 = quantile(x, probs = 0.75, na.rm= TRUE),
    q99 = quantile(x, probs = 0.99, na.rm= TRUE),
    skew = moments::skewness(x, na.rm= TRUE),
    kurtosis = moments::kurtosis(x, na.rm = TRUE)

  )
}

#' twelvepointsummary
#'
#' @param x
#'
#' @return tibble
#' @export

twelvepointsummary <- function(x){
  tibble(
    n = length(x),
    NAs = length(x) - length(na.omit(x)),
    q25 = quantile(x, probs = 0.25, na.rm= TRUE),
    median = quantile(x, probs = 0.5, na.rm= TRUE),
    CI.Mean.L = t.test(x)$"conf.int"[[1]],
    mean  = mean(x, na.rm = TRUE),
    CI.Mean.U = t.test(x)$"conf.int"[[2]],
    sd = sd(x, na.rm = TRUE),
    q75 = quantile(x, probs = 0.75, na.rm= TRUE),
    q99 = quantile(x, probs = 0.99, na.rm= TRUE),
    skew = moments::skewness(x, na.rm= TRUE),
    kurtosis = moments::kurtosis(x, na.rm = TRUE)

  )
}


#' eightpointsummary
#' @param x
#'
#' @return tibble
#' @export

eightpointsummary <- function(x){
  tibble(
    min = min(x),
    mean  = mean(x, na.rm = TRUE),
    median = quantile(x, probs = 0.5, na.rm= TRUE),
    max = max(x),
    std.dev = sd(x, na.rm = TRUE),
    "95% CI.L" = t.test(x)$"conf.int"[[1]],
    "95% CI.U" = t.test(x)$"conf.int"[[2]],
    n = length(x),
      )
}
