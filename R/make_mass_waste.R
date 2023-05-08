
## mass of waste per household per week

#' make_mass_waste
#' Get the overall average by taking the average of the stand averages. This
#' means that each stand gets the same weight (/#of stands) in the calculation.
#' When multiplier is specified, the average is calculated per "multiplier",
#' for example per capita.
#' @param df tibble
#' @param group Charcter.
#' @param idvar Charcter.
#' @param dataset Charcter.
#' @param indicatorOut Logic
#' @param multiplier Charcter. Defalut NA
#' @param pername Charcter.
#' @param period Charcter.
#' @param srcpth Charcter.
#' @param measure Charcter.
#' @param unt Charcter.
#' @param category Currently ignored - for campatability with other functions in family to enable uniform arg list
#' @param namewash Currently ignored - for campatability with other functions in family to enable uniform arg list
#'
#' @return
#' @export
#'
#' @examples
#' make_mass_waste(df = ls_ana$data[[1]], multiplierdf = ls_bd$data[[1]], pername = "capita", group = "CAP")
#'
#'
make_mass_waste <- function(df,
                            group = "",
                            idvar = "ID",
                            dataset = "HWS2022",
                            indicatorOut = TRUE,
                            measure = c("MASS", "VOL")[1],
                            multiplier = c(NA, "number_members_residing")[1],
                            pername = c("household", "capita")[1],
                            unt = c("kg", "l")[1],
                            period = "WEEK",
                            srcpth = "",
                            ...){


  if (!is.na(multiplier)) {
    res <- df %>% mutate(sample_weight = dry_sample_weight + wet_sample_weight) %>%
      select({{idvar}}, {{multiplier}}, sample_weight) %>%
      group_by(!!sym(idvar)) %>%
      summarise(kg_pw_phh = mean( sample_weight /  !!sym(multiplier))) %>%
      summarise(kg_pw = novaIndicators::summaryX(kg_pw_phh, allowNegCIL = FALSE)) %>%
      `[[`(1)
  } else {
    res <- df %>% mutate(sample_weight = dry_sample_weight + wet_sample_weight) %>%
      select({{idvar}}, sample_weight) %>%
      group_by(!!sym(idvar)) %>%
      summarise(kg_pw_phh = mean(sample_weight)) %>%
      summarise(kg_pw = novaIndicators::summaryX(kg_pw_phh, allowNegCIL = FALSE)) %>%
      `[[`(1)
  }
if (!indicatorOut) return(res)

  indic <- new("indicator")
  indic@name <- sprintf("%s_WASTE_P%s_P%s_%s", measure, group, period, dataset)
  indic@type <- "continuous"
  indic@val <- res
  indic@unit <- unt
  indic@description <- sprintf("Average %s of waste per %s per week. Unit: %s. Data set: %s.", tolower(measure), pername, unt, dataset)
  indic@sourcepath <- srcpth
  indic@date_created <- as.character(Sys.time())

  #assign(x = indic@name, value = indic, envir = indicators[[st]][[twn]])
  indic
}
