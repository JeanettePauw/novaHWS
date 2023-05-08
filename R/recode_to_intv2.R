#' recode_to_intv2
#'
#' @param l list
#' @param idvar character with variable containg id. Default: "ID"
#'
#' @param instancevar character
#' @param typepatt character
#' @param wideout logical
#'
#' @return tibble containing total waste per main categories
#'  (recyclable, non-recyclable, sanitary, organic)
#' @export

recode_to_intv2 <- function(df,
                            idvar = "ID",
                            instancevar = "instanceid",
                            typepatt = c("waste_type_sample_weight_","")[2],
                            wideout = TRUE){

  df %>%
    select({{idvar}}, {{instancevar}}, place, variable, value) %>%
    mutate(variable = gsub(typepatt, "", variable)) %>%
    group_by(!!sym(idvar), variable) %>%
    mutate(class = case_when(variable == "organic_waste" ~ "organic",
                             variable %in% c("cardboard", "glass", "metals", "paper", "plastics") ~ "recyclable",
                             variable %in% c("textiles","construction_and_demolition_waste", "electronic_waste", "hazardous_waste",
                                             "other", "soil_ash_dust") ~ "non_recyclable",
                             variable %in% c("sanitary_waste") ~ "sanitary")) %>%
    ungroup() %>%
    select(-variable) %>% rename("variable" = "class") %>%
    group_by(ID, instanceid, variable, place) %>%
    summarise(value = sum(value)) %>%
    pivot_wider( names_from = variable, values_from = value) %>%
    pivot_longer(-c(ID, instanceid, place), names_to = "variable", values_to = "value") %>%
    replace_na(list(value = 0))

}
