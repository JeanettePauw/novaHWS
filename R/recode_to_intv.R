#load("RDAs/dfResII.Rda")
#l = df_ana$loops[[1]]

#' recode_to_intv
#'
#' @param l list
#'
#' @return
#' @export
#'
#' @examples
recode_to_intv <- function(l){

  dfS <- l$sample_analysis %>%
    select(ID,instanceid, variable, value) %>%
    filter(variable == "waste_type_sample_weight_organic_waste") %>%
    mutate(intv_cat = "organic")

  dfR <- l$recyclable_analysis
  dfR <- dfR[,c("ID","instanceid", "variable", "value")]
  names(dfR) <- gsub(pattern = "_r$", replacement = "", x = names(dfR))
  dfR$intv_cat <- "recyclable"

  dfNr <- l$non_recyclable_analysis
  dfNr <- dfNr[,c("ID","instanceid", "variable", "value")]
  names(dfNr) <- gsub(pattern = "_nr$", replacement = "", x = names(dfNr))
  dfNr$intv_cat <- "non_recyclable"

  df <- rbind(dfS, dfR, dfNr); rm(dfR, dfNr)
  df$intv_cat[which(df$variable == "waste_type_non_recycling_weight_sanitary_waste")] <- "sanitary"

  df %>% select(-variable) %>% rename("variable" = "intv_cat") %>%
    group_by(ID, variable, instanceid) %>%
    nest() %>%
    summarise(value = lapply(data, sum)) %>%
    unnest(cols = c(value))

}

###
recode_to_intv2 <- function(l,
                            idvar = "ID",
                            instancevar = "instanceid",
                            typepatt = "waste_type_sample_weight_",
                            wideout = TRUE){

  dfS <- l$sample_analysis %>%
    select({{idvar}}, {{instancevar}}, variable, value, date) %>%
    mutate(variable = gsub(typepatt, "", variable)) %>%
    group_by(!!sym(idvar), variable) %>%
    mutate(class = case_when(variable == "organic_waste" ~ "organic",
                             variable %in% c("cardboard", "glass", "metals", "paper", "plastics") ~ "recyclable",
                             variable %in% c("textiles","construction_and_demolition_waste", "electronic_waste", "hazardous_waste",
                                             "other", "soil_ash_dust") ~ "non_recyclable",
                             variable %in% c("sanitary_waste") ~ "sanitary")) %>%
    ungroup() %>%
    select(-variable) %>% rename("variable" = "class") %>%
    group_by(ID, instanceid, variable, date) %>%
    summarise(value = sum(value)) %>%
    pivot_wider( names_from = variable, values_from = value) %>%
    pivot_longer(-c(ID, instanceid, date), names_to = "variable", values_to = "value") %>%
    replace_na(list(value = 0))

}
#y1 <- recode_to_intv2(l)
#y2 <- summarise_loop_cat(y1)


