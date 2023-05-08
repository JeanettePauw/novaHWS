#' summarise_loop_cat
#' @description Summarise a loop section by household and category including 0 if not present
#' @param loop tibble
#' @param patt regular expression
#'
#' @return tibble
#' @export
#'
#' @examples summarise_loop_cat(loop = df_ana$loops[[1]][[1]], patt = "waste_type_non_recycling_weight_")

#patt = "waste_type_sample_weight_"

summarise_loop_cat_JP <- function(df = df_main,
                                       patt = "",
                                       idvar = c("ID"),
                                       recordid = "instanceid",
                                       analysis_var = "tot_weight",
                                       summary_by = "place",
                                       useMean = TRUE,
                                       longformat = TRUE,
                                       summary_table = TRUE){
  if ("loopname" %in% colnames(df)){
    df <- df %>% select(-(loopname))
  } else df <- df
  # Convert to long format
  if (longformat){
    df <- df
  } else {
    df <- df %>%
      select({{idvar}}, {{recordid}}, {{analysis_var}}) %>%
      pivot_longer(-c({{idvar}}, {{recordid}}), names_to = "variable", values_to = "value")
  }

  # Get summary per unit/site
  data <- df %>%
    mutate(variable = gsub(patt, "", variable)) %>%
    pivot_wider(id_cols = c({{recordid}}, {{idvar}}),
                names_from = variable,
                values_from = value,
                values_fn = sum) %>%
    pivot_longer(-c({{idvar}}, {{recordid}}), names_to = "variable", values_to = "value") %>%
    replace_na(list(value = 0)) %>%
    select(-{{recordid}})


  data <- data %>% group_by(!!!syms(idvar), variable)
  if (useMean) {
    data <- data %>% summarise(m = mean(value, na.rm = TRUE)) %>%
      pivot_wider( names_from = variable, values_from = m) %>%
      mutate(total = rowSums(across(where(is.numeric))))
  } else {
    data <- data %>% summarise(m = sum(value, na.rm = TRUE)) %>%
      pivot_wider( names_from = variable, values_from = m) %>%
      mutate(total = rowSums(across(where(is.numeric))))
  }

  data <- data %>% pivot_longer(-c(idvar), names_to = "variable", values_to = "value")
  if (summary_table){
    summary_tab <- data %>%
      filter(variable != "total") %>%
      group_by(!!!syms(summary_by), variable) %>%
      summarise(eightpointsummary(value))
    return(summary_tab)
  } else {
    return(data)
  }
}
