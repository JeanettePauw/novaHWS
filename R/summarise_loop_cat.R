
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

summarise_loop_cat <- function(loop = df_ana$loops[[1]][[4]],
                               patt = "",
                               idvar = "ID",
                               recordid = "instanceid",
                               useMean = TRUE, wideout = TRUE){
  if ("loopname" %in% colnames(loop)){
    loop <- loop %>% select(-(loopname))
  } else loop <- loop
  data <- loop %>%
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

  if (wideout) return(data)

  data <- data %>% pivot_longer(-c({{idvar}}), names_to = "variable", values_to = "value")

}

#y1 <- summarise_loop_cat(df_ana$loops[[1]][[4]],patt = "waste_type_sample_weight_",
 #                        sum = FALSE, wideout = FALSE)





