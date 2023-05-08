
report.indicator.df <- function(dfRes, 
                                indicator = "MASS_WASTE_PHH_PWEEK_HWS2020", 
                                indicname = "indic", 
                                indiclistvar = "ll", ...){
  df <- dfRes %>% 
    filter(!!sym(indicname) == {{indicator}}) %>% 
    select({{indiclistvar}}) 
  
  novaIndicators::report.indicator(df[[1]][[1]], ...)
}
