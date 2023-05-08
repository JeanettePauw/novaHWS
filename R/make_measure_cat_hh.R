

#' make_measure_cat_hh
#'
#' @param idvar Character. 
#' @param loopdf 
#' @param loopname 
#' @param namewash 
#' @param indicatorOut 
#' @param bycat 
#' @param measure 
#' @param group 
#' @param category 
#' @param pername 
#' @param period 
#' @param unt 
#' @param srcpth 
#' @param digits 
#' @param dataset 
#' @param ... 
#'
#' @return tibble
#' @export
#'
#' @examples make_measure_cat_hh(loopdf = df_ana$loops[[1]][[1]], namewash = "waste_type_non_recycling_weight_", idvar = "ID")

make_measure_cat_hh <- function(loopdf = NULL, 
                                loopname = NULL,
                                namewash = c("", "waste_type_non_recycling_weight_")[1],
                                idvar = "ID", 
                                indicatorOut = TRUE, 
                                bycat = c(NA_character_, "BYCAT_", "BYINTVCAT")[1],
                                measure = c("MASS", "VOLUME")[1],  
                                group = c("HH"), 
                                category = c("", "RCLB", "NONRCLB")[1],
                                pername = c("", "household", "capita")[1], 
                                period = "WEEK",  
                                unt = "kg", 
                                srcpth = "",
                                digits = 2,
                                dataset = NULL, ...){
  
  message("binne-in make_measure_cat_hh \n" )
  if (!is.null(loopname)) {
    if (length(loopdf[[loopname]]) == 0) {
      loopdf <- loopdf %>% purrr::flatten() %>% purrr::flatten() %>% `[[`(loopname)
    } else {loopdf <- loopdf[[loopname]]}
    
  }
  
  message("loopdf klaar")
  
  if (is.na(bycat)) bycat <- ""; message("length bycat ", length(bycat))
  
  if (is.null(loopdf)) stop("Jy moet loopdf gee, my ou!")
  
  
  
  res <- summarise_loop_cat(loop = loopdf, patt = namewash, wideout = FALSE) %>% 
    select(-{{idvar}}) %>% 
    group_by(variable) %>% 
    summarise(res  = summaryX(value, digits = digits)) %>% 
    unnest(res)
  
  if (!indicatorOut) return(res)
  
  indic <- new("indicator")
  indic@name <- sprintf("%s_%sWASTE_P%s_P%s_%s%s", measure, category, group, period, bycat, dataset)
  indic@type <- "continuous"
  indic@val <- res
  indic@unit <- unt
  indic@description <- sprintf("Average amount of waste per %s per week, given by waste category. Unit: %s. Data set: %s", pername, unt, dataset)
  indic@sourcepath <- srcpth
  indic@date_created <- as.character(Sys.time())
  
  #assign(x = indic@name, value = indic, envir = indicators[[st]][[twn]])
  indic
  
}
