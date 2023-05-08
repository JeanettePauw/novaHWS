

#' make_measure_waste_cat
#' @description Make the mass per category indicator from the results of a looping question with measurements of each relevant category 
#'
#' @param loopdf List of data.frames with results of measurement loop per category with a name and value
#' @param loopname Name in loopdf of the list item with the tibbe to use
#' @param measure Character. Abbreviation for type of measure. "MASS" or "VOL"
#' @param group Character. Abbreviation  of level at which data is collected. "HH" for household
#' @param varname Character. Name in loopdf with class
#' @param valuename Character. Name in loopdf with value
#' @param dataset Character. Description of dataset
#' @param namewash Character. Portion of varname to remove, "" is nothing
#' @param pername Character. Description of level at which data is collected. Full text of group
#' @param period Character. Period over which measurement is averaged
#' @param srcpth Character. Source path
#' @param df NULL. For campatibility
#' @param multiplier NULL. For campatibility
#' @param unt Character. Unit of measure
#' @param bycat Character.
#' @param category Character.
#' @param idvar Character.
#'
#' @return
#' @export
#'
#' @examples
#' make_measure_waste_cat(loopdf, measure = "MASS", group = "HH",  pername = "household", varname = "n1",namewash = "waste_type_sample_weight_", dataset = "HWS2022",  period = "WEEK")
#' make_measure_waste_cat(ls_ana$loops[[1]][[5]], measure = "MASS", category = RCL", group = "HH", pername = "household", varname = "n1", namewash = "waste_type_recycling_weight_", dataset = "HWS2022",  period = "WEEK")
#' make_measure_waste_cat(ls_ana$loops[[1]][[7]], measure = "MASS", category = "NONRCLB", group = "HH", pername = "household", varname = "n1", namewash = "waste_type_non_recycling_weight_", dataset = "HWS2022",  period = "WEEK")


make_measure_waste <- function(loopdf, 
                               loopname = NULL,  
                               bycat = c(NA_character_, "BYCAT_", "BYINTVCAT")[1],
                               measure = c("MASS", "VOLUME")[1],  
                               group = c("HH"), 
                               category = c("", "RCLB", "NONRCLB")[1],
                               varname = "variable",
                               idvar = c("instanceid", "ID")[1],
                               valuename = "value", 
                               dataset = "", 
                               namewash = c("", "waste_type_sample_weight_")[1],
                               pername = c("", "household", "capita")[1], 
                               period = "WEEK",  
                               unt = "kg", 
                               srcpth = "", 
                               df = NULL, 
                               multiplier = NULL,
                               digits = 4,
                               ...){
  {
    # It is very important to assign 0 (before calculations) to waste categories
    # that weren't represented in the waste for a specific household for a
    # specific week. This was done in the prepscript.

    if (!is.null(loopname)) loopdf <- loopdf[[loopname]]
    if (is.na(bycat)) bycat <- ""; message("length bycat ", length(bycat))
    
    if (bycat == ""){
      res <- loopdf %>% select({{idvar}}, {{varname}}, {{valuename}}) %>% 
        rename("Class" = !!varname) %>% 
        group_by(!!!syms(idvar))  %>% 
        mutate(Class  = gsub(namewash, "", Class)) %>% 
        summarise(sum = sum(!!sym(valuename))) %>%
        summarise(summaryX(sum, allowNegCIL = FALSE, digits = digits))
    } else {
      res <- loopdf %>% select({{varname}}, {{valuename}}) %>% 
        rename("Class" = !!varname) %>% 
        group_by(Class)  %>% 
        mutate(Class  = gsub(namewash, "", Class)) %>% 
        summarise(summaryX(!!sym(valuename), allowNegCIL = FALSE))
    }
    
    

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
}
