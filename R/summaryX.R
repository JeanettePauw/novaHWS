#' Summary X
#'
#' Summarise continuous variable
#'
#' @family indicatify
#' @family summaryFuncs
#'
#' @param x Numeric vector
#' @param digits Numeric vector; indicates number of significant digits.
#' @param NAsToZero Logical value; substitutes NA values with 0 if TRUE .
#'   Default is FALSE.
#' @param allowNegCIL Logical value; allows negative conf.int if TRUE.
#' @param combinedCI Numeric value; combined conf.int
#'
#' @note Use with reporting function 'reportContinuous'.
#' @export

summaryX <- function(x,
                     digits = 3,
                     NAsToZero = FALSE,
                     allowNegCIL = TRUE,
                     combinedCI = FALSE) {

        # TODO: NB! tans stel dit negatiewe CILs net na 0 indien allowNegCIL == FALSE. Daar
        # is egter 'n ordentlike manier om CIs te bereken wanneer negatiewe CILs nie
        # moontlik is nie. Vind by CJP uit wat daardie fix is en werk dit hier in!

        if (NAsToZero) {
                x[which(is.na(x))] <- 0
        }

        smmry <- round(summary(x), digits = digits)
        nms <- names(smmry)
        smmry <- as.vector(smmry)
        df <- data.frame(matrix(data = smmry,
                                nrow = 1,
                                ncol = length(smmry)))
        names(df) <- nms
        df$n <- length(x[which(!is.na(x))])
        CIs <- CI(x = x, ci = 0.95, na.rm = T, as.df = TRUE)
        if (!allowNegCIL) {
                idxx <- which(CIs[["Lower"]] < 0)
                if (length(idxx) > 0) {CIs[idxx,"Lower"] <- 0}
        }
        if (!combinedCI) {
                df[["95% CI L."]] <- round(CIs[["Lower"]], digits = digits)
                df[["95% CI U."]] <- round(CIs[["Upper"]], digits = digits)
        } else {
                if (any(!is.nan(c(CIs[["Lower"]], CIs[["Upper"]])))) {
                        df[["95% CI"]] <- sprintf("(%s, %s)",
                                                  round(CIs[["Lower"]], digits = digits),
                                                  round(CIs[["Upper"]], digits = digits))
                } else {
                        df[["95% CI"]] <- NA_character_
                }
        }
        df[["Std. Dev"]] <- round(sd(x, na.rm = TRUE), digits = digits)

        for (v in 1:ncol(df)) {
                df[[v]][which(is.nan(df[[v]]))] <- NA_real_
        }

        return(df)
}
