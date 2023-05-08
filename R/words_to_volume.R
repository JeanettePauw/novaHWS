

#' words_to_volume
#'
#' @param x
#' @param words
#' @param weights
#' @param base
#'
#' @return
#' @export
#'
#' @examples
words_to_volume <- function(x,
                            words = c("Full", "3/4", "Half", "Quarter", "Less than a quarter"),
                            weights = c(1, 0.75, 0.5, 0.25, 0.125),
                            base = c(70, 20)[1]){
   names(weights) <- words
   as.numeric(weights[match(x, names(weights))]) * base

}

