# helper function for calculating standard error
sderr <- function(x, na.rm = TRUE){
  sd(x, na.rm = na.rm)/sqrt(length(x))
}