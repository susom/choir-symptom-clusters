#' Function to label new datasets with cluster labels
#'
#' @param newdata A data.frame with the clustering variables in proper order.
#'
#' @return The input data.frame with a cluster labels column.
#' @export
#'
#' @examples
label_new_data <- function(indata) {
  # load the centroids to use
  centroids <- readRDS(here::here("0_data/clean/centroids.rds"))
    
    # label the new data
    outdata <- cbind(
      indata
      , cluster = as.factor(
        apply(
          indata
          , 1
          , function(x) {
            # return the cluster assignment based on the centroids
            clus <- which.min(
              apply(
                centroids
                , 1
                , function(c, y) {
                  dist(rbind(c,y))
                }
                , y = x
              )
            )
            return(clus)
          }
        )
      )
    )
  
  # return the newly labeled data
  return(outdata)
}
