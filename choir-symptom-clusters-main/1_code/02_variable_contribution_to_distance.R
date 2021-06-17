# Variable Contribution to Distance and Separability
# 
# Author: Eric Cramer <emcramer@stanford.edu>
# 
# Description:
# Calculates the variable contribution to the distance and separability of clusters (importance)
# 

# helper functions
# calculate the variable importance using the Euclidean distance
# importance is the amount of distance each variable contributes to the distance between 
# cluster centroids on average
calc_var_imp <- function(centroids) {
  combos <- as.data.frame(combn(1:max(centroids[,1]), 2)) # get all combinations of 2 clusters 
  diffs <- apply(combos, 2, function(x) {
    # calculate and return the raw differences
    d1 <- (centroids[x[1], -1] - centroids[x[2], -1])^2
    return(d1) 
  })
  diffs2 <- do.call(rbind, diffs) # collapse to df
  perc_imp <- colSums(diffs2)/sum(colSums(diffs2)) # calculate percent importance
  return(perc_imp)
}

# plotting helper
plot_var_imp <- function(v, typ = "bar", fsize = 6) {
  df <- data.frame(
    "Variable"= names(v),
    "Importance" = v*100
  )
  df$Variable <- factor(df$Variable, levels = df$Variable[order(df$Importance)])
  # if(typ == "lolli") {
  #   p <- ggplot2::ggplot(data = df) + 
  #     ggplot2::geom_segment(
  #       aes(x = Variable, xend = Variable, y = 0, yend = Importance)
  #       , color = 'snow4'
  #       , alpha = 0.6
  #     ) + 
  #     ggplot2::geom_point(
  #       aes(x = Variable, y = Importance)
  #       , color = "royalblue4"
  #       , alpha = 0.8
  #       , size = 8
  #     ) +
  #     ggplot2::geom_text(aes(x = Variable, y = Importance + 2.75, label = round(Importance, 2))
  #                        , color = 'black'
  #                        , size = 3
  #     ) +
  #     labs(title = "Variable Contribution to Distance"
  #          , x = "Variable"
  #          , y = "% Importance"
  #     ) +
  #     ggplot2::coord_flip() + 
  #     ggplot2::theme_bw()
  # } else if(typ == "bar") {
  p <- ggplot2::ggplot(data = df) + 
    ggplot2::geom_bar(
      aes(
        x = Variable
        , y = Importance
      )
      , fill = "grey"
      , alpha = 0.6
      , stat = "identity"
      , color = "black"
    ) + 
    ggplot2::geom_text(
      aes(
        x = Variable
        , y = Importance - 1
        , label = sprintf("%.2f", Importance)
      )
      , color = 'black'
      , size = fsize * 5 / 14
    ) +
    labs(
      title = "Variable Contribution to Distance"
      , x = "Variable"
      , y = "% Importance"
    ) +
    ggplot2::coord_flip() + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(
      legend.position = "none"
      , panel.grid.major = element_blank()
      , axis.title = element_text(size = fsize + 2)
      , title = element_text(size = fsize + 3)
      , axis.text = element_text(size = fsize)
    )
  # }
  return(p)
}

variable_contribution <- function(fsize = 6){
  #--------------------------------------------------
  # LIBRARIES AND WORKSPACE
  #--------------------------------------------------
  
  # Load the required packages
  libs <- c("tidyverse", "glue", "magrittr", "here", "ggplot2", "flextable", "officer")
  suppressPackageStartupMessages(easypackages::libraries(libs))
  
  # source utility functions
  utility_dir <- here("1_code/utilities")
  if(length(list.files(utility_dir)) > 0) {
    utility_files <- paste(utility_dir,list.files(utility_dir), sep="/")
    invisible(sapply(utility_files, source))
  }
  
  #--------------------------------------------------
  # LOADING DATA
  #--------------------------------------------------
  load(file = here("0_data/clean/data.RData"))
  hc_res <- readRDS(here("2_output/models/hc_res.rds"))
  
  # isolate a labeled data frame
  labeled_data <- add_column(hc_res$data, cluster = hc_res$cluster)
  
  #--------------------------------------------------
  # CALCULATING VARIABLE CONTRIBUTION
  #--------------------------------------------------
  # calculate centroids
  # calculate the clustering centroids
  centroids <- labeled_data %>%
    group_by(cluster) %>%
    select(all_of(ind_vars)) %>%
    summarise_all(list(mean))
  
  # write out the centroids
  write_csv(centroids, here("2_output/tables/train_centroids.csv"))
  
  # calculate 95-ci for the centroids
  centroids_ci <- labeled_data %>%
    group_by(cluster) %>%
    select(all_of(ind_vars)) %>%
    summarise_all(
      list(
        mean = mean
        , lwr = function(x) mean(x) - 2*sderr(x)
        , upr = function(x) mean(x) + 2*sderr(x)
        , sd = sd
        , median = median
        , range = function(x) paste(as.character(range(x)), collapse = ",")
        , iqr = IQR
      )
    )
  write_csv(centroids_ci, here("2_output/tables/centroids_ci.csv"))
  
  # calculate variable "importance"
  var_imp <- calc_var_imp(centroids = centroids)
  names(var_imp) <- c("Fatigue", "Depression", "Anxiety",
                      "Sleep Disturbance", "Sleep Impairment", "Anger",
                      "Social Isolation", "Emotional Support",
                      "Satisfaction with Social Roles")
  saveRDS(var_imp, here("2_output/tables/varimp.rds"))
  
  # plotting
  p <- plot_var_imp(var_imp, fsize)
  saveRDS(p, here("2_output/img/variable_importance.rds"))
  my_ggsave(p, here("2_output/img/variable_importance"))
}
