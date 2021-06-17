#
#
#

bootstrap_se <- function(){
  
  # Load the required packages
  libs <- c("tidyverse", "glue", "here", "readxl", "ggplot2",
            "cowplot", "gridExtra", "ggpubr")
  suppressPackageStartupMessages(easypackages::libraries(libs))
  
  # source utility functions
  utility_dir <- here("1_code/utilities")
  if(length(list.files(utility_dir)) > 0) {
    utility_files <- paste(utility_dir,list.files(utility_dir), sep="/")
    invisible(sapply(utility_files, source))
  }
  
  #----------
  # load data
  tp1_df <- read_csv(here("2_output/tables/sm_labeled.csv")) %>%
    dplyr::rename(orignial_cluster = cluster) %>%
    filter(tp == "tp1")
  load(file = here("0_data/clean/data.RData"))
  hc_res <- readRDS(here("2_output/models/hc_res.rds"))
  
  centroids <- read_csv(here("2_output/tables/train_centroids.csv"))
  
  # function to label new data
  centroid_classifier <- function(x, centers = centroids) {
    # return the cluster assignment based on the centroids
    return(which.min(apply(centers, 1, function(c, y){dist(rbind(c,y))}, y=x)))
  }
  
  n_iters <- 1000
  bootstrap_labels <- matrix(ncol = n_iters, nrow = nrow(tp1_df))
  
  for(i in 1:n_iters){
    set.seed(i)
    temp_df <- tp1_df %>%
      select(all_of(ind_vars)) %>%
      apply(2, function(x) x + sample(-3:3, length(x), replace = TRUE))
    
    # assign cluster labels
    bootstrap_labels[, i] <- apply(temp_df, 1, centroid_classifier)
  }
  colnames(bootstrap_labels) <- paste("Iter ", 1:n_iters)
  
  misses <- c()
  class_breakdown <- list()
  for(i in 1:nrow(bootstrap_labels)){
    misses <- c(misses, sum(bootstrap_labels[i, ] != tp1_df$orignial_cluster[i])/n_iters*100)
    class_breakdown[[i]] <- c(
      sum(bootstrap_labels[i, ] == 1)
      , sum(bootstrap_labels[i, ] == 2)
      , sum(bootstrap_labels[i, ] == 3)
    )
  }
  class_breakdown <- do.call(rbind, class_breakdown) %>% as_tibble()
  colnames(class_breakdown) <- paste0("c", 1:3)
  class_breakdown <- class_breakdown %>%
    add_column(
      .before = TRUE
      , MRN = tp1_df$MRN
      , original_cluster = tp1_df$orignial_cluster
      , perc_misclass = misses) %>%
    mutate(avg_cluster = round((1*c1 + 2*c2 + 3*c3)/3, 0))
  
  return(list(breakdown = class_breakdown, labels = bootstrap_labels))
}

# run if boot regeneration is necessary
# se_boot_res <- bootstrap_se()
# saveRDS(se_boot_res, here::here("2_output/models/se_bootstrap.rds"))

# otherwise, load previous run
se_boot_res <- readRDS(here::here("2_output/models/se_bootstrap.rds"))

# additional analyses

# which clusters were most commonly misclassified after perturbation?
abs(mean(se_boot_res$breakdown$c1 - se_boot_res$breakdown$c2))
abs(mean(se_boot_res$breakdown$c1 - se_boot_res$breakdown$c3))
abs(mean(se_boot_res$breakdown$c2 - se_boot_res$breakdown$c3))

breakdown <- se_boot_res$breakdown %>%
  mutate(avg_cluster = round((1*c1 + 2*c2 + 3*c3)/1000, 0)) %>%
  mutate(complete_miss = abs(original_cluster - avg_cluster))

write_csv(breakdown, here("2_output/tables/se_perturbation_breakdown.csv"))

# percent changing during each iteration?
change_by_iter <- apply(se_boot_res$labels, 2, 
                        function(x) sum(x != se_boot_res$breakdown$original_cluster)/length(x)*100)
change_test <- t.test(change_by_iter, mu = 30.95)
change_sd <- sd(change_by_iter)
change_avg <- mean(change_by_iter)
change_se <- sderr(change_by_iter)

# macro view of the distribution with observed change bwtn timepoints
se_boot_macro_p <- ggplot() +
  aes(change_by_iter) +
  geom_density(color = "black", fill = "cornflowerblue") +
  geom_segment(
    aes(x = 30.95, xend = 30.95, y = 0, yend = max(density(change_by_iter)$y))
    , linetype="dashed", color = "red") +
  geom_text(
    aes(
      x = 30.95 - 5
      , y = max(density(change_by_iter)$y) - 0.05
      , label = "Observed % Change\n Between Timepoints\n = 30.95%, p-value < 2.2e-16")
    , size = 3) +
  labs(
    title = "% Change During Jitter Simulations Within PROMIS SE"
    , x = "Average % Change on Iteration"
    , y = "Density"
  ) +
  theme_bw() +
  theme(
    title = element_text(size = 8)
  )

############################
# zoomed in on the distribution...
se_boot_micro_p <- ggplot() +
  aes(change_by_iter) +
  geom_histogram(aes(y=..density..), binwidth = 0.25, color = "black", fill = "grey", alpha = 0.5) +
  geom_density(color = "black", fill = "cornflowerblue", alpha = 0.8) +
  geom_segment(
    aes(
      x = mean(change_by_iter)
      , xend = mean(change_by_iter)
      , y = 0
      , yend = max(density(change_by_iter)$y) + 0.05)
    , linetype="dashed", color = "red") +
  geom_text(
    aes(
      x = mean(change_by_iter) - 0.5
      , y = max(density(change_by_iter)$y) - 0.025
      , label = paste("Mean: ", round(mean(change_by_iter), 2))), size = 3) +
  labs(
    title = "% Change During Jitter Simulations Within PROMIS SE"
    , x = "Average % Change on Iteration"
    , y = "Density"
  ) +
  theme_bw() +
  theme(
    title = element_text(size = 8)
  )

# put macro and micro together
prow <- plot_grid(se_boot_macro_p, se_boot_micro_p, nrow = 1)

ggsave(
  here::here("2_output/img/se_bootstrap_distributions.png")
  , plot = prow
  , dpi = 300
  , width = 10
  , height = 8
  , units = "in")
ggsave(
  here::here("2_output/img/se_bootstrap_distributions.svg")
  , plot = prow
  , width = 10
  , height = 8
  , units = "in")

# save out the plots as R objects
saveRDS(se_boot_micro_p, here("3_docs/Figures/se_boot_micro_p.rds"))
saveRDS(se_boot_macro_p, here("3_docs/Figures/se_boot_macro_p.rds"))

###############################################################################
# make a single figure with the bootstrap results?

# font sizes...
fsize <- 6

tbltheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 2.0)),
  colhead = list(fg_params=list(cex = 1.0)),
  rowhead = list(fg_params=list(cex = 1.0))
)

# make a table with the means...
mutbl <- cbind(
  c("Bootstrapped Mean", "Dataset Mean", "t-Statistic", "p-value")
  , c(
    round(unname(change_test$estimate), 2)
    , 30.95
    , formatC(change_test$statistic, digits = 2, format = "e")
    , formatC(change_test$p.value, digits = 2, format = "e")
  )
)
mutbl_gg <- ggtexttable(
  mutbl
  , rows = NULL
  , theme = ttheme(base_style = "classic", base_size = fsize * 2)
)
saveRDS(mutbl_gg, here("3_docs/Figures/mutbl_grob.rds"))
############################################

# make a plot with the means
mu_plot <- data.frame(
  x = factor(c("Bootstrapped\nMean", "Dataset\nMean")
             , levels = c("Bootstrapped\nMean", "Dataset\nMean")
  )
  , y = c(round(unname(change_test$estimate), 2), 30.95)
  , sd = c(change_sd, 0)
  , se = c(change_se, 0)
) %>%
  ggplot() +
  aes(
    x = x
    , y = y
  ) +
  geom_bar(
    stat = "identity"
    , fill = "grey"
    , color = "black"
  ) +
  geom_errorbar(
    aes(
      x = x
      , ymin = y
      , ymax = y + se
    )
    , width = 0.25
  ) +
  geom_errorbar(
    aes(
      x = x
      , ymin = y
      , ymax = y - se
    )
    , width = 0.25
  ) +
  geom_segment(
    aes(
      x = 1
      , xend = 2
      , y = 33
      , yend = 33
    )
  ) +
  geom_segment(
    aes(
      x = 1
      , xend = 1
      , y = 33
      , yend = 32
    )
  ) +
  geom_segment(
    aes(
      x = 2
      , xend = 2
      , y = 33
      , yend = 32
    )
  ) +
  geom_text(
    aes(
      x = 1.5
      , y = 36
      , label = "p<0.0001"
      #   paste0(
      #   "p-value = "
      #   , formatC(change_test$p.value, digits = 2, format = "e")
      # )
    )
    , size = (fsize + 2) / 15 * 4
  ) +
  # scale_y_log10() +
  labs(
    y = "% Change"
    , x = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank()
  )
mu_plot
saveRDS(mu_plot, here::here("3_docs/Figures/mu_plot.rds"))

# make the histogram
boot_plot <- ggplot() +
  aes(change_by_iter) +
  geom_histogram(
    aes(y=..density..)
    , binwidth = 0.25
    , color = "black"
    , fill = "grey"
    , alpha = 0.5
    , size = 1
  ) +
  geom_density(
    color = "grey20"
    , fill = "cornflowerblue"
    , alpha = 0.8
    , size = 1
    , linetype = "dashed"
  ) +
  geom_segment(
    aes(
      x = mean(change_by_iter)
      , xend = mean(change_by_iter)
      , y = 0
      , yend = max(density(change_by_iter)$y) + 0.05
    )
    , linetype="dashed"
    , color = "red"
    , size = 1.2
    , alpha = 0.8
  ) +
  # adding a median line --> unnecessary
  # geom_segment(
  #   aes(
  #     x = median(change_by_iter)
  #     , xend = median(change_by_iter)
  #     , y = 0
  #     , yend = max(density(change_by_iter)$y) + 0.05
  #   )
  #   , linetype = "dashed"
  #   , color = "black"
  #   , size = 1.5
  #   , alpha = 0.8
  # ) +
  labs(
    title = "Bootstrapped distribution of\n % patients moving across clusters"
    , x = "Average % change"
    , y = "Relative Likelihood" # https://en.wikipedia.org/wiki/Probability_density_function
  ) +
  theme_minimal() +
  theme(
    title = element_text(size = fsize * 1.5)
    , axis.title = element_text(size = fsize * 1.5)
    , axis.text = element_text(size = fsize)
    , panel.grid.major.x = element_blank()
    , panel.grid.minor.x = element_blank()
  )
boot_plot
saveRDS(boot_plot, here::here("3_docs/Figures/boot_plot.rds"))
