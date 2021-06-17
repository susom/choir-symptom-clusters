# Calculating the number of bodymap regions endorsed per cluster
# 
# Author: Eric Cramer <emcramer@stanford.edu>
# 
# Description:
# Further characterization of the clusters identified by running `2_cluster_selection.R` 
# Run this after the selection step is complete, or if there is a ready/available model object
# This file is for use in the pain interference large cohort pipeline
# 

bodymap_analysis <- function(){
  #----------
  # LIBRARIES AND WORKSPACE
  
  # clear the workspace of everything including hidden files and detach packages not in the base
  rm(list = ls(all.names = TRUE))
  
  # Load the required packages
  list_of_packages <- c("tidyverse", "glue", "magrittr", 
                        "here", "ggplot2", "openxlsx",
                        "cowplot")
  invisible(lapply(list_of_packages, library, character.only = TRUE))
  
  # source utility files
  utility_dir <- here("1_code/utilities")
  if(length(list.files(utility_dir)) > 0) {
    utility_files <- paste(utility_dir,list.files(utility_dir), sep="/")
    invisible(sapply(utility_files, source))
  }
  
  #----------
  # LOAD DATA
  # load the labeled data
  train_labeled <- read_csv(here("2_output/tables/train_labeled.csv"))
  test_labeled <- read_csv(here("2_output/tables/test_labeled.csv"))
  tp1_labeled <- read_csv(here("2_output/tables/sm_labeled.csv")) %>%
    filter(tp == "tp1")
  tp2_labeled <- read_csv(here("2_output/tables/sm_labeled.csv")) %>%
    filter(tp == "tp2")
  
  # get the number of clusters
  k <- max(train_labeled$cluster, na.rm = TRUE)
  
  #----------
  # NUMBER OF SEGMENTS
  # calculate the means for each cluster
  train_bm_summary <- bm_summary(train_labeled)
  test_bm_summary <- bm_summary(test_labeled)
  tp1_bm_summary <- bm_summary(tp1_labeled)
  tp2_bm_summary <- bm_summary(tp2_labeled)
  
  # save the cluster means to an excel file
  bm_dfs <- list(
    "training" = train_bm_summary
    , "testing" = test_bm_summary
    , "time point 1" = tp1_bm_summary
    , "time point 2" = tp2_bm_summary)
  write.xlsx(bm_dfs, file = here("2_output/tables/bm_summary.xlsx"))
  
  # visualize the cluster means
  train_p <- plot_means(train_bm_summary, "Training")
  test_p <- plot_means(test_bm_summary, "Testing")
  tp1_p <- plot_means(tp1_bm_summary, "Time Point 1")
  tp2_p <- plot_means(tp2_bm_summary, "Time Point 2")
  
  # assemble into single cowplot
  bm_cowplot <- plot_grid(train_p, test_p, tp1_p, tp2_p,
                          labels = c('A', 'B', 'C', 'D'), label_size = 12) %>%
    add_cowplot_title("Mean Number of CHOIR BodyMap Segments")
  
  # save the plots
  save_ggplots(bm_cowplot, here("2_output/img/bm_summary_plots"))
  
  # visualize the distribution
  train_p2 <- plot_dists (train_labeled, train_bm_summary, k, "Training")
  test_p2 <- plot_dists(test_labeled, test_bm_summary, k, "Testing")
  tp1_p2 <- plot_dists(tp1_labeled, tp1_bm_summary, k, "Time Point 1")
  tp2_p2 <- plot_dists(tp2_labeled, tp2_bm_summary, k, "Time Point 2")
  bm_dist_cowplot <- plot_grid(train_p2, test_p2, tp1_p2, tp2_p2,
                               labels = c('A', 'B', 'C', 'D'), 
                               label_size = 12,
                               ncol = 2,
                               nrow = 2) %>%
    add_cowplot_title("Distribution of CHOIR BodyMap Segments")
  save_ggplots(bm_dist_cowplot, here("2_output/img/bm_dist_plots"))
  
  #-----------
  # CHARACTERIZING REGIONS OF BODY BY CLUSTER
  train_bm_tab <- bm_tabulation(train_labeled, k)
  test_bm_tab <- bm_tabulation(test_labeled, k)
  tp1_bm_tab <- bm_tabulation(tp1_labeled, k)
  tp2_bm_tab <- bm_tabulation(tp2_labeled, k)
  
  # writeout scaled values
  bm_tab_dfs <- list(
    "training tabulated" = train_bm_tab$tabbed_regions
    , "testing tabulated" = test_bm_tab$tabbed_regions
    , "time point 1 tabulated" = tp1_bm_tab$tabbed_regions
    , "time point 2 tabulated" = tp2_bm_tab$tabbed_regions
    , "training perc" = train_bm_tab$scaled_regions
    , "testing perc" = test_bm_tab$scaled_regions
    , "time point 1 perc" = tp1_bm_tab$scaled_regions
    , "time point 2 perc" = tp2_bm_tab$scaled_regions)
  write.xlsx(bm_tab_dfs, file = here("2_output/tables/bm_tabulated.xlsx"))
  
  # visualize with a heatmap
  all_bm_long <- rbind(
    cbind(
      train_bm_tab$long_df
      , perc = train_bm_tab$long_df$Count/nrow(train_labeled)*100
      , type = "Training")
    , cbind(
      test_bm_tab$long_df 
      , perc = test_bm_tab$long_df$Count/nrow(test_labeled)*100
      , type = "Testing")
    , cbind(
      tp1_bm_tab$long_df
      , perc = tp2_bm_tab$long_df$Count/nrow(tp1_labeled)*100
      , type = "Time Point 1")
    , cbind(
      tp2_bm_tab$long_df
      , perc = tp2_bm_tab$long_df$Count/nrow(tp2_labeled)*100
      , type = "Time Point 2")
  )
  hm_p <- plot_hm(all_bm_long, "CHOIR BodyMap Endorsement Percentages")
  save_ggplots(hm_p, here("2_output/img/bm_endorse_perc"))
}

# helper function to calculate bodymap summary
bm_summary <- function(x) {
  df <- x %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(avg = mean(N_BODYMAP_SEGMENTS), 
                     sd = sd(N_BODYMAP_SEGMENTS), 
                     stderr = sderr(N_BODYMAP_SEGMENTS)
    )
  return(df)
}

# helper function to plot the means
plot_means <- function(dset, set_name) {
  p <- ggplot2::ggplot(data = dset) +
    ggplot2::geom_bar(aes(x = cluster, y = avg)
                      , stat = 'identity'
                      , fill = '#999999'
                      , color = 'black') +
    ggplot2::geom_text(
      aes(x = cluster, y = 5, label = round(avg, 2))
      , color = "white"
    ) +
    ggplot2::geom_errorbar(aes(x = cluster, ymin = avg-stderr, ymax = avg+stderr)
                           , width = 0.2
                           , size = 1) + 
    ggplot2::labs(title = set_name
                  , x = 'Cluster'
                  , y = '# Segments') +
    ggplot2::theme_bw()
  return(p)
}

# helper function to save plots
save_ggplots <- function(p, pname, ht = 20, wh = 20) {
  ggplot2::ggsave(glue::glue('{pname}.png')
                  , plot=p
                  , device="png"
                  , dpi="retina"
                  , width=20
                  , height=20
                  , units="cm")
  ggplot2::ggsave(glue::glue("{pname}.svg")
                  , plot=p
                  , width=20
                  , height=20
                  , device="svg")
}

# helper function to create cowplot titles
add_cowplot_title <- function(otr_plots, ttl) {
  # now add the title
  title <- ggdraw() +
    draw_label(
      ttl,
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  p <- plot_grid(
    title, otr_plots,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
  return(p)
}

# helper function to plot distributions
plot_dists <- function(dset, dset_sum, k, pname){
  facet_labels <- paste("Cluster", seq(1, k))
  names(facet_labels) <- seq(1,k)
  p <- ggplot2::ggplot(dset) +
    ggplot2::geom_density(aes(x = N_BODYMAP_SEGMENTS, fill = factor(cluster))
                          # , bins = 50
                          , alpha = 0.7
                          , color='black') +
    ggplot2::geom_vline(data = dset_sum, aes(xintercept = avg)
                        , color='black'
                        , size = 1
                        , linetype = 'dashed') + 
    ggplot2::facet_wrap(~ cluster, scales = "free_y"
                        , labeller = labeller(cluster = facet_labels)) +
    ggplot2::labs(title = pname
                  , x = "# Regions" 
                  , y = "# Patients"
                  , fill = "Cluster") +
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = "none")
  return(p)
}

# helper function to tabulate the regions endorsed by each cluster
bm_tabulation <- function(df, k){
  
  # get the number of people in each cluster
  clust_sizes <- table(df$cluster)
  
  # split data into clusters
  cluster_list <- list()
  for(i in 1:k){
    cluster_list[[i]] <- df %>%
      filter(cluster == i) %>%
      select(BODYMAP_REGIONS)
  }
  
  my_tabulate <- function(x) {
    # split each individual's list of regions
    # unlist to get master list of all regions in each cluster
    # tally within each cluster
    table(unlist(lapply(x, strsplit, split="\\|")))
  }
  cluster_regions_tabbed <- lapply(cluster_list, my_tabulate)
  
  # export the tabbed regions for visualization with a bodymap in D3
  tabbed_regions <- as.data.frame(do.call(cbind, cluster_regions_tabbed)) %>%
    add_column(.before = TRUE, regions = row.names(.))
  colnames(tabbed_regions) <- c("region", paste("cluster", 1:k, sep = "_"))
  
  # scaling
  tabbed_regions_scaled <- tabbed_regions
  tabbed_regions_scaled[, 2:k+1] <- apply(tabbed_regions[,2:k+1], 2, function(x) x/max(x))
  
  # concatenate the list of tables into a data frame and select just the frequencies observed in each cluster
  chart_df_long <- tabbed_regions %>%
    tidyr::pivot_longer(
      cols = starts_with("cluster")
      , names_to = "Cluster"
      , values_to = "Count")
  
  # package to return
  ret_vals <- list(
    tabbed_regions = tabbed_regions
    , scaled_regions = tabbed_regions_scaled
    , long_df = chart_df_long
  )
  return(ret_vals)
}

# helper function to make a heatmap
plot_hm <- function(dset, pname = ""){
  p <- dset %>%
    # mutate(perc = 100*(Count/n)) %>%
    ggplot2::ggplot() + 
    geom_tile(
      aes(
        y = region
        , x = Cluster
        , fill = perc
      )
    ) + 
    scale_fill_viridis_c(option = "B", direction = -1) +
    coord_fixed() + 
    facet_wrap(~ type, nrow = 1) + 
    labs(
      title = pname
      , x = "Cluster"
      , y = "BodyMap Region"
      , fill = "Percent Endorsement"
    ) +
    # theme_void() +
    theme(
      axis.text.x = element_text(angle = 90)
    )
  return(p)
}

# run analysis
# bodymap_analysis()