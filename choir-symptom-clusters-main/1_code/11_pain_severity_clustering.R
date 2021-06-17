# single dimension clustering
#--------------------------------------------------
# LIBRARIES AND WORKSPACE
#--------------------------------------------------
# clear the workspace of everything including hidden files
rm(list = ls(all.names = TRUE))

# Load the required packages
libs <- c(
  "tidyverse", "glue", "magrittr", "here", 
  "cluster", "dendextend", "ggplot2")
suppressPackageStartupMessages(easypackages::libraries(libs))

# source my custom functions
source(here::here("1_code/my_ggsave.R"))
source(here::here("1_code/my_sderr.R"))

# Set hard-coded variables
data_dir <- here::here("0_data")
out_dir <- here::here("2_output/no-pain-predictors")
temp_dir <- here::here("0_data/temp")

#--------------------------------------------------
# LOADING DATA
#--------------------------------------------------
load(file = glue("{data_dir}/temp/data.RData"))
pca_res <- readRDS(glue("{out_dir}/models/pca.rds"))

# isolate the PROMIS measures as predictors
pred_data <- train %>% 
  dplyr::select(all_of(ind_vars))

#--------------------------------------------------
# CLUSTERING
#--------------------------------------------------

# dists <- dist(train$PAIN_SEVERITY)
# hc_res <- hclust(dists, method = 'ward.D2')

# cluster the data
# using Euclidean distance and Ward's method
hc_res2 <- eclust(
  train$PAIN_SEVERITY
  , FUNcluster = "hclust"
  , method = "ward.D2"
  , stand = TRUE
  , nboot = 25)
saveRDS(hc_res2, glue("{out_dir}/models/pain_severity_hc_res.rds"))

# get the # of clusters with the optimal gap statistic
get_gap_stat <- function(x){
  k <- which(diff(x) < 0)[1] # get the first local maxima
  if(k == 1) {
    return(k)
  }
  i <- 1
  d <- x[k] - x[k - i] # check the difference with lesser values of k
  while(d < sderr(x)) {
    i <- i + 1
    d <- x[k] - x[k - i]
  }
  return(k - i + 1)
}
opt_k <- get_gap_stat(hc_res2$gap_stat$Tab[, 3])

# plot the gap statistic
gs_plot <- fviz_gap_stat(hc_res2$gap_stat)
gs_plot$layers[[1]]$aes_params$size <- 1.5
gs_plot$layers[[2]]$aes_params$size <- 2
gs_plot$layers[[3]]$aes_params$size <- 1
gs_plot$layers[[4]]$aes_params$size <- 1
gs_plot$layers[[1]]$aes_params$colour <- "black"
gs_plot$layers[[2]]$aes_params$colour <- "black"
gs_plot$layers[[3]]$aes_params$colour <- "black"
gs_plot$layers[[4]]$aes_params$colour <- "red"
gs_plot <- gs_plot +
  labs(x = "Number of clusters, k", y = "Gap statistic")
saveRDS(gs_plot, here::here("2_output/img/pain_severity_gap_statistic.rds"))
my_ggsave(gs_plot, "pain_severity_gap_statistic")

#--------------------------------------------------
# PLOTTING
#--------------------------------------------------

# plot the dendrogram
dend_plot_bw <- hc_res2 %>% 
  as.dendrogram() %>%
  # set("branches_k_color", k = 3) %>%
  set("labels", NULL) %>%
  as.ggdend() %>%
  ggplot() +
  labs(
    title = "Dendrogram of training dataset with Pain Intensity"
    # , subtitle = paste0("Optimum # of clusters is ", opt_k)
    # , y = "Tree Height"
    # , x = "Observations (Patients)"
  )
saveRDS(dend_plot_bw, here::here("2_output/img/ps_dendrogram_bw.rds"))
my_ggsave(dend_plot_bw, "pain_severity_dend_bw")

dend_plot_col <- hc_res2 %>% 
  as.dendrogram() %>%
  set("branches_k_color", k = 3) %>%
  set("labels", NULL) %>%
  as.ggdend() %>%
  ggplot() +
  labs(title = "Clustering with pain severity"
       , subtitle = paste0("Colored with k = 3 while optimum # of clusters is k = ", opt_k)
       # , y = "Tree Height"
       # , x = "Observations (Patients)"
  ) + 
  theme(axis.title.y = element_text(angle = 90))
saveRDS(dend_plot_col, here::here("2_output/img/ps_dendrogram_col.rds"))
my_ggsave(dend_plot_col, "pain_severity_dend_col")

# 3D PCA plots

coords <- as_tibble(pca_res$ind$coord) %>%
  add_column(cluster = as.factor(cutree(hc_res2, k = 3)))

# 3d plot
p <- ggplot(
  data = coords
  , aes(x = Dim.1, y = Dim.2, z = Dim.3, color = cluster)
) +
  axes_3D() + 
  stat_3D() +
  labs(
    title = "3D Representation of Clustering"
    , x = "PC 1"
    , y = "PC 2"
    , z = "PC 3"
    , color = "Cluster"
  ) + 
  scale_colour_manual(
    values = c("red", "blue", "green")
  ) +
  theme_bw() 
p   

# rotated ggplots
rot_plot <- function(ang) {
  rp <- ggplot(
    data = coords
    , aes(x = Dim.1, y = Dim.2, z = Dim.3, color = cluster)) +
    axes_3D(theta = ang) + 
    stat_3D(theta = ang) +
    labs(
      title = glue("{ang} degrees")
      , x = "PC 1"
      , y = "PC 2"
      , z = "PC 3"
      , color = "PC 4"
    ) + 
    scale_colour_manual(
      values = c("red", "blue", "green")
    ) + 
    theme_void() +
    theme(legend.position = "none")
  return(rp)
}

angles <- seq.int(0, 360, by = 45)
rps <- lapply(angles, rot_plot)

leg <- get_legend(p)
prow <- plot_grid(
  plotlist = rps,
  leg, 
  ncol = 5,
  nrow = 2)

# now add the title
title <- ggdraw() + 
  draw_label(
    "Univariate Clustering by Pain Severity on Rotated PCA Plots",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

p_uni <- plot_grid(
  title, 
  prow,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
p_uni

my_ggsave(p_uni, "rotated_univariate_pain_severity_clustering_pca_3d")

