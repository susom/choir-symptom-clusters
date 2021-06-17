
# libraries
library(tidyverse)
library(here)
library(plotly)
library(FactoMineR)
library(gg3D)

pca_res <- readRDS(here("2_output/models/pca.rds"))
hc_res <- readRDS(here("2_output/models/hc_res.rds"))
load(file = here("0_data/temp/data.RData"))

# plotting multi-factor clustering
coords <- as_tibble(pca_res$ind$coord) %>%
  add_column(cluster = as.factor(hc_res$cluster)) %>%
  mutate(cluster = paste0("Cluster", cluster))

# ggplot version
ggfig <- coords %>%
  ggplot() +
  aes(
    x = Dim.1
    , y = Dim.2
    , z = Dim.3
    , color = cluster
  ) + 
  labs(color = "", x = "", y = "") +
  labs_3D(
    labs = c("PC 1", "PC 2", "PC 3"),
    hjust = c(0, 1, 1.2), vjust = c(1, 1, 1), angle = c(0, 0, 0)
    ) +
  scale_color_brewer(palette = "Blues") +
  theme_void() +
  axes_3D() +
  stat_3D(alpha = 0.9, shape = 16) + 
  theme(
    legend.position = "bottom"
    # , axis.text = element_text(size = )
  )
ggfig
saveRDS(ggfig, here::here("2_output/img/gg_clustering_pca_with_9_factors.rds"))

# plotly version
fig <- plot_ly(
  coords
  , x = ~Dim.1
  , y = ~Dim.2
  , z = ~Dim.3
  , color = ~cluster
  , marker = list(line = list(color = "black", width = 0.25, opacity = 0.8))
  , colors = c("#DEEBF7", "#9ECAE1", "#3182BD")
  , opacity = 1
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "PC 1")
      , yaxis = list(title = "PC 2")
      , zaxis = list(title = "PC 3")
    )
    , legend = list(orientation = "h", title=list(text='<b> Cluster </b>'))
    , title = "Clustering with Nine Pain-Agnostic Factors"
  )
fig

# save plotly bundle
saveRDS(fig, here("2_output/img/clustering_pca_with_9_factors.rds"))
htmlwidgets::saveWidget(
  partial_bundle(fig, local = TRUE, minified = TRUE)
  , here("2_output/img/clustering_pca_with_9_factors.html")
)

#--------------------------------------------------
# plotting pain severity clustering
# isolate the PROMIS measures as predictors
pred_data <- train %>% 
  dplyr::select(all_of(ind_vars))

# cluster the data
# using Euclidean distance and Ward's method
hc_res2 <- eclust(
  train$PAIN_SEVERITY
  , FUNcluster = "hclust"
  , method = "ward.D2"
  , stand = TRUE
  , nboot = 25)
saveRDS(hc_res2, here("2_output/models/pain_severity_hc_res.rds"))

hc_res2 <- readRDS(here::here("2_output/models/pain_severity_hc_res.rds"))


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
# my_ggsave(gs_plot, "pain_severity_gap_statistic")

#--------------------------------------------------
# PLOTTING
#--------------------------------------------------

# plot the dendrogram
dend_plot <- hc_res2 %>% 
  as.dendrogram() %>%
  set("branches_k_color", k = 3, value =  c("#DEEBF7", "#9ECAE1", "#3182BD")) %>%
  set("labels", "") %>%
  # set("labels", as.factor(cutree(hc_res2, k = 3))) %>%    # change labels
  as.ggdend() %>%
  ggplot() +
  labs(title = "Clustering with Pain Severity and K = 3"
       , subtitle = glue("Optimum # of Clusters is {opt_k}")
       , y = "Tree Height"
       , x = "Observations") + 
  theme(axis.title.y = element_text(angle = 90))
my_ggsave(dend_plot, "pain_severity_dend")

# 3D PCA plot
coords2 <- as_tibble(pca_res$ind$coord) %>%
  add_column(cluster = as.factor(cutree(hc_res2, k = 3)))

# ggplot version
# ggplot version
ggfig2 <- coords2 %>%
  ggplot() +
  aes(
    x = Dim.1
    , y = Dim.2
    , z = Dim.3
    , color = cluster
  ) + 
  labs(color = "Cluster", x = "", y = "") +
  labs_3D(
    labs = c("PC 1", "PC 2", "PC 3"),
    hjust = c(0, 1, 1.2), vjust = c(1, 1, 1), angle = c(0, 0, 0)
  ) +
  # scale_color_brewer(palette = "Blues") +
  theme_void() +
  axes_3D() +
  stat_3D(alpha = 0.4, shape = 16) + 
  theme(
    legend.position = "bottom"
  )
ggfig2
saveRDS(ggfig2, here::here("2_output/img/gg_clustering_pca_with_pain_severity.rds"))

# plotly version
fig2 <- plot_ly(
  coords2
  , x = ~Dim.1
  , y = ~Dim.2
  , z = ~Dim.3
  , color = ~cluster
  , marker = list(line = list(color = "black", width = 0.25, opacity = 0.8))
  # , colors = c("#DEEBF7", "#9ECAE1", "#3182BD")
  , opacity = 1
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "PC 1")
      , yaxis = list(title = "PC 2")
      , zaxis = list(title = "PC 3")
    )
    , legend = list(orientation = "h", title=list(text='<b> Cluster </b>'))
    , title = "Clustering with Pain Severity Alone"
  )
fig2

# save plotly bundle
saveRDS(fig2, here("2_output/img/clustering_pca_with_pain_severity.rds"))
htmlwidgets::saveWidget(
  partial_bundle(fig2, local = TRUE, minified = TRUE)
  , here("2_output/img/clustering_pca_with_pain_severity.html")
)


#----------
# labeling with GH Mental
# 3D PCA plot
gh_data <- read.csv(here("0_data/raw/init_updata_20190830.csv")) %>%
  mutate(MRN = as.character(MRN))
gh_bins <- seq(
  from = min(gh_data$GH_MENTAL_SCORE, na.rm = TRUE)
  , to = max(gh_data$GH_MENTAL_SCORE, na.rm = TRUE)
  , by = (max(gh_data$GH_MENTAL_SCORE, na.rm = TRUE)-min(gh_data$GH_MENTAL_SCORE, na.rm = TRUE))/3)

# get the GH score for each id
gh_scores <- c()
for(id in train$MRN){
  gh_scores <- c(
    gh_scores
    , gh_data$GH_MENTAL_SCORE[which(gh_data$MRN == id)[1]]
  )
}

coords3 <- as_tibble(pca_res$ind$coord) %>%
  add_column(.before = TRUE, MRN = train$MRN, GH_MENTAL_SCORE = gh_scores) %>% 
  mutate(cluster = as.factor(cut(GH_MENTAL_SCORE, breaks = gh_bins, labels = 1:3)))

# writeout
coords3 %>%
  select(MRN, cluster, GH_MENTAL_SCORE) %>%
  dplyr::rename(gh_cluster = cluster) %>%
  write_csv(here("2_output/tables/gh_cluster_assignments.csv"))

fig3 <- plot_ly(
  coords3
  , x = ~Dim.1
  , y = ~Dim.2
  , z = ~Dim.3
  , color = ~cluster
  , marker = list(line = list(color = "black", width = 0.25, opacity = 0.8))
  , colors = c("#DEEBF7", "#9ECAE1", "#3182BD")
  , opacity = 1
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "PC 1")
      , yaxis = list(title = "PC 2")
      , zaxis = list(title = "PC 3")
    )
    , legend = list(orientation = "h", title=list(text='<b> Cluster </b>'))
    , title = "Clustering with GH Mental"
  )
fig3

# save plotly bundle
htmlwidgets::saveWidget(
  partial_bundle(fig3, local = TRUE, minified = TRUE)
  , here("2_output/img/clustering_pca_with_gh_mental.html")
)
