# PCA and visualizations
# 
# Author: Eric Cramer <emcramer@stanford.edu>
# 
# Description:
# Visualization of the clustering with PCA
# 

libs <- c("factoextra", "ggplot2", "tidyverse", "plotly", "FactoMineR", "ggcorrplot", "here")
suppressPackageStartupMessages(easypackages::libraries(libs))
source(here::here("1_code/utilities/my_ggsave.R"))

load(file = here::here("0_data/clean/data.RData"))

# isolate the PROMIS measures as predictors
pred_data <- train %>% 
  dplyr::select(all_of(ind_vars))

hc_res <- readRDS(here::here("2_output/models/hc_res.rds"))

# pca of the data for scree plotting
# pca_res <- PCA(pred_data, graph = FALSE)
# saveRDS(pca_res, here("2_output/models/pca.rds"))
pca_res <- readRDS(here::here("2_output/models/pca.rds"))

# plot the first 2 PCs against each other and look at clusters
c_plot <- fviz_cluster(hc_res
                       , geom = "point"
                       , ellipse.type = "norm"
                       , ggtheme = theme_minimal())
my_ggsave(c_plot, here("2_output/img/cluster_pca"))

# plot a scree plot of the PCA
scree_plot <- fviz_screeplot(pca_res)
scree_plot$layers[[1]]$aes_params$colour <- "black"
scree_plot$layers[[1]]$aes_params$fill <- "grey"
scree_plot <- scree_plot +
  geom_text(
    aes(
      x = 1:9
      , y = scree_plot$data$eig + 2
      , label = sprintf("%.2f", scree_plot$data$eig)
    )
    , size = 6 / 15 * 4
  )
saveRDS(scree_plot, here::here("2_output/img/scree_plot"))
my_ggsave(scree_plot, here("2_output/img/scree_plot"))

# save scree plot as table
pca_res$eig[,2:3] %>% write.csv(here("2_output/tables/pca_scree_data.csv"))

# plot the variable contribution to PCs
varpc_plot <- fviz_pca_var(pca_res, col.var = "black", labelsize = 2, repel = TRUE) +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))
my_ggsave(varpc_plot, here("2_output/img/variable_pc_contributions"))

# variable contributions to the first 3 dimensions
var_contrib_plot <- fviz_contrib(pca_res, choice = "var", axes = 1:3) +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))
my_ggsave(var_contrib_plot, here("2_output/img/variable_contributions_pcs_1_to_3"))

# another way of looking at the variable contributions
# var_contrib_plot2 <- ggcorrplot(
#   pca_res$var$contrib[, 1:3]
#   , method = "circle"
#   , ggtheme = ggplot2::theme_bw
#   , title = "Variable Contribution to Dimension/Principle Component") +
#   coord_flip() +
#   scale_x_discrete(labels = c("Fatigue", "Depression", "Anxiety",
#                               "Sleep Disturbance", "Sleep Impairment",
#                               "Anger", "Social Isolation",
#                               "Emotional Support", "Satisfaction with Social Roles")
#   ) +
#   scale_y_discrete(labels = paste("PC", 1:5)) +
#   theme(
#     panel.grid.major = element_blank()
#     , panel.grid.minor = element_blank()
#     , axis.title = element_blank()
#   )
# my_ggsave(var_contrib_plot2, here("2_output/img/variable_contributions_pcs_1_to_3"))


# # cos2 plot
# var <- get_pca_var(pca_res)
# svg(here("2_output/img/variable_contributions_pcs_1_to_5"))
# corrplot::corrplot(
#   var$contrib
#   , is.corr=FALSE
#   , tl.col = "black"
#   , tl.cex = 1
#   , title = "Variable Contribution to Dimension/Principle Component"
# )
# dev.off()

# The quality of representation of the variables on factor map is called 
# cos2 (square cosine, squared coordinates).
# viz for 1-5
var_contrib_plot3 <- var$contrib %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(
    Variable = plyr::mapvalues(
      Variable
      , from = row.names(var$contrib)
      , to = c("Fatigue", "Depression", "Anxiety",
               "Sleep Disturbance", "Sleep Impairment",
               "Anger", "Social Isolation",
               "Emotional Support", "Satisfaction with Social Roles")
    )
  ) %>%
  pivot_longer(cols = contains("Dim"), names_to = "PC", values_to = "Contribution") %>%
  ggplot() +
  aes(x = PC, y = Variable, fill = Contribution) +
  geom_tile(color = "white") +
  geom_text(aes(x = PC, y = Variable, label = round(Contribution, 1)), color = "white") +
  labs(
    title = "Variable Contribution to Dimension/Principle Component"
  ) +
  scale_x_discrete(labels = paste("PC", 1:5)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , axis.title = element_blank()
  )
saveRDS(var_contrib_plot3, here("2_output/img/variable_contributions_pcs_1_to_5.rds"))
my_ggsave(var_contrib_plot3, here("2_output/img/variable_contributions_pcs_1_to_5"))

# viz for 1-3
var_contrib_plot4 <- var$contrib[, 1:3] %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(
    Variable = factor(
      plyr::mapvalues(
        Variable
        , from = row.names(var$contrib)
        , to = c("Fatigue", "Depression", "Anxiety",
                 "Sleep Disturbance", "Sleep Impairment",
                 "Anger", "Social Isolation",
                 "Emotional Support", "Satisfaction with Social Roles")
      )
      , levels = rev(c("Fatigue", "Sleep Disturbance", "Sleep Impairment",
                   "Depression", "Anxiety", "Anger", 
                   "Social Isolation", "Emotional Support",
                   "Satisfaction with Social Roles"))
    )
  ) %>%
  pivot_longer(cols = contains("Dim"), names_to = "PC", values_to = "Contribution") %>%
  ggplot() +
  aes(x = PC, y = Variable, fill = Contribution) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "gray25", high = "gray75") +
  geom_text(aes(x = PC, y = Variable, label = round(Contribution, 1)), color = "white") +
  labs(
    title = "Variable Contribution to Dimension/Principle Component"
  ) +
  scale_x_discrete(labels = paste("PC", 1:3)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , axis.title = element_blank()
    , legend.position = "bottom"
  )
saveRDS(var_contrib_plot4, here::here("2_output/img/variable_contributions_pcs_1_to_3.rds"))
my_ggsave(var_contrib_plot4, here("2_output/img/variable_contributions_pcs_1_to_3"))
