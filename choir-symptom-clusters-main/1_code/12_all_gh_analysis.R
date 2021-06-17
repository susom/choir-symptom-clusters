# libraries
library(tidyverse)
library(ggplot2)
library(readxl)

# load data
gh_cluster_assignments <- read_excel("0_data/clean/ghMental_for_Eric.xlsx", sheet = 1) %>%
  dplyr::rename(gh_cluster = my_cluster, MRN2 = MRN) %>%
  arrange(MRN2)
train_labeled <- read_csv("2_output/tables/train_labeled.csv") %>%
  arrange(MRN)
pca_res <- readRDS(here::here("2_output/models/pca.rds"))

combination_df <- cbind(train_labeled, gh_cluster_assignments) %>%
  select(MRN, cluster, gh_cluster, gh_mental)

crosstbl <- table(
  "clustering" = combination_df$cluster
  , "GH" = combination_df$gh_cluster
) %>%
  as.data.frame()

# plotting
cross_p <- crosstbl %>%
  ggplot() +
  geom_tile(aes(x = clustering, y = GH, fill = Freq), color = "white") +
  geom_text(aes(x = clustering, y = GH, label = Freq), color = "white") + 
  labs(
    title = "Congruence between GH Mental tertiles\n and Symptoms-based Clusters"
    , x = "Symptom-based Clusters"
    , y = "GH Mental"
    , fill = "Count"
  ) +
  theme_minimal()
saveRDS(cross_p, here::here("2_output/img/congruence_crosstbl.rds"))

pca_coords <- gh_cluster_assignments %>%
  rename(Cluster = 2) %>%
  select(Dim.1, Dim.2, gh_mental, Cluster, gh_cluster)

pca_plot <- pca_coords %>%
               mutate(Cluster = paste0("Cluster", Cluster)) %>%
               ggplot() +
               geom_point(
                 aes(
                   x = Dim.1
                   , y = gh_mental
                   , color = Cluster
                 )
                 , alpha = 0.8
                 , shape = 16 
               ) + 
               geom_smooth(
                 aes(
                   y = gh_mental
                   , x = Dim.1
                 )
                 , method = "lm"
                 , color = "black"
               ) +
               labs(
                 title = "Relationship between GH Mental and\n PC 1 of Clustering Symptoms"
                 , x = "Principal Component 1"
                 , y = "GH Mental T Score"
                 , color = "") +
               scale_color_brewer(palette = "Blues") +
               theme_minimal()
saveRDS(pca_plot, here::here("2_output/img/gh_vs_pc1.rds"))
