hc_res <- readRDS(here::here("2_output/models/hc_res.rds"))
# plot the dendrogram
dend_plot <- hc_res %>% 
  as.dendrogram() %>%
  # set("branches_k_color", k = opt_k) %>%
  color_branches(h = 1000, col = c("#DEEBF7", "#3182BD", "#9ECAE1")) %>%
  set("branches_lwd", 2) %>%
  set("labels", NULL) 
saveRDS(dend_plot, here::here("2_output/img/train_dend.rds"))

par(mar = c(0, 0, 2, 0))
plot(dend_plot
     , yaxt="n"
     , main = "Dendrogram of training data set"
     , ylim = c(-160, 2500)
     , mai = rep(0, 4)
     , omi = rep(1, 4)
     , mar = rep(0, 4)
     ) %>%
  text(
    x = c(1300, 8700, 4200)
    , y = rep(-150, 3) #rep(1300, 3)
    , labels = c("Cluster1\nn=2943", "Cluster2\nn=5802", "Cluster3\nn=2703")
  ) 

