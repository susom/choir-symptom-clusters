# Set hard-coded variables
data_dir <- here("0_data")
out_dir <- here("2_output/no-pain-predictors")
temp_dir <- here("0_data/temp")

full_labeled <- read_csv(glue("{out_dir}/tables/full_labeled.csv"))

test_labeled <- read_csv(glue("{out_dir}/tables/test_labeled.csv"))
train_labeled <- read_csv(glue("{out_dir}/tables/train_labeled.csv"))


# CHARACTERIZING REGIONS OF BODY by CLUSTER
# get the number of people in each cluster
clust_nums <- table(train_labeled$cluster)

# split data into clusters
cluster_list <- list()
for(i in 1:max(train_labeled$cluster)){
  cluster_list[[i]] <- train_labeled %>%
    filter(cluster==i) %>%
    select(BODYMAP_REGIONS)
}

my_tabulate <- function(x) {
  # split each individual's list of regions
  # unlist to get master list of all regions in each cluster
  # tally within each cluster
  table(unlist(lapply(x, strsplit, split="\\|")))
}

cluster_regions_tabbed <- lapply(cluster_list, my_tabulate)
tabbed_regions <- as.data.frame(do.call(cbind, cluster_regions_tabbed))
tabbed_regions$region <- row.names(tabbed_regions)
row.names(tabbed_regions) <- NULL
colnames(tabbed_regions) <- c(paste("Cluster", 1:max(train_labeled$cluster)), "region")
write_csv(tabbed_regions, glue("{out_dir}/tables/train_bm_regions_cnt.csv")) # writeout raw #s

# writeout scaled values
tabbed_regions_scaled <- tabbed_regions
tabbed_regions_scaled[, 1:max(train_labeled$cluster)] <- apply(
  tabbed_regions[,1:max(train_labeled$cluster)]
  , 2
  , function(x) round(100*x/max(x), 2))
write_csv(tabbed_regions_scaled
          , glue("{out_dir}/tables/train_bm_regions_perc.csv")) 

#----------
# testing
# split data into clusters
cluster_list <- list()
for(i in 1:max(test_labeled$cluster)){
  cluster_list[[i]] <- test_labeled %>%
    filter(cluster==i) %>%
    select(BODYMAP_REGIONS)
}
cluster_regions_tabbed <- lapply(cluster_list, my_tabulate)
tabbed_regions <- as.data.frame(do.call(cbind, cluster_regions_tabbed))
tabbed_regions$region <- row.names(tabbed_regions)
row.names(tabbed_regions) <- NULL
colnames(tabbed_regions) <- c(paste("Cluster", 1:max(train_labeled$cluster)), "region")
write_csv(tabbed_regions, glue("{out_dir}/tables/test_bm_regions_cnt.csv")) # writeout raw #s

# writeout scaled values
tabbed_regions_scaled <- tabbed_regions
tabbed_regions_scaled[, 1:max(train_labeled$cluster)] <- apply(
  tabbed_regions[,1:max(train_labeled$cluster)]
  , 2
  , function(x) round(100*x/max(x), 2))
write_csv(tabbed_regions_scaled
          , glue("{out_dir}/tables/test_bm_regions_perc.csv")) 
