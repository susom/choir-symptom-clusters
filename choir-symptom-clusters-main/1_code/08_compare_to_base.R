# comparing clusters to the population which they were derived from
# 
# Author: Eric Cramer <emcramer@stanford.edu>
# 
# Description:
# Comparing the cluster centroids to the centroid of the overall population.
# 

compare_to_base <- function(){
  ###############################################################################################
  # LIBRARIES AND WORKSPACE
  # clear the workspace of everything including hidden files and detach packages not in the base
  rm(list = ls(all.names = TRUE))
  # lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
  # invisible(lapply(paste0('package:', 
  #                         names(sessionInfo()$otherPkgs)), 
  #                  detach, 
  #                  character.only=TRUE, 
  #                  unload=TRUE, 
  #                  force=TRUE))
  
  # Load the required packages
  list_of_packages <- c("tidyverse", "glue", "magrittr", "here", "ggplot2", "xtable", "kableExtra")
  invisible(lapply(list_of_packages, library, character.only = TRUE))
  
  # Set hard-coded variables
  data_dir <- here::here("0_data/raw")
  out_dir <- here::here("2_output/no-pain-predictors")
  temp_dir <- here::here("0_data/temp")
  
  ###############################################################################################
  # LOAD MODEL AND DATA
  chosen_model <- readRDS(glue::glue('{out_dir}/models/hierarchical_model.rds'))
  df <- readRDS(glue::glue("{temp_dir}/large_cohort_data.rds"))
  
  # get the labels for each patient
  labeled_data <- df %>%
    tibble::add_column(cluster = cutree(chosen_model, k=4))
  ###############################################################################################
  # OVERALL COMPARISON
  # Wilk-Shapiro test of normality
  data_vars <- labeled_data %>%
    select_if(is.numeric) %>%
    select(-cluster) %>%
    sample_n(5000)
  sw <- mvnormtest::mshapiro.test(t(data_vars))
  if(sw$p.value < 0.05){
    print("The overall multivariate distribution of the population is NOT normal. Procede with Wilcoxon test.")
  } else{
    print("The overall multivariate distribution of the population IS normal. Procede with Hotelling test.")
  }
  
  population_data <- labeled_data %>%
    select_if(is.numeric) %>%
    select(-cluster)
  
  wilcox_wrapper <- function(v1,v2){
    wt <- wilcox.test(v1,v2)
    return(wt$p.value)
  }

  pvals <- matrix(nrow = ncol(population_data))
  for(i in 1:4){
    curr_pval <- p.adjust(mapply(wilcox_wrapper, 
                        population_data, 
                        select(filter(labeled_data, 
                                      cluster==i),
                               -c(cluster, BODYMAP_REGIONS))), method='BH')
    pvals <- cbind(pvals, curr_pval)
  }
  pvals[,1] <- row.names(pvals)
  row.names(pvals) <- NULL
  pvals <- as.data.frame(pvals)
  pvals[,2:5] <- apply(pvals[2:5], 2, as.numeric)
  colnames(pvals) <- c("Measure",paste("Cluster", 1:4))
  
  # save the table of p-values as csv and tex
  readr::write_excel_csv(pvals, glue::glue('{out_dir}/tables/cluster_to_pop_pvals.csv'))
  print(xtable::xtable(pvals), 
        file=glue::glue('{out_dir}/tables/cluster_to_pop_pvals.txt'),
        type='html',
        booktabs=TRUE)
  
  # save the table as an image for presentation
  # kable(pvals, "latex", booktabs=TRUE) %>%
  #   kable_styling(latex_options = c("striped", "scale_down")) %>%
  #   as_image(file=glue::glue('{out_dir}/img/cluster_to_pop_pvals.png'))
  
  pvals_long <- tidyr::pivot_longer(pvals, cols = -Measure, names_to = "cluster", values_to = "pvalue")

  # plot the p-values in a heatmap
  p1 <- ggplot2::ggplot(data=pvals_long, aes(x=cluster, y=Measure, fill=pvalue)) +
    ggplot2::geom_tile() +
    ggplot2::theme_bw() +
    #ggplot2::theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE) +
    ggplot2::guides(fill=guide_legend(title="p-value")) +
    ggplot2::labs(title="p-values of Difference to Population", x="Cluster", y="Measure")
  p1
  # save the plots
  ggplot2::ggsave(glue::glue("{out_dir}/img/cluster_to_pop_pvals.png"), plot=p1, device="png", dpi="retina")
  ggplot2::ggsave(glue::glue("{out_dir}/img/cluster_to_pop_pvals.svg"), plot=p1, device="svg")
  
}
