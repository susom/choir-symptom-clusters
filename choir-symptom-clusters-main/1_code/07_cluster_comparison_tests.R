# Characterizing Clusters from the Hierarchical Model
# 
# Author: Eric Cramer <emcramer@stanford.edu>
# 
# Description:
# After characterization, compare the clusters
# 

cluster_comparison_tests <- function(){
  #--------------------------------------------------
  # LIBRARIES AND WORKSPACE
  #--------------------------------------------------
  # clear the workspace of everything including hidden files
  rm(list = ls(all.names = TRUE))
  
  # Load the required packages
  libs <- c("tidyverse", "glue", "magrittr", "here", "ggpubr", "broom")
  suppressPackageStartupMessages(easypackages::libraries(libs))
  
  # source my custom functions
  source(here("1_code/my_ggsave.R"))
  source(here("1_code/my_sderr.R"))
  source(here("1_code/my_simple_cap.R"))
  source(here("1_code/my_empty_plot.R"))
  
  # Set hard-coded variables
  data_dir <- here("0_data")
  out_dir <- here("2_output/no-pain-predictors")
  temp_dir <- here("0_data/temp")
  
  #--------------------------------------------------
  # LOAD MODEL AND DATA
  #--------------------------------------------------
  train_labeled <- read_csv(glue("{out_dir}/tables/train_labeled.csv"))
  test_labeled <- read_csv(glue("{out_dir}/tables/test_labeled.csv"))
  
  #--------------------------------------------------
  # VISUALIZING MEANS AND SPREAD
  #--------------------------------------------------
  # set the variables to compare
  vars <- head(colnames(select_if(train_labeled, is.numeric)), -1)
  
  # set the clusters to compare
  combos <- t(combn(max(as.numeric(train_labeled$cluster)), 2))
  my_comparisons <- lapply(seq_len(nrow(combos)), function(i) combos[i,])
  
  # function for visualizing comparisons for each cluster with p values
  plot_comps <- function(y, df) {
    ys <- extract(df, y)
    p <- ggpubr::ggboxplot(
      train_labeled
      , x = "cluster"
      , y = y
      , color = "cluster"
      , order = 1:max(as.numeric(df[['cluster']]))
      , ylab = y
      , xlab = "Cluster"
      , title = glue("ANOVA of {y}")
    ) +
      grids() +
      stat_compare_means(
        method="wilcox.test"
        , comparisons = my_comparisons
        , p.adjust.method = "BH"
        , mapping = aes(label = ..p.signif..)
        , size = 3
        ) +
      stat_compare_means(
        method="anova"
        , vjust = -7.5
        , size = 3
        )
    return(p)
  }
  
  # TRAINING
  #---------------------
  train_comps <- lapply(vars, plot_comps, df = train_labeled)
  names(train_comps) <- vars
  catcher <- lapply(vars, function(x) {
    my_ggsave(train_comps[[x]], glue("training_comps/{x}"))
  })
  
  # TESTING
  #---------------------
  test_comps <- lapply(vars, plot_comps, df = test_labeled)
  names(test_comps) <- vars
  catcher <- lapply(vars, function(x) {
    my_ggsave(test_comps[[x]], glue("testing_comps/{x}"))
  })
  
  ###############################################################################################
  # ANOVA
  # We can first use a one-way ANOVA to compare the clusters to each other and determine if there 
  # are significant differences
  
  anova_wrapper <- function(y, df) {
    anv <- aov(as.formula(glue("{y} ~ cluster")), data = df)
    return(tidy(anv)[1, ])
  }
  # anova for training
  train_aovs <- lapply(vars, anova_wrapper, df = train_labeled) %>%
    (function(x) do.call(rbind, x)) %>%
    add_column(.before = TRUE, Variable = vars) %>%
    select(Variable, statistic, p.value) %>%
    mutate(padj = p.adjust(p.value, method = "BH"))
  write_csv(train_aovs, glue("{out_dir}/tables/train_anovas.csv"))
  
  # anova for testing
  test_aovs <- lapply(vars, anova_wrapper, df = test_labeled) %>%
    (function(x) do.call(rbind, x)) %>%
    add_column(.before = TRUE, Variable = vars) %>%
    select(Variable, statistic, p.value) %>%
    mutate(padj = p.adjust(p.value, method = "BH"))
  write_csv(test_aovs, glue("{out_dir}/tables/test_anovas.csv"))
}

# # ANOVA assumes that the population each sample group is drawn from is homoscedastic, and 
# # follows a normal distribution. We can check this visually with a plot of the fitted values to 
# # the residuals of the ANOVA.
# par(mfrow=c(2,2))
# svg(glue::glue('{out_dir}/img/anova_plots.svg'))
# plot(anova)
# dev.off()
# png(glue::glue('{out_dir}/img/anova_plots.png'), width=1080, height=1080, units='px')
# plot(anova)
# dev.off()
# par(mfrow=c(1,1))
# 
# # From the output above we can see that the p-value is less than the significance level of 0.05. This means there is 
# # evidence the variance across groups is statistically significantly different. Therefore, we cannot 
# # assume the homogeneity of variances in the different treatment groups. Thus, the population variances are not 
# # homoscedastic (https://en.wikipedia.org/wiki/Homoscedasticity). 
# # This undercuts findings by anova and t-tests b/c they assume a homoscedastic population.
# 
# ###############################################################################################
# # PAIRWISE COMPARISONS WITH TUKEY
# # If we proceed despite heteroscedacity in the dataset, we can compute the Tukey Honest Significant Differences. this gives
# # a pairwise comparison of each cluster to another. The differences between one cluster to another are given, along with 
# # the upper and lower limits of their 95% CIs, and the associated p-value.
# tukey <- TukeyHSD(anova)
# tukey
# capture.output(tukey, file = glue::glue('{out_dir}/txt/tukeyhsd_summary.txt'))
# 
# svg(glue::glue('{out_dir}/img/tukeyhsd_base.svg'))
# plot(tukey)
# dev.off()
# png(glue::glue('{out_dir}/img/tukeyhsd_base.png'), width=1080, height=1080, units='px')
# plot(tukey)
# dev.off()
# # based on the TukeyHSD results, each cluster is significantly different from the other, with an adjusted p-value < 0.05.
# 
# # visualize the tukey hsd
# tukey_data <- as_tibble(tukey[[1]])
# tukey_rows <- row.names(tukey[[1]])
# p3 <- ggplot(data = tukey_data) +
#   geom_pointrange(stat='identity', aes(x=factor(1:nrow(tukey_data)), diff, ymin=lwr, ymax=upr), color="black") + 
#   geom_point(stat='identity', aes(x=factor(1:nrow(tukey_data)), lwr), color="red", size=1) +
#   geom_point(stat='identity', aes(x=factor(1:nrow(tukey_data)), upr), color="blue", size=1) +
#   scale_x_discrete(name="Clusters Compared", breaks=1:nrow(tukey_data), labels=tukey_rows) +
#   labs(title="Tukey HSD", y="Difference Interval")
# p3
# # save the plots
# ggplot2::ggsave(glue::glue("{out_dir}/img/cluster_pi_tukeyhsd.png"), plot=p3, device="png", dpi="retina")
# ggplot2::ggsave(glue::glue("{out_dir}/img/cluster_pi_tukeyhsd.svg"), plot=p3, device="svg")
# 
# # visualizing the Tukey plot as a bar plot
# tukey_data$comps <- factor(tukey_rows)
# p4 <- ggplot2::ggplot(data=tukey_data, aes(x=comps, y=diff)) +
#   ggplot2::geom_bar(stat='identity', color='black') +
#   ggplot2::geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2) +
#   labs(x="Comparisons", y="Difference", title="Tukey Honestly Significant Differences") +
#   theme_bw()
# # now add the * to indicate significance
# signif_df <- data.frame(comps=1:6, diff=c(tukey_data$upr[1]+1,
#                                           tukey_data$lwr[2]-1,
#                                           tukey_data$lwr[4]-1,
#                                           tukey_data$upr[3]+1,
#                                           tukey_data$upr[5]+1,
#                                           tukey_data$upr[6]+1))
# p4 <- p4 + ggplot2::geom_text(data = signif_df, label="*", size=10)
# p4
# # save the plots
# ggplot2::ggsave(glue::glue("{out_dir}/img/tukeyhsd_bar.png"), plot=p4, device="png", dpi="retina")
# ggplot2::ggsave(glue::glue("{out_dir}/img/tukeyhsd_bar.svg"), plot=p4, device="svg")
# 
# ###############################################################################################
# # KRUSKAL-WALLIS
# # Since the underlying data is heteroscedastic, we may opt to use a Wilcoxon-based test which does not assume 
# # homoscedasticity.
# kw <- kruskal.test(PROMIS_PAIN_INTERFERENCE ~ cluster, data = labeled_data)
# kw
# # Since the p-value is less than 0.05, we can conclude that there are significant differences between each of the 
# # groups/clusters (despite the appearance of heteroscedacity). 
# 
# # We can do a pairwise comparison for each of the clusters to quantify one-to-one cluster comparisons. Again, we'll
# # use the Bnjamini-Hochberg correction.
# pkw <- pairwise.wilcox.test(labeled_data$PROMIS_PAIN_INTERFERENCE, 
#                             labeled_data$cluster, 
#                             data = labeled_data, 
#                             p.adjust.method = 'BH')
# capture.output(pkw, file=glue::glue('{out_dir}/txt/pairwise_kruskal_wallis_test.txt'))
# # The pairwise comparisons show that each of the clusters exhibits significantly different physical function levels
# # (as measured by the PROMIS) from each other. We can then move on to quantifying the effect size via Kruskal-Wallis
# # statistic.