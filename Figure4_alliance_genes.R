# This script will plot expression of exacerbation genes:
# only replicated in Alliance cohort
library(tidyr)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)
library(readxl)
library(rstatix)

setwd("~/Work/RP2/ATLANTIS")

# add names as in ATLANTIS
de.results.exac.ATL <- read.csv("./Exacerbations/DE.genes.exac.yes.no.303.csv")

# ALLIANCE data load 
sample_list <- read_excel("./Exacerbations/from_Rui/3.3.Clinical_data_single_expression_wide.xlsx") %>%
  select(1:29) # select columns with genes


alliance_expr_df <- sample_list %>%
  dplyr::select(-c("pseudo_ID", "pseudo.t_timepoint_2", "gender", "smoking",
                   "acq6_score", "ex_last_y", "B_FEV1PNVG", "LABEOSV", "exacerbation", "GINA_mild_severe")) %>%
  tidyr::pivot_longer(-`pseudo.t_timepoint_1`, names_to = "Gene", values_to = "cpm_expression") %>%
  left_join(sample_list %>%
              dplyr::select(c(`pseudo.t_timepoint_1`, exacerbation))) %>%
  mutate(Gene = str_replace(Gene, "_[^_]*$", "")) %>%
  rename("Exacerbators" = "exacerbation") %>%
  left_join(de.results.exac.ATL %>%
              dplyr::select(c(Gene, external_gene_name)), by = c("Gene" = "Gene")) %>%
  mutate(Gene = if_else(is.na(external_gene_name), Gene, external_gene_name)) %>%
  dplyr::select(-c(external_gene_name))


# test genes in Alliance: 

stat.test.alliance.genes <- alliance_expr_df %>%
  group_by(Gene) %>%
  rstatix::t_test(cpm_expression ~ Exacerbators) %>%
  mutate(p_adj = p.adjust(p, method = "fdr")) %>%
  mutate(p_label = rstatix::p_format(p, digits = 2),
         p_label_adj = rstatix::p_format(p_adj, digits = 2)) %>%
  add_y_position(step.increase = 0.15) 

sign_different_genes <- alliance_expr_df %>%
  filter(Gene %in% c(stat.test.alliance.genes[stat.test.alliance.genes$p < 0.05, ]$Gene))

# plot only significant genes 

alliance_violins <- ggplot(sign_different_genes, aes(x = Exacerbators, 
                                                 y = cpm_expression)) +
  geom_violin(aes(fill = Exacerbators), trim = FALSE, alpha = 1) +
  geom_boxplot(alpha = 0) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.4) +  # add individual data points
  facet_wrap(~Gene, scales = "free_y") +
  
  ggpubr::stat_pvalue_manual(
    stat.test.alliance.genes %>%
      filter(p < 0.05),
    label = "p_label") +
  scale_fill_manual(values = c(
    "No" = "#B7E5CD",
    "Yes"     = "#758A93" 
  )) +
  ylab(expression("Normalized gene expression")) +
  xlab(expression("Exacerbators")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 0.6),
        axis.text = element_text(face = "plain", size = 14, colour = 'black'),
        axis.title = element_text(face = "plain", size = 14, colour = 'black'),
        legend.position = "none") 

library(colorBlindness)
cvdPlot(alliance_violins)

saveRDS(alliance_violins, "./Exacerbations/Figure4_Alliance_violin_2_genes.rds")
png("./Exacerbations/Figure4_Alliance_violin_2_genes.png",
    width = 800, 
    height = 800, res = 150)
print(alliance_violins)
dev.off()


###### Plot all genes 

alliance_violins <- ggplot(alliance_expr_df, aes(x = Exacerbators, 
                                                 y = cpm_expression)) +
  geom_violin(aes(fill = Exacerbators), trim = FALSE, alpha = 1) +
  geom_boxplot(alpha = 0) +
  geom_jitter(width = 0.2, size = 0.5, alpha = 0.4) + # add individual data points
  facet_wrap(~Gene, scales = "free") +
  ggpubr::stat_pvalue_manual(
    stat.test.alliance.genes,
    label = "p_label") +
  scale_fill_manual(values = c(
    "No" = "#B7E5CD",
    "Yes"     = "#758A93" 
  )) +
  ylab(expression(bold('Normalized gene expression'))) +
  xlab(expression(bold('Exacerbators'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 0.6),
        axis.text = element_text(face = "plain", size = 14, colour = 'black'),
        axis.title = element_text(face = "plain", size = 14, colour = 'black'),
        legend.position = "bottom") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))


png("./Exacerbations/Supl_Figure4_Alliance_violin_all_genes.png",
    width = 1400, 
    height = 1800, res = 150)
print(alliance_violins)
dev.off()

## Calculate Mean and Median
summary_stats <- alliance_expr_df %>%
  group_by(Gene, Exacerbators) %>%
  summarise(
    Mean_Expression = mean(cpm_expression, na.rm = TRUE),
    Median_Expression = median(cpm_expression, na.rm = TRUE),
    .groups = 'drop' 
  ) %>%
  tidyr::pivot_wider(
    names_from = Exacerbators,
    values_from = c(Mean_Expression, Median_Expression),
    names_prefix = "Group_" 
  ) %>%
  dplyr::mutate(
    Mean_Diff = Mean_Expression_Group_Yes - Mean_Expression_Group_No,
    Median_Diff = Median_Expression_Group_Yes - Median_Expression_Group_No
  )
