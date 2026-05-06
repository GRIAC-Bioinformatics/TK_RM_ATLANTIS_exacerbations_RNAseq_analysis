# This script will combine volcano plot and violin plot 
library(patchwork)

setwd("~/Work/RP2/ATLANTIS")
volcano <- readRDS("./Exacerbations/Exacerbations_DE_volcano.rds")
violin <- readRDS("./Exacerbations/Figure4_Alliance_violin_2_genes.rds")

violin_modified <- (plot_spacer() / violin / plot_spacer()) + 
  plot_layout(heights = c(1, 10, 1))

figure1_combined <- (volcano | violin_modified) +
  plot_layout(widths = c(2, 1)) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.title = element_text(size = 18),  # Adjust title size
        plot.subtitle = element_text(size = 14),
        plot.tag = element_text(face = "bold"))

png("./Exacerbations/Figure1_combined.png",
    width = 1500, 
    height = 900, res = 150)
print(figure1_combined)
dev.off()
