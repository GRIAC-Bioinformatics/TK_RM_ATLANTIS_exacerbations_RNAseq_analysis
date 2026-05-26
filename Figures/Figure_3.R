# This script will plot cumuulattive addition of the predictors vs sensitiity for ATLANTIS and ALLIANCe
library(dplyr)
library(ggplot2)
library(patchwork)
library(here)
source(here("config.R"))
knitr::opts_knit$set(root.dir = ROOT_DIR)


incremental_table_ATL <- read.csv(OUTPUTS$incremental_table)
incremental_table_ALLIANCE <- read.csv(OUTPUTS$alliance_incremental_table)


## Plot sensitivity by cumulative covariate addition ATLANTIS

model_perf <- incremental_table_ATL %>%
  dplyr::select(Model, Sensitivity, Sensitivity_CI_low, Sensitivity_CI_high) %>%
  mutate(
    Model               = factor(Model, levels = Model),
    Sensitivity         = Sensitivity * 100,
    Sensitivity_CI_low  = Sensitivity_CI_low * 100,
    Sensitivity_CI_high = Sensitivity_CI_high * 100
  )

custom_labels <- c(
  "Sex + Smoking", "+ ACQ-6",
  expression("+ FEV"[1] * " % pred"),
  "+ Exacerbation history", "+ Blood eosinophils", "+ GINA", "+ ssGSEA"
)

p_ATL <- ggplot(model_perf, aes(x = Model, y = Sensitivity, group = 1))  +
  geom_ribbon(
    aes(ymin = Sensitivity_CI_low, ymax = Sensitivity_CI_high),
    fill  = "#2c7fb8",
    alpha = 0.2
  ) +
  geom_line(
    colour    = "#2c7fb8",
    linewidth = 1
  ) +
  geom_point(
    colour = "#2c7fb8",
    size   = 3
  ) +
  geom_text(
    aes(label = paste0(Sensitivity, "%")),
    vjust = -1, size = 4
  ) +
  labs(
    title = "ATLANTIS",
    x     = "Model (Cumulative Addition of Predictors)",
    y     = "Sensitivity (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = custom_labels) +
  ylim(0, 70)

## Plot sensitivity by cumulative covariate addition ALLIANCE

model_perf_ALLIANCE <- incremental_table_ALLIANCE %>%
  dplyr::select(Model, Sensitivity, Sensitivity_CI_low, Sensitivity_CI_high) %>%
  mutate(
    Model               = factor(Model, levels = Model),
    Sensitivity         = Sensitivity * 100,
    Sensitivity_CI_low  = Sensitivity_CI_low * 100,
    Sensitivity_CI_high = Sensitivity_CI_high * 100
  )

custom_labels <- c(
  "Sex + Smoking", "+ ACQ-6",
  expression("+ FEV"[1] * " % pred"),
  "+ Exacerbation history", "+ Blood eosinophils", "+ GINA", "+ ssGSEA"
)

p_ALLIANCE <- ggplot(model_perf_ALLIANCE, aes(x = Model, y = Sensitivity, group = 1))  +
  geom_ribbon(
    aes(ymin = Sensitivity_CI_low, ymax = Sensitivity_CI_high),
    fill  = "#2c7fb8",
    alpha = 0.2
  ) +
  geom_line(
    colour    = "#2c7fb8",
    linewidth = 1
  ) +
  geom_point(
    colour = "#2c7fb8",
    size   = 3
  ) +
  geom_text(
    aes(label = paste0(Sensitivity, "%")),
    vjust = -1, size = 4
  ) +
  labs(
    title = "ALLIANCE",
    x     = "Model (Cumulative Addition of Predictors)",
    y     = "Sensitivity (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = custom_labels) +
  ylim(0, 70)

# Combine with A/B labels
p_combined <- p_ATL + p_ALLIANCE +
  plot_layout(axes = "collect") +
  plot_annotation(tag_levels = "A")

png(OUTPUTS$combined_plt_incr_models, width = 1200, height = 600, res = 150)
print(p_combined)
dev.off()
