# config_template.R
# Copy this file to config.R and fill in your local paths
# config.R is gitignored and should never be committed

# ── Root ──────────────────────────────────────────────────────────────────────
ROOT_DIR <- "YOUR/PATH/TO/ATLANTIS"

# ── Input files ───────────────────────────────────────────────────────────────
PATHS <- list(
  metadata   = file.path(ROOT_DIR, "Exacerbations/ATLANTIS_master_table_exacerbation.csv"),
  expression = file.path(ROOT_DIR, "Umi_dedup/20201107_ATLANTIS_raw_readcount_dedup_FINAL.csv"),
  clinical   = file.path(ROOT_DIR, "atlantis_patient_data.csv")
)

# ── Output files ──────────────────────────────────────────────────────────────
OUTPUTS <- list(
  volcano_rds = file.path(ROOT_DIR, "Exacerbations/Exacerbations_DE_volcano.rds"),
  volcano_png = file.path(ROOT_DIR, "Exacerbations/Exacerbations_DE_volcano.png")
)
