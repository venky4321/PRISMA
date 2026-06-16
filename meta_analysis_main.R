# =============================================================================
# Meta-Analysis: Quantum Machine Learning vs Classical Deep Learning
# for Dermatological Image Analysis
#
# Paper: "Quantum Machine Learning for Dermatological Image Analysis:
#         A PRISMA-Compliant Systematic Review, Random-Effects Meta-Analysis,
#         and Staged Roadmap"
#
# Authors: Mekala Srinivasa Rao, Venkatesh Koreddi, Siva Ramakrishna Sani,
#          Kshatriya Vinaya Sree Bai
#
# R Version: 4.3+
# Required packages: meta, metafor, ggplot2, dplyr, readr, gridExtra, scales
#
# Usage:
#   source("meta_analysis_main.R")
#   All outputs (plots, tables) saved to ./outputs/
# =============================================================================

# ── 0. Setup ──────────────────────────────────────────────────────────────────

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(meta, metafor, ggplot2, dplyr, readr, gridExtra,
               scales, tibble, knitr, patchwork)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

set.seed(2024)   # reproducibility

# ── 1. Load Data ──────────────────────────────────────────────────────────────

df <- read_csv("prisma_extraction_database.csv", show_col_types = FALSE)

cat("=== Dataset Overview ===\n")
cat("Total studies loaded:", nrow(df), "\n")
cat("Studies by hardware type:\n")
print(table(df$hardware_type))
cat("\nStudies by dataset:\n")
print(table(df$dataset))

# Derived variables
df <- df %>%
  mutate(
    # Standard error approximation: SE = sqrt(acc*(1-acc)/n_test)
    # n_test inferred from dataset_size * test_split
    n_test = round(dataset_size * test_split),
    # SE for the difference (conservative: assume independence)
    se_qml       = sqrt((qml_accuracy/100) * (1 - qml_accuracy/100) / n_test),
    se_classical = sqrt((classical_accuracy/100) * (1 - classical_accuracy/100) / n_test),
    se_delta     = sqrt(se_qml^2 + se_classical^2) * 100,   # back to pp
    # Subgroup flags
    is_simulator   = hardware_type == "Simulator",
    is_real_hw     = hardware_type == "Real",
    is_binary      = n_classes == 2,
    is_multiclass  = n_classes > 2,
    is_ham10k      = dataset == "HAM10000",
    is_weak_base   = classical_baseline %in% c("ResNet-18","ResNet-50","ResNet-101",
                                                "DenseNet-121","InceptionV3",
                                                "Logistic Regression","SVM-RBF",
                                                "SVM-Linear","Classical SVM"),
    is_modern_base = !is_weak_base,
    qubit_group    = ifelse(n_qubits <= 8, "<=8 qubits", ">8 qubits"),
    # Hedges' g style label for forest plot
    study_label    = paste0(first_author, " (", year, ") — ", dataset)
  )

# ── 2. Primary Random-Effects Meta-Analysis ───────────────────────────────────

cat("\n\n=== PRIMARY META-ANALYSIS: All 48 Studies ===\n")

ma_all <- metagen(
  TE      = df$delta_accuracy,
  seTE    = df$se_delta,
  studlab = df$study_label,
  data    = df,
  sm      = "MD",
  fixed   = FALSE,
  random  = TRUE,
  method.tau = "DL",         # DerSimonian-Laird
  hakn   = TRUE,             # Hartung-Knapp-Sidik-Jonkman correction
  title  = "QML vs Classical: All Studies"
)

print(summary(ma_all))

cat(sprintf(
  "\nPooled ∆ = %.2f%% (95%% CI: %.2f%% to %.2f%%), p = %.3f\n",
  ma_all$TE.random,
  ma_all$lower.random,
  ma_all$upper.random,
  ma_all$pval.random
))
cat(sprintf("I² = %.1f%%, τ² = %.3f, Q-test p = %.4f\n",
            ma_all$I2 * 100, ma_all$tau^2, ma_all$pval.Q))

# ── 3. Subgroup Analyses ──────────────────────────────────────────────────────

run_subgroup <- function(data, label) {
  m <- metagen(
    TE      = data$delta_accuracy,
    seTE    = data$se_delta,
    studlab = data$study_label,
    sm      = "MD",
    fixed   = FALSE,
    random  = TRUE,
    method.tau = "DL",
    hakn   = TRUE,
    title  = label
  )
  tibble(
    Subgroup          = label,
    k                 = nrow(data),
    Delta_pct         = round(m$TE.random, 2),
    CI_lower          = round(m$lower.random, 2),
    CI_upper          = round(m$upper.random, 2),
    p_value           = round(m$pval.random, 3),
    I2_pct            = round(m$I2 * 100, 1),
    tau2              = round(m$tau^2, 3)
  )
}

subgroups <- bind_rows(
  run_subgroup(df,                                 "All studies"),
  run_subgroup(filter(df, is_simulator),           "Simulator only"),
  run_subgroup(filter(df, is_real_hw),             "Real hardware only"),
  run_subgroup(filter(df, is_weak_base),           "Weak baseline (≤2020 CNN/SVM)"),
  run_subgroup(filter(df, is_modern_base),         "Modern baseline (≥EfficientNet-B4)"),
  run_subgroup(filter(df, is_binary),              "Binary classification"),
  run_subgroup(filter(df, is_multiclass),          "Multi-class (≥7 classes)"),
  run_subgroup(filter(df, is_ham10k),              "HAM10000 only"),
  run_subgroup(filter(df, qubit_group == "<=8 qubits"), "≤8 qubits"),
  run_subgroup(filter(df, qubit_group == ">8 qubits"),  ">8 qubits")
)

cat("\n\n=== SUBGROUP META-ANALYSIS RESULTS ===\n")
print(subgroups, n = Inf)

write_csv(subgroups, "outputs/tables/Table_S1_subgroup_results.csv")
cat("\nSubgroup table saved → outputs/tables/Table_S1_subgroup_results.csv\n")

# ── 4. Publication Bias: Egger's Test & Funnel Plot ──────────────────────────

cat("\n\n=== PUBLICATION BIAS ===\n")

# Egger's test via metafor
res_metafor <- rma(
  yi   = df$delta_accuracy,
  sei  = df$se_delta,
  method = "DL"
)

egger <- regtest(res_metafor, model = "lm")
cat("Egger's test: z =", round(egger$zval, 3),
    ", p =", round(egger$pval, 4), "\n")

# Trim-and-fill
tf <- trimfill(res_metafor)
cat("Trim-and-fill adjusted estimate:",
    round(tf$b, 3), "(", round(tf$ci.lb, 3), ",", round(tf$ci.ub, 3), ")\n")
cat("Number of imputed studies:", tf$k0, "\n")

# ── 5. Funnel Plot ────────────────────────────────────────────────────────────

png("outputs/figures/Fig_S1_funnel_plot.png", width = 1800, height = 1400, res = 200)

funnel_data <- df %>%
  mutate(
    precision = 1 / se_delta,
    hw_label  = ifelse(is_real_hw, "Real Hardware", "Simulator")
  )

p_funnel <- ggplot(funnel_data, aes(x = delta_accuracy, y = precision,
                                     shape = hw_label, colour = hw_label)) +
  geom_vline(xintercept = ma_all$TE.random, linetype = "dashed",
             colour = "grey40", linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "solid",
             colour = "black", linewidth = 0.4) +
  geom_point(size = 3, alpha = 0.85) +
  scale_shape_manual(values = c("Real Hardware" = 15, "Simulator" = 1)) +
  scale_colour_manual(values = c("Real Hardware" = "#d62728", "Simulator" = "#1f77b4")) +
  scale_y_continuous(name = "Precision (1/SE, inverted)",
                     trans  = "reverse",
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_x_continuous(name = "Accuracy Difference Δ (QML − Classical, %)",
                     breaks = seq(-20, 20, 5)) +
  labs(
    title    = "Funnel Plot — QML vs Classical Accuracy Difference (n = 48 studies)",
    subtitle = sprintf("Pooled ∆ = %.2f%% (dashed line) | Egger p = %.3f",
                       ma_all$TE.random, egger$pval),
    shape    = "Hardware", colour = "Hardware"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_funnel)
dev.off()
cat("Funnel plot saved → outputs/figures/Fig_S1_funnel_plot.png\n")

# ── 6. Forest Plot (All Studies) ──────────────────────────────────────────────

png("outputs/figures/Fig_S2_forest_plot_all.png", width = 2400, height = 3600, res = 200)
forest(ma_all,
       sortvar    = df$delta_accuracy,
       xlim       = c(-25, 25),
       xlab       = "Accuracy Difference (QML − Classical, %)",
       smlab      = "∆ Accuracy (%)",
       col.diamond = "#2171b5",
       col.square  = "grey40",
       leftcols    = c("studlab", "n_classes", "hardware_type"),
       leftlabs    = c("Study", "Classes", "Hardware"),
       rightcols   = c("effect", "ci"),
       rightlabs   = c("∆ (%)", "95% CI"),
       print.I2    = TRUE,
       print.tau2  = TRUE,
       print.pval.Q = TRUE,
       cex         = 0.65)
dev.off()
cat("Forest plot saved → outputs/figures/Fig_S2_forest_plot_all.png\n")

# ── 7. Forest Plot — Real HW vs Simulator Comparison ─────────────────────────

ma_sim <- metagen(
  TE = filter(df, is_simulator)$delta_accuracy,
  seTE = filter(df, is_simulator)$se_delta,
  studlab = filter(df, is_simulator)$study_label,
  sm = "MD", fixed = FALSE, random = TRUE, method.tau = "DL"
)

ma_real <- metagen(
  TE = filter(df, is_real_hw)$delta_accuracy,
  seTE = filter(df, is_real_hw)$se_delta,
  studlab = filter(df, is_real_hw)$study_label,
  sm = "MD", fixed = FALSE, random = TRUE, method.tau = "DL"
)

png("outputs/figures/Fig_S3_forest_plot_hardware_comparison.png",
    width = 2400, height = 2000, res = 200)
par(mfrow = c(1, 2))
forest(ma_sim,
       xlim = c(-20, 22),
       xlab = "∆ Accuracy (%)",
       main = sprintf("Simulator Studies (n=%d)\nPooled ∆ = %.2f%%, p=%.3f",
                      nrow(filter(df, is_simulator)),
                      ma_sim$TE.random, ma_sim$pval.random),
       col.diamond = "#2171b5", cex = 0.7)
forest(ma_real,
       xlim = c(-20, 5),
       xlab = "∆ Accuracy (%)",
       main = sprintf("Real Hardware Studies (n=%d)\nPooled ∆ = %.2f%%, p=%.3f",
                      nrow(filter(df, is_real_hw)),
                      ma_real$TE.random, ma_real$pval.random),
       col.diamond = "#d62728", cex = 0.85)
dev.off()
cat("Hardware comparison forest plot saved → outputs/figures/Fig_S3_forest_plot_hardware_comparison.png\n")

# ── 8. Accuracy Gap Bar Chart (Figure 6 equivalent) ──────────────────────────

gap_data <- tibble(
  model = c(
    "Best QML\n(Real HW, IBM Eagle 2024)",
    "Best QML\n(Simulator only)",
    "EfficientNet-B5\n(classical CNN)",
    "Swin-Large\nTransformer",
    "Swin-L + Diffusion\nAugmentation",
    "SkinGPT\nFoundation Model",
    "Ensemble\nViT+Swin+ConvNeXt"
  ),
  accuracy = c(83.7, 89.4, 92.0, 95.8, 96.3, 97.2, 97.6),
  category = c("QML-Real", "QML-Sim", "Classical", "Classical",
               "Classical", "Classical", "Classical")
) %>%
  arrange(accuracy) %>%
  mutate(model = factor(model, levels = model))

p_gap <- ggplot(gap_data, aes(x = accuracy, y = model, fill = category)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%", accuracy)),
            hjust = -0.15, size = 3.5, fontface = "bold") +
  geom_vline(xintercept = 97.6, linetype = "dashed", colour = "grey30", linewidth = 0.6) +
  scale_fill_manual(
    values = c("QML-Real" = "#d62728", "QML-Sim" = "#ff7f0e", "Classical" = "#2171b5"),
    labels = c("QML (Real Hardware)", "QML (Simulator)", "Classical Deep Learning")
  ) +
  scale_x_continuous(limits = c(60, 101), breaks = seq(60, 100, 5),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "HAM10000 Accuracy Gap: QML vs Classical Deep Learning",
    subtitle = "7-class top-1 accuracy | Gap between best real-HW QML and best classical = 13.9 pp",
    x        = "Top-1 Accuracy on HAM10000 (%)",
    y        = NULL,
    fill     = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave("outputs/figures/Fig_S4_accuracy_gap.png", p_gap,
       width = 10, height = 6, dpi = 200)
cat("Accuracy gap chart saved → outputs/figures/Fig_S4_accuracy_gap.png\n")

# ── 9. Publication Trend Plot ─────────────────────────────────────────────────

pub_trend <- tibble(
  year          = 2019:2025,
  all_qml       = c(3,  8, 18, 31, 47, 67, 48),    # 2025 partial
  real_hw       = c(0,  0,  1,  1,  2,  2,  0),
  hybrid_qc     = c(1,  3,  7, 12, 18, 28, 18)
)

p_trend <- pub_trend %>%
  tidyr::pivot_longer(-year, names_to = "type", values_to = "count") %>%
  mutate(type = recode(type,
    all_qml  = "All QML dermatology papers",
    real_hw  = "Real quantum hardware",
    hybrid_qc = "Hybrid QC-classical"
  )) %>%
  ggplot(aes(x = year, y = count, colour = type, linetype = type)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  annotate("text", x = 2025.1, y = 51, label = "2025 partial\n(to Mar 2026)",
           size = 3, hjust = 0, colour = "grey40") +
  scale_colour_manual(values = c("#1f77b4", "#d62728", "#2ca02c")) +
  scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
  scale_x_continuous(breaks = 2019:2025) +
  labs(
    title    = "Annual Publication Count: QML Dermatology Papers (2019–2025)",
    subtitle = "Journal articles only | Total QML dermatology: 222 within 650 included papers",
    x        = "Year", y = "Number of Publications",
    colour   = NULL, linetype = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = c(0.25, 0.78),
    legend.background = element_rect(fill = "white", colour = "grey80"),
    plot.title        = element_text(face = "bold"),
    panel.grid.minor  = element_blank()
  )

ggsave("outputs/figures/Fig_S5_publication_trend.png", p_trend,
       width = 10, height = 6, dpi = 200)
cat("Publication trend saved → outputs/figures/Fig_S5_publication_trend.png\n")

# ── 10. Sensitivity Analysis: Leave-One-Out ───────────────────────────────────

cat("\n\n=== LEAVE-ONE-OUT SENSITIVITY ANALYSIS ===\n")

loo_results <- tibble(
  study_removed = character(),
  pooled_delta  = numeric(),
  ci_lower      = numeric(),
  ci_upper      = numeric(),
  p_value       = numeric()
)

for (i in seq_len(nrow(df))) {
  df_loo <- df[-i, ]
  m_loo  <- metagen(
    TE     = df_loo$delta_accuracy,
    seTE   = df_loo$se_delta,
    sm     = "MD", fixed = FALSE, random = TRUE, method.tau = "DL"
  )
  loo_results <- add_row(loo_results,
    study_removed = df$study_label[i],
    pooled_delta  = round(m_loo$TE.random, 3),
    ci_lower      = round(m_loo$lower.random, 3),
    ci_upper      = round(m_loo$upper.random, 3),
    p_value       = round(m_loo$pval.random, 4)
  )
}

cat("LOO range: ∆ from",
    round(min(loo_results$pooled_delta), 2), "to",
    round(max(loo_results$pooled_delta), 2), "%\n")
cat("All LOO estimates non-significant (p > 0.05):",
    all(loo_results$p_value > 0.05), "\n")

write_csv(loo_results, "outputs/tables/Table_S2_leave_one_out.csv")
cat("LOO table saved → outputs/tables/Table_S2_leave_one_out.csv\n")

# ── 11. PROBAST Risk of Bias Summary ─────────────────────────────────────────

rob_summary <- df %>%
  count(probast_analysis, probast_participant, overall_risk) %>%
  arrange(overall_risk)

cat("\n\n=== PROBAST RISK OF BIAS SUMMARY ===\n")
print(rob_summary)

rob_plot_data <- df %>%
  summarise(
    across(c(probast_participant, probast_predictor,
             probast_outcome, probast_analysis),
           ~ list(table(.)))
  ) %>%
  tidyr::pivot_longer(everything(), names_to = "domain", values_to = "counts") %>%
  mutate(domain = recode(domain,
    probast_participant = "Participants",
    probast_predictor   = "Predictors",
    probast_outcome     = "Outcome",
    probast_analysis    = "Analysis"
  ))

write_csv(
  df %>% select(study_label, starts_with("probast"), overall_risk),
  "outputs/tables/Table_S3_probast_risk_of_bias.csv"
)
cat("PROBAST table saved → outputs/tables/Table_S3_probast_risk_of_bias.csv\n")

# ── 12. Supplementary Table S4: Full Extraction Data ─────────────────────────

supp_table <- df %>%
  select(
    study_id, first_author, year, journal, dataset,
    qml_method, n_qubits, hardware_type, hardware_platform,
    classical_baseline, qml_accuracy, classical_accuracy, delta_accuracy,
    auroc_qml, auroc_classical, overall_risk, code_available
  ) %>%
  arrange(year, first_author)

write_csv(supp_table, "outputs/tables/Table_S4_full_extraction.csv")
cat("Full extraction table saved → outputs/tables/Table_S4_full_extraction.csv\n")

# ── 13. Session Info ──────────────────────────────────────────────────────────

cat("\n\n=== SESSION INFO ===\n")
sink("outputs/session_info.txt")
sessionInfo()
sink()
cat("Session info saved → outputs/session_info.txt\n")

cat("\n✓ All analyses complete.\n")
cat("  Outputs in: outputs/figures/ and outputs/tables/\n")
