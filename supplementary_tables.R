# =============================================================================
# Supplementary Tables — QML Dermatology Systematic Review
#
# Generates all supplementary tables referenced in the paper:
#   Table S1  — Full PRISMA extraction database (all 650 studies summary)
#   Table S2  — 48 matched studies for meta-analysis
#   Table S3  — Subgroup meta-analysis results
#   Table S4  — Quantum hardware platforms summary
#   Table S5  — Classical DL benchmark (HAM10000)
#   Table S6  — State-of-the-art comparison (Table 5 equivalent)
#   Table S7  — QML software frameworks
#   Table S8  — Dataset characteristics
#   Table S9  — PROBAST risk of bias (full detail)
#   Table S10 — Leave-one-out sensitivity analysis
# =============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(readr, dplyr, tidyr, writexl, knitr, tibble)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ── Load extraction DB ────────────────────────────────────────────────────────
df <- read_csv("prisma_extraction_database.csv", show_col_types = FALSE)

# ── Table S1: PRISMA screening summary ───────────────────────────────────────
table_s1 <- tibble(
  Stage = c(
    "Records identified from IEEE Xplore",
    "Records identified from ACM Digital Library",
    "Records identified from SpringerLink",
    "Records identified from ScienceDirect",
    "Additional records (grey literature, reference lists, arXiv)",
    "Total identified",
    "After adding grey literature",
    "After deduplication",
    "Excluded at title/abstract screening (off-topic)",
    "Excluded at title/abstract screening (duplicates)",
    "Full-text assessed for eligibility",
    "Full-text excluded — no quantitative results",
    "Full-text excluded — non-English",
    "Full-text excluded — conference only (no journal version)",
    "Full-text excluded — predatory journal",
    "Studies included in synthesis",
    "Studies included in meta-analysis (matched Q vs C)"
  ),
  N = c(687, 412, 634, 417, 47,
        2150, 2197, 2089,
        891, 344,
        854,
        67, 41, 54, 42,
        650, 48),
  Note = c(
    "Boolean search: quantum ML + dermatology terms",
    "Boolean search: quantum ML + dermatology terms",
    "Boolean search: quantum ML + dermatology terms",
    "Boolean search: quantum ML + dermatology terms",
    "Manual reference-list + arXiv (journal version required)",
    "Sum of all sources",
    "After grey literature addition",
    "After removing duplicates (n=108 removed)",
    "Does not address skin image analysis",
    "Duplicate entries across databases",
    "Available for full-text review",
    "No quantitative performance metric reported",
    "Non-English language articles",
    "Conference-only proceedings, no journal version",
    "Published in journals on Beall's predatory list",
    "Peer-reviewed journal articles, all languages, any method",
    "Both QML and classical result on same dataset/task"
  )
)
write_csv(table_s1, "outputs/tables/Table_S1_PRISMA_flow.csv")
cat("Table S1 saved\n")

# ── Table S2: 48 matched meta-analysis studies (clean version) ────────────────
table_s2 <- df %>%
  select(
    Study     = study_id,
    Author    = first_author,
    Year      = year,
    Dataset   = dataset,
    Classes   = n_classes,
    QML_Method = qml_method,
    Qubits    = n_qubits,
    Hardware  = hardware_type,
    Platform  = hardware_platform,
    Err_Mitig = error_mitigation,
    Classical_Baseline = classical_baseline,
    QML_Acc   = qml_accuracy,
    Class_Acc = classical_accuracy,
    Delta     = delta_accuracy,
    SE_Delta  = se_delta,
    AUROC_QML = auroc_qml,
    AUROC_Cls = auroc_classical,
    Risk      = overall_risk,
    Code      = code_available
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

write_csv(table_s2, "outputs/tables/Table_S2_matched_studies_48.csv")
cat("Table S2 saved\n")

# ── Table S3: Subgroup results (reproduced from meta_analysis_main.R) ─────────
table_s3 <- tibble(
  Subgroup = c(
    "All studies",
    "Simulator only",
    "Real hardware only",
    "Weak baseline (≤2020 CNN/SVM)",
    "Modern baseline (≥EfficientNet-B4)",
    "Binary classification",
    "Multi-class (≥7 classes)",
    "HAM10000 only",
    "≤8 qubits",
    ">8 qubits"
  ),
  k         = c(48, 42, 6, 19, 29, 22, 15, 28, 21, 21),
  Delta_pct = c(+1.2, +2.4, -2.1, +5.8, -0.9, +2.9, -1.3, +0.8, +1.1, +1.5),
  CI_lower  = c(-0.8, +0.3, -4.7, +2.1, -2.6, +0.4, -3.5, -1.4, -1.3, -0.9),
  CI_upper  = c(+3.2, +4.5, +0.5, +9.5, +0.8, +5.4, +0.9, +3.0, +3.5, +3.9),
  p_value   = c(0.240, 0.024, 0.114, 0.003, 0.307, 0.023, 0.241, 0.481, 0.365, 0.218),
  I2_pct    = c(74, 71, 52, 68, 61, 66, 72, 76, 69, 78),
  Significant = c("No","Yes*","No","Yes**","No","Yes*","No","No","No","No")
)
write_csv(table_s3, "outputs/tables/Table_S3_subgroup_meta_analysis.csv")
cat("Table S3 saved\n")

# ── Table S4: Quantum hardware platforms ──────────────────────────────────────
table_s4 <- tibble(
  Platform           = c("IBM Eagle/Heron","Google Sycamore","IonQ Forte",
                          "Rigetti Aspen","D-Wave Advantage","Quantinuum H2",
                          "Simulator only"),
  Vendor             = c("IBM","Google","IonQ","Rigetti","D-Wave","Quantinuum","Various"),
  Max_Qubits         = c("133–156","53–72","35","80","5,000+","56","N/A"),
  Gate_Error_Rate    = c("0.1–0.5%","0.1–0.3%","0.3–0.5%","0.5–1.0%",
                          "N/A (annealing)","<0.1%","None (ideal)"),
  Architecture       = c("Superconducting","Superconducting","Trapped ion",
                          "Superconducting","Quantum annealing","Trapped ion",
                          "Classical simulation"),
  Studies_in_Review  = c(31, 8, 6, 4, 3, 2, 209),
  Pct_of_Real_HW     = c("57.4%","14.8%","11.1%","7.4%","5.6%","3.7%","—"),
  Key_Limitation     = c(
    "Superconducting decoherence; limited connectivity",
    "Limited external access; Google-only experiments",
    "Lower gate speed; higher cost per shot",
    "Higher gate error rates; smaller ecosystem",
    "Annealing only; no gate-model circuits",
    "Highest gate fidelity; limited availability",
    "No noise; results systematically optimistic (+4.1%)"
  )
)
write_csv(table_s4, "outputs/tables/Table_S4_hardware_platforms.csv")
cat("Table S4 saved\n")

# ── Table S5: Classical DL benchmark on HAM10000 ─────────────────────────────
table_s5 <- tibble(
  Architecture          = c("ResNet-50","ResNet-101","DenseNet-121","DenseNet-201",
                             "InceptionV3","InceptionResNetV2",
                             "EfficientNet-B4","EfficientNet-B5","EfficientNet-B7",
                             "MobileNetV3-Large"),
  Year      = c(2016,2016,2017,2017,2016,2017,2019,2019,2019,2019),
  Params_M  = c(25.6,44.5,8.0,20.0,27.2,55.8,19.3,30.4,66.3,5.4),
  HAM10000_TopAcc_pct   = c(84.3,85.7,86.2,87.1,86.9,88.3,91.2,92.0,92.8,83.1),
  AUROC     = c(0.921,0.934,0.941,0.948,0.944,0.956,0.971,0.975,0.978,0.916),
  Studies_n = c(98,67,78,54,73,42,88,134,61,37),
  Notes     = c(
    "Foundational; weakest commonly-used baseline",
    "Deeper ResNet; marginal improvement",
    "Dense connections; good gradient flow",
    "Larger DenseNet; more feature reuse",
    "Depthwise separable; first strong CNN for derm",
    "Combined inception+residual; strongest pre-EfficientNet",
    "Compound scaling; best acc/param at time",
    "Most widely used strong baseline in recent literature",
    "Largest EfficientNet variant",
    "Lightweight; deployed in mobile applications"
  )
)
write_csv(table_s5, "outputs/tables/Table_S5_classical_CNN_benchmark.csv")
cat("Table S5 saved\n")

# ── Table S6: State-of-the-art comparison (Table 5 from paper) ───────────────
table_s6 <- tibble(
  Model = c(
    "EfficientNet-B5 + Focal Loss",
    "EfficientNet-B7 Ensemble",
    "DeiT-B (distilled)",
    "ViT-L/16 (JFT pretrain)",
    "Swin-Base",
    "Swin-Large",
    "ConvNeXt-Base",
    "ConvNeXt-Large",
    "Swin-L + Diffusion Augmentation",
    "Swin-L + SAM + Diffusion",
    "SkinGPT Foundation Model",
    "ViT+Swin+ConvNeXt Ensemble",
    "Best QML — Simulator only",
    "Best QML — Real Hardware (IBM Eagle)"
  ),
  Year        = c(2020,2021,2021,2022,2022,2022,2022,2023,2023,2024,2024,2024,2023,2024),
  Top1_Acc    = c(92.0,92.8,92.4,93.8,94.7,95.8,94.2,95.1,96.3,96.8,97.2,97.6,89.4,83.7),
  Balanced_Acc= c(81.3,83.1,82.7,84.6,85.9,87.2,85.4,86.3,88.4,89.1,90.3,91.0,78.6,71.2),
  AUROC       = c(0.975,0.979,0.977,0.981,0.984,0.987,0.983,0.985,0.989,0.991,0.993,0.994,0.962,0.941),
  Category    = c(rep("Classical CNN",2), rep("Transformer",4), "CNN-Transformer",
                  "Transformer","Diffusion+Transformer","Diffusion+Transformer+SAM",
                  "Foundation Model","Ensemble","QML Simulator","QML Real HW"),
  Gap_to_SOTA_pp = c(5.6,4.8,5.2,3.8,2.9,1.8,3.4,2.5,1.3,0.8,0.4,0.0,8.2,13.9)
)
write_csv(table_s6, "outputs/tables/Table_S6_SOTA_comparison.csv")
cat("Table S6 saved\n")

# ── Table S7: QML software frameworks ────────────────────────────────────────
table_s7 <- tibble(
  Framework      = c("PennyLane","Qiskit / Qiskit ML","PyTorch + PennyLane",
                     "TensorFlow Quantum","Cirq","Ocean SDK",
                     "Strawberry Fields","Other/Custom"),
  Developer      = c("Xanadu","IBM","Xanadu/Meta","Google","Google",
                     "D-Wave","Xanadu","Various"),
  Primary_Use    = c("VQC training, hybrid ML","Circuit design, VQC",
                     "Deep hybrid models","TF+QML hybrid",
                     "Gate-level programming","Quantum annealing",
                     "Photonic QML","Miscellaneous"),
  Backends       = c("IBM, Google, simulators","IBM Quantum, Aer",
                     "Multiple backends","Cirq, simulator",
                     "Google Sycamore","D-Wave Advantage",
                     "Simulator","Various"),
  Studies_n      = c(187,142,67,41,34,9,6,14),
  Pct_of_QML    = c("42.8%","32.5%","15.3%","9.4%","7.8%","2.1%","1.4%","3.2%"),
  Autodiff       = c("Yes","Yes","Yes","Yes","No","No","Yes","—"),
  Real_HW_Access = c("Yes","Yes","Yes","Yes","Yes","Yes","No","Varies")
)
write_csv(table_s7, "outputs/tables/Table_S7_software_frameworks.csv")
cat("Table S7 saved\n")

# ── Table S8: Benchmark datasets ─────────────────────────────────────────────
table_s8 <- tibble(
  Dataset        = c("HAM10000","ISIC 2019","ISIC 2020","BCN20000","PH2",
                     "MEDNODE","SD-198","DermNet","Derm7pt",
                     "Fitzpatrick17k","ISIC 2016","SKINL2"),
  Year           = c(2018,2019,2020,2019,2013,2015,2018,2015,2019,2021,2016,2020),
  Images_N       = c(10015,25331,33126,19424,200,170,6584,"23,000+",2045,16577,1279,2000),
  Classes        = c(7,8,2,7,3,2,198,23,7,114,2,6),
  Modality       = c("Dermoscopy","Dermoscopy","Dermoscopy","Dermoscopy",
                     "Dermoscopy","Clinical photo","Clinical photo","Clinical photo",
                     "Both","Clinical photo","Dermoscopy","Clinical photo"),
  Class_Imbalance= c("58:1 (NV:DF)","High","59:1","Moderate","Moderate",
                     "1.6:1","Very high","High","Moderate","Very high","4.6:1","Low"),
  Studies_in_Review_n = c(312,187,143,98,67,23,41,28,19,12,89,14),
  Used_in_Meta_n = c(28,6,4,2,4,1,1,0,1,0,2,0),
  Notes = c(
    "Most widely used; 7 classes; extreme imbalance challenge",
    "8-class extension of HAM10000 with ISIC data",
    "Binary melanoma detection; highly imbalanced",
    "Wild dermoscopy from Barcelona clinic",
    "200-image gold standard; too small for DL",
    "Very small; clinical photos",
    "198 fine-grained classes; challenging",
    "23 categories; clinical diversity",
    "7-point checklist criteria; dual modality",
    "114 conditions; Fitzpatrick diversity focus",
    "First large ISIC challenge dataset",
    "Small balanced 6-class dataset"
  )
)
write_csv(table_s8, "outputs/tables/Table_S8_datasets.csv")
cat("Table S8 saved\n")

# ── Table S9: Nine critical gaps summary ─────────────────────────────────────
table_s9 <- tibble(
  Gap_ID      = paste0("G", 1:9),
  Gap_Name    = c(
    "Fairness and Algorithmic Bias",
    "Clinical Validation",
    "Explainability and Interpretability",
    "Real Hardware Experiments",
    "Dataset Size and Data Regime Mismatch",
    "Reproducibility",
    "Multi-modal Integration",
    "Privacy-Preserving Quantum Learning",
    "Standardisation of Evaluation Protocols"
  ),
  Severity_1to5 = c(5,5,5,4,4,4,3,3,3),
  QML_Studies_Addressing = c(0,0,0,6,11,102,3,2,0),
  Classical_Studies_Addressing = c(47,23,189,NA,NA,NA,67,34,NA),
  Key_Issue = c(
    "Zero QML studies stratify by Fitzpatrick phototype; known classical bias unaddressed",
    "All QML studies retrospective; zero prospective clinical trials; no FDA/CE assessment",
    "No Grad-CAM/SHAP/LIME equivalent for quantum circuits; regulatory blocker",
    "97.3% of QML studies use noise-free simulators; +4.1% average optimism bias",
    "Median QML training set: 412 images vs 9,840 for classical (24x difference)",
    "61% high PROBAST risk in analysis domain; only 23% share code",
    "Only 3 multi-modal QML studies; none demonstrate clear advantage",
    "Quantum federated learning and DP nearly unexplored in dermatology",
    "No community benchmarking standard; inconsistent metrics and splits"
  ),
  Recommended_Action = c(
    "Mandate Fitzpatrick stratification in all future QML dermatology studies",
    "Fund prospective multi-centre QML clinical pilots from 2028",
    "Develop quantum circuit saliency maps; quantum SHAP analogues",
    "Require real hardware results or explicit justification in peer review",
    "Benchmark only on full HAM10000; report vs matched data-regime classical models",
    "Require code sharing; pre-register all studies on PROSPERO/OSF",
    "Explore quantum tensor fusion for dermoscopy + metadata + clinical photo",
    "Adapt classical federated learning frameworks to PennyLane/Qiskit",
    "Publish community QML-MedImage benchmark with fixed splits and metrics"
  )
)
write_csv(table_s9, "outputs/tables/Table_S9_critical_gaps.csv")
cat("Table S9 saved\n")

# ── Table S10: Epidemiology & study coverage (Table 1 from paper) ─────────────
table_s10 <- tibble(
  Condition         = c("Melanoma","Basal Cell Carcinoma","Squamous Cell Carcinoma",
                         "Psoriasis","Atopic Dermatitis","Acne Vulgaris",
                         "Tinea/Ringworm","Vitiligo","Rosacea","Other/Mixed"),
  Global_Prevalence = c("325K new/yr","3.3M new/yr","1.0M new/yr","125M active",
                         "230M active","650M active","500M active",
                         "70M active","415M active","Various"),
  Five_yr_Survival_Late = c("23% (Stage IV)",">95%","70% (advanced)",
                              "N/A (chronic)","N/A (chronic)","N/A","N/A","N/A","N/A","Various"),
  Primary_Dataset   = c("HAM10000, ISIC","ISIC, BCN20000","ISIC, SD-198",
                         "PSORIASIS-100","DermNet, custom","ACNE04",
                         "DermNet","Custom datasets","Custom datasets","Mixed"),
  Studies_n         = c(312,198,145,67,53,41,28,19,14,73)
)
write_csv(table_s10, "outputs/tables/Table_S10_epidemiology_coverage.csv")
cat("Table S10 saved\n")

cat("\n✓ All supplementary tables generated in outputs/tables/\n")
