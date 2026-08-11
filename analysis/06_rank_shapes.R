# ============================================================
# FLIPSY - Shape Analysis
# 06 Rank Shapes
# ============================================================
#
# Goal:
#
# Rank patterns relative to mathematically comparable patterns.
#
# Difficulty metrics are standardised WITHIN:
#
#   n_flippers + gf2_rank
#
# This avoids simply classifying:
#
#   fewer Fs = easy
#   more Fs  = hard
#
# We then:
#
# - calculate within-group structural scores
# - rank every family
# - divide families into difficulty bands
# - select representative play-test shapes
#
# IMPORTANT:
# "difficulty" here means predicted structural difficulty.
# It is NOT yet measured human difficulty.
# ============================================================


# ------------------------------------------------------------
# Load analysis
# ------------------------------------------------------------

difficulty_analysis <- readRDS(
  "analysis/outputs/difficulty_analysis.rds"
)


cat(
  "Families to rank:",
  nrow(difficulty_analysis),
  "\n"
)


# ============================================================
# Helpers
# ============================================================


# ------------------------------------------------------------
# Safe z-score
#
# If a variable has no variation within a group,
# assign zero rather than producing NaN.
# ------------------------------------------------------------

safe_z <- function(x) {
  
  s <- sd(
    x,
    na.rm = TRUE
  )
  
  
  if (
    is.na(s) ||
    s == 0
  ) {
    
    return(
      rep(
        0,
        length(x)
      )
    )
  }
  
  
  (
    x -
      mean(
        x,
        na.rm = TRUE
      )
  ) / s
}


# ------------------------------------------------------------
# Percentile rank
# ------------------------------------------------------------

percentile_rank <- function(x) {
  
  if (length(x) == 1) {
    return(0.5)
  }
  
  
  (
    rank(
      x,
      ties.method = "average"
    ) -
      1
  ) /
    (
      length(x) -
        1
    )
}


# ============================================================
# Create mathematical comparison groups
# ============================================================

difficulty_analysis$comparison_group <- paste(
  
  "F",
  difficulty_analysis$n_flippers,
  
  "_R",
  difficulty_analysis$gf2_rank,
  
  sep = ""
)


cat(
  "\nComparison groups:\n"
)


print(
  table(
    difficulty_analysis$comparison_group
  )
)


# ============================================================
# Calculate within-group metrics
# ============================================================

groups <- split(
  
  difficulty_analysis,
  
  difficulty_analysis$comparison_group
)


ranked_groups <- vector(
  "list",
  length(groups)
)


group_names <- names(groups)


for (g in seq_along(groups)) {
  
  
  df <- groups[[g]]
  
  
  # ----------------------------------------------------------
  # 1. Spatial dispersion
  #
  # More spread-out affected cells may be harder to track.
  # ----------------------------------------------------------
  
  df$z_spatial_dispersion <- safe_z(
    df$effect_mean_pair_distance
  )
  
  
  # ----------------------------------------------------------
  # 2. Local move change
  #
  # Measures how much the effect changes when S moves
  # by one board square.
  # ----------------------------------------------------------
  
  df$z_local_change <- safe_z(
    df$mean_local_hamming
  )
  
  
  # ----------------------------------------------------------
  # 3. Overlap variability
  #
  # Irregular overlap relationships may make consequences
  # harder to predict mentally.
  # ----------------------------------------------------------
  
  df$z_overlap_variability <- safe_z(
    df$sd_move_overlap
  )
  
  
  # ----------------------------------------------------------
  # 4. Asymmetry
  #
  # More distinct orientations = less symmetry.
  #
  # We treat greater asymmetry as potentially harder.
  # ----------------------------------------------------------
  
  df$z_asymmetry <- safe_z(
    df$n_orientations
  )
  
  
  # ----------------------------------------------------------
  # 5. Geometric disconnectedness
  #
  # More separated S/F clusters may increase visual tracking.
  # ----------------------------------------------------------
  
  df$z_components <- safe_z(
    df$adjacent_components
  )
  
  
  # ----------------------------------------------------------
  # Combined structural difficulty score
  #
  # Equal weighting for now.
  #
  # This remains a hypothesis to test experimentally.
  # ----------------------------------------------------------
  
  df$structural_difficulty_score <-
    
    df$z_spatial_dispersion +
    df$z_local_change +
    df$z_overlap_variability +
    df$z_asymmetry +
    df$z_components
  
  
  # ----------------------------------------------------------
  # Rank WITHIN comparison group
  # ----------------------------------------------------------
  
  df$difficulty_percentile <-
    percentile_rank(
      df$structural_difficulty_score
    )
  
  
  df$difficulty_rank_within_group <-
    rank(
      -df$structural_difficulty_score,
      ties.method = "min"
    )
  
  
  # ----------------------------------------------------------
  # Difficulty bands
  # ----------------------------------------------------------
  
  df$difficulty_band <- cut(
    
    df$difficulty_percentile,
    
    breaks = c(
      -Inf,
      0.20,
      0.40,
      0.60,
      0.80,
      Inf
    ),
    
    labels = c(
      "very_easy",
      "easy",
      "medium",
      "hard",
      "very_hard"
    )
  )
  
  
  ranked_groups[[g]] <- df
}


# ============================================================
# Recombine
# ============================================================

ranked_shapes <- do.call(
  rbind,
  ranked_groups
)


rownames(ranked_shapes) <- NULL


ranked_shapes <- ranked_shapes[
  order(
    ranked_shapes$n_flippers,
    ranked_shapes$gf2_rank,
    -ranked_shapes$structural_difficulty_score
  ),
]


# ============================================================
# Summary
# ============================================================

cat(
  "\nDifficulty bands:\n"
)


print(
  table(
    ranked_shapes$difficulty_band
  )
)


cat(
  "\nDifficulty bands by number of Fs:\n"
)


print(
  with(
    ranked_shapes,
    table(
      n_flippers,
      difficulty_band
    )
  )
)


cat(
  "\nMean score by comparison group:\n"
)


print(
  aggregate(
    structural_difficulty_score ~ comparison_group,
    data = ranked_shapes,
    FUN = mean
  )
)


# ============================================================
# PLAY-TEST SET
# ============================================================
#
# Start with FULL-RANK patterns only.
#
# This controls for:
#
# - every board being reachable
# - unique solution
# - identical theoretical PAR distribution
#
# Therefore differences in perceived difficulty should be
# much more plausibly related to shape structure.
# ============================================================

full_rank <- ranked_shapes[
  ranked_shapes$gf2_rank == 25,
]


# ------------------------------------------------------------
# Select representatives from a subset
#
# We want:
#
# 4 easy 2-F
# 4 hard 2-F
#
# 4 easy 4-F
# 4 medium 4-F
# 4 hard 4-F
#
# = 20 initial play-test patterns
# ------------------------------------------------------------


select_evenly <- function(
    df,
    n,
    target_percentile
) {
  
  if (nrow(df) == 0) {
    return(df)
  }
  
  
  df$distance_from_target <-
    abs(
      df$difficulty_percentile -
        target_percentile
    )
  
  
  df <- df[
    order(
      df$distance_from_target,
      df$family_id
    ),
  ]
  
  
  head(
    df,
    n
  )
}


# ------------------------------------------------------------
# 2-F shapes
# ------------------------------------------------------------

two_f <- full_rank[
  full_rank$n_flippers == 2,
]


two_f_easy <- select_evenly(
  two_f,
  n = 4,
  target_percentile = 0.10
)


two_f_hard <- select_evenly(
  two_f,
  n = 4,
  target_percentile = 0.90
)


# ------------------------------------------------------------
# 4-F shapes
# ------------------------------------------------------------

four_f <- full_rank[
  full_rank$n_flippers == 4,
]


four_f_easy <- select_evenly(
  four_f,
  n = 4,
  target_percentile = 0.10
)


four_f_medium <- select_evenly(
  four_f,
  n = 4,
  target_percentile = 0.50
)


four_f_hard <- select_evenly(
  four_f,
  n = 4,
  target_percentile = 0.90
)


# ------------------------------------------------------------
# Label selections
# ------------------------------------------------------------

two_f_easy$test_class <- "2F_easy"
two_f_hard$test_class <- "2F_hard"

four_f_easy$test_class <- "4F_easy"
four_f_medium$test_class <- "4F_medium"
four_f_hard$test_class <- "4F_hard"


# ------------------------------------------------------------
# Combine
# ------------------------------------------------------------

playtest_shapes <- rbind(
  
  two_f_easy,
  two_f_hard,
  
  four_f_easy,
  four_f_medium,
  four_f_hard
)


# Remove helper column
playtest_shapes$distance_from_target <- NULL


# Random order for blind play testing
set.seed(2026)


playtest_shapes$playtest_order <- sample(
  seq_len(
    nrow(playtest_shapes)
  )
)


playtest_shapes <- playtest_shapes[
  order(
    playtest_shapes$playtest_order
  ),
]


rownames(playtest_shapes) <- NULL


# ============================================================
# Display play-test set
# ============================================================

cat(
  "\nPlay-test set:\n\n"
)


print(
  playtest_shapes[
    ,
    c(
      "playtest_order",
      "family_id",
      "canonical_key",
      "n_flippers",
      "gf2_rank",
      "difficulty_percentile",
      "test_class"
    )
  ]
)


# ============================================================
# Save
# ============================================================

saveRDS(
  ranked_shapes,
  "analysis/outputs/ranked_shapes.rds"
)


saveRDS(
  playtest_shapes,
  "analysis/outputs/playtest_shapes.rds"
)


write.csv(
  ranked_shapes,
  "analysis/outputs/ranked_shapes.csv",
  row.names = FALSE
)


write.csv(
  playtest_shapes,
  "analysis/outputs/playtest_shapes.csv",
  row.names = FALSE
)


cat(
  "\nRanked shapes and play-test set saved.\n"
)