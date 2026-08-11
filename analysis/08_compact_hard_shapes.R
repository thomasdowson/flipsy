# ============================================================
# FLIPSY - Shape Analysis
# 08 Compact Hard Shapes
# ============================================================
#
# Goal:
#
# Find the hardest shapes subject to a strict compactness rule.
#
# We do NOT combine difficulty and compactness into one score.
#
# Instead:
#
#   maximise structural difficulty
#   subject to shape fitting within 3x3
#
# This makes compactness a hard constraint rather than
# something that can overpower the difficulty score.
# ============================================================


# ------------------------------------------------------------
# Load previous analysis
# ------------------------------------------------------------

compact_candidates <- readRDS(
  "analysis/outputs/compact_candidates.rds"
)


cat(
  "Compact candidates:",
  nrow(compact_candidates),
  "\n"
)


# ============================================================
# Rank purely by structural difficulty
# ============================================================

compact_hard <- compact_candidates[
  order(
    compact_candidates$structural_difficulty_score,
    decreasing = TRUE
  ),
]


rownames(compact_hard) <- NULL


compact_hard$hard_rank <- seq_len(
  nrow(compact_hard)
)


# ============================================================
# Show top 20
# ============================================================

top_n <- min(
  20,
  nrow(compact_hard)
)


cat(
  "\nTop compact hard shapes:\n\n"
)


print(
  compact_hard[
    seq_len(top_n),
    c(
      "hard_rank",
      "family_id",
      "canonical_key",
      "n_flippers",
      "gf2_rank",
      "visual_width",
      "visual_height",
      "visual_area",
      "connector_cells",
      "total_visible_cells",
      "structural_difficulty_score",
      "difficulty_percentile",
      "n_orientations",
      "symmetry_count"
    )
  ]
)


# ============================================================
# Create a smaller play-test shortlist
# ============================================================
#
# We take:
#
# - top 8 overall
# - then try to preserve some variety in F count
#
# ============================================================

top_overall <- head(
  compact_hard,
  8
)


# ------------------------------------------------------------
# Best 2-F shapes
# ------------------------------------------------------------

best_2f <- compact_hard[
  compact_hard$n_flippers == 2,
]


best_2f <- head(
  best_2f,
  6
)


# ------------------------------------------------------------
# Best 4-F shapes
# ------------------------------------------------------------

best_4f <- compact_hard[
  compact_hard$n_flippers == 4,
]


best_4f <- head(
  best_4f,
  6
)


# ------------------------------------------------------------
# Combine and remove duplicate families
# ------------------------------------------------------------

shortlist <- rbind(
  top_overall,
  best_2f,
  best_4f
)


shortlist <- shortlist[
  !duplicated(
    shortlist$family_id
  ),
]


rownames(shortlist) <- NULL


shortlist$shortlist_order <- seq_len(
  nrow(shortlist)
)


cat(
  "\nCompact shortlist:\n\n"
)


print(
  shortlist[
    ,
    c(
      "shortlist_order",
      "family_id",
      "canonical_key",
      "n_flippers",
      "visual_width",
      "visual_height",
      "connector_cells",
      "structural_difficulty_score"
    )
  ]
)


# ============================================================
# Save
# ============================================================

saveRDS(
  compact_hard,
  "analysis/outputs/compact_hard.rds"
)


saveRDS(
  shortlist,
  "analysis/outputs/compact_hard_shortlist.rds"
)


write.csv(
  compact_hard,
  "analysis/outputs/compact_hard.csv",
  row.names = FALSE
)


write.csv(
  shortlist,
  "analysis/outputs/compact_hard_shortlist.csv",
  row.names = FALSE
)


cat(
  "\nCompact hard-shape ranking saved.\n"
)