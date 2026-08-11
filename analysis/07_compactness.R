# ============================================================
# FLIPSY - Shape Analysis
# 07 Compactness
# ============================================================
#
# Goal:
#
# Find shapes that are structurally difficult WITHOUT relying
# on being visually huge or containing sprawling branches.
#
# We measure:
#
# - S/F bounding-box width and height
# - S/F bounding-box area
# - maximum distance from S
# - automatic visual-shape area
# - number of connector X cells
# - total visible cells
# - visual density
#
# We then calculate:
#
#   visual_complexity_score
#
# and:
#
#   elegance_score =
#       structural difficulty - visual complexity
#
# High elegance_score therefore means:
#
#   "hard despite being compact"
#
# ============================================================


# ============================================================
# Setup
# ============================================================

board_size <- 5


# ------------------------------------------------------------
# Load required V2 functions
# ------------------------------------------------------------

source("R/v_2/shape_engine_v2.R")
source("R/v_2/shape_visualiser_v2.R")


# ------------------------------------------------------------
# Load ranked shape analysis
# ------------------------------------------------------------

ranked_shapes <- readRDS(
  "analysis/outputs/ranked_shapes.rds"
)


cat(
  "Families to analyse:",
  nrow(ranked_shapes),
  "\n"
)


# ============================================================
# Helpers
# ============================================================


# ------------------------------------------------------------
# Parse canonical S/F key
#
# Example:
#
# "-2:0|0:2|2:2"
#
# becomes a data frame of F offsets.
# ------------------------------------------------------------

parse_key <- function(key) {
  
  pieces <- strsplit(
    key,
    "\\|"
  )[[1]]
  
  
  rows <- lapply(
    pieces,
    function(piece) {
      
      x <- as.integer(
        strsplit(
          piece,
          ":",
          fixed = TRUE
        )[[1]]
      )
      
      
      data.frame(
        row_offset = x[1],
        col_offset = x[2]
      )
    }
  )
  
  
  do.call(
    rbind,
    rows
  )
}


# ------------------------------------------------------------
# Safe z-score
#
# Returns zero if there is no variation.
# ------------------------------------------------------------

safe_z <- function(x) {
  
  s <- sd(
    x,
    na.rm = TRUE
  )
  
  
  if (
    length(x) == 0 ||
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


# ============================================================
# Calculate compactness metrics
# ============================================================

compactness_results <- vector(
  "list",
  nrow(ranked_shapes)
)


for (i in seq_len(nrow(ranked_shapes))) {
  
  
  if (i %% 250 == 0) {
    
    cat(
      "Processed",
      i,
      "families\n"
    )
  }
  
  
  key <- ranked_shapes$canonical_key[i]
  
  
  f_offsets <- parse_key(
    key
  )
  
  
  # ----------------------------------------------------------
  # Include S at (0,0)
  # ----------------------------------------------------------
  
  sf_points <- rbind(
    
    data.frame(
      row_offset = 0,
      col_offset = 0
    ),
    
    f_offsets
  )
  
  
  # ----------------------------------------------------------
  # S/F bounding box
  # ----------------------------------------------------------
  
  sf_height <-
    max(sf_points$row_offset) -
    min(sf_points$row_offset) +
    1L
  
  
  sf_width <-
    max(sf_points$col_offset) -
    min(sf_points$col_offset) +
    1L
  
  
  sf_area <-
    sf_height *
    sf_width
  
  
  # ----------------------------------------------------------
  # Maximum distance from S
  #
  # These are given VISUAL-specific names so they do not
  # collide with variables already present in ranked_shapes.
  # ----------------------------------------------------------
  
  visual_max_chebyshev <- max(
    pmax(
      abs(f_offsets$row_offset),
      abs(f_offsets$col_offset)
    )
  )
  
  
  visual_max_manhattan <- max(
    abs(f_offsets$row_offset) +
      abs(f_offsets$col_offset)
  )
  
  
  # ----------------------------------------------------------
  # Automatic visual network
  # ----------------------------------------------------------
  
  visual_matrix <- key_to_shape_matrix_v2(
    key
  )
  
  
  visual_height <- nrow(
    visual_matrix
  )
  
  
  visual_width <- ncol(
    visual_matrix
  )
  
  
  visual_area <-
    visual_height *
    visual_width
  
  
  # ----------------------------------------------------------
  # Count visual cell types
  # ----------------------------------------------------------
  
  connector_cells <- sum(
    visual_matrix == "X"
  )
  
  
  visible_flippers <- sum(
    visual_matrix == "F"
  )
  
  
  visible_start_cells <- sum(
    visual_matrix == "S"
  )
  
  
  total_visible_cells <- sum(
    visual_matrix != "."
  )
  
  
  # ----------------------------------------------------------
  # Visual density
  #
  # Proportion of the visual bounding box actually occupied.
  # ----------------------------------------------------------
  
  visual_density <-
    total_visible_cells /
    visual_area
  
  
  # ----------------------------------------------------------
  # HARD GAME VALIDITY CHECK
  #
  # No F is allowed to land on S after board wrapping.
  #
  # In our current canonical search this should normally be
  # impossible, but we explicitly enforce the rule here.
  # ----------------------------------------------------------
  
  f_on_s <- any(
    
    f_offsets$row_offset %% board_size == 0 &
      
      f_offsets$col_offset %% board_size == 0
  )
  
  
  # ----------------------------------------------------------
  # Also ensure F offsets are distinct after wrapping
  # ----------------------------------------------------------
  
  wrapped_f <- data.frame(
    
    row =
      f_offsets$row_offset %% board_size,
    
    col =
      f_offsets$col_offset %% board_size
  )
  
  
  duplicate_wrapped_f <- any(
    duplicated(
      wrapped_f
    )
  )
  
  
  valid_for_game <-
    !f_on_s &&
    !duplicate_wrapped_f
  
  
  # ----------------------------------------------------------
  # Store
  # ----------------------------------------------------------
  
  compactness_results[[i]] <- data.frame(
    
    family_id =
      ranked_shapes$family_id[i],
    
    sf_width =
      sf_width,
    
    sf_height =
      sf_height,
    
    sf_area =
      sf_area,
    
    visual_max_chebyshev =
      visual_max_chebyshev,
    
    visual_max_manhattan =
      visual_max_manhattan,
    
    visual_width =
      visual_width,
    
    visual_height =
      visual_height,
    
    visual_area =
      visual_area,
    
    connector_cells =
      connector_cells,
    
    visible_flippers =
      visible_flippers,
    
    visible_start_cells =
      visible_start_cells,
    
    total_visible_cells =
      total_visible_cells,
    
    visual_density =
      visual_density,
    
    f_on_s =
      f_on_s,
    
    duplicate_wrapped_f =
      duplicate_wrapped_f,
    
    valid_for_game =
      valid_for_game,
    
    stringsAsFactors = FALSE
  )
}


# ============================================================
# Combine compactness results
# ============================================================

compactness_metrics <- do.call(
  rbind,
  compactness_results
)


rownames(
  compactness_metrics
) <- NULL


# ============================================================
# Join with mathematical ranking
# ============================================================

shape_selection <- merge(
  
  ranked_shapes,
  
  compactness_metrics,
  
  by = "family_id",
  
  all.x = TRUE,
  
  sort = FALSE
)


shape_selection <- shape_selection[
  order(
    shape_selection$family_id
  ),
]


rownames(
  shape_selection
) <- NULL


# ============================================================
# Sanity checks
# ============================================================

stopifnot(
  nrow(shape_selection) ==
    nrow(ranked_shapes)
)


stopifnot(
  !any(
    is.na(
      shape_selection$visual_area
    )
  )
)


cat(
  "\nGame-valid families:",
  sum(shape_selection$valid_for_game),
  "of",
  nrow(shape_selection),
  "\n"
)


cat(
  "F-on-S violations:",
  sum(shape_selection$f_on_s),
  "\n"
)


cat(
  "Wrapped duplicate-F violations:",
  sum(shape_selection$duplicate_wrapped_f),
  "\n"
)


# ============================================================
# Visual complexity score
# ============================================================
#
# Larger score = more visually complicated.
#
# Components:
#
# 1. visual bounding-box area
# 2. number of connector X cells
# 3. total number of visible cells
# 4. maximum Chebyshev distance from S
#
# ============================================================

shape_selection$visual_complexity_score <-
  
  safe_z(
    shape_selection$visual_area
  ) +
  
  safe_z(
    shape_selection$connector_cells
  ) +
  
  safe_z(
    shape_selection$total_visible_cells
  ) +
  
  safe_z(
    shape_selection$visual_max_chebyshev
  )


# ============================================================
# Elegance score
# ============================================================
#
# Structural difficulty was standardised within comparable
# n_flippers/rank groups in analysis 06.
#
# We now reward:
#
#   high predicted difficulty
#
# while penalising:
#
#   large / sprawling visual shapes
#
# ============================================================

shape_selection$elegance_score <-
  
  shape_selection$structural_difficulty_score -
  
  shape_selection$visual_complexity_score


# ============================================================
# GAME CANDIDATES
# ============================================================
#
# For now we restrict game candidates to:
#
# - mathematically valid
# - no S/F wrap collision
# - no duplicate wrapped Fs
# - full GF(2) rank
#
# Full rank means:
#
# - every board state is reachable
# - every board has a unique solution
#
# ============================================================

game_candidates <- shape_selection[
  
  shape_selection$valid_for_game &
    shape_selection$full_rank,
  
]


rownames(
  game_candidates
) <- NULL


cat(
  "\nFull-rank valid game candidates:",
  nrow(game_candidates),
  "\n"
)


# ============================================================
# COMPACT GAME CANDIDATES
# ============================================================
#
# Initial strict definition:
#
# Entire displayed shape must fit inside a 3x3 box.
#
# ============================================================

compact_candidates <- game_candidates[
  
  game_candidates$visual_width <= 3 &
    game_candidates$visual_height <= 3,
  
]


cat(
  "Compact <= 3x3 candidates:",
  nrow(compact_candidates),
  "\n"
)


# ============================================================
# Rank compact candidates by elegance
# ============================================================

compact_candidates <- compact_candidates[
  
  order(
    compact_candidates$elegance_score,
    decreasing = TRUE
  ),
  
]


rownames(
  compact_candidates
) <- NULL


compact_candidates$compact_rank <-
  seq_len(
    nrow(compact_candidates)
  )


# ============================================================
# Top compact difficult shapes
# ============================================================

cat(
  "\nTop 20 compact high-difficulty candidates:\n\n"
)


top_n <- min(
  20,
  nrow(compact_candidates)
)


if (top_n > 0) {
  
  print(
    
    compact_candidates[
      seq_len(top_n),
      c(
        "compact_rank",
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
        "visual_complexity_score",
        "elegance_score"
      )
    ]
    
  )
  
} else {
  
  cat(
    "No candidates passed the 3x3 filter.\n"
  )
}


# ============================================================
# Smallest candidates regardless of 3x3 cutoff
# ============================================================
#
# Sort first by:
#
# - smallest visual area
# - fewest visible cells
#
# Then within those:
#
# - greatest predicted structural difficulty
#
# ============================================================

smallest_candidates <- game_candidates[
  
  order(
    game_candidates$visual_area,
    game_candidates$total_visible_cells,
    -game_candidates$structural_difficulty_score
  ),
  
]


rownames(
  smallest_candidates
) <- NULL


cat(
  "\nSmallest high-difficulty candidates:\n\n"
)


small_n <- min(
  20,
  nrow(smallest_candidates)
)


if (small_n > 0) {
  
  print(
    
    smallest_candidates[
      seq_len(small_n),
      c(
        "family_id",
        "canonical_key",
        "n_flippers",
        "visual_width",
        "visual_height",
        "visual_area",
        "connector_cells",
        "total_visible_cells",
        "structural_difficulty_score",
        "elegance_score"
      )
    ]
    
  )
}


# ============================================================
# Candidate counts by visual area
# ============================================================

cat(
  "\nCandidate count by visual area:\n"
)


print(
  table(
    game_candidates$visual_area
  )
)


# ============================================================
# Mean structural difficulty by visual area
# ============================================================

cat(
  "\nMean structural difficulty by visual area:\n"
)


print(
  
  aggregate(
    
    structural_difficulty_score ~ visual_area,
    
    data = game_candidates,
    
    FUN = mean
  )
  
)


# ============================================================
# Candidate counts by dimensions
# ============================================================

cat(
  "\nCandidate count by visual dimensions:\n"
)


print(
  
  with(
    game_candidates,
    table(
      visual_height,
      visual_width
    )
  )
  
)


# ============================================================
# Top elegance candidates overall
# ============================================================

elegant_candidates <- game_candidates[
  
  order(
    game_candidates$elegance_score,
    decreasing = TRUE
  ),
  
]


rownames(
  elegant_candidates
) <- NULL


cat(
  "\nTop 20 elegance candidates overall:\n\n"
)


elegant_n <- min(
  20,
  nrow(elegant_candidates)
)


print(
  
  elegant_candidates[
    seq_len(elegant_n),
    c(
      "family_id",
      "canonical_key",
      "n_flippers",
      "visual_width",
      "visual_height",
      "visual_area",
      "connector_cells",
      "structural_difficulty_score",
      "visual_complexity_score",
      "elegance_score"
    )
  ]
  
)


# ============================================================
# Save
# ============================================================

saveRDS(
  compactness_metrics,
  "analysis/outputs/compactness_metrics.rds"
)


saveRDS(
  shape_selection,
  "analysis/outputs/shape_selection.rds"
)


saveRDS(
  game_candidates,
  "analysis/outputs/game_candidates.rds"
)


saveRDS(
  compact_candidates,
  "analysis/outputs/compact_candidates.rds"
)


saveRDS(
  elegant_candidates,
  "analysis/outputs/elegant_candidates.rds"
)


write.csv(
  shape_selection,
  "analysis/outputs/shape_selection.csv",
  row.names = FALSE
)


write.csv(
  game_candidates,
  "analysis/outputs/game_candidates.csv",
  row.names = FALSE
)


write.csv(
  compact_candidates,
  "analysis/outputs/compact_candidates.csv",
  row.names = FALSE
)


write.csv(
  elegant_candidates,
  "analysis/outputs/elegant_candidates.csv",
  row.names = FALSE
)


cat(
  "\nCompactness analysis saved.\n"
)