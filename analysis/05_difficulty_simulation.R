# ============================================================
# FLIPSY - Shape Analysis
# 05 Difficulty Metrics
# ============================================================
#
# Goal:
# Explore properties that may explain HUMAN puzzle difficulty.
#
# We already know:
#
# - rank measures algebraic freedom
# - nullity measures solution multiplicity
# - full-rank shapes all have the same theoretical PAR
#   distribution
#
# Therefore this script focuses on how moves interact.
# ============================================================


# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

board_size <- 5


# ------------------------------------------------------------
# Load previous analysis
# ------------------------------------------------------------

family_analysis <- readRDS(
  "analysis/outputs/family_analysis.rds"
)


cat(
  "Families to analyse:",
  nrow(family_analysis),
  "\n"
)


# ============================================================
# Helpers
# ============================================================


parse_pattern_key <- function(key) {
  
  pieces <- strsplit(
    key,
    "\\|"
  )[[1]]
  
  
  coords <- lapply(
    pieces,
    function(piece) {
      
      values <- as.integer(
        strsplit(
          piece,
          ":",
          fixed = TRUE
        )[[1]]
      )
      
      
      data.frame(
        row_offset = values[1],
        col_offset = values[2]
      )
    }
  )
  
  
  do.call(
    rbind,
    coords
  )
}


wrap_index <- function(
    x,
    n = board_size
) {
  
  ((x - 1) %% n) + 1
}


cell_index <- function(
    row,
    col,
    n = board_size
) {
  
  (row - 1) * n + col
}


build_move_matrix <- function(
    f_offsets,
    n = board_size
) {
  
  total_cells <- n * n
  
  
  A <- matrix(
    0L,
    nrow = total_cells,
    ncol = total_cells
  )
  
  
  all_offsets <- rbind(
    
    data.frame(
      row_offset = 0,
      col_offset = 0
    ),
    
    f_offsets
  )
  
  
  for (row in seq_len(n)) {
    
    for (col in seq_len(n)) {
      
      move_id <- cell_index(
        row,
        col,
        n
      )
      
      
      for (i in seq_len(nrow(all_offsets))) {
        
        affected_row <- wrap_index(
          row + all_offsets$row_offset[i],
          n
        )
        
        affected_col <- wrap_index(
          col + all_offsets$col_offset[i],
          n
        )
        
        
        affected_id <- cell_index(
          affected_row,
          affected_col,
          n
        )
        
        
        A[
          affected_id,
          move_id
        ] <- (
          A[
            affected_id,
            move_id
          ] + 1L
        ) %% 2L
      }
    }
  }
  
  
  A
}


# ------------------------------------------------------------
# Pairwise move overlap
#
# For every pair of possible clicks:
# how many board cells do both moves flip?
# ------------------------------------------------------------

move_overlap_metrics <- function(A) {
  
  overlap <- t(A) %*% A
  
  
  values <- overlap[
    upper.tri(overlap)
  ]
  
  
  data.frame(
    
    mean_move_overlap =
      mean(values),
    
    max_move_overlap =
      max(values),
    
    prop_moves_overlap =
      mean(values > 0),
    
    prop_overlap_2plus =
      mean(values >= 2),
    
    sd_move_overlap =
      sd(values)
  )
}


# ------------------------------------------------------------
# Hamming distance between move vectors
#
# Small distance means two different clicks have very
# similar effects on the board.
# ------------------------------------------------------------

move_similarity_metrics <- function(A) {
  
  n_moves <- ncol(A)
  
  distances <- numeric(0)
  
  
  for (i in 1:(n_moves - 1)) {
    
    for (j in (i + 1):n_moves) {
      
      distances <- c(
        distances,
        sum(
          A[, i] != A[, j]
        )
      )
    }
  }
  
  
  data.frame(
    
    mean_move_hamming =
      mean(distances),
    
    min_move_hamming =
      min(distances),
    
    max_move_hamming =
      max(distances),
    
    sd_move_hamming =
      sd(distances)
  )
}


# ------------------------------------------------------------
# Local click similarity
#
# Compare each click with its four orthogonal neighbours.
#
# This asks:
#
# "If I move S one square, how much does the effect change?"
# ------------------------------------------------------------

local_similarity_metrics <- function(
    A,
    n = board_size
) {
  
  distances <- numeric(0)
  
  
  directions <- rbind(
    c(-1, 0),
    c(1, 0),
    c(0, -1),
    c(0, 1)
  )
  
  
  for (row in seq_len(n)) {
    
    for (col in seq_len(n)) {
      
      i <- cell_index(
        row,
        col,
        n
      )
      
      
      for (d in seq_len(nrow(directions))) {
        
        neighbour_row <- wrap_index(
          row + directions[d, 1],
          n
        )
        
        neighbour_col <- wrap_index(
          col + directions[d, 2],
          n
        )
        
        
        j <- cell_index(
          neighbour_row,
          neighbour_col,
          n
        )
        
        
        # Count each pair only once
        if (i < j) {
          
          distances <- c(
            distances,
            sum(
              A[, i] != A[, j]
            )
          )
        }
      }
    }
  }
  
  
  data.frame(
    
    mean_local_hamming =
      mean(distances),
    
    min_local_hamming =
      min(distances),
    
    max_local_hamming =
      max(distances),
    
    sd_local_hamming =
      sd(distances)
  )
}


# ------------------------------------------------------------
# Cancellation metrics
#
# XOR pairs of moves.
#
# If two moves together affect very few cells, they have
# strong cancellation.
# ------------------------------------------------------------

pair_cancellation_metrics <- function(A) {
  
  n_moves <- ncol(A)
  
  combined_weights <- numeric(0)
  
  
  for (i in 1:(n_moves - 1)) {
    
    for (j in (i + 1):n_moves) {
      
      combined <- (
        A[, i] +
          A[, j]
      ) %% 2L
      
      
      combined_weights <- c(
        combined_weights,
        sum(combined)
      )
    }
  }
  
  
  data.frame(
    
    mean_pair_effect =
      mean(combined_weights),
    
    min_pair_effect =
      min(combined_weights),
    
    max_pair_effect =
      max(combined_weights),
    
    sd_pair_effect =
      sd(combined_weights),
    
    prop_pair_effect_small =
      mean(combined_weights <= 4)
  )
}


# ------------------------------------------------------------
# Spatial dispersion of one move
#
# Since every move is a translation of the same shape,
# this can be calculated directly from its offsets.
# ------------------------------------------------------------

spatial_metrics <- function(
    f_offsets
) {
  
  points <- rbind(
    
    data.frame(
      row_offset = 0,
      col_offset = 0
    ),
    
    f_offsets
  )
  
  
  distances <- numeric(0)
  
  
  if (nrow(points) > 1) {
    
    for (i in 1:(nrow(points) - 1)) {
      
      for (j in (i + 1):nrow(points)) {
        
        dr <- abs(
          points$row_offset[i] -
            points$row_offset[j]
        )
        
        dc <- abs(
          points$col_offset[i] -
            points$col_offset[j]
        )
        
        
        # Toroidal shortest distance
        dr <- min(
          dr,
          board_size - dr
        )
        
        dc <- min(
          dc,
          board_size - dc
        )
        
        
        distances <- c(
          distances,
          sqrt(
            dr^2 +
              dc^2
          )
        )
      }
    }
  }
  
  
  data.frame(
    
    effect_mean_pair_distance =
      mean(distances),
    
    effect_max_pair_distance =
      max(distances),
    
    effect_sd_pair_distance =
      sd(distances)
  )
}


# ============================================================
# Analyse all families
# ============================================================

results <- vector(
  "list",
  nrow(family_analysis)
)


for (i in seq_len(nrow(family_analysis))) {
  
  
  if (i %% 250 == 0) {
    
    cat(
      "Processed",
      i,
      "families\n"
    )
  }
  
  
  key <-
    family_analysis$canonical_key[i]
  
  
  offsets <- parse_pattern_key(
    key
  )
  
  
  A <- build_move_matrix(
    f_offsets = offsets,
    n = board_size
  )
  
  
  overlap <-
    move_overlap_metrics(A)
  
  similarity <-
    move_similarity_metrics(A)
  
  local_similarity <-
    local_similarity_metrics(A)
  
  cancellation <-
    pair_cancellation_metrics(A)
  
  spatial <-
    spatial_metrics(offsets)
  
  
  results[[i]] <- cbind(
    
    data.frame(
      family_id =
        family_analysis$family_id[i]
    ),
    
    overlap,
    
    similarity,
    
    local_similarity,
    
    cancellation,
    
    spatial
  )
}


# ============================================================
# Combine
# ============================================================

difficulty_metrics <- do.call(
  rbind,
  results
)


rownames(difficulty_metrics) <- NULL


# ------------------------------------------------------------
# Join with previous analysis
# ------------------------------------------------------------

difficulty_analysis <- merge(
  
  family_analysis,
  
  difficulty_metrics,
  
  by = "family_id",
  
  all.x = TRUE,
  
  sort = FALSE
)


difficulty_analysis <- difficulty_analysis[
  order(difficulty_analysis$family_id),
]


rownames(difficulty_analysis) <- NULL


# ============================================================
# Initial summaries
# ============================================================

cat(
  "\nMean move overlap by number of Fs:\n"
)


print(
  aggregate(
    mean_move_overlap ~ n_flippers,
    data = difficulty_analysis,
    FUN = mean
  )
)


cat(
  "\nMean local Hamming distance by number of Fs:\n"
)


print(
  aggregate(
    mean_local_hamming ~ n_flippers,
    data = difficulty_analysis,
    FUN = mean
  )
)


cat(
  "\nMean pair effect by rank:\n"
)


print(
  aggregate(
    mean_pair_effect ~ gf2_rank,
    data = difficulty_analysis,
    FUN = mean
  )
)


# ============================================================
# Look at full-rank families only
# ============================================================

full_rank_shapes <- difficulty_analysis[
  difficulty_analysis$full_rank,
]


cat(
  "\nFull-rank families:",
  nrow(full_rank_shapes),
  "\n"
)


# ------------------------------------------------------------
# Standardised exploratory difficulty score
#
# IMPORTANT:
# This is NOT yet a claim about actual human difficulty.
#
# It simply selects patterns that are structurally:
#
# - spatially dispersed
# - locally dissimilar
# - variable in overlap
#
# We can later compare this with actual play-testing.
# ------------------------------------------------------------

z_score <- function(x) {
  
  if (sd(x) == 0) {
    return(rep(0, length(x)))
  }
  
  
  as.numeric(
    scale(x)
  )
}


full_rank_shapes$exploratory_score <-
  
  z_score(
    full_rank_shapes$effect_mean_pair_distance
  ) +
  
  z_score(
    full_rank_shapes$mean_local_hamming
  ) +
  
  z_score(
    full_rank_shapes$sd_move_overlap
  )


# Highest structural scores
hard_candidates <- full_rank_shapes[
  order(
    full_rank_shapes$exploratory_score,
    decreasing = TRUE
  ),
]


# Lowest structural scores
easy_candidates <- full_rank_shapes[
  order(
    full_rank_shapes$exploratory_score,
    decreasing = FALSE
  ),
]


cat(
  "\nTop 10 exploratory hard candidates:\n"
)


print(
  hard_candidates[
    1:10,
    c(
      "family_id",
      "canonical_key",
      "n_flippers",
      "geometry_class",
      "exploratory_score"
    )
  ]
)


cat(
  "\nTop 10 exploratory easy candidates:\n"
)


print(
  easy_candidates[
    1:10,
    c(
      "family_id",
      "canonical_key",
      "n_flippers",
      "geometry_class",
      "exploratory_score"
    )
  ]
)


# ============================================================
# Save
# ============================================================

saveRDS(
  difficulty_metrics,
  "analysis/outputs/difficulty_metrics.rds"
)


saveRDS(
  difficulty_analysis,
  "analysis/outputs/difficulty_analysis.rds"
)


saveRDS(
  hard_candidates,
  "analysis/outputs/hard_candidates.rds"
)


saveRDS(
  easy_candidates,
  "analysis/outputs/easy_candidates.rds"
)


write.csv(
  difficulty_analysis,
  "analysis/outputs/difficulty_analysis.csv",
  row.names = FALSE
)


write.csv(
  hard_candidates,
  "analysis/outputs/hard_candidates.csv",
  row.names = FALSE
)


write.csv(
  easy_candidates,
  "analysis/outputs/easy_candidates.csv",
  row.names = FALSE
)


cat(
  "\nDifficulty metrics saved.\n"
)