# ============================================================
# FLIPSY - Shape Analysis
# 01 Generate Shapes
# ============================================================
#
# Generate every mathematical S/F pattern on the 5x5
# wrapping board for a chosen range of F counts.
#
# At this stage:
#
# - S is always at (0, 0)
# - F positions are relative to S
# - X connections are NOT considered
# - rotations/reflections are NOT yet removed
#
# Those steps come later.
# ============================================================


# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

board_size <- 5

min_flippers <- 1
max_flippers <- 4


# ------------------------------------------------------------
# Create all possible relative positions
#
# On a 5x5 wrapping board, offsets -2:2 give one unique
# representative for every possible relative position.
# ------------------------------------------------------------

positions <- expand.grid(
  
  row_offset = -2:2,
  
  col_offset = -2:2
)


# ------------------------------------------------------------
# Remove (0, 0)
#
# This position is always S.
# ------------------------------------------------------------

positions <- positions[
  !(
    positions$row_offset == 0 &
      positions$col_offset == 0
  ),
]


rownames(positions) <- NULL


cat(
  "Available F positions:",
  nrow(positions),
  "\n"
)


# ------------------------------------------------------------
# Give every relative position an ID
# ------------------------------------------------------------

positions$position_id <- seq_len(
  nrow(positions)
)


# ------------------------------------------------------------
# Helper:
# Convert one combination of position IDs into a pattern
# ------------------------------------------------------------

make_pattern <- function(
    position_ids,
    pattern_id
) {
  
  selected <- positions[
    position_ids,
    c(
      "row_offset",
      "col_offset"
    )
  ]
  
  
  data.frame(
    
    pattern_id = pattern_id,
    
    n_flippers = nrow(selected),
    
    row_offset = selected$row_offset,
    
    col_offset = selected$col_offset
  )
}


# ------------------------------------------------------------
# Generate all patterns
# ------------------------------------------------------------

patterns <- list()

pattern_counter <- 1L


for (
  n_f in min_flippers:max_flippers
) {
  
  cat(
    "Generating patterns with",
    n_f,
    "F squares...\n"
  )
  
  
  combinations <- combn(
    seq_len(nrow(positions)),
    n_f
  )
  
  
  for (
    j in seq_len(ncol(combinations))
  ) {
    
    patterns[[pattern_counter]] <-
      make_pattern(
        position_ids = combinations[, j],
        pattern_id = pattern_counter
      )
    
    
    pattern_counter <-
      pattern_counter + 1L
  }
}


# ------------------------------------------------------------
# Combine into one data frame
# ------------------------------------------------------------

all_patterns <- do.call(
  rbind,
  patterns
)


rownames(all_patterns) <- NULL


# ------------------------------------------------------------
# Summary
# ------------------------------------------------------------

pattern_summary <- aggregate(
  
  pattern_id ~ n_flippers,
  
  data = unique(
    all_patterns[
      c(
        "pattern_id",
        "n_flippers"
      )
    ]
  ),
  
  FUN = length
)


names(pattern_summary)[2] <-
  "n_patterns"


cat(
  "\nTotal patterns:",
  length(unique(all_patterns$pattern_id)),
  "\n\n"
)


print(
  pattern_summary
)


# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

saveRDS(
  all_patterns,
  "analysis/outputs/all_patterns_raw.rds"
)


write.csv(
  all_patterns,
  "analysis/outputs/all_patterns_raw.csv",
  row.names = FALSE
)


cat(
  "\nSaved raw pattern library.\n"
)