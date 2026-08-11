# ============================================================
# FLIPSY - Shape Analysis
# 02 Canonicalise Shapes
# ============================================================
#
# Collapse mathematically equivalent S/F patterns under the
# 8 symmetries of the square:
#
# - identity
# - rotations by 90, 180, 270 degrees
# - reflections
#
# S remains fixed at (0, 0).
#
# Output:
# - one canonical key per raw pattern
# - one family ID per unique symmetry class
# ============================================================


# ------------------------------------------------------------
# Load raw patterns
# ------------------------------------------------------------

all_patterns <- readRDS(
  "analysis/outputs/all_patterns_raw.rds"
)


# ------------------------------------------------------------
# Helper: wrap coordinate back to canonical -2:2 range
#
# For board size 5:
#
#  3 -> -2
#  4 -> -1
# -3 ->  2
# -4 ->  1
# ------------------------------------------------------------

canonical_coord <- function(x, n = 5) {
  
  wrapped <- x %% n
  
  ifelse(
    wrapped > floor(n / 2),
    wrapped - n,
    wrapped
  )
}


# ------------------------------------------------------------
# Apply one of the 8 square symmetries
# ------------------------------------------------------------

transform_pattern <- function(
    coords,
    transform_id
) {
  
  r <- coords$row_offset
  c <- coords$col_offset
  
  
  transformed <- switch(
    
    as.character(transform_id),
    
    # Identity
    "1" = data.frame(
      row_offset = r,
      col_offset = c
    ),
    
    # Rotate 90
    "2" = data.frame(
      row_offset = c,
      col_offset = -r
    ),
    
    # Rotate 180
    "3" = data.frame(
      row_offset = -r,
      col_offset = -c
    ),
    
    # Rotate 270
    "4" = data.frame(
      row_offset = -c,
      col_offset = r
    ),
    
    # Reflect across vertical axis
    "5" = data.frame(
      row_offset = r,
      col_offset = -c
    ),
    
    # Reflect across horizontal axis
    "6" = data.frame(
      row_offset = -r,
      col_offset = c
    ),
    
    # Reflect across main diagonal
    "7" = data.frame(
      row_offset = c,
      col_offset = r
    ),
    
    # Reflect across anti-diagonal
    "8" = data.frame(
      row_offset = -c,
      col_offset = -r
    )
  )
  
  
  transformed$row_offset <-
    canonical_coord(
      transformed$row_offset
    )
  
  transformed$col_offset <-
    canonical_coord(
      transformed$col_offset
    )
  
  
  transformed
}


# ------------------------------------------------------------
# Convert coordinate set to a deterministic string
#
# Example:
#
# (-2,0), (0,2)
#
# becomes something like:
#
# "-2:0|0:2"
# ------------------------------------------------------------

pattern_key <- function(coords) {
  
  coords <- coords[
    order(
      coords$row_offset,
      coords$col_offset
    ),
  ]
  
  
  paste(
    paste(
      coords$row_offset,
      coords$col_offset,
      sep = ":"
    ),
    collapse = "|"
  )
}


# ------------------------------------------------------------
# Find canonical representation
#
# Generate all 8 transformed versions and choose the
# lexicographically smallest key.
# ------------------------------------------------------------

canonical_key <- function(coords) {
  
  keys <- character(8)
  
  
  for (i in 1:8) {
    
    transformed <- transform_pattern(
      coords,
      transform_id = i
    )
    
    
    keys[i] <- pattern_key(
      transformed
    )
  }
  
  
  min(keys)
}


# ------------------------------------------------------------
# Split raw data into individual patterns
# ------------------------------------------------------------

pattern_list <- split(
  all_patterns,
  all_patterns$pattern_id
)


cat(
  "Patterns to canonicalise:",
  length(pattern_list),
  "\n"
)


# ------------------------------------------------------------
# Canonicalise every pattern
# ------------------------------------------------------------

canonical_results <- vector(
  "list",
  length(pattern_list)
)


for (i in seq_along(pattern_list)) {
  
  if (i %% 1000 == 0) {
    
    cat(
      "Processed",
      i,
      "patterns\n"
    )
  }
  
  
  pattern <- pattern_list[[i]]
  
  
  coords <- pattern[
    ,
    c(
      "row_offset",
      "col_offset"
    )
  ]
  
  
  canonical_results[[i]] <- data.frame(
    
    pattern_id =
      unique(pattern$pattern_id),
    
    n_flippers =
      unique(pattern$n_flippers),
    
    canonical_key =
      canonical_key(coords),
    
    stringsAsFactors = FALSE
  )
}


canonical_patterns <- do.call(
  rbind,
  canonical_results
)


rownames(canonical_patterns) <- NULL


# ------------------------------------------------------------
# Assign family IDs
# ------------------------------------------------------------

unique_keys <- unique(
  canonical_patterns$canonical_key
)


family_lookup <- data.frame(
  
  canonical_key = unique_keys,
  
  family_id = seq_along(unique_keys),
  
  stringsAsFactors = FALSE
)


canonical_patterns <- merge(
  
  canonical_patterns,
  family_lookup,
  
  by = "canonical_key",
  
  all.x = TRUE,
  
  sort = FALSE
)


# Restore sensible order
canonical_patterns <- canonical_patterns[
  order(canonical_patterns$pattern_id),
]


rownames(canonical_patterns) <- NULL


# ------------------------------------------------------------
# Family summary
# ------------------------------------------------------------

family_summary <- aggregate(
  
  pattern_id ~
    family_id +
    n_flippers,
  
  data = canonical_patterns,
  
  FUN = length
)


names(family_summary)[
  names(family_summary) == "pattern_id"
] <- "family_size"


# ------------------------------------------------------------
# Number of unique families by number of F squares
# ------------------------------------------------------------

unique_family_summary <- aggregate(
  
  family_id ~ n_flippers,
  
  data = unique(
    canonical_patterns[
      c(
        "family_id",
        "n_flippers"
      )
    ]
  ),
  
  FUN = length
)


names(unique_family_summary)[2] <-
  "n_unique_families"


# ------------------------------------------------------------
# Print results
# ------------------------------------------------------------

cat(
  "\nRaw patterns:",
  nrow(canonical_patterns),
  "\n"
)


cat(
  "Unique symmetry families:",
  length(unique(canonical_patterns$family_id)),
  "\n\n"
)


print(
  unique_family_summary
)


# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

saveRDS(
  canonical_patterns,
  "analysis/outputs/canonical_patterns.rds"
)


saveRDS(
  family_lookup,
  "analysis/outputs/family_lookup.rds"
)


saveRDS(
  family_summary,
  "analysis/outputs/family_summary.rds"
)


write.csv(
  canonical_patterns,
  "analysis/outputs/canonical_patterns.csv",
  row.names = FALSE
)


write.csv(
  family_summary,
  "analysis/outputs/family_summary.csv",
  row.names = FALSE
)


cat(
  "\nCanonicalised pattern families saved.\n"
)