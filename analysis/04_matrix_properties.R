# ============================================================
# FLIPSY - Shape Analysis
# 04 Matrix Properties
# ============================================================
#
# For every unique S/F family:
#
# - reconstruct canonical F offsets
# - include S at offset (0,0)
# - build the 25 x 25 move matrix over GF(2)
# - calculate GF(2) rank
# - calculate nullity
# - calculate number of reachable board states
# - calculate number of solutions per reachable board
#
# Board size is fixed at 5x5.
# ============================================================


# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

board_size <- 5


# ------------------------------------------------------------
# Load structural family data
# ------------------------------------------------------------

family_metrics <- readRDS(
  "analysis/outputs/family_structural_metrics.rds"
)


cat(
  "Families to analyse:",
  nrow(family_metrics),
  "\n"
)


# ============================================================
# Helpers
# ============================================================


# ------------------------------------------------------------
# Parse canonical key
# ------------------------------------------------------------

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


# ------------------------------------------------------------
# Wrap board coordinate into 1:n
# ------------------------------------------------------------

wrap_index <- function(
    x,
    n = board_size
) {
  
  ((x - 1) %% n) + 1
}


# ------------------------------------------------------------
# Convert row/column coordinate to vector index
#
# Row-major ordering:
#
# (1,1) -> 1
# (1,2) -> 2
# ...
# (5,5) -> 25
# ------------------------------------------------------------

cell_index <- function(
    row,
    col,
    n = board_size
) {
  
  (row - 1) * n + col
}


# ------------------------------------------------------------
# Build move matrix
#
# Each column = one possible click.
# Each row    = one board square.
#
# Matrix entry = 1 if that board square flips.
# ------------------------------------------------------------

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
  
  
  # Include S itself
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
# GF(2) rank via Gaussian elimination
# ------------------------------------------------------------

gf2_rank <- function(A) {
  
  A <- A %% 2L
  
  n_rows <- nrow(A)
  n_cols <- ncol(A)
  
  pivot_row <- 1L
  rank <- 0L
  
  
  for (col in seq_len(n_cols)) {
    
    if (pivot_row > n_rows) {
      break
    }
    
    
    candidates <- which(
      A[
        pivot_row:n_rows,
        col
      ] == 1L
    )
    
    
    if (length(candidates) == 0) {
      next
    }
    
    
    selected_row <-
      pivot_row +
      candidates[1] -
      1L
    
    
    # Swap
    if (selected_row != pivot_row) {
      
      temp <- A[pivot_row, ]
      
      A[pivot_row, ] <-
        A[selected_row, ]
      
      A[selected_row, ] <-
        temp
    }
    
    
    # Eliminate below
    if (pivot_row < n_rows) {
      
      for (r in (pivot_row + 1L):n_rows) {
        
        if (A[r, col] == 1L) {
          
          A[r, ] <-
            (
              A[r, ] +
                A[pivot_row, ]
            ) %% 2L
        }
      }
    }
    
    
    rank <- rank + 1L
    pivot_row <- pivot_row + 1L
  }
  
  
  rank
}


# ------------------------------------------------------------
# Count distinct row patterns in A
#
# Not fundamental algebraically, but potentially useful
# as another descriptor of move interaction structure.
# ------------------------------------------------------------

count_distinct_rows <- function(A) {
  
  keys <- apply(
    A,
    1,
    paste0,
    collapse = ""
  )
  
  length(
    unique(keys)
  )
}


# ------------------------------------------------------------
# Count distinct column patterns
# ------------------------------------------------------------

count_distinct_columns <- function(A) {
  
  keys <- apply(
    A,
    2,
    paste0,
    collapse = ""
  )
  
  length(
    unique(keys)
  )
}


# ============================================================
# Analyse families
# ============================================================

results <- vector(
  "list",
  nrow(family_metrics)
)


for (i in seq_len(nrow(family_metrics))) {
  
  
  if (i %% 250 == 0) {
    
    cat(
      "Processed",
      i,
      "families\n"
    )
  }
  
  
  family_id <-
    family_metrics$family_id[i]
  
  key <-
    family_metrics$canonical_key[i]
  
  
  f_offsets <- parse_pattern_key(
    key
  )
  
  
  A <- build_move_matrix(
    f_offsets = f_offsets,
    n = board_size
  )
  
  
  rank <- gf2_rank(A)
  
  nullity <-
    ncol(A) - rank
  
  
  # Exact integers become large quickly,
  # so retain both log2 values and numeric values.
  reachable_states_log2 <- rank
  
  solutions_per_reachable_log2 <-
    nullity
  
  
  reachable_states <-
    2^rank
  
  solutions_per_reachable <-
    2^nullity
  
  
  results[[i]] <- data.frame(
    
    family_id = family_id,
    
    gf2_rank = rank,
    
    nullity = nullity,
    
    full_rank = rank == 25,
    
    reachable_states_log2 =
      reachable_states_log2,
    
    reachable_states =
      reachable_states,
    
    solutions_per_reachable_log2 =
      solutions_per_reachable_log2,
    
    solutions_per_reachable =
      solutions_per_reachable,
    
    distinct_move_rows =
      count_distinct_rows(A),
    
    distinct_move_columns =
      count_distinct_columns(A),
    
    stringsAsFactors = FALSE
  )
}


# ============================================================
# Combine
# ============================================================

matrix_properties <- do.call(
  rbind,
  results
)


rownames(matrix_properties) <- NULL


# ------------------------------------------------------------
# Join structural metrics
# ------------------------------------------------------------

family_analysis <- merge(
  
  family_metrics,
  
  matrix_properties,
  
  by = "family_id",
  
  all.x = TRUE,
  
  sort = FALSE
)


family_analysis <- family_analysis[
  order(family_analysis$family_id),
]


rownames(family_analysis) <- NULL


# ============================================================
# Summaries
# ============================================================

cat(
  "\nGF(2) rank distribution:\n"
)


print(
  table(
    family_analysis$gf2_rank
  )
)


cat(
  "\nNullity distribution:\n"
)


print(
  table(
    family_analysis$nullity
  )
)


cat(
  "\nFull-rank families:",
  sum(family_analysis$full_rank),
  "of",
  nrow(family_analysis),
  "\n"
)


cat(
  "\nRank by number of F squares:\n"
)


print(
  with(
    family_analysis,
    table(
      n_flippers,
      gf2_rank
    )
  )
)


# ============================================================
# Save
# ============================================================

saveRDS(
  matrix_properties,
  "analysis/outputs/matrix_properties.rds"
)


saveRDS(
  family_analysis,
  "analysis/outputs/family_analysis.rds"
)


write.csv(
  matrix_properties,
  "analysis/outputs/matrix_properties.csv",
  row.names = FALSE
)


write.csv(
  family_analysis,
  "analysis/outputs/family_analysis.csv",
  row.names = FALSE
)


cat(
  "\nMatrix properties saved.\n"
)