# ============================================================
# FLIPSY V2 - Automatic Shape Visualiser
# ============================================================
#
# Converts mathematical S/F offsets into a visual shape.
#
# Rules:
#
# S = anchor / clicked square
# F = flipping square
# X = connecting square
# . = empty
#
# Each F is connected to S using a shortest 8-direction path:
#
# horizontal
# vertical
# diagonal
#
# Paths may overlap and form branches/intersections.
# ============================================================


# ------------------------------------------------------------
# Parse a canonical pattern key
#
# Example:
#
# "-2:0|0:2|2:2"
# ------------------------------------------------------------

parse_pattern_key_v2 <- function(key) {
  
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
# Build shortest grid path from S = (0,0) to one F
#
# Uses diagonal movement whenever both row and column
# still need changing.
#
# Example:
#
# target = (2, 1)
#
# path:
#
# (0,0)
# (1,1)
# (2,1)
# ------------------------------------------------------------

shortest_path_v2 <- function(
    target_row,
    target_col
) {
  
  current_row <- 0L
  current_col <- 0L
  
  
  path <- data.frame(
    row_offset = current_row,
    col_offset = current_col
  )
  
  
  while (
    current_row != target_row ||
    current_col != target_col
  ) {
    
    
    # Row movement
    row_step <- sign(
      target_row - current_row
    )
    
    
    # Column movement
    col_step <- sign(
      target_col - current_col
    )
    
    
    # Move diagonally if possible.
    #
    # Otherwise this naturally becomes a horizontal
    # or vertical step.
    current_row <-
      current_row + row_step
    
    current_col <-
      current_col + col_step
    
    
    path <- rbind(
      path,
      data.frame(
        row_offset = current_row,
        col_offset = current_col
      )
    )
  }
  
  
  rownames(path) <- NULL
  
  path
}


# ------------------------------------------------------------
# Build complete visual network
#
# Each F gets a shortest path from S.
# Paths are combined.
# ------------------------------------------------------------

build_visual_network_v2 <- function(
    f_offsets
) {
  
  network <- data.frame(
    row_offset = 0L,
    col_offset = 0L
  )
  
  
  for (i in seq_len(nrow(f_offsets))) {
    
    path <- shortest_path_v2(
      target_row =
        f_offsets$row_offset[i],
      
      target_col =
        f_offsets$col_offset[i]
    )
    
    
    network <- rbind(
      network,
      path
    )
  }
  
  
  # Remove repeated X/intersection positions
  network <- unique(
    network
  )
  
  
  rownames(network) <- NULL
  
  
  network
}


# ------------------------------------------------------------
# Convert S/F offsets into a display matrix
# ------------------------------------------------------------

offsets_to_shape_matrix_v2 <- function(
    f_offsets
) {
  
  network <- build_visual_network_v2(
    f_offsets
  )
  
  
  # ----------------------------------------------------------
  # Work out required display bounds
  # ----------------------------------------------------------
  
  all_rows <- c(
    network$row_offset,
    f_offsets$row_offset,
    0
  )
  
  all_cols <- c(
    network$col_offset,
    f_offsets$col_offset,
    0
  )
  
  
  min_row <- min(all_rows)
  max_row <- max(all_rows)
  
  min_col <- min(all_cols)
  max_col <- max(all_cols)
  
  
  n_rows <-
    max_row - min_row + 1L
  
  n_cols <-
    max_col - min_col + 1L
  
  
  shape_matrix <- matrix(
    ".",
    nrow = n_rows,
    ncol = n_cols
  )
  
  
  # ----------------------------------------------------------
  # Helper to translate offsets into matrix coordinates
  # ----------------------------------------------------------
  
  display_row <- function(row_offset) {
    
    row_offset -
      min_row +
      1L
  }
  
  
  display_col <- function(col_offset) {
    
    col_offset -
      min_col +
      1L
  }
  
  
  # ----------------------------------------------------------
  # Add network as X
  # ----------------------------------------------------------
  
  for (i in seq_len(nrow(network))) {
    
    r <- display_row(
      network$row_offset[i]
    )
    
    c <- display_col(
      network$col_offset[i]
    )
    
    
    shape_matrix[r, c] <- "X"
  }
  
  
  # ----------------------------------------------------------
  # Add S
  # ----------------------------------------------------------
  
  shape_matrix[
    display_row(0),
    display_col(0)
  ] <- "S"
  
  
  # ----------------------------------------------------------
  # Add Fs last
  #
  # This ensures an F remains visible even if another
  # path happens to pass through it.
  # ----------------------------------------------------------
  
  for (i in seq_len(nrow(f_offsets))) {
    
    r <- display_row(
      f_offsets$row_offset[i]
    )
    
    c <- display_col(
      f_offsets$col_offset[i]
    )
    
    
    shape_matrix[r, c] <- "F"
  }
  
  
  shape_matrix
}


# ------------------------------------------------------------
# Convert canonical key directly into visual matrix
# ------------------------------------------------------------

key_to_shape_matrix_v2 <- function(key) {
  
  f_offsets <- parse_pattern_key_v2(
    key
  )
  
  
  offsets_to_shape_matrix_v2(
    f_offsets
  )
}


# ------------------------------------------------------------
# Convert canonical key into a normal FLIPSY V2 shape object
#
# This means automatically generated shapes can immediately
# be used by:
#
# make_move_v2()
# solve_board_v2()
# generate_puzzle_v2()
# render_shape_v2()
# ------------------------------------------------------------

key_to_shape_v2 <- function(
    key,
    name = NULL
) {
  
  mat <- key_to_shape_matrix_v2(
    key
  )
  
  
  rows <- apply(
    mat,
    1,
    paste0,
    collapse = ""
  )
  
  
  create_shape_v2(
    rows = rows,
    name = name
  )
}