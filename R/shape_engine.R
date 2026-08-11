# ============================================================
# Shape Puzzle - Shape Engine
# ============================================================


# ------------------------------------------------------------
# Create a shape definition from text rows
#
# Allowed symbols:
#
# S = start square, flips
# F = additional flipping square
# X = visible non-flipping shape square
# . = empty
#
# Example:
#
# c(
#   "S.",
#   "X.",
#   "XF"
# )
# ------------------------------------------------------------

create_shape <- function(rows, name = NULL) {
  
  chars <- strsplit(rows, split = "")
  
  widths <- vapply(
    chars,
    length,
    integer(1)
  )
  
  if (length(unique(widths)) != 1) {
    stop("All shape rows must have the same width.")
  }
  
  
  shape_matrix <- do.call(
    rbind,
    chars
  )
  
  
  valid_symbols <- c(
    "S",
    "F",
    "X",
    "."
  )
  
  
  if (!all(shape_matrix %in% valid_symbols)) {
    stop("Shape contains invalid symbols.")
  }
  
  
  start_positions <- which(
    shape_matrix == "S",
    arr.ind = TRUE
  )
  
  
  if (nrow(start_positions) != 1) {
    stop("Each shape must contain exactly one S.")
  }
  
  
  flipper_positions <- which(
    shape_matrix %in% c("S", "F"),
    arr.ind = TRUE
  )
  
  
  visible_positions <- which(
    shape_matrix %in% c("S", "F", "X"),
    arr.ind = TRUE
  )
  
  
  start_row <- start_positions[1, "row"]
  start_col <- start_positions[1, "col"]
  
  
  # Convert all positions into offsets relative to S
  flipper_offsets <- data.frame(
    row_offset =
      flipper_positions[, "row"] - start_row,
    
    col_offset =
      flipper_positions[, "col"] - start_col
  )
  
  
  visible_offsets <- data.frame(
    row_offset =
      visible_positions[, "row"] - start_row,
    
    col_offset =
      visible_positions[, "col"] - start_col,
    
    type = shape_matrix[
      visible_positions
    ]
  )
  
  
  list(
    name = name,
    matrix = shape_matrix,
    flipper_offsets = flipper_offsets,
    visible_offsets = visible_offsets
  )
}


# ------------------------------------------------------------
# Get wrapped board coordinate
# ------------------------------------------------------------

wrap_coordinate <- function(value, n) {
  
  ((value - 1) %% n) + 1
}


# ------------------------------------------------------------
# Get all board squares affected by a shape
#
# row / col = clicked square
# ------------------------------------------------------------

get_flipper_cells <- function(
    row,
    col,
    shape,
    n
) {
  
  offsets <- shape$flipper_offsets
  
  
  data.frame(
    
    row = wrap_coordinate(
      row + offsets$row_offset,
      n
    ),
    
    col = wrap_coordinate(
      col + offsets$col_offset,
      n
    )
  )
}