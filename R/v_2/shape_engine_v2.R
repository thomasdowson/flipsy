# ============================================================
# FLIPSY V2 - Shape Engine
# ============================================================


# ------------------------------------------------------------
# Create a shape
#
# Symbols:
#
# S = starting/clicked square - flips
# F = additional flipping square
# X = visible shape square - does not flip
# . = empty space
# ------------------------------------------------------------

create_shape_v2 <- function(
    rows,
    name = NULL
) {
  
  # Split each text row into individual characters
  chars <- strsplit(
    rows,
    split = ""
  )
  
  
  # ----------------------------------------------------------
  # Validate row widths
  # ----------------------------------------------------------
  
  widths <- vapply(
    chars,
    length,
    integer(1)
  )
  
  
  if (length(unique(widths)) != 1) {
    
    stop(
      "All shape rows must have the same width."
    )
  }
  
  
  # ----------------------------------------------------------
  # Build shape matrix
  # ----------------------------------------------------------
  
  shape_matrix <- do.call(
    rbind,
    chars
  )
  
  
  # ----------------------------------------------------------
  # Validate symbols
  # ----------------------------------------------------------
  
  valid_symbols <- c(
    "S",
    "F",
    "X",
    "."
  )
  
  
  if (!all(shape_matrix %in% valid_symbols)) {
    
    stop(
      "Shape contains invalid symbols."
    )
  }
  
  
  # ----------------------------------------------------------
  # Find start square
  # ----------------------------------------------------------
  
  start_position <- which(
    shape_matrix == "S",
    arr.ind = TRUE
  )
  
  
  # Force result to remain a matrix
  start_position <- matrix(
    start_position,
    ncol = 2,
    dimnames = list(
      NULL,
      c("row", "col")
    )
  )
  
  
  if (nrow(start_position) != 1) {
    
    stop(
      "Each shape must contain exactly one S."
    )
  }
  
  
  start_row <- start_position[1, "row"]
  start_col <- start_position[1, "col"]
  
  
  # ----------------------------------------------------------
  # Find flipping squares
  #
  # Both S and F flip.
  # ----------------------------------------------------------
  
  flippers <- which(
    shape_matrix == "S" |
      shape_matrix == "F",
    arr.ind = TRUE
  )
  
  
  flippers <- matrix(
    flippers,
    ncol = 2,
    dimnames = list(
      NULL,
      c("row", "col")
    )
  )
  
  
  flipper_offsets <- data.frame(
    
    row_offset =
      flippers[, "row"] -
      start_row,
    
    col_offset =
      flippers[, "col"] -
      start_col
  )
  
  
  # ----------------------------------------------------------
  # Find every visible square
  #
  # S, F and X are visible.
  # ----------------------------------------------------------
  
  visible <- which(
    shape_matrix == "S" |
      shape_matrix == "F" |
      shape_matrix == "X",
    arr.ind = TRUE
  )
  
  
  visible <- matrix(
    visible,
    ncol = 2,
    dimnames = list(
      NULL,
      c("row", "col")
    )
  )
  
  
  visible_types <- vapply(
    
    seq_len(nrow(visible)),
    
    function(i) {
      
      shape_matrix[
        visible[i, "row"],
        visible[i, "col"]
      ]
    },
    
    character(1)
  )
  
  
  visible_offsets <- data.frame(
    
    row_offset =
      visible[, "row"] -
      start_row,
    
    col_offset =
      visible[, "col"] -
      start_col,
    
    type = visible_types,
    
    stringsAsFactors = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Return shape object
  # ----------------------------------------------------------
  
  list(
    
    name = name,
    
    matrix = shape_matrix,
    
    flipper_offsets = flipper_offsets,
    
    visible_offsets = visible_offsets
  )
}


# ------------------------------------------------------------
# Wrap coordinate around board
#
# On a 5x5 board:
#
#  6 -> 1
#  7 -> 2
#  0 -> 5
# -1 -> 4
# ------------------------------------------------------------

wrap_coordinate_v2 <- function(
    value,
    n
) {
  
  ((value - 1) %% n) + 1
}


# ------------------------------------------------------------
# Find all board cells affected by a move
#
# row / col represent the square clicked by the player.
#
# The S position is placed on that square.
# Every F position is then calculated relative to S.
# ------------------------------------------------------------

get_flipper_cells_v2 <- function(
    row,
    col,
    shape,
    n
) {
  
  offsets <- shape$flipper_offsets
  
  data.frame(
    
    row = wrap_coordinate_v2(
      row + offsets$row_offset,
      n
    ),
    
    col = wrap_coordinate_v2(
      col + offsets$col_offset,
      n
    )
  )
}

# ------------------------------------------------------------
# Validate shape for a particular board size
#
# Checks whether two flipping positions become the same
# board position after wrapping.
# ------------------------------------------------------------

validate_shape_v2 <- function(
    shape,
    n = 5
) {
  
  offsets <- shape$flipper_offsets
  
  wrapped <- data.frame(
    
    row = offsets$row_offset %% n,
    
    col = offsets$col_offset %% n
  )
  
  
  duplicated_positions <- duplicated(wrapped)
  
  
  if (any(duplicated_positions)) {
    
    warning(
      paste(
        "Shape",
        shape$name,
        "contains overlapping flippers on a",
        paste0(n, "x", n),
        "board."
      )
    )
    
    return(FALSE)
  }
  
  
  TRUE
}