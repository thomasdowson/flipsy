# ============================================================
# FLIPSY V2 - Game Engine
# ============================================================


# ------------------------------------------------------------
# Create solved board
#
# TRUE  = black
# FALSE = white
# ------------------------------------------------------------

create_board_v2 <- function(n = 5) {
  
  matrix(
    TRUE,
    nrow = n,
    ncol = n
  )
}


# ------------------------------------------------------------
# Flip one square
# ------------------------------------------------------------

flip_square_v2 <- function(
    board,
    row,
    col
) {
  
  board[row, col] <-
    !board[row, col]
  
  board
}


# ------------------------------------------------------------
# Make a move
#
# The clicked square corresponds to S.
#
# S and every F flip.
# X squares are visual only.
#
# The whole shape wraps around the board.
# ------------------------------------------------------------

make_move_v2 <- function(
    board,
    row,
    col,
    shape
) {
  
  n <- nrow(board)
  
  
  affected <- get_flipper_cells_v2(
    row = row,
    col = col,
    shape = shape,
    n = n
  )
  
  
  for (i in seq_len(nrow(affected))) {
    
    board <- flip_square_v2(
      board = board,
      row = affected$row[i],
      col = affected$col[i]
    )
  }
  
  
  board
}


# ------------------------------------------------------------
# Check whether puzzle is solved
# ------------------------------------------------------------

is_solved_v2 <- function(board) {
  
  all(board)
}