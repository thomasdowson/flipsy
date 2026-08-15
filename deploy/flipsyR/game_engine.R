# ============================================================
# Shape Puzzle - Game Engine
# ============================================================


# Create a solved board
# TRUE  = black
# FALSE = white
create_board <- function(n = 5) {
  
  matrix(
    TRUE,
    nrow = n,
    ncol = n
  )
}


# ------------------------------------------------------------
# Flip a single square
# ------------------------------------------------------------

flip_square <- function(board, row, col) {
  
  board[row, col] <- !board[row, col]
  
  board
}


# ------------------------------------------------------------
# Find the endpoint of a shape
#
# offset[1] = vertical movement
# offset[2] = horizontal movement
#
# Example:
# c(2, 1) = 2 rows down, 1 column right
#
# Movement wraps around the edges of the board.
# ------------------------------------------------------------

get_endpoint <- function(row, col, offset, n) {
  
  new_row <- ((row - 1 + offset[1]) %% n) + 1
  new_col <- ((col - 1 + offset[2]) %% n) + 1
  
  c(
    row = new_row,
    col = new_col
  )
}


# ------------------------------------------------------------
# Make a move
#
# Flips:
#   1. The clicked square
#   2. The square at the endpoint of the shape
# ------------------------------------------------------------

make_move <- function(board, row, col, offset) {
  
  n <- nrow(board)
  
  endpoint <- get_endpoint(
    row = row,
    col = col,
    offset = offset,
    n = n
  )
  
  # Flip clicked square
  board <- flip_square(
    board,
    row,
    col
  )
  
  # Flip endpoint
  board <- flip_square(
    board,
    endpoint["row"],
    endpoint["col"]
  )
  
  board
}


# ------------------------------------------------------------
# Check whether puzzle is solved
# ------------------------------------------------------------

is_solved <- function(board) {
  
  all(board)
}