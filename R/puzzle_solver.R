# ============================================================
# Shape Puzzle - Puzzle Solver
# ============================================================


# ------------------------------------------------------------
# Convert board to binary vector
#
# Black = 0
# White = 1
# ------------------------------------------------------------

board_to_vector <- function(board) {
  
  as.integer(!as.vector(t(board)))
}


# ------------------------------------------------------------
# Build move matrix
#
# Each column represents clicking one square.
# Each row represents a square affected by that click.
# ------------------------------------------------------------

build_move_matrix <- function(n, offset) {
  
  total_squares <- n * n
  
  A <- matrix(
    0L,
    nrow = total_squares,
    ncol = total_squares
  )
  
  for (row in seq_len(n)) {
    
    for (col in seq_len(n)) {
      
      move_number <- (row - 1) * n + col
      
      endpoint <- get_endpoint(
        row = row,
        col = col,
        offset = offset,
        n = n
      )
      
      clicked_number <- move_number
      
      endpoint_number <-
        (endpoint["row"] - 1) * n +
        endpoint["col"]
      
      # Clicked square flips
      A[clicked_number, move_number] <-
        (A[clicked_number, move_number] + 1L) %% 2L
      
      # Endpoint flips
      A[endpoint_number, move_number] <-
        (A[endpoint_number, move_number] + 1L) %% 2L
    }
  }
  
  A
}


# ------------------------------------------------------------
# Row-reduce augmented matrix over GF(2)
#
# Returns:
#   reduced matrix
#   pivot columns
#   free columns
#
# Returns NULL if the system is inconsistent.
# ------------------------------------------------------------

reduce_gf2 <- function(A, b) {
  
  A <- A %% 2L
  b <- b %% 2L
  
  augmented <- cbind(A, b)
  
  n_rows <- nrow(A)
  n_cols <- ncol(A)
  
  pivot_row <- 1L
  pivot_cols <- integer(0)
  
  for (col in seq_len(n_cols)) {
    
    if (pivot_row > n_rows) {
      break
    }
    
    candidates <- which(
      augmented[pivot_row:n_rows, col] == 1L
    )
    
    if (length(candidates) == 0) {
      next
    }
    
    selected_row <- pivot_row + candidates[1] - 1L
    
    # Swap selected row into pivot position
    if (selected_row != pivot_row) {
      
      temp <- augmented[pivot_row, ]
      
      augmented[pivot_row, ] <-
        augmented[selected_row, ]
      
      augmented[selected_row, ] <- temp
    }
    
    # Eliminate pivot column from all other rows
    for (row in seq_len(n_rows)) {
      
      if (
        row != pivot_row &&
        augmented[row, col] == 1L
      ) {
        
        augmented[row, ] <-
          (augmented[row, ] +
             augmented[pivot_row, ]) %% 2L
      }
    }
    
    pivot_cols <- c(
      pivot_cols,
      col
    )
    
    pivot_row <- pivot_row + 1L
  }
  
  
  # ----------------------------------------------------------
  # Check for contradiction:
  #
  # 0 0 0 ... 0 | 1
  # ----------------------------------------------------------
  
  for (row in seq_len(n_rows)) {
    
    if (
      all(augmented[row, seq_len(n_cols)] == 0L) &&
      augmented[row, n_cols + 1L] == 1L
    ) {
      
      return(NULL)
    }
  }
  
  
  free_cols <- setdiff(
    seq_len(n_cols),
    pivot_cols
  )
  
  
  list(
    augmented = augmented,
    pivot_cols = pivot_cols,
    free_cols = free_cols
  )
}


# ------------------------------------------------------------
# Construct a solution from chosen free variables
# ------------------------------------------------------------

construct_solution <- function(
    reduced,
    free_values
) {
  
  augmented <- reduced$augmented
  pivot_cols <- reduced$pivot_cols
  free_cols <- reduced$free_cols
  
  n_cols <- ncol(augmented) - 1L
  
  solution <- integer(n_cols)
  
  
  # Set free variables
  if (length(free_cols) > 0) {
    
    solution[free_cols] <- free_values
  }
  
  
  # Calculate pivot variables
  if (length(pivot_cols) > 0) {
    
    for (i in seq_along(pivot_cols)) {
      
      pivot_col <- pivot_cols[i]
      
      rhs <- augmented[i, n_cols + 1L]
      
      contribution <- sum(
        augmented[i, seq_len(n_cols)] *
          solution
      ) %% 2L
      
      solution[pivot_col] <-
        (rhs + contribution) %% 2L
    }
  }
  
  solution
}


# ------------------------------------------------------------
# Find minimum-move solution
#
# Enumerates all possible assignments to free variables and
# chooses the solution containing the fewest clicks.
# ------------------------------------------------------------

solve_minimum_gf2 <- function(A, b) {
  
  reduced <- reduce_gf2(
    A = A,
    b = b
  )
  
  if (is.null(reduced)) {
    return(NULL)
  }
  
  number_free <- length(
    reduced$free_cols
  )
  
  
  # ----------------------------------------------------------
  # Unique solution
  # ----------------------------------------------------------
  
  if (number_free == 0) {
    
    return(
      construct_solution(
        reduced = reduced,
        free_values = integer(0)
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Enumerate all free-variable combinations
  # ----------------------------------------------------------
  
  number_combinations <- 2^number_free
  
  best_solution <- NULL
  best_moves <- Inf
  
  
  for (combination in 0:(number_combinations - 1)) {
    
    free_values <- as.integer(
      intToBits(combination)
    )[seq_len(number_free)]
    
    solution <- construct_solution(
      reduced = reduced,
      free_values = free_values
    )
    
    moves <- sum(solution)
    
    
    if (moves < best_moves) {
      
      best_solution <- solution
      best_moves <- moves
    }
  }
  
  
  best_solution
}


# ------------------------------------------------------------
# Solve board using minimum number of moves
# ------------------------------------------------------------

solve_board <- function(board, offset) {
  
  n <- nrow(board)
  
  A <- build_move_matrix(
    n = n,
    offset = offset
  )
  
  b <- board_to_vector(board)
  
  solution <- solve_minimum_gf2(
    A = A,
    b = b
  )
  
  
  if (is.null(solution)) {
    return(NULL)
  }
  
  
  clicked <- which(
    solution == 1L
  )
  
  
  data.frame(
    row = ((clicked - 1L) %/% n) + 1L,
    col = ((clicked - 1L) %% n) + 1L
  )
}


# ------------------------------------------------------------
# Return minimum number of moves required
# ------------------------------------------------------------

minimum_moves <- function(board, offset) {
  
  solution <- solve_board(
    board = board,
    offset = offset
  )
  
  if (is.null(solution)) {
    return(Inf)
  }
  
  nrow(solution)
}