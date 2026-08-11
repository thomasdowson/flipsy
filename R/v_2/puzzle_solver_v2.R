# ============================================================
# FLIPSY V2 - Puzzle Solver
# ============================================================


# ------------------------------------------------------------
# Convert board position to vector index
# ------------------------------------------------------------

cell_index_v2 <- function(
    row,
    col,
    n
) {
  
  (row - 1) * n + col
}


# ------------------------------------------------------------
# Build move matrix
#
# Each column represents clicking one square.
#
# Each row represents one board square.
#
# A 1 means that board square flips when that move is made.
# ------------------------------------------------------------

build_move_matrix_v2 <- function(
    n,
    shape
) {
  
  total <- n * n
  
  A <- matrix(
    0L,
    nrow = total,
    ncol = total
  )
  
  
  for (row in seq_len(n)) {
    
    for (col in seq_len(n)) {
      
      move_index <- cell_index_v2(
        row,
        col,
        n
      )
      
      
      affected <- get_flipper_cells_v2(
        row = row,
        col = col,
        shape = shape,
        n = n
      )
      
      
      for (i in seq_len(nrow(affected))) {
        
        affected_index <- cell_index_v2(
          affected$row[i],
          affected$col[i],
          n
        )
        
        
        # Addition modulo 2 is important.
        #
        # If two F positions wrap onto the same square,
        # they cancel because that square flips twice.
        
        A[
          affected_index,
          move_index
        ] <- (
          A[
            affected_index,
            move_index
          ] + 1L
        ) %% 2L
      }
    }
  }
  
  
  A
}


# ------------------------------------------------------------
# Gaussian elimination over GF(2)
#
# Solves:
#
# A x = b  (mod 2)
#
# Returns:
#
# particular = one solution
# null_basis = basis vectors for all alternative solutions
#
# Every solution is:
#
# particular + combination of null_basis vectors
# ------------------------------------------------------------

gf2_solve_v2 <- function(
    A,
    b
) {
  
  A <- A %% 2L
  b <- b %% 2L
  
  m <- nrow(A)
  n <- ncol(A)
  
  
  augmented <- cbind(
    A,
    b
  )
  
  
  pivot_cols <- integer(0)
  
  pivot_row <- 1L
  
  
  # ----------------------------------------------------------
  # Row reduction
  # ----------------------------------------------------------
  
  for (col in seq_len(n)) {
    
    candidates <- which(
      augmented[
        pivot_row:m,
        col
      ] == 1L
    )
    
    
    if (length(candidates) == 0) {
      next
    }
    
    
    selected_row <-
      candidates[1] +
      pivot_row -
      1L
    
    
    # Swap pivot row into position
    if (selected_row != pivot_row) {
      
      temp <- augmented[pivot_row, ]
      
      augmented[pivot_row, ] <-
        augmented[selected_row, ]
      
      augmented[selected_row, ] <-
        temp
    }
    
    
    # Eliminate this column from every other row
    other_rows <- which(
      augmented[, col] == 1L
    )
    
    other_rows <- setdiff(
      other_rows,
      pivot_row
    )
    
    
    if (length(other_rows) > 0) {
      
      for (r in other_rows) {
        
        augmented[r, ] <-
          (
            augmented[r, ] +
              augmented[pivot_row, ]
          ) %% 2L
      }
    }
    
    
    pivot_cols <- c(
      pivot_cols,
      col
    )
    
    
    pivot_row <- pivot_row + 1L
    
    
    if (pivot_row > m) {
      break
    }
  }
  
  
  # ----------------------------------------------------------
  # Check for inconsistency
  #
  # 0 0 0 ... 0 | 1
  #
  # means no solution.
  # ----------------------------------------------------------
  
  coefficient_part <-
    augmented[, seq_len(n), drop = FALSE]
  
  rhs <-
    augmented[, n + 1]
  
  
  zero_rows <- which(
    rowSums(coefficient_part) == 0
  )
  
  
  if (
    length(zero_rows) > 0 &&
    any(rhs[zero_rows] == 1L)
  ) {
    
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # Particular solution
  #
  # Set every free variable to zero.
  # ----------------------------------------------------------
  
  particular <- integer(n)
  
  
  if (length(pivot_cols) > 0) {
    
    for (i in seq_along(pivot_cols)) {
      
      particular[
        pivot_cols[i]
      ] <- augmented[
        i,
        n + 1
      ]
    }
  }
  
  
  # ----------------------------------------------------------
  # Find free variables
  # ----------------------------------------------------------
  
  free_cols <- setdiff(
    seq_len(n),
    pivot_cols
  )
  
  
  # No free variables = unique solution
  if (length(free_cols) == 0) {
    
    return(
      list(
        particular = particular,
        null_basis = matrix(
          integer(0),
          nrow = n,
          ncol = 0
        )
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Construct null-space basis
  # ----------------------------------------------------------
  
  null_basis <- matrix(
    0L,
    nrow = n,
    ncol = length(free_cols)
  )
  
  
  for (j in seq_along(free_cols)) {
    
    free_col <- free_cols[j]
    
    vector <- integer(n)
    
    vector[free_col] <- 1L
    
    
    for (i in seq_along(pivot_cols)) {
      
      pivot_col <- pivot_cols[i]
      
      vector[pivot_col] <-
        augmented[
          i,
          free_col
        ]
    }
    
    
    null_basis[, j] <- vector
  }
  
  
  list(
    particular = particular,
    null_basis = null_basis
  )
}


# ------------------------------------------------------------
# Convert board into target vector
#
# TRUE  = black = already correct
# FALSE = white = needs flipping
# ------------------------------------------------------------

board_target_v2 <- function(board) {
  
  as.integer(
    !as.vector(
      t(board)
    )
  )
}


# ------------------------------------------------------------
# Find minimum solution
# ------------------------------------------------------------

solve_board_v2 <- function(
    board,
    shape
) {
  
  n <- nrow(board)
  
  
  A <- build_move_matrix_v2(
    n = n,
    shape = shape
  )
  
  
  b <- board_target_v2(
    board
  )
  
  
  solution_space <- gf2_solve_v2(
    A,
    b
  )
  
  
  if (is.null(solution_space)) {
    return(NULL)
  }
  
  
  particular <-
    solution_space$particular
  
  null_basis <-
    solution_space$null_basis
  
  
  # ----------------------------------------------------------
  # If solution is unique, we're done
  # ----------------------------------------------------------
  
  if (ncol(null_basis) == 0) {
    
    best_solution <- particular
    
  } else {
    
    # --------------------------------------------------------
    # Search every solution in the affine solution space
    #
    # For our 5x5 game this is practical for the shapes
    # we're using.
    # --------------------------------------------------------
    
    number_free <- ncol(
      null_basis
    )
    
    
    total_combinations <-
      2^number_free
    
    
    best_solution <- NULL
    best_moves <- Inf
    
    
    for (
      combination in
      0:(total_combinations - 1)
    ) {
      
      bits <- as.integer(
        intToBits(combination)
      )[seq_len(number_free)]
      
      
      candidate <- particular
      
      
      selected <- which(
        bits == 1L
      )
      
      
      if (length(selected) > 0) {
        
        addition <- rowSums(
          null_basis[
            ,
            selected,
            drop = FALSE
          ]
        ) %% 2L
        
        
        candidate <-
          (
            candidate +
              addition
          ) %% 2L
      }
      
      
      moves <- sum(candidate)
      
      
      if (moves < best_moves) {
        
        best_moves <- moves
        best_solution <- candidate
      }
    }
  }
  
  
  # ----------------------------------------------------------
  # Convert solution vector into row/column clicks
  # ----------------------------------------------------------
  
  clicked <- which(
    best_solution == 1L
  )
  
  
  if (length(clicked) == 0) {
    
    return(
      data.frame(
        row = integer(0),
        col = integer(0)
      )
    )
  }
  
  
  data.frame(
    
    row =
      ((clicked - 1) %/% n) + 1,
    
    col =
      ((clicked - 1) %% n) + 1
  )
}


# ------------------------------------------------------------
# Minimum number of moves
# ------------------------------------------------------------

minimum_moves_v2 <- function(
    board,
    shape
) {
  
  solution <- solve_board_v2(
    board = board,
    shape = shape
  )
  
  
  if (is.null(solution)) {
    return(NA_integer_)
  }
  
  
  nrow(solution)
}