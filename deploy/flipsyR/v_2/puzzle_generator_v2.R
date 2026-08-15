# ============================================================
# FLIPSY V2 - Puzzle Generator
# ============================================================


# ------------------------------------------------------------
# Generate puzzle with exact minimum solution length
#
# Starts with a solved all-black board.
#
# We apply target_moves random moves to create a puzzle.
# The solver then checks that the TRUE minimum solution
# is exactly target_moves.
# ------------------------------------------------------------

generate_puzzle_v2 <- function(
    n = 5,
    shape,
    target_moves = 5,
    max_attempts = 10000
) {
  
  for (attempt in seq_len(max_attempts)) {
    
    
    # --------------------------------------------------------
    # Start solved
    # --------------------------------------------------------
    
    board <- create_board_v2(n)
    
    
    # --------------------------------------------------------
    # Choose scramble moves
    #
    # Use different board positions so we don't accidentally
    # click the same square twice and cancel that move.
    # --------------------------------------------------------
    
    total_cells <- n * n
    
    
    selected_cells <- sample(
      seq_len(total_cells),
      size = target_moves,
      replace = FALSE
    )
    
    
    rows <-
      ((selected_cells - 1) %/% n) + 1
    
    
    cols <-
      ((selected_cells - 1) %% n) + 1
    
    
    # --------------------------------------------------------
    # Apply scramble
    # --------------------------------------------------------
    
    for (i in seq_len(target_moves)) {
      
      board <- make_move_v2(
        board = board,
        row = rows[i],
        col = cols[i],
        shape = shape
      )
    }
    
    
    # --------------------------------------------------------
    # Calculate TRUE minimum
    # --------------------------------------------------------
    
    par <- minimum_moves_v2(
      board = board,
      shape = shape
    )
    
    
    # --------------------------------------------------------
    # Accept exact PAR only
    # --------------------------------------------------------
    
    if (
      !is.na(par) &&
      par == target_moves
    ) {
      
      return(
        list(
          
          board = board,
          
          par = par,
          
          scramble = data.frame(
            row = rows,
            col = cols
          ),
          
          attempts = attempt
        )
      )
    }
  }
  
  
  stop(
    paste(
      "Could not generate a",
      target_moves,
      "move puzzle after",
      max_attempts,
      "attempts."
    )
  )
}