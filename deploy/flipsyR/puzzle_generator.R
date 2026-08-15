# ============================================================
# Shape Puzzle - Puzzle Generator
# V1
# ============================================================


# ------------------------------------------------------------
# Generate a puzzle with an exact minimum solution length
#
# The puzzle starts from an all-black board.
# Random moves are applied to scramble it.
#
# The solver then checks the TRUE minimum number of moves
# required to solve the resulting board.
#
# Only puzzles whose minimum solution equals target_moves
# are accepted.
# ------------------------------------------------------------

generate_puzzle <- function(
    n = 5,
    offset = c(2, 1),
    target_moves = 5,
    max_attempts = 10000
) {
  
  for (attempt in seq_len(max_attempts)) {
    
    # Start with solved board
    board <- create_board(n)
    
    
    # --------------------------------------------------------
    # Generate random scramble
    #
    # We use target_moves random clicks.
    # Because moves are reversible, the resulting puzzle
    # is guaranteed to be solvable in AT MOST target_moves.
    # --------------------------------------------------------
    
    rows <- sample(
      seq_len(n),
      target_moves,
      replace = TRUE
    )
    
    cols <- sample(
      seq_len(n),
      target_moves,
      replace = TRUE
    )
    
    
    # Apply scramble moves
    for (i in seq_len(target_moves)) {
      
      board <- make_move(
        board = board,
        row = rows[i],
        col = cols[i],
        offset = offset
      )
    }
    
    
    # --------------------------------------------------------
    # Find TRUE minimum solution
    # --------------------------------------------------------
    
    par <- minimum_moves(
      board = board,
      offset = offset
    )
    
    
    # --------------------------------------------------------
    # Accept only if minimum exactly matches target
    # --------------------------------------------------------
    
    if (!is.na(par) && par == target_moves) {
      
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