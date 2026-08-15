# ============================================================
# FLIPSY MVP - Quick Play Engine
# ============================================================
#
# Quick Play rules:
#
# - 5x5 board
# - Exactly 2 Fs
# - Every puzzle has a minimum solution of exactly 5 moves
# - Player chooses Easy / Medium / Hard
# - Random family within selected difficulty
# - Random rotation/reflection
# - TO GO is visible in Quick Play
#
# ============================================================


# ============================================================
# Settings
# ============================================================

quick_grid_size <- 5
quick_target_moves <- 5


# ============================================================
# Difficulty mapping
# ============================================================
#
# V3 currently has provisional difficulty ratings 1-5.
#
# Public MVP collapses these into:
#
# EASY   = 1 or 2
# MEDIUM = 3
# HARD   = 4 or 5
#
# ============================================================

quick_difficulty_levels <- list(
  
  easy = c(1, 2),
  
  medium = 3,
  
  hard = c(4, 5)
)


# ============================================================
# Validate difficulty
# ============================================================

validate_quick_difficulty <- function(difficulty) {
  
  difficulty <- tolower(
    difficulty
  )
  
  
  if (
    !difficulty %in%
    names(quick_difficulty_levels)
  ) {
    
    stop(
      paste(
        "Unknown Quick Play difficulty:",
        difficulty
      )
    )
  }
  
  
  difficulty
}


# ============================================================
# Get eligible family indices
# ============================================================

get_quick_family_indices <- function(difficulty) {
  
  difficulty <- validate_quick_difficulty(
    difficulty
  )
  
  
  allowed_ratings <-
    quick_difficulty_levels[[difficulty]]
  
  
  which(
    v3_shape_families$difficulty %in%
      allowed_ratings
  )
}


# ============================================================
# Choose a family
# ============================================================

choose_quick_family <- function(
    difficulty,
    exclude_family_index = NULL
) {
  
  candidates <- get_quick_family_indices(
    difficulty
  )
  
  
  # Avoid immediate repetition where possible
  if (
    !is.null(exclude_family_index) &&
    length(candidates) > 1
  ) {
    
    candidates <- setdiff(
      candidates,
      exclude_family_index
    )
  }
  
  
  sample(
    candidates,
    1
  )
}


# ============================================================
# Generate Quick Play puzzle
# ============================================================

generate_quick_puzzle <- function(
    difficulty,
    exclude_family_index = NULL
) {
  
  difficulty <- validate_quick_difficulty(
    difficulty
  )
  
  
  # ----------------------------------------------------------
  # Choose family
  # ----------------------------------------------------------
  
  family_index <- choose_quick_family(
    
    difficulty =
      difficulty,
    
    exclude_family_index =
      exclude_family_index
  )
  
  
  # ----------------------------------------------------------
  # Choose random orientation/reflection
  # ----------------------------------------------------------
  
  oriented <- get_random_v3_shape(
    family_index
  )
  
  
  shape <- oriented$shape
  
  info <- oriented$info
  
  
  # ----------------------------------------------------------
  # Generate board whose true minimum solution is 5
  # ----------------------------------------------------------
  
  puzzle <- generate_puzzle_v2(
    
    n =
      quick_grid_size,
    
    shape =
      shape,
    
    target_moves =
      quick_target_moves
  )
  
  
  # ----------------------------------------------------------
  # Verify rather than assume
  # ----------------------------------------------------------
  
  actual_distance <- minimum_moves_v2(
    
    board =
      puzzle$board,
    
    shape =
      shape
  )
  
  
  if (
    actual_distance !=
    quick_target_moves
  ) {
    
    stop(
      paste(
        "Quick Play puzzle generation failed:",
        "expected distance",
        quick_target_moves,
        "but got",
        actual_distance
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Return game state
  # ----------------------------------------------------------
  
  list(
    
    mode =
      "quick",
    
    difficulty =
      difficulty,
    
    board =
      puzzle$board,
    
    starting_board =
      puzzle$board,
    
    shape =
      shape,
    
    family_index =
      family_index,
    
    family_id =
      info$family_id,
    
    predicted_difficulty =
      info$difficulty,
    
    structural_score =
      info$structural_score,
    
    orientation =
      oriented$orientation,
    
    n_orientations =
      oriented$n_orientations,
    
    oriented_key =
      oriented$oriented_key,
    
    optimal_moves =
      quick_target_moves,
    
    moves =
      0,
    
    to_go =
      actual_distance,
    
    solved =
      FALSE
  )
}


# ============================================================
# Make Quick Play move
# ============================================================

make_quick_move <- function(
    game,
    row,
    col
) {
  
  if (game$solved) {
    
    return(
      game
    )
  }
  
  
  # ----------------------------------------------------------
  # Apply move
  # ----------------------------------------------------------
  
  game$board <- make_move_v2(
    
    board =
      game$board,
    
    row =
      row,
    
    col =
      col,
    
    shape =
      game$shape
  )
  
  
  # ----------------------------------------------------------
  # Count move
  # ----------------------------------------------------------
  
  game$moves <-
    game$moves + 1
  
  
  # ----------------------------------------------------------
  # Exact current distance
  # ----------------------------------------------------------
  
  game$to_go <- minimum_moves_v2(
    
    board =
      game$board,
    
    shape =
      game$shape
  )
  
  
  # ----------------------------------------------------------
  # Solved?
  # ----------------------------------------------------------
  
  game$solved <- is_solved_v2(
    game$board
  )
  
  
  if (game$solved) {
    
    game$to_go <- 0
  }
  
  
  game
}


# ============================================================
# Reset Quick Play puzzle
# ============================================================

reset_quick_puzzle <- function(game) {
  
  game$board <-
    game$starting_board
  
  game$moves <- 0
  
  game$to_go <-
    quick_target_moves
  
  game$solved <- FALSE
  
  
  game
}


# ============================================================
# Quick Play result
# ============================================================

quick_result_text <- function(game) {
  
  if (!game$solved) {
    
    return(
      NULL
    )
  }
  
  
  if (
    game$moves ==
    game$optimal_moves
  ) {
    
    return(
      "PERFECT! SOLVED IN 5 MOVES"
    )
  }
  
  
  paste(
    "SOLVED IN",
    game$moves,
    "MOVES"
  )
}


# ============================================================
# Sanity check
# ============================================================

cat(
  "FLIPSY Quick Play engine loaded.\n"
)

cat(
  "Easy families:",
  length(
    get_quick_family_indices("easy")
  ),
  "\n"
)

cat(
  "Medium families:",
  length(
    get_quick_family_indices("medium")
  ),
  "\n"
)

cat(
  "Hard families:",
  length(
    get_quick_family_indices("hard")
  ),
  "\n"
)