# ============================================================
# FLIPSY MVP - Daily FLIPSY Engine
# ============================================================
#
# Rules:
#
# - 5x5 board
# - Exactly 2 Fs
# - Every puzzle has an exact 5-move solution
# - Same date = same puzzle for everyone
# - Maximum 15 moves
# - No reset
# - No TO GO shown to player
#
# ============================================================


# ============================================================
# Settings
# ============================================================

daily_grid_size <- 5
daily_target_moves <- 5
daily_move_limit <- 15


# ============================================================
# Date -> deterministic seed
# ============================================================

daily_seed_from_date <- function(date = Sys.Date()) {
  
  date <- as.Date(date)
  
  as.integer(
    format(
      date,
      "%Y%m%d"
    )
  )
}


# ============================================================
# Daily puzzle number
# ============================================================
#
# Temporary launch date.
# We can change this when FLIPSY actually launches.
#
# ============================================================

daily_number <- function(
    date = Sys.Date(),
    launch_date = as.Date("2026-08-15")
) {
  
  date <- as.Date(date)
  
  as.integer(
    date - launch_date
  ) + 1L
}


# ============================================================
# Generate deterministic Daily FLIPSY
# ============================================================

generate_daily_puzzle <- function(
    date = Sys.Date()
) {
  
  date <- as.Date(date)
  
  seed <- daily_seed_from_date(
    date
  )
  
  
  # ----------------------------------------------------------
  # Save existing random-number state
  #
  # This means generating the Daily will not interfere with
  # random Quick Play generation.
  # ----------------------------------------------------------
  
  old_seed_exists <- exists(
    ".Random.seed",
    envir = .GlobalEnv
  )
  
  
  if (old_seed_exists) {
    
    old_seed <- get(
      ".Random.seed",
      envir = .GlobalEnv
    )
  }
  
  
  on.exit({
    
    if (old_seed_exists) {
      
      assign(
        ".Random.seed",
        old_seed,
        envir = .GlobalEnv
      )
      
    } else {
      
      if (
        exists(
          ".Random.seed",
          envir = .GlobalEnv
        )
      ) {
        
        rm(
          ".Random.seed",
          envir = .GlobalEnv
        )
      }
    }
    
  }, add = TRUE)
  
  
  # ----------------------------------------------------------
  # Fix random generation to today's date
  # ----------------------------------------------------------
  
  set.seed(
    seed
  )
  
  
  # ----------------------------------------------------------
  # Select V3 family
  # ----------------------------------------------------------
  
  family_index <- sample(
    seq_len(
      n_v3_shapes()
    ),
    size = 1
  )
  
  
  # ----------------------------------------------------------
  # Get all valid orientations for this family
  # ----------------------------------------------------------
  
  orientations <- get_v3_orientations(
    family_index
  )
  
  
  # ----------------------------------------------------------
  # Select orientation
  # ----------------------------------------------------------
  
  orientation_number <- sample(
    seq_along(
      orientations
    ),
    size = 1
  )
  
  
  # IMPORTANT:
  # Keep this [[ ]] expression on one line.
  
  offsets <- orientations[[orientation_number]]
  
  
  # ----------------------------------------------------------
  # Build orientation key
  # ----------------------------------------------------------
  
  oriented_key <- offset_key_v3(
    offsets
  )
  
  
  # ----------------------------------------------------------
  # Family information
  # ----------------------------------------------------------
  
  info <- get_v3_shape_info(
    family_index
  )
  
  
  # ----------------------------------------------------------
  # Convert offsets into playable shape
  # ----------------------------------------------------------
  
  shape <- key_to_shape_v2(
    
    key = oriented_key,
    
    name = info$shape_name
  )
  
  
  # ----------------------------------------------------------
  # Generate exact 5-move puzzle
  #
  # Because the RNG seed is fixed above, the same date will
  # produce the same board.
  # ----------------------------------------------------------
  
  puzzle <- generate_puzzle_v2(
    
    n = daily_grid_size,
    
    shape = shape,
    
    target_moves = daily_target_moves
  )
  
  
  # ----------------------------------------------------------
  # Independently verify starting distance
  # ----------------------------------------------------------
  
  actual_distance <- minimum_moves_v2(
    
    board = puzzle$board,
    
    shape = shape
  )
  
  
  if (
    actual_distance != daily_target_moves
  ) {
    
    stop(
      paste(
        "Daily generation failed:",
        "expected distance",
        daily_target_moves,
        "but got",
        actual_distance
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Return initial Daily state
  # ----------------------------------------------------------
  
  list(
    
    mode = "daily",
    
    date = date,
    
    daily_number = daily_number(
      date
    ),
    
    seed = seed,
    
    board = puzzle$board,
    
    starting_board = puzzle$board,
    
    shape = shape,
    
    family_index = family_index,
    
    family_id = info$family_id,
    
    predicted_difficulty = info$difficulty,
    
    structural_score = info$structural_score,
    
    orientation = orientation_number,
    
    n_orientations = length(
      orientations
    ),
    
    oriented_key = oriented_key,
    
    optimal_moves = daily_target_moves,
    
    move_limit = daily_move_limit,
    
    moves = 0L,
    
    solved = FALSE,
    
    failed = FALSE,
    
    finished = FALSE
  )
}


# ============================================================
# Make Daily move
# ============================================================

make_daily_move <- function(
    game,
    row,
    col
) {
  
  # ----------------------------------------------------------
  # No moves after game has ended
  # ----------------------------------------------------------
  
  if (
    game$finished ||
    game$solved ||
    game$failed
  ) {
    
    return(
      game
    )
  }
  
  
  # ----------------------------------------------------------
  # Apply move
  # ----------------------------------------------------------
  
  game$board <- make_move_v2(
    
    board = game$board,
    
    row = row,
    
    col = col,
    
    shape = game$shape
  )
  
  
  # ----------------------------------------------------------
  # Increment move counter
  # ----------------------------------------------------------
  
  game$moves <- game$moves + 1L
  
  
  # ----------------------------------------------------------
  # Check for solution
  # ----------------------------------------------------------
  
  game$solved <- is_solved_v2(
    game$board
  )
  
  
  if (game$solved) {
    
    game$finished <- TRUE
    
    return(
      game
    )
  }
  
  
  # ----------------------------------------------------------
  # Check move limit
  # ----------------------------------------------------------
  
  if (
    game$moves >= game$move_limit
  ) {
    
    game$failed <- TRUE
    
    game$finished <- TRUE
  }
  
  
  game
}


# ============================================================
# Daily result text
# ============================================================

daily_result_text <- function(game) {
  
  if (!game$finished) {
    
    return(
      NULL
    )
  }
  
  
  # ----------------------------------------------------------
  # Solved
  # ----------------------------------------------------------
  
  if (game$solved) {
    
    if (
      game$moves == game$optimal_moves
    ) {
      
      return(
        "PERFECT! SOLVED IN 5 MOVES"
      )
    }
    
    
    return(
      paste(
        "SOLVED IN",
        game$moves,
        "MOVES"
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Failed
  # ----------------------------------------------------------
  
  if (game$failed) {
    
    return(
      paste(
        "FAILED —",
        game$move_limit,
        "MOVE LIMIT REACHED"
      )
    )
  }
  
  
  NULL
}

# ============================================================
# Find one optimal solution
# ============================================================
#
# Starting from the current board:
#
# - calculate its exact distance
# - try every possible S position
# - find a move that reduces distance by exactly 1
# - repeat until solved
#
# For a Daily puzzle this should produce:
#
# 5 -> 4 -> 3 -> 2 -> 1 -> 0
#
# ============================================================

find_optimal_solution_v2 <- function(
    board,
    shape
) {
  
  current_board <- board
  
  current_distance <- minimum_moves_v2(
    board = current_board,
    shape = shape
  )
  
  
  # Already solved
  if (current_distance == 0) {
    
    return(
      data.frame(
        move = integer(0),
        row = integer(0),
        col = integer(0)
      )
    )
  }
  
  
  solution_moves <- list()
  
  
  for (move_number in seq_len(current_distance)) {
    
    distance_before <- minimum_moves_v2(
      board = current_board,
      shape = shape
    )
    
    
    move_found <- FALSE
    
    
    # --------------------------------------------------------
    # Try every possible S position
    # --------------------------------------------------------
    
    for (row in seq_len(nrow(current_board))) {
      
      for (col in seq_len(ncol(current_board))) {
        
        candidate_board <- make_move_v2(
          
          board = current_board,
          
          row = row,
          
          col = col,
          
          shape = shape
        )
        
        
        candidate_distance <- minimum_moves_v2(
          
          board = candidate_board,
          
          shape = shape
        )
        
        
        # ----------------------------------------------------
        # An optimal move must reduce distance by exactly 1
        # ----------------------------------------------------
        
        if (
          candidate_distance ==
          distance_before - 1
        ) {
          
          solution_moves[[length(solution_moves) + 1]] <-
            data.frame(
              
              move = move_number,
              
              row = row,
              
              col = col
            )
          
          
          current_board <- candidate_board
          
          move_found <- TRUE
          
          break
        }
      }
      
      
      if (move_found) {
        break
      }
    }
    
    
    # --------------------------------------------------------
    # Safety check
    # --------------------------------------------------------
    
    if (!move_found) {
      
      stop(
        paste(
          "Could not find optimal move",
          move_number,
          "from distance",
          distance_before
        )
      )
    }
  }
  
  
  solution <- do.call(
    rbind,
    solution_moves
  )
  
  
  # ----------------------------------------------------------
  # Verify final board
  # ----------------------------------------------------------
  
  if (
    !is_solved_v2(
      current_board
    )
  ) {
    
    stop(
      "Optimal solution search did not finish on a solved board."
    )
  }
  
  
  solution
}


# ============================================================
# Build solution-board sequence
# ============================================================
#
# Returns the board BEFORE each of the five optimal moves,
# together with the row/column where S should be placed.
#
# Also returns the final solved board.
#
# This is what the UI solution viewer will eventually use.
#
# ============================================================

build_solution_sequence <- function(
    board,
    shape
) {
  
  solution <- find_optimal_solution_v2(
    
    board = board,
    
    shape = shape
  )
  
  
  current_board <- board
  
  steps <- vector(
    "list",
    nrow(solution)
  )
  
  
  for (i in seq_len(nrow(solution))) {
    
    row <- solution$row[i]
    col <- solution$col[i]
    
    
    steps[[i]] <- list(
      
      move = i,
      
      board = current_board,
      
      row = row,
      
      col = col
    )
    
    
    current_board <- make_move_v2(
      
      board = current_board,
      
      row = row,
      
      col = col,
      
      shape = shape
    )
  }
  
  
  list(
    
    moves = solution,
    
    steps = steps,
    
    final_board = current_board
  )
}


# ============================================================
# Attach solution to a Daily game
# ============================================================

add_daily_solution <- function(game) {
  
  solution <- build_solution_sequence(
    
    board = game$starting_board,
    
    shape = game$shape
  )
  
  
  game$optimal_solution <-
    solution$moves
  
  game$solution_steps <-
    solution$steps
  
  game$solution_final_board <-
    solution$final_board
  
  
  game
}

# ============================================================
# Load message
# ============================================================

cat(
  "FLIPSY Daily engine loaded.\n"
)