# ============================================================
# FLIPSY V3
# ============================================================
#
# Core rules:
#
# - 5x5 board
# - Exactly 2 Fs per shape
# - Random family
# - Random rotation/reflection
# - Exact PAR puzzle generation
# - Live exact TO GO value after every move
# - Separate shape difficulty rating
#
# ============================================================


library(shiny)


# ============================================================
# Source game code
# ============================================================

source("R/v_2/shape_engine_v2.R")
source("R/v_2/game_engine_v2.R")
source("R/v_2/puzzle_solver_v2.R")
source("R/v_2/puzzle_generator_v2.R")
source("R/v_2/shape_visualiser_v2.R")
source("R/v_3_shapes.R")


# ============================================================
# Settings
# ============================================================

grid_size <- 5

target_par <- 5


# ============================================================
# Shape renderer
# ============================================================

render_shape_v3 <- function(shape) {
  
  mat <- shape$matrix
  
  cells <- list()
  
  
  for (row in seq_len(nrow(mat))) {
    
    for (col in seq_len(ncol(mat))) {
      
      value <- mat[row, col]
      
      
      if (value == ".") {
        next
      }
      
      
      label <- if (
        value %in% c("S", "F")
      ) {
        
        value
        
      } else {
        
        ""
      }
      
      
      cells[[length(cells) + 1]] <- div(
        
        class = "shape-block",
        
        style = sprintf(
          "grid-row:%s; grid-column:%s;",
          row,
          col
        ),
        
        label
      )
    }
  }
  
  
  div(
    
    class = "shape-grid",
    
    style = sprintf(
      "
      grid-template-rows: repeat(%s, 24px);
      grid-template-columns: repeat(%s, 24px);
      ",
      nrow(mat),
      ncol(mat)
    ),
    
    cells
  )
}


# ============================================================
# Generate one V3 game
# ============================================================

generate_game_v3 <- function(
    previous_family_index = NULL
) {
  
  # ----------------------------------------------------------
  # Choose family
  #
  # Prevent immediate repeat.
  # ----------------------------------------------------------
  
  family_index <- random_v3_shape_index(
    exclude = previous_family_index
  )
  
  
  # ----------------------------------------------------------
  # Random orientation
  # ----------------------------------------------------------
  
  oriented <- get_random_v3_shape(
    family_index
  )
  
  
  shape <- oriented$shape
  
  info <- oriented$info
  
  
  # ----------------------------------------------------------
  # Generate exact PAR board
  # ----------------------------------------------------------
  
  puzzle <- generate_puzzle_v2(
    
    n = grid_size,
    
    shape = shape,
    
    target_moves = target_par
  )
  
  
  # ----------------------------------------------------------
  # Return everything needed by app
  # ----------------------------------------------------------
  
  list(
    
    board =
      puzzle$board,
    
    shape =
      shape,
    
    family_index =
      family_index,
    
    family_id =
      info$family_id,
    
    difficulty =
      info$difficulty,
    
    structural_score =
      info$structural_score,
    
    orientation =
      oriented$orientation,
    
    n_orientations =
      oriented$n_orientations,
    
    oriented_key =
      oriented$oriented_key,
    
    par =
      puzzle$par
  )
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$style(
      HTML("

        body {
          background: white;
          color: #111;
          font-family: Arial, sans-serif;
          text-align: center;
        }


        .game-container {
          max-width: 520px;
          margin: 30px auto;
        }


        .game-title {
          font-size: 36px;
          font-weight: bold;
          letter-spacing: 3px;
          margin-bottom: 5px;
        }


        .instruction {
          font-size: 16px;
          color: #444;
          margin-bottom: 24px;
        }


        .shape-label {
          font-size: 12px;
          letter-spacing: 3px;
          margin-bottom: 10px;
        }


        .shape-grid {
          display: grid;
          gap: 3px;
          justify-content: center;
          width: fit-content;
          margin: 0 auto 15px auto;
        }


        .shape-block {
          width: 22px;
          height: 22px;

          background: #111;
          color: white;

          border-radius: 2px;

          display: flex;
          align-items: center;
          justify-content: center;

          font-size: 12px;
          font-weight: bold;
        }


        .difficulty {
          font-size: 14px;
          letter-spacing: 1px;
          margin-bottom: 14px;
          color: #555;
        }


        .difficulty-stars {
          font-size: 16px;
          letter-spacing: 2px;
          color: #111;
        }


        .top-stats {
          display: flex;
          justify-content: center;
          gap: 34px;
          margin: 12px 0 18px 0;
        }


        .stat-box {
          min-width: 80px;
        }


        .stat-label {
          font-size: 11px;
          letter-spacing: 2px;
          color: #666;
          margin-bottom: 2px;
        }


        .stat-value {
          font-size: 24px;
          font-weight: bold;
        }


        .game-board {
          display: grid;
          grid-template-columns: repeat(5, 65px);
          gap: 5px;
          justify-content: center;
          margin: 15px auto 20px auto;
        }


        .square-button {
          width: 65px;
          height: 65px;

          border: 2px solid #555;
          border-radius: 4px;

          padding: 0;

          cursor: pointer;

          outline: none;
          box-shadow: none;
        }


        .black-square {
          background: #111;
        }


        .white-square {
          background: #f7f7f7;
        }


        .black-square:hover,
        .black-square:focus,
        .black-square:active {
          background: #111;
        }


        .white-square:hover,
        .white-square:focus,
        .white-square:active {
          background: #f7f7f7;
        }


        .square-button:hover {
          border-color: #000;
        }


        .move-row {
          margin-top: 8px;
          margin-bottom: 18px;
        }


        .move-label {
          font-size: 11px;
          letter-spacing: 2px;
          color: #666;
        }


        .move-value {
          font-size: 22px;
          font-weight: bold;
        }


        .controls {
          margin-top: 10px;
        }


        .controls .btn {
          margin: 0 5px;

          background: white;
          color: #111;

          border: 1px solid #111;
          border-radius: 4px;

          min-width: 80px;
        }


        .controls .btn:hover {
          background: #111;
          color: white;
        }


        .solved-message {
          margin-top: 18px;
          font-size: 22px;
          font-weight: bold;
        }


        .perfect {
          font-size: 14px;
          color: #555;
          margin-top: 4px;
        }

      ")
    )
  ),
  
  
  div(
    
    class = "game-container",
    
    
    div(
      class = "game-title",
      "FLIPSY"
    ),
    
    
    div(
      class = "instruction",
      "Turn all squares black"
    ),
    
    
    div(
      class = "shape-label",
      "SHAPE"
    ),
    
    
    uiOutput(
      "shape_display"
    ),
    
    
    uiOutput(
      "difficulty_display"
    ),
    
    
    div(
      
      class = "top-stats",
      
      
      div(
        
        class = "stat-box",
        
        div(
          class = "stat-label",
          "PAR"
        ),
        
        div(
          class = "stat-value",
          textOutput(
            "par_value",
            inline = TRUE
          )
        )
      ),
      
      
      div(
        
        class = "stat-box",
        
        div(
          class = "stat-label",
          "TO GO"
        ),
        
        div(
          class = "stat-value",
          textOutput(
            "distance_value",
            inline = TRUE
          )
        )
      )
    ),
    
    
    uiOutput(
      "game_board"
    ),
    
    
    div(
      
      class = "move-row",
      
      div(
        class = "move-label",
        "MOVES"
      ),
      
      div(
        class = "move-value",
        textOutput(
          "moves_value",
          inline = TRUE
        )
      )
    ),
    
    
    div(
      
      class = "controls",
      
      actionButton(
        "reset",
        "RESET"
      ),
      
      actionButton(
        "new_puzzle",
        "NEW"
      )
    ),
    
    
    uiOutput(
      "solved_message"
    )
  )
)


# ============================================================
# Server
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # Initial game
  # ==========================================================
  
  initial_game <- generate_game_v3()
  
  
  game <- reactiveValues(
    
    board =
      initial_game$board,
    
    starting_board =
      initial_game$board,
    
    shape =
      initial_game$shape,
    
    family_index =
      initial_game$family_index,
    
    family_id =
      initial_game$family_id,
    
    difficulty =
      initial_game$difficulty,
    
    orientation =
      initial_game$orientation,
    
    par =
      initial_game$par,
    
    distance =
      initial_game$par,
    
    moves = 0,
    
    solved = FALSE
  )
  
  
  # ==========================================================
  # Shape display
  # ==========================================================
  
  output$shape_display <- renderUI({
    
    render_shape_v3(
      game$shape
    )
  })
  
  
  # ==========================================================
  # Difficulty display
  # ==========================================================
  
  output$difficulty_display <- renderUI({
    
    stars <- paste0(
      rep(
        "★",
        game$difficulty
      ),
      collapse = ""
    )
    
    
    empty <- paste0(
      rep(
        "☆",
        5 - game$difficulty
      ),
      collapse = ""
    )
    
    
    div(
      
      class = "difficulty",
      
      span(
        "DIFFICULTY "
      ),
      
      span(
        class = "difficulty-stars",
        paste0(
          stars,
          empty
        )
      )
    )
  })
  
  
  # ==========================================================
  # PAR
  # ==========================================================
  
  output$par_value <- renderText({
    
    game$par
  })
  
  
  # ==========================================================
  # Live distance
  # ==========================================================
  
  output$distance_value <- renderText({
    
    game$distance
  })
  
  
  # ==========================================================
  # Moves
  # ==========================================================
  
  output$moves_value <- renderText({
    
    game$moves
  })
  
  
  # ==========================================================
  # Board
  # ==========================================================
  
  output$game_board <- renderUI({
    
    board <- game$board
    
    
    squares <- lapply(
      
      seq_len(
        grid_size *
          grid_size
      ),
      
      function(i) {
        
        row <-
          ((i - 1) %/% grid_size) + 1
        
        
        col <-
          ((i - 1) %% grid_size) + 1
        
        
        colour_class <- if (
          board[row, col]
        ) {
          
          "black-square"
          
        } else {
          
          "white-square"
        }
        
        
        tags$button(
          
          type = "button",
          
          class = paste(
            "square-button",
            colour_class
          ),
          
          onclick = sprintf(
            "
            Shiny.setInputValue(
              'square_click',
              '%s',
              {priority: 'event'}
            );
            ",
            paste(
              row,
              col,
              sep = "_"
            )
          )
        )
      }
    )
    
    
    div(
      class = "game-board",
      squares
    )
  })
  
  
  # ==========================================================
  # Square click
  # ==========================================================
  
  observeEvent(
    input$square_click,
    {
      
      if (game$solved) {
        return()
      }
      
      
      coords <- strsplit(
        input$square_click,
        "_"
      )[[1]]
      
      
      row <- as.integer(
        coords[1]
      )
      
      
      col <- as.integer(
        coords[2]
      )
      
      
      # -------------------------------------------------------
      # Make move
      # -------------------------------------------------------
      
      new_board <- make_move_v2(
        
        board =
          game$board,
        
        row =
          row,
        
        col =
          col,
        
        shape =
          game$shape
      )
      
      
      game$board <- new_board
      
      
      # -------------------------------------------------------
      # Count move
      # -------------------------------------------------------
      
      game$moves <-
        game$moves + 1
      
      
      # -------------------------------------------------------
      # Recalculate EXACT minimum moves remaining
      # -------------------------------------------------------
      
      game$distance <- minimum_moves_v2(
        
        board =
          game$board,
        
        shape =
          game$shape
      )
      
      
      # -------------------------------------------------------
      # Solved?
      # -------------------------------------------------------
      
      if (
        is_solved_v2(
          game$board
        )
      ) {
        
        game$solved <- TRUE
        
        game$distance <- 0
      }
    },
    
    ignoreInit = TRUE
  )
  
  
  # ==========================================================
  # Reset
  # ==========================================================
  
  observeEvent(
    input$reset,
    {
      
      game$board <-
        game$starting_board
      
      game$moves <- 0
      
      game$distance <-
        game$par
      
      game$solved <- FALSE
    }
  )
  
  
  # ==========================================================
  # New puzzle
  # ==========================================================
  
  observeEvent(
    input$new_puzzle,
    {
      
      new_game <- generate_game_v3(
        
        previous_family_index =
          game$family_index
      )
      
      
      game$board <-
        new_game$board
      
      game$starting_board <-
        new_game$board
      
      game$shape <-
        new_game$shape
      
      game$family_index <-
        new_game$family_index
      
      game$family_id <-
        new_game$family_id
      
      game$difficulty <-
        new_game$difficulty
      
      game$orientation <-
        new_game$orientation
      
      game$par <-
        new_game$par
      
      game$distance <-
        new_game$par
      
      game$moves <- 0
      
      game$solved <- FALSE
    }
  )
  
  
  # ==========================================================
  # Solved message
  # ==========================================================
  
  output$solved_message <- renderUI({
    
    if (!game$solved) {
      return(NULL)
    }
    
    
    if (
      game$moves ==
      game$par
    ) {
      
      div(
        
        class = "solved-message",
        
        "PERFECT!",
        
        div(
          class = "perfect",
          paste(
            "Solved in PAR",
            game$par
          )
        )
      )
      
    } else {
      
      div(
        
        class = "solved-message",
        
        paste(
          "SOLVED IN",
          game$moves,
          "MOVES"
        ),
        
        div(
          class = "perfect",
          paste(
            game$moves - game$par,
            "over PAR"
          )
        )
      )
    }
  })
}


# ============================================================
# Run app
# ============================================================

shinyApp(
  ui = ui,
  server = server
)