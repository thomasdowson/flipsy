# ============================================================
# FLIPSY V3 - 10 Puzzle Play-Test
# ============================================================
#
# Purpose:
#
# Test whether our mathematically predicted shape difficulty
# corresponds to actual human performance.
#
# Design:
#
# - Exactly 10 puzzles
# - PAR fixed at 5
# - 2-F V3 shapes only
# - Different family for each puzzle
# - Random orientation
# - Record:
#     family
#     predicted difficulty
#     structural score
#     moves
#     excess moves
#     maximum TO GO reached
#
# ============================================================


library(shiny)


# ============================================================
# Source code
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
n_tests <- 10


# ============================================================
# Select 10 different families
# ============================================================
#
# Random order, but no family repeats during this session.
# ============================================================

test_families <- sample(
  seq_len(n_v3_shapes()),
  n_tests,
  replace = FALSE
)


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
      
      label <- if (value %in% c("S", "F")) {
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
# Generate play-test puzzle
# ============================================================

generate_test_game <- function(test_number) {
  
  family_index <- test_families[test_number]
  
  oriented <- get_random_v3_shape(
    family_index
  )
  
  shape <- oriented$shape
  info <- oriented$info
  
  
  puzzle <- generate_puzzle_v2(
    
    n = grid_size,
    
    shape = shape,
    
    target_moves = target_par
  )
  
  
  list(
    
    board = puzzle$board,
    
    shape = shape,
    
    family_index = family_index,
    
    family_id = info$family_id,
    
    difficulty = info$difficulty,
    
    structural_score = info$structural_score,
    
    orientation = oriented$orientation,
    
    oriented_key = oriented$oriented_key,
    
    par = puzzle$par
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
          max-width: 540px;
          margin: 25px auto;
        }

        .game-title {
          font-size: 34px;
          font-weight: bold;
          letter-spacing: 3px;
        }

        .test-progress {
          margin: 5px 0 18px 0;
          color: #666;
          letter-spacing: 2px;
          font-size: 12px;
        }

        .shape-grid {
          display: grid;
          gap: 3px;
          justify-content: center;
          width: fit-content;
          margin: 12px auto 15px auto;
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

        .stats {
          display: flex;
          justify-content: center;
          gap: 35px;
          margin: 15px 0;
        }

        .stat-label {
          font-size: 10px;
          letter-spacing: 2px;
          color: #666;
        }

        .stat-value {
          font-size: 23px;
          font-weight: bold;
        }

        .game-board {
          display: grid;
          grid-template-columns: repeat(5, 65px);
          gap: 5px;
          justify-content: center;
          margin: 18px auto;
        }

        .square-button {
          width: 65px;
          height: 65px;
          border: 2px solid #555;
          border-radius: 4px;
          padding: 0;
          cursor: pointer;
          outline: none;
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

        .solved-message {
          margin-top: 18px;
          font-size: 20px;
          font-weight: bold;
        }

        .next-button {
          margin-top: 12px;
        }

        .results {
          max-width: 850px;
          margin: 30px auto;
          text-align: left;
        }

      ")
    )
  ),
  
  
  conditionalPanel(
    
    condition = "output.finished == false",
    
    div(
      
      class = "game-container",
      
      div(
        class = "game-title",
        "FLIPSY"
      ),
      
      div(
        class = "test-progress",
        textOutput(
          "progress",
          inline = TRUE
        )
      ),
      
      uiOutput(
        "shape_display"
      ),
      
      div(
        
        class = "stats",
        
        div(
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
        ),
        
        div(
          div(
            class = "stat-label",
            "MOVES"
          ),
          div(
            class = "stat-value",
            textOutput(
              "moves_value",
              inline = TRUE
            )
          )
        )
      ),
      
      uiOutput(
        "game_board"
      ),
      
      uiOutput(
        "solved_message"
      )
    )
  ),
  
  
  conditionalPanel(
    
    condition = "output.finished == true",
    
    div(
      
      class = "results",
      
      h2(
        "Play-test complete"
      ),
      
      p(
        "10 puzzles completed."
      ),
      
      tableOutput(
        "results_table"
      ),
      
      h3(
        "Summary"
      ),
      
      verbatimTextOutput(
        "summary"
      )
    )
  )
)


# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Start first test
  # ----------------------------------------------------------
  
  initial_game <- generate_test_game(1)
  
  
  game <- reactiveValues(
    
    test_number = 1,
    
    board = initial_game$board,
    
    starting_board = initial_game$board,
    
    shape = initial_game$shape,
    
    family_index = initial_game$family_index,
    
    family_id = initial_game$family_id,
    
    difficulty = initial_game$difficulty,
    
    structural_score = initial_game$structural_score,
    
    orientation = initial_game$orientation,
    
    oriented_key = initial_game$oriented_key,
    
    par = initial_game$par,
    
    distance = initial_game$par,
    
    max_distance = initial_game$par,
    
    moves = 0,
    
    solved = FALSE,
    
    finished = FALSE,
    
    results = data.frame()
  )
  
  
  # ==========================================================
  # Finished state
  # ==========================================================
  
  output$finished <- reactive({
    game$finished
  })
  
  outputOptions(
    output,
    "finished",
    suspendWhenHidden = FALSE
  )
  
  
  # ==========================================================
  # Progress
  # ==========================================================
  
  output$progress <- renderText({
    
    paste(
      "PUZZLE",
      game$test_number,
      "OF",
      n_tests
    )
  })
  
  
  # ==========================================================
  # Shape
  # ==========================================================
  
  output$shape_display <- renderUI({
    
    render_shape_v3(
      game$shape
    )
  })
  
  
  # ==========================================================
  # Stats
  # ==========================================================
  
  output$par_value <- renderText({
    game$par
  })
  
  
  output$distance_value <- renderText({
    game$distance
  })
  
  
  output$moves_value <- renderText({
    game$moves
  })
  
  
  # ==========================================================
  # Board
  # ==========================================================
  
  output$game_board <- renderUI({
    
    board <- game$board
    
    
    squares <- lapply(
      
      seq_len(grid_size * grid_size),
      
      function(i) {
        
        row <- ((i - 1) %/% grid_size) + 1
        
        col <- ((i - 1) %% grid_size) + 1
        
        
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
  # Player move
  # ==========================================================
  
  observeEvent(
    input$square_click,
    {
      
      if (
        game$solved ||
        game$finished
      ) {
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
      
      
      # Make move
      game$board <- make_move_v2(
        
        board = game$board,
        
        row = row,
        
        col = col,
        
        shape = game$shape
      )
      
      
      game$moves <- game$moves + 1
      
      
      # Exact distance from current board
      game$distance <- minimum_moves_v2(
        
        board = game$board,
        
        shape = game$shape
      )
      
      
      # Record furthest distance reached
      game$max_distance <- max(
        game$max_distance,
        game$distance
      )
      
      
      # -------------------------------------------------------
      # Solved
      # -------------------------------------------------------
      
      if (
        is_solved_v2(
          game$board
        )
      ) {
        
        game$solved <- TRUE
        
        game$distance <- 0
        
        
        # -----------------------------------------------------
        # Record result
        # -----------------------------------------------------
        
        result <- data.frame(
          
          test_number =
            game$test_number,
          
          family_id =
            game$family_id,
          
          family_index =
            game$family_index,
          
          orientation =
            game$orientation,
          
          predicted_difficulty =
            game$difficulty,
          
          structural_score =
            game$structural_score,
          
          par =
            game$par,
          
          moves =
            game$moves,
          
          excess_moves =
            game$moves - game$par,
          
          max_to_go =
            game$max_distance,
          
          max_drift =
            game$max_distance - game$par,
          
          solved_at_par =
            game$moves == game$par,
          
          stringsAsFactors = FALSE
        )
        
        
        game$results <- rbind(
          game$results,
          result
        )
      }
    },
    
    ignoreInit = TRUE
  )
  
  
  # ==========================================================
  # Solved message / next puzzle
  # ==========================================================
  
  output$solved_message <- renderUI({
    
    if (!game$solved) {
      return(NULL)
    }
    
    
    tagList(
      
      div(
        
        class = "solved-message",
        
        if (
          game$moves == game$par
        ) {
          
          paste(
            "PERFECT —",
            game$moves,
            "MOVES"
          )
          
        } else {
          
          paste(
            "SOLVED —",
            game$moves,
            "MOVES"
          )
        }
      ),
      
      
      if (
        game$test_number < n_tests
      ) {
        
        actionButton(
          "next_test",
          "NEXT PUZZLE",
          class = "next-button"
        )
        
      } else {
        
        actionButton(
          "finish_test",
          "SEE RESULTS",
          class = "next-button"
        )
      }
    )
  })
  
  
  # ==========================================================
  # Next puzzle
  # ==========================================================
  
  observeEvent(
    input$next_test,
    {
      
      next_number <-
        game$test_number + 1
      
      
      new_game <- generate_test_game(
        next_number
      )
      
      
      game$test_number <-
        next_number
      
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
      
      game$structural_score <-
        new_game$structural_score
      
      game$orientation <-
        new_game$orientation
      
      game$oriented_key <-
        new_game$oriented_key
      
      game$par <-
        new_game$par
      
      game$distance <-
        new_game$par
      
      game$max_distance <-
        new_game$par
      
      game$moves <- 0
      
      game$solved <- FALSE
    }
  )
  
  
  # ==========================================================
  # Finish test
  # ==========================================================
  
  observeEvent(
    input$finish_test,
    {
      
      game$finished <- TRUE
    }
  )
  
  
  # ==========================================================
  # Results table
  # ==========================================================
  
  output$results_table <- renderTable({
    
    req(
      game$finished
    )
    
    
    game$results[
      ,
      c(
        "test_number",
        "family_id",
        "predicted_difficulty",
        "par",
        "moves",
        "excess_moves",
        "max_to_go",
        "max_drift"
      )
    ]
  })
  
  
  # ==========================================================
  # Summary
  # ==========================================================
  
  output$summary <- renderPrint({
    
    req(
      game$finished
    )
    
    
    results <- game$results
    
    
    cat(
      "Mean moves:",
      round(
        mean(results$moves),
        2
      ),
      "\n"
    )
    
    
    cat(
      "Mean excess moves:",
      round(
        mean(results$excess_moves),
        2
      ),
      "\n"
    )
    
    
    cat(
      "Solved at PAR:",
      sum(results$solved_at_par),
      "of",
      nrow(results),
      "\n"
    )
    
    
    cat(
      "Largest TO GO:",
      max(results$max_to_go),
      "\n"
    )
    
    
    cat(
      "Largest drift from PAR:",
      max(results$max_drift),
      "\n"
    )
    
    
    # --------------------------------------------------------
    # Correlations
    #
    # With only 10 observations these are exploratory only.
    # --------------------------------------------------------
    
    if (
      length(
        unique(
          results$predicted_difficulty
        )
      ) > 1
    ) {
      
      cat(
        "\nCorrelation:\n"
      )
      
      
      cat(
        "Predicted difficulty vs excess moves:",
        round(
          cor(
            results$predicted_difficulty,
            results$excess_moves,
            method = "spearman"
          ),
          3
        ),
        "\n"
      )
    }
    
    
    if (
      length(
        unique(
          results$structural_score
        )
      ) > 1
    ) {
      
      cat(
        "Structural score vs excess moves:",
        round(
          cor(
            results$structural_score,
            results$excess_moves,
            method = "spearman"
          ),
          3
        ),
        "\n"
      )
    }
  })
}


# ============================================================
# Run
# ============================================================

shinyApp(
  ui = ui,
  server = server
)