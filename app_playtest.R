# ============================================================
# FLIPSY - Blind Difficulty Play Test
# ============================================================

library(shiny)

source("R/v_2/shape_engine_v2.R")
source("R/v_2/game_engine_v2.R")
source("R/v_2/puzzle_solver_v2.R")
source("R/v_2/puzzle_generator_v2.R")
source("R/v_2/shape_visualiser_v2.R")


# ============================================================
# Settings
# ============================================================

grid_size <- 5
target_par <- 5

results_file <- "analysis/outputs/playtest_results.csv"


# ============================================================
# Load play-test shapes
# ============================================================

playtest_shapes <- readRDS(
  "analysis/outputs/playtest_shapes.rds"
)

playtest_shapes <- playtest_shapes[
  order(playtest_shapes$playtest_order),
]


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
          font-size: 34px;
          font-weight: bold;
          letter-spacing: 2px;
          margin-bottom: 5px;
        }

        .progress {
          font-size: 14px;
          margin-bottom: 20px;
          background: none;
          box-shadow: none;
          height: auto;
        }

        .shape-label {
          font-size: 12px;
          letter-spacing: 3px;
          margin-top: 20px;
          margin-bottom: 10px;
        }

        .shape-display {
          display: inline-grid;
          gap: 3px;
          margin: 5px auto 18px auto;
        }

        .shape-cell {
          width: 28px;
          height: 28px;

          display: flex;
          align-items: center;
          justify-content: center;

          font-size: 13px;
          font-weight: bold;
        }

        .shape-filled {
          background: #111;
          color: white;
          border-radius: 2px;
        }

        .shape-empty {
          background: transparent;
        }

        .par {
          font-size: 19px;
          font-weight: bold;
          margin-bottom: 18px;
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

        /* Hover should NOT change square colour */

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

        .move-counter {
          font-size: 19px;
          margin: 18px 0;
          letter-spacing: 1px;
        }

        .control-button {
          margin: 0 5px;
        }

        .solved-box {
          margin: 20px auto;
          padding: 20px;
          max-width: 400px;
          border: 1px solid #ddd;
          border-radius: 6px;
        }

        .rating-button {
          margin: 4px;
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
    
    uiOutput("progress_ui"),
    
    div(
      class = "shape-label",
      "SHAPE"
    ),
    
    uiOutput("shape_ui"),
    
    div(
      class = "par",
      paste("PAR", target_par)
    ),
    
    uiOutput("board_ui"),
    
    div(
      class = "move-counter",
      textOutput("moves_text")
    ),
    
    actionButton(
      "reset",
      "RESET",
      class = "control-button"
    ),
    
    uiOutput("solved_ui")
  )
)


# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # State
  # ----------------------------------------------------------
  
  current_index <- reactiveVal(1)
  
  current_shape <- reactiveVal(NULL)
  
  board <- reactiveVal(NULL)
  
  starting_board <- reactiveVal(NULL)
  
  moves <- reactiveVal(0)
  
  resets <- reactiveVal(0)
  
  start_time <- reactiveVal(NULL)
  
  solve_time <- reactiveVal(NULL)
  
  puzzle_solved <- reactiveVal(FALSE)
  
  result_saved <- reactiveVal(FALSE)
  
  
  # ==========================================================
  # Load puzzle
  # ==========================================================
  
  load_puzzle <- function(index) {
    
    metadata <- playtest_shapes[index, ]
    
    
    shape <- key_to_shape_v2(
      key = metadata$canonical_key,
      name = paste0(
        "Playtest ",
        index
      )
    )
    
    
    puzzle <- generate_puzzle_v2(
      n = grid_size,
      shape = shape,
      target_moves = target_par
    )
    
    
    current_shape(shape)
    
    board(puzzle$board)
    
    starting_board(puzzle$board)
    
    moves(0)
    
    resets(0)
    
    solve_time(NULL)
    
    puzzle_solved(FALSE)
    
    result_saved(FALSE)
    
    start_time(Sys.time())
  }
  
  
  # Initial puzzle
  load_puzzle(1)
  
  
  # ==========================================================
  # Progress
  # ==========================================================
  
  output$progress_ui <- renderUI({
    
    index <- current_index()
    
    
    if (index > nrow(playtest_shapes)) {
      
      return(
        div(
          class = "progress",
          strong("PLAY TEST COMPLETE")
        )
      )
    }
    
    
    div(
      class = "progress",
      paste(
        "Puzzle",
        index,
        "of",
        nrow(playtest_shapes)
      )
    )
  })
  
  
  # ==========================================================
  # Shape
  # ==========================================================
  
  output$shape_ui <- renderUI({
    
    req(current_shape())
    
    
    mat <- current_shape()$matrix
    
    cells <- list()
    
    
    for (r in seq_len(nrow(mat))) {
      
      for (c in seq_len(ncol(mat))) {
        
        value <- mat[r, c]
        
        
        cell_class <- if (value == ".") {
          
          "shape-cell shape-empty"
          
        } else {
          
          "shape-cell shape-filled"
        }
        
        
        label <- if (
          value %in% c("S", "F")
        ) {
          
          value
          
        } else {
          
          ""
        }
        
        
        cells[[length(cells) + 1]] <- div(
          class = cell_class,
          label
        )
      }
    }
    
    
    div(
      
      class = "shape-display",
      
      style = sprintf(
        "grid-template-columns: repeat(%s, 28px);",
        ncol(mat)
      ),
      
      cells
    )
  })
  
  
  # ==========================================================
  # Board
  #
  # IMPORTANT:
  # Plain HTML buttons + one Shiny input.
  #
  # This avoids dynamically creating nested observers.
  # ==========================================================
  
  output$board_ui <- renderUI({
    
    req(board())
    
    
    if (current_index() > nrow(playtest_shapes)) {
      return(NULL)
    }
    
    
    b <- board()
    
    
    squares <- lapply(
      
      seq_len(grid_size * grid_size),
      
      function(i) {
        
        row <-
          ((i - 1) %/% grid_size) + 1
        
        col <-
          ((i - 1) %% grid_size) + 1
        
        
        colour_class <- if (
          b[row, col]
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
  # ONE click observer
  # ==========================================================
  
  observeEvent(
    input$square_click,
    {
      
      if (puzzle_solved()) {
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
      
      
      new_board <- make_move_v2(
        
        board = board(),
        
        row = row,
        
        col = col,
        
        shape = current_shape()
      )
      
      
      board(new_board)
      
      moves(
        moves() + 1
      )
      
      
      if (is_solved_v2(new_board)) {
        
        puzzle_solved(TRUE)
        
        
        solve_time(
          as.numeric(
            difftime(
              Sys.time(),
              start_time(),
              units = "secs"
            )
          )
        )
      }
    },
    
    ignoreInit = TRUE
  )
  
  
  # ==========================================================
  # Moves
  # ==========================================================
  
  output$moves_text <- renderText({
    
    paste(
      "MOVES",
      moves()
    )
  })
  
  
  # ==========================================================
  # Reset
  # ==========================================================
  
  observeEvent(
    input$reset,
    {
      
      req(starting_board())
      
      
      if (puzzle_solved()) {
        return()
      }
      
      
      board(
        starting_board()
      )
      
      moves(0)
      
      resets(
        resets() + 1
      )
    }
  )
  
  
  # ==========================================================
  # Solved UI
  # ==========================================================
  
  output$solved_ui <- renderUI({
    
    if (!puzzle_solved()) {
      return(NULL)
    }
    
    
    div(
      
      class = "solved-box",
      
      h3("SOLVED"),
      
      p(
        paste(
          "Solved in",
          moves(),
          "moves"
        )
      ),
      
      p(
        paste(
          "Moves over PAR:",
          moves() - target_par
        )
      ),
      
      h4(
        "How difficult did that feel?"
      ),
      
      div(
        
        actionButton(
          "rate_1",
          "1 - Very easy",
          class = "rating-button"
        ),
        
        actionButton(
          "rate_2",
          "2",
          class = "rating-button"
        ),
        
        actionButton(
          "rate_3",
          "3",
          class = "rating-button"
        ),
        
        actionButton(
          "rate_4",
          "4",
          class = "rating-button"
        ),
        
        actionButton(
          "rate_5",
          "5 - Very hard",
          class = "rating-button"
        )
      )
    )
  })
  
  
  # ==========================================================
  # Save result
  # ==========================================================
  
  save_result <- function(rating) {
    
    index <- current_index()
    
    
    if (
      index > nrow(playtest_shapes) ||
      !puzzle_solved() ||
      result_saved()
    ) {
      
      return()
    }
    
    
    result_saved(TRUE)
    
    
    metadata <- playtest_shapes[index, ]
    
    
    result <- data.frame(
      
      playtest_order =
        metadata$playtest_order,
      
      family_id =
        metadata$family_id,
      
      canonical_key =
        metadata$canonical_key,
      
      n_flippers =
        metadata$n_flippers,
      
      gf2_rank =
        metadata$gf2_rank,
      
      par =
        target_par,
      
      moves_used =
        moves(),
      
      moves_over_par =
        moves() - target_par,
      
      solve_time_seconds =
        round(
          solve_time(),
          2
        ),
      
      resets =
        resets(),
      
      difficulty_rating =
        rating,
      
      predicted_percentile =
        metadata$difficulty_percentile,
      
      predicted_class =
        metadata$test_class,
      
      stringsAsFactors = FALSE
    )
    
    
    file_has_data <-
      file.exists(results_file) &&
      file.info(results_file)$size > 0
    
    
    write.table(
      
      result,
      
      file = results_file,
      
      sep = ",",
      
      row.names = FALSE,
      
      col.names = !file_has_data,
      
      append = file_has_data
    )
    
    
    # --------------------------------------------------------
    # Next puzzle
    # --------------------------------------------------------
    
    next_index <- index + 1L
    
    current_index(
      next_index
    )
    
    
    if (
      next_index <= nrow(playtest_shapes)
    ) {
      
      load_puzzle(
        next_index
      )
      
    } else {
      
      board(NULL)
      
      current_shape(NULL)
    }
  }
  
  
  # ==========================================================
  # Ratings
  # ==========================================================
  
  observeEvent(
    input$rate_1,
    save_result(1),
    ignoreInit = TRUE
  )
  
  
  observeEvent(
    input$rate_2,
    save_result(2),
    ignoreInit = TRUE
  )
  
  
  observeEvent(
    input$rate_3,
    save_result(3),
    ignoreInit = TRUE
  )
  
  
  observeEvent(
    input$rate_4,
    save_result(4),
    ignoreInit = TRUE
  )
  
  
  observeEvent(
    input$rate_5,
    save_result(5),
    ignoreInit = TRUE
  )
}


# ============================================================
# Run
# ============================================================

shinyApp(
  ui = ui,
  server = server
)