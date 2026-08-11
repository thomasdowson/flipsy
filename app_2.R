library(shiny)


# ============================================================
# FLIPSY V2
# ============================================================

source("R/v_2/shape_engine_v2.R")
source("R/v_2/game_engine_v2.R")
source("R/v_2/puzzle_solver_v2.R")
source("R/v_2/puzzle_generator_v2.R")
source("R/v_2/shapes_v2.R")


# ============================================================
# Settings
# ============================================================

grid_size <- 5

min_par <- 4
max_par <- 7


# ============================================================
# Shape Renderer
# ============================================================

render_shape_v2 <- function(shape) {
  
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
# Generate Game
# ============================================================

generate_game_v2 <- function(
    current_shape = NULL
) {
  
  shape <- random_shape_v2(
    current_shape
  )
  
  
  target_par <- sample(
    min_par:max_par,
    size = 1
  )
  
  
  puzzle <- generate_puzzle_v2(
    n = grid_size,
    shape = shape,
    target_moves = target_par
  )
  
  
  list(
    board = puzzle$board,
    shape = shape,
    par = puzzle$par
  )
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$style(HTML("

      body {
        font-family: Arial, sans-serif;
        text-align: center;
        background: white;
        color: #111;
      }

      .game-container {
        max-width: 500px;
        margin: 30px auto;
      }

      .game-title {
        font-size: 34px;
        font-weight: bold;
        letter-spacing: 2px;
        margin-bottom: 5px;
      }

      .instruction {
        font-size: 17px;
        margin-bottom: 25px;
      }

      .shape-label {
        font-size: 12px;
        letter-spacing: 3px;
        margin-bottom: 12px;
      }

      .shape-grid {
        display: grid;
        gap: 3px;
        justify-content: center;
        width: fit-content;
        margin: 0 auto 20px auto;
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

      .par {
        font-size: 19px;
        font-weight: bold;
        margin-bottom: 15px;
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

        cursor: pointer;
        padding: 0;
      }

      .square-button:hover {
        border-color: #000;
      }

      .black-square {
        background: #111;
      }

      .white-square {
        background: #f7f7f7;
      }

      .move-counter {
        font-size: 19px;
        margin: 18px 0;
        letter-spacing: 1px;
      }

      .solved-message {
        font-size: 22px;
        font-weight: bold;
        margin: 15px 0;
      }

      .controls {
        margin-top: 15px;
      }

      .controls .btn {
        margin: 0 5px;

        background: white;
        color: #111;

        border: 1px solid #111;
        border-radius: 4px;

        min-width: 75px;
      }

      .controls .btn:hover {
        background: #111;
        color: white;
      }

    "))
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
    
    
    div(
      class = "par",
      textOutput("par")
    ),
    
    
    uiOutput(
      "game_board"
    ),
    
    
    div(
      class = "move-counter",
      textOutput("move_counter")
    ),
    
    
    uiOutput(
      "solved_message"
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
  
  
  # ----------------------------------------------------------
  # Initial Game
  # ----------------------------------------------------------
  
  initial_game <- generate_game_v2()
  
  
  game <- reactiveValues(
    
    board =
      initial_game$board,
    
    starting_board =
      initial_game$board,
    
    shape =
      initial_game$shape,
    
    par =
      initial_game$par,
    
    moves = 0,
    
    solved = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Shape
  # ----------------------------------------------------------
  
  output$shape_display <- renderUI({
    
    render_shape_v2(
      game$shape
    )
  })
  
  
  # ----------------------------------------------------------
  # PAR
  # ----------------------------------------------------------
  
  output$par <- renderText({
    
    paste(
      "PAR",
      game$par
    )
  })
  
  
  # ----------------------------------------------------------
  # Board
  # ----------------------------------------------------------
  
  output$game_board <- renderUI({
    
    board <- game$board
    
    
    squares <- lapply(
      
      seq_len(
        grid_size * grid_size
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
            )
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
  
  
  # ----------------------------------------------------------
  # Square Click
  # ----------------------------------------------------------
  
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
      
      
      game$board <- make_move_v2(
        board = game$board,
        row = row,
        col = col,
        shape = game$shape
      )
      
      
      game$moves <-
        game$moves + 1
      
      
      if (
        is_solved_v2(
          game$board
        )
      ) {
        
        game$solved <- TRUE
      }
    }
  )
  
  
  # ----------------------------------------------------------
  # Move Counter
  # ----------------------------------------------------------
  
  output$move_counter <- renderText({
    
    paste(
      "MOVES",
      game$moves
    )
  })
  
  
  # ----------------------------------------------------------
  # Solved Message
  # ----------------------------------------------------------
  
  output$solved_message <- renderUI({
    
    if (!game$solved) {
      return(NULL)
    }
    
    
    if (
      game$moves == game$par
    ) {
      
      message <- "PERFECT!"
      
    } else {
      
      message <- paste(
        "SOLVED IN",
        game$moves,
        "MOVES"
      )
    }
    
    
    div(
      class = "solved-message",
      message
    )
  })
  
  
  # ----------------------------------------------------------
  # RESET
  # ----------------------------------------------------------
  
  observeEvent(
    input$reset,
    {
      
      game$board <-
        game$starting_board
      
      game$moves <- 0
      
      game$solved <- FALSE
    }
  )
  
  
  # ----------------------------------------------------------
  # NEW
  # ----------------------------------------------------------
  
  observeEvent(
    input$new_puzzle,
    {
      
      new_game <- generate_game_v2(
        current_shape = game$shape
      )
      
      
      game$board <-
        new_game$board
      
      game$starting_board <-
        new_game$board
      
      game$shape <-
        new_game$shape
      
      game$par <-
        new_game$par
      
      game$moves <- 0
      
      game$solved <- FALSE
    }
  )
}


# ============================================================
# Run
# ============================================================

shinyApp(
  ui = ui,
  server = server
)