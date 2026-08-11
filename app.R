library(shiny)

source("R/game_engine.R")
source("R/puzzle_solver.R")
source("R/puzzle_generator.R")
source("R/shapes.R")


# ============================================================
# Settings
# ============================================================

grid_size <- 5

min_par <- 3
max_par <- 7


# ============================================================
# Shape renderer
# ============================================================

render_shape <- function(offset) {
  
  vertical <- offset[1]
  horizontal <- offset[2]
  
  
  # ----------------------------------------------------------
  # Construct coordinates for the visual path
  #
  # Start at (0, 0)
  # Move vertically first
  # Then move horizontally
  # ----------------------------------------------------------
  
  vertical_steps <- if (vertical == 0) {
    
    0
    
  } else {
    
    seq(
      0,
      vertical,
      by = sign(vertical)
    )
  }
  
  
  horizontal_steps <- if (horizontal == 0) {
    
    0
    
  } else {
    
    seq(
      sign(horizontal),
      horizontal,
      by = sign(horizontal)
    )
  }
  
  
  # Vertical section
  path_rows <- vertical_steps
  
  path_cols <- rep(
    0,
    length(vertical_steps)
  )
  
  
  # Horizontal section
  if (horizontal != 0) {
    
    path_rows <- c(
      path_rows,
      rep(
        vertical,
        length(horizontal_steps)
      )
    )
    
    path_cols <- c(
      path_cols,
      horizontal_steps
    )
  }
  
  
  # ----------------------------------------------------------
  # Shift coordinates so everything starts at 1
  #
  # CSS grid positions cannot be zero or negative.
  # ----------------------------------------------------------
  
  display_rows <-
    path_rows -
    min(path_rows) +
    1
  
  
  display_cols <-
    path_cols -
    min(path_cols) +
    1
  
  
  number_rows <- max(display_rows)
  number_cols <- max(display_cols)
  
  
  # ----------------------------------------------------------
  # Create blocks
  # ----------------------------------------------------------
  
  cells <- lapply(
    seq_along(display_rows),
    
    function(i) {
      
      div(
        class = "shape-block",
        
        style = sprintf(
          "
          grid-row: %s;
          grid-column: %s;
          ",
          display_rows[i],
          display_cols[i]
        )
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # Shape grid
  # ----------------------------------------------------------
  
  div(
    
    class = "shape-grid",
    
    style = sprintf(
      "
      grid-template-rows: repeat(%s, 18px);
      grid-template-columns: repeat(%s, 18px);
      ",
      number_rows,
      number_cols
    ),
    
    cells
  )
}

# ============================================================
# Generate complete game
# ============================================================

generate_game <- function(current_shape = NULL) {
  
  # Choose a random shape, excluding the current one
  offset <- random_shape(
    current_shape
  )
  
  # Choose target PAR
  target_par <- sample(
    min_par:max_par,
    size = 1
  )
  
  # Generate board
  puzzle <- generate_puzzle(
    n = grid_size,
    offset = offset,
    target_moves = target_par
  )
  
  list(
    board = puzzle$board,
    offset = offset,
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
        letter-spacing: 1px;
        margin-bottom: 5px;
      }


      .instruction {
        font-size: 17px;
        margin-bottom: 35px;
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
        margin: 0 auto 25px auto;
      }


      .shape-block {
        width: 18px;
        height: 18px;
        background: #111;
        border-radius: 2px;
      }


      .par {
        font-size: 20px;
        font-weight: bold;
        letter-spacing: 1px;
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
    
    
    uiOutput("shape_display"),
    
    
    div(
      class = "par",
      textOutput("par")
    ),
    
    
    uiOutput("game_board"),
    
    
    div(
      class = "move-counter",
      textOutput("move_counter")
    ),
    
    
    uiOutput("solved_message"),
    
    
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

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Initial game
  # ----------------------------------------------------------
  
  initial_game <- generate_game()
  
  
  game <- reactiveValues(
    
    board = initial_game$board,
    
    starting_board = initial_game$board,
    
    offset = initial_game$offset,
    
    par = initial_game$par,
    
    moves = 0,
    
    solved = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Shape
  # ----------------------------------------------------------
  
  output$shape_display <- renderUI({
    
    render_shape(
      game$offset
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
  # Draw board
  # ----------------------------------------------------------
  
  output$game_board <- renderUI({
    
    board <- game$board
    
    
    squares <- lapply(
      
      seq_len(grid_size * grid_size),
      
      function(i) {
        
        row <- ((i - 1) %/% grid_size) + 1
        col <- ((i - 1) %% grid_size) + 1
        
        
        colour_class <- if (board[row, col]) {
          
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
            "Shiny.setInputValue(
              'square_click',
              '%s',
              {priority: 'event'}
            )",
            paste(row, col, sep = "_")
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
  # Square click
  # ----------------------------------------------------------
  
  observeEvent(input$square_click, {
    
    if (game$solved) {
      return()
    }
    
    
    coords <- strsplit(
      input$square_click,
      "_"
    )[[1]]
    
    
    row <- as.integer(coords[1])
    col <- as.integer(coords[2])
    
    
    game$board <- make_move(
      board = game$board,
      row = row,
      col = col,
      offset = game$offset
    )
    
    
    game$moves <- game$moves + 1
    
    
    if (is_solved(game$board)) {
      
      game$solved <- TRUE
    }
  })
  
  
  # ----------------------------------------------------------
  # Move counter
  # ----------------------------------------------------------
  
  output$move_counter <- renderText({
    
    paste(
      "MOVES",
      game$moves
    )
  })
  
  
  # ----------------------------------------------------------
  # Solved message
  # ----------------------------------------------------------
  
  output$solved_message <- renderUI({
    
    if (!game$solved) {
      return(NULL)
    }
    
    
    if (game$moves == game$par) {
      
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
  # Reset
  #
  # Same board.
  # Same shape.
  # Same PAR.
  # ----------------------------------------------------------
  
  observeEvent(input$reset, {
    
    game$board <- game$starting_board
    
    game$moves <- 0
    
    game$solved <- FALSE
  })
  
  
  # ----------------------------------------------------------
  # New game
  #
  # New board.
  # New shape.
  # New PAR.
  # ----------------------------------------------------------
  
  observeEvent(input$new_puzzle, {
    
    new_game <- generate_game(
      current_shape = game$offset
    )
    
    
    game$board <- new_game$board
    
    game$starting_board <- new_game$board
    
    game$offset <- new_game$offset
    
    game$par <- new_game$par
    
    game$moves <- 0
    
    game$solved <- FALSE
  })
}


# ============================================================
# Run app
# ============================================================

shinyApp(
  ui = ui,
  server = server
)

###
# - Should all be solvable in X turns
# - allow for 2 or more of the shape squares to be flippers
# - allow for shapes like crosses etc (intersections)
# - Display game shape simply, 
# - starting square says S and flipping squares say F