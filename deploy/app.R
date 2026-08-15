# ============================================================
# FLIPSY - Minimum Viable Game
# ============================================================

library(shiny)


# ============================================================
# Source engines
# ============================================================

source("flipsyR/v_2/shape_engine_v2.R")
source("flipsyR/v_2/game_engine_v2.R")
source("flipsyR/v_2/puzzle_solver_v2.R")
source("flipsyR/v_2/puzzle_generator_v2.R")
source("flipsyR/v_2/shape_visualiser_v2.R")

source("flipsyR/v_3_shapes.R")

source("flipsyR/mvp/quick_play.R")
source("flipsyR/mvp/daily_flipsy.R")


# ============================================================
# Shape renderer
# ============================================================

render_shape_mvp <- function(shape) {
  
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
      grid-template-rows: repeat(%s, 25px);
      grid-template-columns: repeat(%s, 25px);
      ",
      nrow(mat),
      ncol(mat)
    ),
    
    cells
  )
}


# ============================================================
# Normal board renderer
# ============================================================

build_board_ui <- function(
    board,
    input_name,
    disabled = FALSE
) {
  
  n <- nrow(board)
  
  squares <- lapply(
    
    seq_len(n * n),
    
    function(i) {
      
      row <- ((i - 1) %/% n) + 1
      col <- ((i - 1) %% n) + 1
      
      colour_class <- if (
        board[row, col]
      ) {
        "black-square"
      } else {
        "white-square"
      }
      
      onclick_code <- ""
      
      if (!disabled) {
        
        onclick_code <- sprintf(
          "
          Shiny.setInputValue(
            '%s',
            '%s',
            {priority: 'event'}
          );
          ",
          input_name,
          paste(row, col, sep = "_")
        )
      }
      
      tags$button(
        
        type = "button",
        
        class = paste(
          "square-button",
          colour_class
        ),
        
        onclick = onclick_code,
        
        disabled = if (disabled) {
          "disabled"
        } else {
          NULL
        }
      )
    }
  )
  
  div(
    class = "game-board",
    squares
  )
}


# ============================================================
# Solution mini-board
# ============================================================
#
# The optimal S position is shown in light blue.
# ============================================================

build_solution_board <- function(
    board,
    highlight_row = NULL,
    highlight_col = NULL
) {
  
  n <- nrow(board)
  
  squares <- lapply(
    
    seq_len(n * n),
    
    function(i) {
      
      row <- ((i - 1) %/% n) + 1
      col <- ((i - 1) %% n) + 1
      
      highlighted <- (
        !is.null(highlight_row) &&
          !is.null(highlight_col) &&
          row == highlight_row &&
          col == highlight_col
      )
      
      if (highlighted) {
        
        square_class <- "solution-square solution-highlight"
        
        label <- "S"
        
      } else if (board[row, col]) {
        
        square_class <- "solution-square solution-black"
        
        label <- ""
        
      } else {
        
        square_class <- "solution-square solution-white"
        
        label <- ""
      }
      
      div(
        class = square_class,
        label
      )
    }
  )
  
  div(
    class = "solution-board",
    squares
  )
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    
    tags$script(
      HTML("

    // ========================================================
    // FLIPSY Daily localStorage
    // ========================================================

    Shiny.addCustomMessageHandler(
      'save_daily_state',
      function(message) {

        var key = 'flipsy_daily_' + message.date;

        localStorage.setItem(
          key,
          JSON.stringify(message)
        );
      }
    );


    Shiny.addCustomMessageHandler(
      'load_daily_state',
      function(message) {

        var key = 'flipsy_daily_' + message.date;

        var saved = localStorage.getItem(key);

        var parsed = null;

        if (saved !== null) {

          try {

            parsed = JSON.parse(saved);

          } catch (e) {

            console.error(
              'Could not parse saved FLIPSY Daily state.',
              e
            );
          }
        }


        Shiny.setInputValue(
          'daily_saved_state',
          parsed,
          {priority: 'event'}
        );
      }
    );

  ")
    ),
    
    
    tags$style(
      
      HTML("

        html,
        body {
          margin: 0;
          padding: 0;
          background: #ffffff;
          color: #111111;
          font-family: Arial, Helvetica, sans-serif;
        }

        body {
          min-height: 100vh;
        }

        .app-shell {
          width: 100%;
          max-width: 620px;
          margin: 0 auto;
          padding: 35px 20px 50px 20px;
          text-align: center;
          box-sizing: border-box;
        }


        /* BRAND */

        .logo {
          font-size: 42px;
          font-weight: 800;
          letter-spacing: 5px;
          margin-top: 5px;
          margin-bottom: 8px;
        }

        .tagline {
          font-size: 15px;
          color: #555555;
          line-height: 1.5;
          margin-bottom: 36px;
        }


        /* HEADINGS */

        .screen-title {
          font-size: 16px;
          font-weight: bold;
          letter-spacing: 3px;
          margin-bottom: 10px;
        }

        .screen-subtitle {
          font-size: 13px;
          color: #666666;
          margin-bottom: 28px;
        }

        .daily-number {
          font-size: 12px;
          color: #777777;
          letter-spacing: 2px;
          margin-bottom: 18px;
        }


        /* BUTTONS */

        .main-button,
        .difficulty-button,
        .control-button,
        .back-button {

          background: #ffffff;
          color: #111111;

          border: 2px solid #111111;
          border-radius: 5px;

          font-weight: bold;
          letter-spacing: 1px;

          box-shadow: none;
          outline: none;
        }

        .main-button:hover,
        .difficulty-button:hover,
        .control-button:hover,
        .back-button:hover {

          background: #111111;
          color: #ffffff;
          border-color: #111111;
        }

        .main-button {
          width: 240px;
          height: 52px;
          margin: 7px auto;
          display: block;
          font-size: 14px;
        }

        .difficulty-button {
          width: 150px;
          height: 48px;
          margin: 7px auto;
          display: block;
          font-size: 13px;
        }

        .control-button {
          min-width: 105px;
          height: 40px;
          margin: 4px;
          font-size: 11px;
        }

        .back-button {
          min-width: 90px;
          height: 38px;
          margin-top: 22px;
          border-width: 1px;
          font-size: 11px;
        }


        /* SHAPE */

        .difficulty-name {
          font-size: 12px;
          letter-spacing: 3px;
          color: #555555;
          margin-bottom: 18px;
        }

        .shape-title {
          font-size: 10px;
          letter-spacing: 3px;
          color: #777777;
          margin-bottom: 9px;
        }

        .shape-grid {
          display: grid;
          gap: 3px;
          justify-content: center;
          width: fit-content;
          margin: 0 auto 22px auto;
        }

        .shape-block {
          width: 23px;
          height: 23px;
          background: #111111;
          color: #ffffff;
          display: flex;
          justify-content: center;
          align-items: center;
          border-radius: 2px;
          font-size: 11px;
          font-weight: bold;
        }


        /* GAME BOARD */

        .game-board {
          display: grid;
          grid-template-columns: repeat(5, 62px);
          gap: 5px;
          justify-content: center;
          margin: 18px auto 22px auto;
        }

        .square-button {
          width: 62px;
          height: 62px;
          padding: 0;
          border: 2px solid #555555;
          border-radius: 4px;
          cursor: pointer;
          outline: none;
          box-shadow: none;
        }

        .square-button:disabled {
          opacity: 1;
          cursor: default;
        }

        .black-square {
          background: #111111;
        }

        .white-square {
          background: #f7f7f7;
        }

        .black-square:hover,
        .black-square:focus,
        .black-square:active {
          background: #111111;
        }

        .white-square:hover,
        .white-square:focus,
        .white-square:active {
          background: #f7f7f7;
        }


        /* STATS */

        .stats-row {
          display: flex;
          justify-content: center;
          gap: 55px;
          margin-top: 8px;
          margin-bottom: 17px;
        }

        .stat {
          min-width: 80px;
        }

        .stat-label {
          font-size: 10px;
          letter-spacing: 2px;
          color: #777777;
          margin-bottom: 3px;
        }

        .stat-value {
          font-size: 25px;
          font-weight: bold;
        }

        .daily-moves {
          margin: 10px auto 16px auto;
        }


        /* RESULTS */

        .result {
          margin-top: 18px;
          font-size: 20px;
          font-weight: bold;
          letter-spacing: 1px;
        }

        .result-perfect {
          font-size: 13px;
          color: #555555;
          margin-top: 5px;
        }

        .failed-result {
          margin-top: 18px;
          font-size: 20px;
          font-weight: bold;
          letter-spacing: 2px;
        }


        /* HOW TO PLAY */

        .how-to {
          max-width: 360px;
          margin: 0 auto;
          text-align: left;
          line-height: 1.6;
          font-size: 14px;
          color: #333333;
        }

        .how-to strong {
          color: #111111;
        }


        /* SOLUTION VIEWER */

        .solution-intro {
          max-width: 420px;
          margin: 0 auto 28px auto;
          color: #555555;
          font-size: 13px;
          line-height: 1.5;
        }

        .solution-step {
          margin: 0 auto 25px auto;
        }

        .solution-step-title {
          font-size: 11px;
          font-weight: bold;
          letter-spacing: 2px;
          margin-bottom: 8px;
        }

        .solution-board {
          display: grid;
          grid-template-columns: repeat(5, 30px);
          gap: 3px;
          justify-content: center;
          margin: 0 auto;
        }

        .solution-square {
          width: 30px;
          height: 30px;
          box-sizing: border-box;
          border: 1px solid #555555;
          border-radius: 2px;
          display: flex;
          justify-content: center;
          align-items: center;
          font-size: 11px;
          font-weight: bold;
        }

        .solution-black {
          background: #111111;
          color: #ffffff;
        }

        .solution-white {
          background: #f7f7f7;
          color: #111111;
        }

        .solution-highlight {
          background: #9fd8f5;
          color: #111111;
          border: 2px solid #4a9bc5;
        }

        .solution-arrow {
          font-size: 18px;
          margin: 7px 0;
          color: #777777;
        }

        .solved-label {
          margin-top: 8px;
          font-size: 11px;
          letter-spacing: 2px;
          font-weight: bold;
        }


        /* MOBILE */

        @media (max-width: 420px) {

          .app-shell {
            padding-left: 10px;
            padding-right: 10px;
          }

          .logo {
            font-size: 36px;
          }

          .game-board {
            grid-template-columns: repeat(5, 54px);
            gap: 4px;
          }

          .square-button {
            width: 54px;
            height: 54px;
          }

          .stats-row {
            gap: 35px;
          }
        }

      ")
    )
  ),
  
  div(
    class = "app-shell",
    uiOutput("screen")
  )
)


# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  # ==========================================================
  # Application state
  # ==========================================================
  
  state <- reactiveValues(
    
    screen = "home",
    
    selected_difficulty = NULL,
    
    quick_game = NULL,
    
    daily_game = NULL
  )
  
  
  # ==========================================================
  # Start Quick Play
  # ==========================================================
  
  start_quick_game <- function(
    difficulty,
    exclude_family_index = NULL
  ) {
    
    state$selected_difficulty <- difficulty
    
    state$quick_game <- generate_quick_puzzle(
      
      difficulty = difficulty,
      
      exclude_family_index = exclude_family_index
    )
    
    state$screen <- "quick_game"
  }
  
  
  # ==========================================================
  # Start Daily
  # ==========================================================
  save_daily_to_browser <- function(game) {
    
    moves <- lapply(
      game$move_history,
      function(move) {
        
        list(
          row = as.integer(move$row),
          col = as.integer(move$col)
        )
      }
    )
    
    
    session$sendCustomMessage(
      
      "save_daily_state",
      
      list(
        date = as.character(game$date),
        moves = moves
      )
    )
  }
  
  
  start_daily_game <- function() {
    
    today <- Sys.Date()
    
    
    # Generate the Daily immediately.
    # This gives us something valid to display while the browser
    # checks whether an existing attempt is stored.
    
    game <- generate_daily_puzzle(
      today
    )
    
    game <- add_daily_solution(
      game
    )
    
    
    state$daily_game <- game
    
    state$screen <- "daily"
    
    
    # Ask JavaScript/localStorage whether this browser already
    # has an attempt for today's puzzle.
    
    session$sendCustomMessage(
      
      "load_daily_state",
      
      list(
        date = as.character(today)
      )
    )
  }
  
  observeEvent(
    input$daily_saved_state,
    {
      
      saved <- input$daily_saved_state
      
      if (
        is.null(saved) ||
        is.null(saved$moves)
      ) {
        return()
      }
      
      move_history <- saved$moves
      
      restored <- restore_daily_game(
        date = Sys.Date(),
        move_history = move_history
      )
      
      state$daily_game <- restored
    },
    
    ignoreInit = TRUE
  )
  # ==========================================================
  # HOME
  # ==========================================================
  
  home_screen <- function() {
    
    tagList(
      
      div(
        class = "logo",
        "FLIPSY"
      ),
      
      div(
        
        class = "tagline",
        
        HTML(
          "Every puzzle has a<br>
           <strong>5-move solution.</strong>"
        )
      ),
      
      actionButton(
        "go_quick",
        "QUICK PLAY",
        class = "main-button"
      ),
      
      actionButton(
        "go_daily",
        "DAILY FLIPSY",
        class = "main-button"
      ),
      
      actionButton(
        "go_help",
        "HOW TO PLAY",
        class = "main-button"
      )
    )
  }
  
  
  # ==========================================================
  # DIFFICULTY SELECT
  # ==========================================================
  
  difficulty_screen <- function() {
    
    tagList(
      
      div(
        class = "logo",
        "FLIPSY"
      ),
      
      div(
        class = "screen-title",
        "QUICK PLAY"
      ),
      
      div(
        class = "screen-subtitle",
        "Choose your difficulty"
      ),
      
      actionButton(
        "choose_easy",
        "EASY",
        class = "difficulty-button"
      ),
      
      actionButton(
        "choose_medium",
        "MEDIUM",
        class = "difficulty-button"
      ),
      
      actionButton(
        "choose_hard",
        "HARD",
        class = "difficulty-button"
      ),
      
      actionButton(
        "difficulty_home",
        "← BACK",
        class = "back-button"
      )
    )
  }
  
  
  # ==========================================================
  # QUICK PLAY
  # ==========================================================
  
  quick_game_screen <- function() {
    
    game <- state$quick_game
    
    req(game)
    
    tagList(
      
      div(
        class = "logo",
        "FLIPSY"
      ),
      
      div(
        class = "difficulty-name",
        toupper(game$difficulty)
      ),
      
      div(
        class = "shape-title",
        "SHAPE"
      ),
      
      render_shape_mvp(
        game$shape
      ),
      
      div(
        
        class = "stats-row",
        
        div(
          
          class = "stat",
          
          div(
            class = "stat-label",
            "MOVES"
          ),
          
          div(
            class = "stat-value",
            game$moves
          )
        ),
        
        div(
          
          class = "stat",
          
          div(
            class = "stat-label",
            "TO GO"
          ),
          
          div(
            class = "stat-value",
            game$to_go
          )
        )
      ),
      
      build_board_ui(
        board = game$board,
        input_name = "quick_square_click",
        disabled = game$solved
      ),
      
      if (game$solved) {
        
        tagList(
          
          div(
            
            class = "result",
            
            if (
              game$moves ==
              game$optimal_moves
            ) {
              
              "PERFECT!"
              
            } else {
              
              paste(
                "SOLVED IN",
                game$moves,
                "MOVES"
              )
            }
          ),
          
          if (
            game$moves ==
            game$optimal_moves
          ) {
            
            div(
              class = "result-perfect",
              "Solved in the optimal 5 moves"
            )
          }
        )
      },
      
      div(
        
        actionButton(
          "quick_reset",
          "RESET",
          class = "control-button"
        ),
        
        actionButton(
          "quick_new",
          "NEW",
          class = "control-button"
        )
      ),
      
      actionButton(
        "quick_home",
        "← HOME",
        class = "back-button"
      )
    )
  }
  
  
  # ==========================================================
  # DAILY FLIPSY
  # ==========================================================
  
  daily_screen <- function() {
    
    game <- state$daily_game
    
    req(game)
    
    tagList(
      
      div(
        class = "logo",
        "FLIPSY"
      ),
      
      div(
        class = "screen-title",
        "DAILY FLIPSY"
      ),
      
      div(
        class = "daily-number",
        paste0(
          "#",
          game$daily_number
        )
      ),
      
      div(
        class = "shape-title",
        "SHAPE"
      ),
      
      render_shape_mvp(
        game$shape
      ),
      
      div(
        
        class = "daily-moves",
        
        div(
          class = "stat-label",
          "MOVES"
        ),
        
        div(
          class = "stat-value",
          paste0(
            game$moves,
            " / ",
            game$move_limit
          )
        )
      ),
      
      build_board_ui(
        board = game$board,
        input_name = "daily_square_click",
        disabled = game$finished
      ),
      
      if (game$solved) {
        
        tagList(
          
          div(
            
            class = "result",
            
            if (
              game$moves ==
              game$optimal_moves
            ) {
              
              "PERFECT!"
              
            } else {
              
              paste(
                "SOLVED IN",
                game$moves,
                "MOVES"
              )
            }
          ),
          
          if (
            game$moves ==
            game$optimal_moves
          ) {
            
            div(
              class = "result-perfect",
              "Optimal solution found"
            )
          }
        )
      },
      
      if (game$failed) {
        
        div(
          class = "failed-result",
          "DAILY FLIPSY FAILED"
        )
      },
      
      if (game$finished) {
        
        actionButton(
          "see_solution",
          "SEE SOLUTION",
          class = "main-button"
        )
      },
      
      actionButton(
        "daily_home",
        "← HOME",
        class = "back-button"
      )
    )
  }
  
  
  # ==========================================================
  # SOLUTION VIEWER
  # ==========================================================
  
  solution_screen <- function() {
    
    game <- state$daily_game
    
    req(game)
    req(game$finished)
    req(game$solution_steps)
    
    step_ui <- list()
    
    for (
      i in seq_along(
        game$solution_steps
      )
    ) {
      
      step <- game$solution_steps[[i]]
      
      step_ui[[length(step_ui) + 1]] <-
        
        div(
          
          class = "solution-step",
          
          div(
            class = "solution-step-title",
            paste(
              "MOVE",
              i
            )
          ),
          
          build_solution_board(
            
            board = step$board,
            
            highlight_row = step$row,
            
            highlight_col = step$col
          )
        )
      
      
      step_ui[[length(step_ui) + 1]] <-
        
        div(
          class = "solution-arrow",
          "↓"
        )
    }
    
    
    final_board_ui <- div(
      
      class = "solution-step",
      
      build_solution_board(
        board = game$solution_final_board
      ),
      
      div(
        class = "solved-label",
        "SOLVED"
      )
    )
    
    
    tagList(
      
      div(
        class = "logo",
        "FLIPSY"
      ),
      
      div(
        class = "screen-title",
        "THE 5-MOVE SOLUTION"
      ),
      
      div(
        
        class = "solution-intro",
        
        HTML(
          "The light-blue square shows where to place
           <strong>S</strong> for each move."
        )
      ),
      
      step_ui,
      
      final_board_ui,
      
      actionButton(
        "solution_back",
        "← DAILY",
        class = "back-button"
      )
    )
  }
  
  
  # ==========================================================
  # HOW TO PLAY
  # ==========================================================
  
  help_screen <- function() {
    
    tagList(
      
      div(
        class = "logo",
        "FLIPSY"
      ),
      
      div(
        class = "screen-title",
        "HOW TO PLAY"
      ),
      
      div(
        
        class = "how-to",
        
        p(
          "Your goal is to turn every square black."
        ),
        
        p(
          
          HTML(
            "Place the shape's <strong>S</strong>
             on a board square to make a move."
          )
        ),
        
        p(
          
          HTML(
            "The <strong>S</strong> square and both
             <strong>F</strong> squares flip colour."
          )
        ),
        
        p(
          "The shape wraps around the edges of the board."
        ),
        
        p(
          
          HTML(
            "Every puzzle has an
             <strong>optimal 5-move solution.</strong>"
          )
        ),
        
        p(
          
          HTML(
            "<strong>Quick Play:</strong>
             TO GO tells you the minimum number of moves
             currently required to solve the puzzle."
          )
        ),
        
        p(
          
          HTML(
            "<strong>Daily FLIPSY:</strong>
             you have 15 moves, with no TO GO and no reset."
          )
        )
      ),
      
      actionButton(
        "help_home",
        "← HOME",
        class = "back-button"
      )
    )
  }
  
  
  # ==========================================================
  # Main screen renderer
  # ==========================================================
  
  output$screen <- renderUI({
    
    switch(
      
      state$screen,
      
      home =
        home_screen(),
      
      difficulty =
        difficulty_screen(),
      
      quick_game =
        quick_game_screen(),
      
      daily =
        daily_screen(),
      
      solution =
        solution_screen(),
      
      help =
        help_screen(),
      
      home_screen()
    )
  })
  
  
  # ==========================================================
  # Navigation
  # ==========================================================
  
  observeEvent(
    input$go_quick,
    {
      state$screen <- "difficulty"
    }
  )
  
  
  observeEvent(
    input$go_daily,
    {
      start_daily_game()
    }
  )
  
  
  observeEvent(
    input$go_help,
    {
      state$screen <- "help"
    }
  )
  
  
  observeEvent(
    input$difficulty_home,
    {
      state$screen <- "home"
    }
  )
  
  
  observeEvent(
    input$quick_home,
    {
      state$screen <- "home"
    }
  )
  
  
  observeEvent(
    input$daily_home,
    {
      state$screen <- "home"
    }
  )
  
  
  observeEvent(
    input$help_home,
    {
      state$screen <- "home"
    }
  )
  
  
  observeEvent(
    input$see_solution,
    {
      state$screen <- "solution"
    }
  )
  
  
  observeEvent(
    input$solution_back,
    {
      state$screen <- "daily"
    }
  )
  
  
  # ==========================================================
  # Difficulty selection
  # ==========================================================
  
  observeEvent(
    input$choose_easy,
    {
      start_quick_game("easy")
    }
  )
  
  
  observeEvent(
    input$choose_medium,
    {
      start_quick_game("medium")
    }
  )
  
  
  observeEvent(
    input$choose_hard,
    {
      start_quick_game("hard")
    }
  )
  
  
  # ==========================================================
  # Quick Play move
  # ==========================================================
  
  observeEvent(
    input$quick_square_click,
    {
      
      req(state$quick_game)
      
      if (
        state$screen != "quick_game" ||
        state$quick_game$solved
      ) {
        return()
      }
      
      
      coords <- strsplit(
        input$quick_square_click,
        "_"
      )[[1]]
      
      
      row <- as.integer(
        coords[1]
      )
      
      col <- as.integer(
        coords[2]
      )
      
      
      state$quick_game <- make_quick_move(
        
        game = state$quick_game,
        
        row = row,
        
        col = col
      )
    },
    
    ignoreInit = TRUE
  )
  
  
  # ==========================================================
  # Quick Play reset
  # ==========================================================
  
  observeEvent(
    input$quick_reset,
    {
      
      req(state$quick_game)
      
      state$quick_game <-
        reset_quick_puzzle(
          state$quick_game
        )
    }
  )
  
  
  # ==========================================================
  # Quick Play new
  # ==========================================================
  
  observeEvent(
    input$quick_new,
    {
      
      req(state$quick_game)
      
      old_family <-
        state$quick_game$family_index
      
      
      start_quick_game(
        
        difficulty =
          state$selected_difficulty,
        
        exclude_family_index =
          old_family
      )
    }
  )
  
  
  # ==========================================================
  # Daily move
  # ==========================================================
  
  observeEvent(
    input$daily_square_click,
    {
      
      req(state$daily_game)
      
      
      if (
        state$screen != "daily" ||
        state$daily_game$finished
      ) {
        return()
      }
      
      
      coords <- strsplit(
        input$daily_square_click,
        "_"
      )[[1]]
      
      
      row <- as.integer(
        coords[1]
      )
      
      col <- as.integer(
        coords[2]
      )
      
      
      state$daily_game <- make_daily_move(
        
        game = state$daily_game,
        
        row = row,
        
        col = col
      )
      
      
      save_daily_to_browser(
        state$daily_game
      )
    },
    
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
