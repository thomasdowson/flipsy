# ============================================================
# FLIPSY - Minimum Viable Game
# ============================================================
#
# MVP structure:
#
# HOME
#   |
#   +-- QUICK PLAY
#   |      |
#   |      +-- EASY
#   |      +-- MEDIUM
#   |      +-- HARD
#   |
#   +-- DAILY FLIPSY
#          |
#          +-- Coming next
#
# Every puzzle has a minimum 5-move solution.
#
# ============================================================


library(shiny)


# ============================================================
# Source engines
# ============================================================

source("R/v_2/shape_engine_v2.R")
source("R/v_2/game_engine_v2.R")
source("R/v_2/puzzle_solver_v2.R")
source("R/v_2/puzzle_generator_v2.R")
source("R/v_2/shape_visualiser_v2.R")

source("R/v_3_shapes.R")
source("R/mvp/quick_play.R")


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
# UI
# ============================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    
    tags$style(
      
      HTML("

        /* ==================================================
           PAGE
           ================================================== */

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
          max-width: 520px;
          margin: 0 auto;
          padding: 35px 20px 50px 20px;
          text-align: center;
          box-sizing: border-box;
        }


        /* ==================================================
           BRAND
           ================================================== */

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


        /* ==================================================
           HEADINGS
           ================================================== */

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


        /* ==================================================
           BUTTONS
           ================================================== */

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

          min-width: 90px;
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


        /* ==================================================
           QUICK PLAY GAME
           ================================================== */

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


        /* ==================================================
           BOARD
           ================================================== */

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


        .square-button:hover {
          border-color: #000000;
        }


        /* ==================================================
           STATS
           ================================================== */

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


        /* ==================================================
           RESULT
           ================================================== */

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


        /* ==================================================
           DAILY PLACEHOLDER
           ================================================== */

        .daily-placeholder {

          margin: 45px auto 20px auto;

          max-width: 300px;

          padding: 25px;

          border: 1px solid #cccccc;

          border-radius: 5px;
        }


        .coming-soon {

          font-size: 14px;

          letter-spacing: 3px;

          font-weight: bold;

          margin-bottom: 12px;
        }


        /* ==================================================
           HOW TO PLAY
           ================================================== */

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


        /* ==================================================
           MOBILE
           ================================================== */

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
    
    uiOutput(
      "screen"
    )
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
    
    screen =
      "home",
    
    selected_difficulty =
      NULL,
    
    quick_game =
      NULL
  )
  
  
  # ==========================================================
  # Helper: start Quick Play game
  # ==========================================================
  
  start_quick_game <- function(
    difficulty,
    exclude_family_index = NULL
  ) {
    
    state$selected_difficulty <-
      difficulty
    
    
    state$quick_game <-
      generate_quick_puzzle(
        
        difficulty =
          difficulty,
        
        exclude_family_index =
          exclude_family_index
      )
    
    
    state$screen <-
      "quick_game"
  }
  
  
  # ==========================================================
  # HOME SCREEN
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
  # DIFFICULTY SCREEN
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
  # QUICK PLAY SCREEN
  # ==========================================================
  
  quick_game_screen <- function() {
    
    game <- state$quick_game
    
    
    req(
      game
    )
    
    
    difficulty_text <- toupper(
      game$difficulty
    )
    
    
    tagList(
      
      div(
        class = "logo",
        "FLIPSY"
      ),
      
      
      div(
        class = "difficulty-name",
        difficulty_text
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
        game
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
  # BOARD BUILDER
  # ==========================================================
  
  build_board_ui <- function(game) {
    
    board <- game$board
    
    
    squares <- lapply(
      
      seq_len(
        quick_grid_size *
          quick_grid_size
      ),
      
      function(i) {
        
        row <-
          ((i - 1) %/%
             quick_grid_size) + 1
        
        
        col <-
          ((i - 1) %%
             quick_grid_size) + 1
        
        
        colour_class <- if (
          board[row, col]
        ) {
          
          "black-square"
          
        } else {
          
          "white-square"
        }
        
        
        tags$button(
          
          type =
            "button",
          
          class =
            paste(
              "square-button",
              colour_class
            ),
          
          onclick =
            sprintf(
              
              "
              Shiny.setInputValue(
                'quick_square_click',
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
  }
  
  
  # ==========================================================
  # DAILY PLACEHOLDER
  # ==========================================================
  
  daily_screen <- function() {
    
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
        
        class = "daily-placeholder",
        
        div(
          class = "coming-soon",
          "COMING NEXT"
        ),
        
        
        div(
          "One puzzle. 15 moves. No reset."
        )
      ),
      
      
      actionButton(
        
        "daily_home",
        
        "← HOME",
        
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
            "In Quick Play, <strong>TO GO</strong>
             tells you the minimum number of moves
             currently required to solve the puzzle."
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
      
      state$screen <-
        "difficulty"
    }
  )
  
  
  observeEvent(
    input$go_daily,
    {
      
      state$screen <-
        "daily"
    }
  )
  
  
  observeEvent(
    input$go_help,
    {
      
      state$screen <-
        "help"
    }
  )
  
  
  observeEvent(
    input$difficulty_home,
    {
      
      state$screen <-
        "home"
    }
  )
  
  
  observeEvent(
    input$quick_home,
    {
      
      state$screen <-
        "home"
    }
  )
  
  
  observeEvent(
    input$daily_home,
    {
      
      state$screen <-
        "home"
    }
  )
  
  
  observeEvent(
    input$help_home,
    {
      
      state$screen <-
        "home"
    }
  )
  
  
  # ==========================================================
  # Difficulty selection
  # ==========================================================
  
  observeEvent(
    input$choose_easy,
    {
      
      start_quick_game(
        "easy"
      )
    }
  )
  
  
  observeEvent(
    input$choose_medium,
    {
      
      start_quick_game(
        "medium"
      )
    }
  )
  
  
  observeEvent(
    input$choose_hard,
    {
      
      start_quick_game(
        "hard"
      )
    }
  )
  
  
  # ==========================================================
  # Quick Play move
  # ==========================================================
  
  observeEvent(
    input$quick_square_click,
    {
      
      req(
        state$quick_game
      )
      
      
      if (
        state$screen !=
        "quick_game"
      ) {
        
        return()
      }
      
      
      if (
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
      
      
      state$quick_game <-
        make_quick_move(
          
          game =
            state$quick_game,
          
          row =
            row,
          
          col =
            col
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
      
      req(
        state$quick_game
      )
      
      
      state$quick_game <-
        reset_quick_puzzle(
          state$quick_game
        )
    }
  )
  
  
  # ==========================================================
  # Quick Play new puzzle
  # ==========================================================
  
  observeEvent(
    input$quick_new,
    {
      
      req(
        state$quick_game
      )
      
      
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
}


# ============================================================
# Run app
# ============================================================

shinyApp(
  ui = ui,
  server = server
)