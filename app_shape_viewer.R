# ============================================================
# FLIPSY - Compact Shape Viewer
# ============================================================
#
# Displays the top compact hard shapes as visual cards.
#
# Purpose:
# visually inspect mathematically promising shapes
# before deciding which ones deserve play-testing.
# ============================================================


library(shiny)


# ------------------------------------------------------------
# Source visualisation functions
# ------------------------------------------------------------

source("R/v_2/shape_engine_v2.R")
source("R/v_2/shape_visualiser_v2.R")


# ------------------------------------------------------------
# Load ranked compact shapes
# ------------------------------------------------------------

compact_hard <- readRDS(
  "analysis/outputs/compact_hard.rds"
)


# ------------------------------------------------------------
# How many to show
# ------------------------------------------------------------

n_show <- min(
  20,
  nrow(compact_hard)
)


viewer_shapes <- compact_hard[
  seq_len(n_show),
]


# ============================================================
# Shape rendering helper
# ============================================================

render_shape_card <- function(
    key,
    rank_number,
    family_id,
    n_flippers
) {
  
  shape <- key_to_shape_v2(
    key = key,
    name = paste0(
      "Family ",
      family_id
    )
  )
  
  
  mat <- shape$matrix
  
  cells <- list()
  
  
  for (row in seq_len(nrow(mat))) {
    
    for (col in seq_len(ncol(mat))) {
      
      value <- mat[row, col]
      
      
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
    
    class = "shape-card",
    
    div(
      class = "rank-label",
      paste0(
        "#",
        rank_number
      )
    ),
    
    div(
      
      class = "shape-grid",
      
      style = sprintf(
        "grid-template-columns: repeat(%s, 30px);",
        ncol(mat)
      ),
      
      cells
    ),
    
    div(
      class = "family-label",
      paste(
        "Family",
        family_id
      )
    ),
    
    div(
      class = "flipper-label",
      paste(
        n_flippers,
        "F"
      )
    )
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
          background: #f5f5f5;
          font-family: Arial, sans-serif;
          color: #111;
        }

        .viewer-container {
          max-width: 1100px;
          margin: 30px auto;
        }

        .viewer-title {
          text-align: center;
          font-size: 34px;
          font-weight: bold;
          letter-spacing: 2px;
          margin-bottom: 5px;
        }

        .viewer-subtitle {
          text-align: center;
          font-size: 15px;
          margin-bottom: 30px;
          color: #555;
        }

        .card-grid {
          display: grid;
          grid-template-columns: repeat(
            auto-fit,
            minmax(180px, 1fr)
          );
          gap: 18px;
        }

        .shape-card {
          background: white;
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 18px;
          text-align: center;
        }

        .rank-label {
          font-size: 20px;
          font-weight: bold;
          margin-bottom: 14px;
        }

        .shape-grid {
          display: inline-grid;
          gap: 3px;
          justify-content: center;
          margin: 0 auto 14px auto;
        }

        .shape-cell {
          width: 30px;
          height: 30px;

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

        .family-label {
          font-size: 14px;
          font-weight: bold;
          margin-top: 5px;
        }

        .flipper-label {
          font-size: 12px;
          color: #666;
          margin-top: 3px;
        }

      ")
    )
  ),
  
  
  div(
    
    class = "viewer-container",
    
    div(
      class = "viewer-title",
      "FLIPSY"
    ),
    
    div(
      class = "viewer-subtitle",
      "Top compact mathematically difficult shapes"
    ),
    
    uiOutput(
      "shape_cards"
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
  
  
  output$shape_cards <- renderUI({
    
    
    cards <- lapply(
      
      seq_len(nrow(viewer_shapes)),
      
      function(i) {
        
        render_shape_card(
          
          key =
            viewer_shapes$canonical_key[i],
          
          rank_number =
            viewer_shapes$hard_rank[i],
          
          family_id =
            viewer_shapes$family_id[i],
          
          n_flippers =
            viewer_shapes$n_flippers[i]
        )
      }
    )
    
    
    div(
      class = "card-grid",
      cards
    )
  })
}


# ============================================================
# Run
# ============================================================

shinyApp(
  ui = ui,
  server = server
)