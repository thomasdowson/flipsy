# ============================================================
# FLIPSY V2 - Shape Library
# ============================================================
#
# S = square clicked by player - flips
# F = additional square that flips
# X = connecting part of shape - does not flip
# . = empty
#
# Design rule:
# Every branch should have a purpose.
# X blocks should connect S to one or more F blocks.
# ============================================================


shapes_v2 <- list(
  
  
  # ----------------------------------------------------------
  # 1. CLASSIC L
  #
  # Simple introductory shape.
  #
  # S
  # X
  # X F
  # ----------------------------------------------------------
  
  create_shape_v2(
    c(
      "S.",
      "X.",
      "XF"
    ),
    name = "Classic L"
  ),
  
  
  # ----------------------------------------------------------
  # 2. DOUBLE ENDED
  #
  # S sits between two flippers.
  #
  # F X S X F
  # ----------------------------------------------------------
  
  create_shape_v2(
    c(
      "FXSXF"
    ),
    name = "Double Ended"
  ),
  
  
  # ----------------------------------------------------------
  # 3. THREE WAY
  #
  # S is a junction rather than an endpoint.
  #
  #     F
  #     X
  # F X S X F
  # ----------------------------------------------------------
  
  create_shape_v2(
    c(
      "..F..",
      "..X..",
      "FXSXF"
    ),
    name = "Three Way"
  ),
  
  
  # ----------------------------------------------------------
  # 4. OFFSET T
  #
  # Three flippers.
  # One lies behind S.
  #
  # F X F
  #   X
  #   S
  #   X
  #   F
  # ----------------------------------------------------------
  
  create_shape_v2(
    c(
      "FXF",
      ".X.",
      ".S.",
      ".X.",
      ".F."
    ),
    name = "Offset T"
  ),
  
  
  # ----------------------------------------------------------
  # 5. FOUR WAY
  #
  # S in the middle of a cross.
  #
  #     F
  #     X
  # F X S X F
  #     X
  #     F
  # ----------------------------------------------------------
  
  create_shape_v2(
    c(
      "..F..",
      "..X..",
      "FXSXF",
      "..X..",
      "..F.."
    ),
    name = "Four Way"
  ),
  
  
  # ----------------------------------------------------------
  # 6. NEAR AND FAR
  #
  # Two F squares occur on the same branch.
  #
  # S X F X F
  #
  # Player must notice BOTH F positions.
  # ----------------------------------------------------------
  
  create_shape_v2(
    c(
      "SXFXF"
    ),
    name = "Near and Far"
  ),
  
  
  # ----------------------------------------------------------
  # 7. ASYMMETRIC
  #
  # Different branch lengths make the shape harder
  # to mentally place on the board.
  #
  #       F
  #       X
  # F X S X X F
  #       X
  #       F
  # ----------------------------------------------------------
  
  # ----------------------------------------------------------
  # 7. ASYMMETRIC
  #
  # S is inside the shape rather than at an endpoint.
  #
  #   F
  #   X
  #   S X F
  #   X
  #   X X F
  # ----------------------------------------------------------
  
  create_shape_v2(
    c(
      ".F...",
      ".X...",
      ".SXF.",
      ".X...",
      ".XXF."
    ),
    name = "Asymmetric"
  ),
  
  
  # ----------------------------------------------------------
  # 8. BENT BRANCH
  #
  # Not every F lies on a straight line from S.
  #
  # S X X F
  # X
  # X F
  # ----------------------------------------------------------
  
  create_shape_v2(
    c(
      "SXXF",
      "X...",
      "XF.."
    ),
    name = "Bent Branch"
  )
)


# ============================================================
# Random Shape
#
# Never immediately repeats the current shape.
# ============================================================

random_shape_v2 <- function(
    current_shape = NULL
) {
  
  available <- shapes_v2
  
  
  if (!is.null(current_shape)) {
    
    same_shape <- vapply(
      available,
      function(shape) {
        
        identical(
          shape$name,
          current_shape$name
        )
      },
      logical(1)
    )
    
    
    available <- available[
      !same_shape
    ]
  }
  
  
  available[[
    sample(
      seq_along(available),
      size = 1
    )
  ]]
}