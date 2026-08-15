# ============================================================
# FLIPSY V3 - Shape Library
# ============================================================
#
# V3 design rules:
#
# - Exactly 2 Fs
# - S always flips
# - Compact shapes preferred
# - Full GF(2) rank on the 5x5 board
# - Rotations/reflections allowed during gameplay
#
# The canonical families below come from the exhaustive
# mathematical shape analysis.
#
# Difficulty values are PROVISIONAL.
# They will eventually be replaced by human play-test data.
# ============================================================


# ============================================================
# Curated V3 family library
# ============================================================

v3_shape_families <- data.frame(
  
  family_id = c(
    7,
    26,
    39,
    20,
    24,
    10,
    6,
    11,
    28,
    21,
    47,
    27,
    40,
    19,
    37
  ),
  
  canonical_key = c(
    "-2:-2|-2:0",
    "-2:-1|0:-2",
    "-2:0|0:-2",
    "-1:-2|1:-2",
    "-1:-1|1:-2",
    "-2:-2|-1:-1",
    "-2:-2|-1:-2",
    "-2:-2|-1:0",
    "-1:-2|1:0",
    "-2:-1|-1:-2",
    "-1:-1|1:1",
    "-1:-2|-1:0",
    "-1:0|0:-2",
    "-1:-2|0:-2",
    "-1:-1|0:-2"
  ),
  
  structural_score = c(
    3.38175627,
    3.03926278,
    1.61626237,
    1.27376888,
    1.07811084,
    0.05292137,
    -0.25592255,
    -0.25592255,
    -0.25592255,
    -0.68738306,
    -0.82982558,
    -1.05226812,
    -1.05226812,
    -1.05226812,
    -1.33689319
  ),
  
  stringsAsFactors = FALSE
)


# ============================================================
# Provisional difficulty
# ============================================================
#
# This is NOT yet measured human difficulty.
#
# It converts our mathematical ranking into provisional
# game tiers until we have play-test data.
#
# 5 = predicted hardest
# 1 = predicted easiest
# ============================================================

v3_shape_families$difficulty <- c(
  
  5,  # Family 7
  5,  # Family 26
  
  4,  # Family 39
  4,  # Family 20
  4,  # Family 24
  
  3,  # Family 10
  3,  # Family 6
  3,  # Family 11
  3,  # Family 28
  
  2,  # Family 21
  2,  # Family 47
  2,  # Family 27
  2,  # Family 40
  
  1,  # Family 19
  1   # Family 37
)


# ============================================================
# Shape names
# ============================================================

v3_shape_families$shape_name <- paste0(
  "Shape ",
  seq_len(
    nrow(v3_shape_families)
  )
)


# ============================================================
# Basic library validation
# ============================================================

stopifnot(
  all(
    v3_shape_families$difficulty %in% 1:5
  )
)


stopifnot(
  !anyDuplicated(
    v3_shape_families$family_id
  )
)


stopifnot(
  !anyDuplicated(
    v3_shape_families$canonical_key
  )
)


# ============================================================
# Get one canonical V3 shape
# ============================================================

get_v3_shape <- function(index) {
  
  row <- v3_shape_families[
    index,
  ]
  
  
  key_to_shape_v2(
    
    key =
      row$canonical_key,
    
    name =
      row$shape_name
  )
}


# ============================================================
# Get metadata
# ============================================================

get_v3_shape_info <- function(index) {
  
  v3_shape_families[
    index,
  ]
}


# ============================================================
# Number of V3 shape families
# ============================================================

n_v3_shapes <- function() {
  
  nrow(
    v3_shape_families
  )
}


# ============================================================
# Random family index
# ============================================================

random_v3_shape_index <- function(
    exclude = NULL
) {
  
  available <- seq_len(
    n_v3_shapes()
  )
  
  
  if (!is.null(exclude)) {
    
    available <- setdiff(
      available,
      exclude
    )
  }
  
  
  if (length(available) == 0) {
    
    stop(
      "No V3 shapes available after exclusions."
    )
  }
  
  
  sample(
    available,
    1
  )
}


# ============================================================
# Filter by provisional difficulty
# ============================================================

v3_shapes_by_difficulty <- function(
    difficulty
) {
  
  which(
    v3_shape_families$difficulty ==
      difficulty
  )
}


# ============================================================
# Canonical shape + metadata
# ============================================================

make_v3_shape <- function(index) {
  
  list(
    
    shape =
      get_v3_shape(index),
    
    info =
      get_v3_shape_info(index),
    
    index =
      index
  )
}


# ============================================================
# Validate 2-F rule
# ============================================================
#
# create_shape_v2() stores:
#
# S = (0,0)
# F = first F
# F = second F
#
# Therefore every V3 shape should contain exactly THREE
# flipping offsets in total.
# ============================================================

validate_v3_library <- function() {
  
  results <- logical(
    n_v3_shapes()
  )
  
  
  for (
    i in seq_len(
      n_v3_shapes()
    )
  ) {
    
    shape <- get_v3_shape(i)
    
    
    offsets <-
      shape$flipper_offsets
    
    
    s_count <- sum(
      offsets$row_offset == 0 &
        offsets$col_offset == 0
    )
    
    
    total_flipping_positions <-
      nrow(offsets)
    
    
    results[i] <-
      s_count == 1 &&
      total_flipping_positions == 3
  }
  
  
  results
}


# ============================================================
# ROTATIONS AND REFLECTIONS
# ============================================================
#
# Each entry in v3_shape_families represents one mathematical
# family modulo symmetry.
#
# During gameplay we can present any unique rotation or
# reflection of that family.
#
# S remains fixed at (0,0).
# ============================================================


# ------------------------------------------------------------
# Rotate 90 degrees
#
# Coordinate transformation:
#
# (row, col) -> (col, -row)
# ------------------------------------------------------------
# ============================================================
# ROTATIONS AND REFLECTIONS
# ============================================================


# Rotate offsets 90 degrees
rotate_offsets_90 <- function(offsets) {
  
  data.frame(
    row_offset = offsets$col_offset,
    col_offset = -offsets$row_offset
  )
}


# Reflect offsets across vertical axis
reflect_offsets <- function(offsets) {
  
  data.frame(
    row_offset = offsets$row_offset,
    col_offset = -offsets$col_offset
  )
}


# Create deterministic key
offset_key_v3 <- function(offsets) {
  
  offsets <- offsets[
    order(
      offsets$row_offset,
      offsets$col_offset
    ),
  ]
  
  paste(
    paste0(
      offsets$row_offset,
      ":",
      offsets$col_offset
    ),
    collapse = "|"
  )
}


# ============================================================
# Generate unique orientations
# ============================================================

get_v3_orientations <- function(index) {
  
  info <- get_v3_shape_info(index)
  
  original <- parse_pattern_key_v2(
    info$canonical_key
  )
  
  orientations <- list()
  
  # Four rotations
  current <- original
  
  for (i in 1:4) {
    
    orientations[[length(orientations) + 1]] <- current
    
    current <- rotate_offsets_90(current)
  }
  
  # Reflect original
  current <- reflect_offsets(original)
  
  # Four rotations of reflection
  for (i in 1:4) {
    
    orientations[[length(orientations) + 1]] <- current
    
    current <- rotate_offsets_90(current)
  }
  
  # Generate keys
  keys <- vapply(
    orientations,
    offset_key_v3,
    character(1)
  )
  
  # Remove symmetry duplicates
  orientations <- orientations[
    !duplicated(keys)
  ]
  
  return(orientations)
}


# ============================================================
# Random orientation
# ============================================================

get_random_v3_shape <- function(index) {
  
  orientations <- get_v3_orientations(index)
  
  orientation_number <- sample(
    seq_along(orientations),
    1
  )
  
  offsets <- orientations[[orientation_number]]
  
  oriented_key <- offset_key_v3(offsets)
  
  info <- get_v3_shape_info(index)
  
  shape <- key_to_shape_v2(
    key = oriented_key,
    name = info$shape_name
  )
  
  list(
    shape = shape,
    info = info,
    family_index = index,
    orientation = orientation_number,
    n_orientations = length(orientations),
    oriented_key = oriented_key
  )
}


# ============================================================
# Orientation count
# ============================================================

n_v3_orientations <- function(index) {
  
  length(
    get_v3_orientations(index)
  )
}


# ============================================================
# Validate whole library
# ============================================================

library_validation <- validate_v3_library()

if (!all(library_validation)) {
  
  stop(
    paste(
      "Invalid V3 shapes:",
      paste(
        which(!library_validation),
        collapse = ", "
      )
    )
  )
}


# ============================================================
# Orientation summary
# ============================================================

orientation_counts <- vapply(
  seq_len(n_v3_shapes()),
  n_v3_orientations,
  integer(1)
)


cat(
  "FLIPSY V3 shape library loaded:",
  n_v3_shapes(),
  "compact 2-F families\n"
)

cat(
  "Unique orientations per family:",
  paste(
    orientation_counts,
    collapse = ", "
  ),
  "\n"
)