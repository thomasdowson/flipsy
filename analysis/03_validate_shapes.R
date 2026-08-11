# ============================================================
# FLIPSY - Shape Analysis
# 03 Validate and Describe Shape Families
# ============================================================
#
# For every unique symmetry family:
#
# - reconstruct its canonical F offsets
# - validate it on the 5x5 torus
# - measure distance/spread
# - classify axial / diagonal / oblique positions
# - measure symmetry
# - count distinct orientations
# - measure local S/F connectivity
#
# X blocks are still NOT considered.
# ============================================================


# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

board_size <- 5


# ------------------------------------------------------------
# Load canonical families
# ------------------------------------------------------------

family_lookup <- readRDS(
  "analysis/outputs/family_lookup.rds"
)

canonical_patterns <- readRDS(
  "analysis/outputs/canonical_patterns.rds"
)


cat(
  "Families to analyse:",
  nrow(family_lookup),
  "\n"
)


# ============================================================
# Helpers
# ============================================================


# ------------------------------------------------------------
# Parse canonical key back into coordinates
#
# Example:
#
# "-2:0|0:2"
#
# becomes:
#
# row_offset col_offset
#    -2          0
#     0          2
# ------------------------------------------------------------

parse_pattern_key <- function(key) {
  
  pieces <- strsplit(
    key,
    "\\|"
  )[[1]]
  
  
  coords <- lapply(
    pieces,
    function(piece) {
      
      values <- as.integer(
        strsplit(
          piece,
          ":",
          fixed = TRUE
        )[[1]]
      )
      
      
      data.frame(
        row_offset = values[1],
        col_offset = values[2]
      )
    }
  )
  
  
  do.call(
    rbind,
    coords
  )
}


# ------------------------------------------------------------
# Convert coordinate back into canonical -2:2 form
# ------------------------------------------------------------

canonical_coord <- function(
    x,
    n = board_size
) {
  
  wrapped <- x %% n
  
  ifelse(
    wrapped > floor(n / 2),
    wrapped - n,
    wrapped
  )
}


# ------------------------------------------------------------
# Deterministic key for coordinate set
# ------------------------------------------------------------

pattern_key <- function(coords) {
  
  coords <- coords[
    order(
      coords$row_offset,
      coords$col_offset
    ),
  ]
  
  
  paste(
    paste(
      coords$row_offset,
      coords$col_offset,
      sep = ":"
    ),
    collapse = "|"
  )
}


# ------------------------------------------------------------
# Apply one of eight square symmetries
# ------------------------------------------------------------

transform_pattern <- function(
    coords,
    transform_id
) {
  
  r <- coords$row_offset
  c <- coords$col_offset
  
  
  transformed <- switch(
    
    as.character(transform_id),
    
    "1" = data.frame(
      row_offset = r,
      col_offset = c
    ),
    
    "2" = data.frame(
      row_offset = c,
      col_offset = -r
    ),
    
    "3" = data.frame(
      row_offset = -r,
      col_offset = -c
    ),
    
    "4" = data.frame(
      row_offset = -c,
      col_offset = r
    ),
    
    "5" = data.frame(
      row_offset = r,
      col_offset = -c
    ),
    
    "6" = data.frame(
      row_offset = -r,
      col_offset = c
    ),
    
    "7" = data.frame(
      row_offset = c,
      col_offset = r
    ),
    
    "8" = data.frame(
      row_offset = -c,
      col_offset = -r
    )
  )
  
  
  transformed$row_offset <-
    canonical_coord(
      transformed$row_offset
    )
  
  transformed$col_offset <-
    canonical_coord(
      transformed$col_offset
    )
  
  
  transformed
}


# ------------------------------------------------------------
# Toroidal difference
#
# Returns shortest distance along one board dimension.
# ------------------------------------------------------------

toroidal_difference <- function(
    a,
    b,
    n = board_size
) {
  
  d <- abs(a - b)
  
  pmin(
    d,
    n - d
  )
}


# ------------------------------------------------------------
# Mean pairwise toroidal Euclidean distance
#
# Includes S at (0,0).
# ------------------------------------------------------------

mean_pairwise_distance <- function(coords) {
  
  points <- rbind(
    
    data.frame(
      row_offset = 0,
      col_offset = 0
    ),
    
    coords
  )
  
  
  if (nrow(points) < 2) {
    return(0)
  }
  
  
  distances <- numeric(0)
  
  
  for (i in 1:(nrow(points) - 1)) {
    
    for (j in (i + 1):nrow(points)) {
      
      dr <- toroidal_difference(
        points$row_offset[i],
        points$row_offset[j]
      )
      
      dc <- toroidal_difference(
        points$col_offset[i],
        points$col_offset[j]
      )
      
      
      distances <- c(
        distances,
        sqrt(
          dr^2 +
            dc^2
        )
      )
    }
  }
  
  
  mean(distances)
}


# ------------------------------------------------------------
# Count connected components of S/F points
#
# Two points are considered adjacent if they are a king-move
# apart on the torus:
#
# horizontal, vertical or diagonal distance <= 1.
#
# This does NOT imply X blocks cannot later connect them.
# It simply describes immediate S/F adjacency.
# ------------------------------------------------------------

count_adjacent_components <- function(coords) {
  
  points <- rbind(
    
    data.frame(
      row_offset = 0,
      col_offset = 0
    ),
    
    coords
  )
  
  
  n_points <- nrow(points)
  
  adjacency <- matrix(
    FALSE,
    nrow = n_points,
    ncol = n_points
  )
  
  
  for (i in seq_len(n_points)) {
    
    for (j in seq_len(n_points)) {
      
      if (i == j) {
        next
      }
      
      
      dr <- toroidal_difference(
        points$row_offset[i],
        points$row_offset[j]
      )
      
      dc <- toroidal_difference(
        points$col_offset[i],
        points$col_offset[j]
      )
      
      
      adjacency[i, j] <-
        max(dr, dc) == 1
    }
  }
  
  
  visited <- rep(
    FALSE,
    n_points
  )
  
  components <- 0L
  
  
  for (start in seq_len(n_points)) {
    
    if (visited[start]) {
      next
    }
    
    
    components <- components + 1L
    
    queue <- start
    
    visited[start] <- TRUE
    
    
    while (length(queue) > 0) {
      
      current <- queue[1]
      
      queue <- queue[-1]
      
      
      neighbours <- which(
        adjacency[current, ] &
          !visited
      )
      
      
      if (length(neighbours) > 0) {
        
        visited[neighbours] <- TRUE
        
        queue <- c(
          queue,
          neighbours
        )
      }
    }
  }
  
  
  components
}


# ------------------------------------------------------------
# Geometry classification
# ------------------------------------------------------------

geometry_class <- function(coords) {
  
  is_axis <-
    coords$row_offset == 0 |
    coords$col_offset == 0
  
  
  is_diagonal <-
    coords$row_offset != 0 &
    coords$col_offset != 0 &
    abs(coords$row_offset) ==
    abs(coords$col_offset)
  
  
  is_oblique <-
    !is_axis &
    !is_diagonal
  
  
  if (all(is_axis)) {
    
    return("axis_only")
  }
  
  
  if (all(is_diagonal)) {
    
    return("diagonal_only")
  }
  
  
  if (all(!is_axis) && any(is_oblique)) {
    
    return("off_axis")
  }
  
  
  "mixed"
}


# ============================================================
# Analyse each family
# ============================================================

results <- vector(
  "list",
  nrow(family_lookup)
)


for (i in seq_len(nrow(family_lookup))) {
  
  
  if (i %% 250 == 0) {
    
    cat(
      "Processed",
      i,
      "families\n"
    )
  }
  
  
  family_id <-
    family_lookup$family_id[i]
  
  key <-
    family_lookup$canonical_key[i]
  
  
  coords <- parse_pattern_key(
    key
  )
  
  
  n_flippers <- nrow(coords)
  
  
  # ----------------------------------------------------------
  # Validation
  # ----------------------------------------------------------
  
  wrapped_positions <- data.frame(
    
    row =
      coords$row_offset %% board_size,
    
    col =
      coords$col_offset %% board_size
  )
  
  
  no_duplicate_flippers <-
    !any(
      duplicated(
        wrapped_positions
      )
    )
  
  
  no_flipper_on_s <-
    !any(
      coords$row_offset %% board_size == 0 &
        coords$col_offset %% board_size == 0
    )
  
  
  valid <-
    no_duplicate_flippers &&
    no_flipper_on_s
  
  
  # ----------------------------------------------------------
  # Distance from S
  # ----------------------------------------------------------
  
  euclidean_distance <- sqrt(
    coords$row_offset^2 +
      coords$col_offset^2
  )
  
  
  manhattan_distance <-
    abs(coords$row_offset) +
    abs(coords$col_offset)
  
  
  chebyshev_distance <- pmax(
    abs(coords$row_offset),
    abs(coords$col_offset)
  )
  
  
  # ----------------------------------------------------------
  # Direction types
  # ----------------------------------------------------------
  
  is_axis <-
    coords$row_offset == 0 |
    coords$col_offset == 0
  
  
  is_diagonal <-
    coords$row_offset != 0 &
    coords$col_offset != 0 &
    abs(coords$row_offset) ==
    abs(coords$col_offset)
  
  
  is_oblique <-
    !is_axis &
    !is_diagonal
  
  
  # ----------------------------------------------------------
  # Symmetry
  #
  # Count number of distinct versions among the 8
  # rotations/reflections.
  # ----------------------------------------------------------
  
  transformed_keys <- vapply(
    
    1:8,
    
    function(transform_id) {
      
      transformed <- transform_pattern(
        coords,
        transform_id
      )
      
      
      pattern_key(
        transformed
      )
    },
    
    character(1)
  )
  
  
  n_orientations <-
    length(
      unique(
        transformed_keys
      )
    )
  
  
  symmetry_count <-
    8 / n_orientations
  
  
  # ----------------------------------------------------------
  # Spatial measures
  # ----------------------------------------------------------
  
  mean_pairwise <-
    mean_pairwise_distance(
      coords
    )
  
  
  components <-
    count_adjacent_components(
      coords
    )
  
  
  # ----------------------------------------------------------
  # Store
  # ----------------------------------------------------------
  
  results[[i]] <- data.frame(
    
    family_id = family_id,
    
    canonical_key = key,
    
    n_flippers = n_flippers,
    
    valid = valid,
    
    n_axis = sum(is_axis),
    
    n_diagonal = sum(is_diagonal),
    
    n_oblique = sum(is_oblique),
    
    geometry_class =
      geometry_class(coords),
    
    mean_distance_from_s =
      mean(euclidean_distance),
    
    max_distance_from_s =
      max(euclidean_distance),
    
    mean_manhattan_from_s =
      mean(manhattan_distance),
    
    max_manhattan_from_s =
      max(manhattan_distance),
    
    max_chebyshev_from_s =
      max(chebyshev_distance),
    
    mean_pairwise_distance =
      mean_pairwise,
    
    adjacent_components =
      components,
    
    n_orientations =
      n_orientations,
    
    symmetry_count =
      symmetry_count,
    
    stringsAsFactors = FALSE
  )
}


# ============================================================
# Combine results
# ============================================================

family_structural_metrics <- do.call(
  rbind,
  results
)


rownames(
  family_structural_metrics
) <- NULL


# ============================================================
# Summary
# ============================================================

cat(
  "\nValid families:",
  sum(family_structural_metrics$valid),
  "of",
  nrow(family_structural_metrics),
  "\n\n"
)


cat(
  "Geometry classes:\n"
)


print(
  table(
    family_structural_metrics$geometry_class
  )
)


cat(
  "\nNumber of orientations:\n"
)


print(
  table(
    family_structural_metrics$n_orientations
  )
)


cat(
  "\nSymmetry counts:\n"
)


print(
  table(
    family_structural_metrics$symmetry_count
  )
)


# ============================================================
# Save
# ============================================================

saveRDS(
  family_structural_metrics,
  "analysis/outputs/family_structural_metrics.rds"
)


write.csv(
  family_structural_metrics,
  "analysis/outputs/family_structural_metrics.csv",
  row.names = FALSE
)


cat(
  "\nStructural family metrics saved.\n"
)