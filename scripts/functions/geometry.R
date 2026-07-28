signed_center_to_zone_ft <- function(plate_x, plate_z, sz_top, sz_bot) {
  half_width_ft <- 17 / 24
  qx <- abs(plate_x) - half_width_ft
  qz <- pmax(sz_bot - plate_z, plate_z - sz_top)
  sqrt(pmax(qx, 0)^2 + pmax(qz, 0)^2) + pmin(pmax(qx, qz), 0)
}

abs_edge_distance_inches <- function(
  plate_x, plate_z, sz_top, sz_bot, ball_radius_inches = 1.45
) {
  12 * signed_center_to_zone_ft(plate_x, plate_z, sz_top, sz_bot) -
    ball_radius_inches
}

classify_abs_call <- function(edge_distance_inches) {
  ifelse(is.na(edge_distance_inches), NA_character_,
    ifelse(edge_distance_inches <= 0, "called_strike", "ball")
  )
}

add_abs_geometry <- function(pitches) {
  pitches <- data.table::as.data.table(pitches)
  pitches[, edge_distance_inches := abs_edge_distance_inches(
    plate_x, plate_z, sz_top, sz_bot
  )]
  pitches[, abs_call := classify_abs_call(edge_distance_inches)]
  pitches[, geometry_available := !is.na(edge_distance_inches)]
  pitches[]
}

classify_opportunity <- function(pitches, sensitivity_inches = 0.25) {
  x <- data.table::as.data.table(pitches)
  x[, adverse_team_id := data.table::fifelse(
    initial_call == "called_strike", bat_team_id,
    data.table::fifelse(initial_call == "ball", fld_team_id, NA_integer_)
  )]
  x[, adverse_role := data.table::fifelse(
    initial_call == "called_strike", "offense",
    data.table::fifelse(initial_call == "ball", "defense", NA_character_)
  )]
  x[, correctable_opportunity := !is.na(abs_call) & !is.na(initial_call) &
    abs_call != initial_call]
  x[, boundary_sensitivity_excluded := !is.na(edge_distance_inches) &
    abs(edge_distance_inches) <= sensitivity_inches]
  x[]
}

