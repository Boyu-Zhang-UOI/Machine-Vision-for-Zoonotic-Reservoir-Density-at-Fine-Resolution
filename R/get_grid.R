#' Jitter data
#'
#' @param cleaned_data
#' @param x_range
#' @param grid_size_degrees
#' @param n_jitters
#'
#' @return
#' @export
#'
#' @examples
get_grid <- function(cleaned_data,
                     x_range,
                     y_range,
                     grid_size_degrees,
                     n_jitters = 5) {

  # Jittering moves the boundaries of the 50 spatial blocks by
  # shifting each by a constant amount in the x and y direction.
  # The following produces 5 randomly shifted datasets as a list.
  # We also need to identify which grid each point in cleaned_data belongs to
  # this is called 'boxi'
  jittered_data <- map(1:n_jitters, function(jitter) {

    # How much do we jitter in each direction? Note that this is proportional to the grid size.
    # That means we could potentially shift the entire grid over by close to the width of one cell.
    djx = grid_size_degrees*runif(1)
    djy = grid_size_degrees*runif(1)

    # Widen the ranges of each direction to account for jittering.
    # This is like moving the carriage of a typewriter all the way to
    # the left and top then entering in a random number of spaces and
    # carriage returns to get back onto the page. You end up with
    # different margin widths.
    x_range = x_range + c(-grid_size_degrees, grid_size_degrees)
    y_range = y_range + c(-grid_size_degrees, grid_size_degrees)

    # Apply the jitter. This is shifting back onto the page.
    x_range = x_range + djx
    y_range = y_range + djy

    # Now that we have the boundaries, overlay the grid on the
    # data and identify which cell each point falls within.
    x_grid <- seq(x_range[1], x_range[2], by = grid_size_degrees)
    y_grid <- seq(y_range[1], y_range[2], by = grid_size_degrees)

    # How many cells are there in each direction in the 50m grid?
    nx = length(x_grid)
    ny = length(y_grid)

    # numpy.searchsorted() equivalent
    # Find the indices in a sorted arrays (x_grid and y_grid) a such that, if the corresponding
    # elements of cleaned_data (Longitude and Latitude) were inserted before the indices,
    # the order of the arrays would be preserved. Bunch of jargon for which cell does the
    # point go in.
    colx <- map_int(cleaned_data$Longitude, ~which(x_grid <= .x) |> tail(n=1))
    rowy <- map_int(cleaned_data$Latitude, ~which(y_grid <= .x) |> tail(n=1))
    boxi <- nx*(rowy - 1) + colx # python zero indexes R doesn't

    # Add new columns to the cleaned_data
    cleaned_data_gridded <- cleaned_data |> mutate(Colx = colx, Rowy = rowy, Boxi = boxi)

    # Just take the mean of every column not in the grouping variables
    # plus add in Tot_Mn and Tot_traps.
    cleaned_data_agg <- cleaned_data_gridded |>
      select(-c(Longitude, Latitude, Trap_ID)) |>
      group_by(Site, House, Visit, Night, Boxi) |>
      summarize(Jitter = jitter,
                Tot_Mn = sum(ifelse(Mna == 1, Trap.weight, 0)),
                Tot_Traps = sum(Trap.weight),
                Tot_Other = Tot_Traps - Tot_Mn,
                TS_Mn = Tot_Mn / Tot_Traps,
                across(where(is.numeric), ~mean(.x, na.rm=T)),
                .groups = "drop")
  }) |> bind_rows()
}
