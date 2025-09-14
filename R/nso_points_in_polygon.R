


nso_points_in_polygon <- function(nso_polygon, nso_points, ea_explore){

   # Changing to a common polygon
  nso_points <- sf::st_transform(nso_points, sf::st_crs(nso_polygon))

  # Add row numbers within EA
  nso_points <- nso_points |>
    dplyr::group_by(hh04) |>
    dplyr::mutate(hh_no = dplyr::row_number())

  # Keep only unique values of EA code
  eas <- unique(nso_points$hh04)
  # cat(eas)
  # If EAs are specified, check only them,
  # otherwise take EAs from the data points
  ea_explore <- as.numeric(ea_explore)

  #if(sum(ea_explore %in% eas) == length(ea_explore))
  if(all(ea_explore %in% eas))
  {
    eas = ea_explore
  }
  #cat(eas)

  # empty vectors to populate
  ea_points <- c()          # for our EA
  hh_no <- c()              # hh counter
  points_within <- c()      # inside or not (0/1)

  coordinates_x <- c()      # for out x coordinate
  coordinates_y <- c()      # for out y coordinate

  hh_labels <- c()          # hh label
  ea_codes <- c()
  results_df <- c()


  distances_to_polygon <- c() # vector to store distances from point to poly

  for(sel_ea in eas)
  {

    # Select polygon for a specific enumeration EA
    selected_ea_poly <- nso_polygon |>
      filter(as.numeric(ea_number) == sel_ea)

    # Select the corresponding points for the selected EA
    selected_ea_points <- nso_points |>
      filter(as.numeric(hh04) == sel_ea)

    number_of_points = nrow(selected_ea_points)

    # A vector marking rows (points) falling inside a polygon with 1 otherwise 0
    points_within <- c(points_within, as.vector(lengths(st_within(selected_ea_points, selected_ea_poly))))

    # Calculate distances to polygon for all points
    # For points within polygon, distance will be 0
    # For points outside polygon, it will be the minimum distance to polygon boundary
    point_distances <- sf::st_distance(selected_ea_points, selected_ea_poly)


    # Convert distance matrix to vector (taking the minimum distance for each point)
    # point_distances_vector <- as.numeric(point_distances[, 1])
    point_distances_vector <- as.numeric(point_distances[, 1])


    # print(sort(point_distances_vector))

    # For points that are within the polygon, set distance to 0
    # point_distances_vector[points_within == 1] <- 0


    distances_to_polygon <- c(distances_to_polygon, point_distances_vector)
    # print(distances_to_polygon[distances_to_polygon > 0])

    # Container keeping EAs in a vector
    #ea_points <- c(ea_points, rep(sel_ea, nrow(selected_ea_points)))
    ea_points <- c(ea_points, rep(sel_ea, number_of_points))

    #Container keeping row numbers of hhs
    #hh_no <- c(hh_no, 1:nrow(selected_ea_points))
    hh_no <- c(hh_no, 1:number_of_points)

    # Extract coordinates from hh_latitud and hh_longitu columns
    #coordinates_x <- c(coordinates_x, selected_ea_points$hh_longitu)
    #coordinates_y <- c(coordinates_y, selected_ea_points$hh_latitud)

    # Extract hh_label and ea_code (assuming these are column names in your data)
    # Adjust column names if they're different in your actual data
    hh_labels <- c(hh_labels, selected_ea_points$hh_label)
    ea_codes <- c(ea_codes, selected_ea_points$hh04)

    count_ea_40 <- sum(as.vector(lengths(st_within(selected_ea_points, selected_ea_poly))))

    results_df <- rbind( results_df,
                         data.frame(ea_number = sel_ea,
                                    total_pts = number_of_points,
                                    within_ea = count_ea_40)
                         )
  }

  results_df <- results_df |>
    mutate(pcts_in = within_ea*100/total_pts)

  #cat(length(ea_codes), length(hh_no), length(points_within), length(coordinates_x), length(coordinates_y), length(hh_labels), length(distances_to_polygon))
  marked_points <- data.frame(ea_codes, hh_no, points_within,
                              #longitude = coordinates_x,
                              #latitude = coordinates_y,
                              hh_label = hh_labels,
                              distance_to_polygon = distances_to_polygon)

  return(list(marked_points = marked_points, summary_pts = results_df))
  #return (ls(results_df = results_df, marked_points = marked_points))

}


#
# ea_1_points <- nso_points_in_polygon(nso_polygon, nso_points, 1)
# view(ea_1_points)
#
#
# #Looping through EA POINTS TO MERGE THEM
#
# #eas <- unique(nso_points$hh04)
# eas <- c(55, 801)
# ea_results <- list()
# for (i in eas){
#
#
#   #ea_10 <- nso_points_in_polygon(nso_polygon, nso_points, i)
#
#   ea_results[[as.character(i)]] <- nso_points_in_polygon(nso_polygon, nso_points, i)
#
# }
#
# all_ea_points <- do.call(rbind, ea_results)
#
# view(all_ea_points)
#
