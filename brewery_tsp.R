library(googleway)
library(TSP)
library(tidyverse)
library(stringr)
library(leaflet)
library(httr)

# Put your API keys here, or set them as environment variables
MAPS_API_KEY <- Sys.getenv("MAPS_API_KEY")
LYFT_API_KEY <- Sys.getenv("LYFT_API_KEY")

dat <- tibble(address = c("aeuronaut brewery, somerville ma",
               "harpoon brewery, boston ma",
               "sam adams brewery, boston ma",
               "turtle swamp brewing, boston ma",
               "boston beer works, boston ma",
               "downeast cider house, boston ma",
               "winter hill brewing company, somerville ma",
               "cambridge brewing company, cambridge ma",
               "trillium brewing company, boston ma",
               "john harvards brewery, cambridge ma",
               "lamplighter brewing company, cambridge ma",
               "dorchester brewing company, boston ma",
               "bantam cider company, somerville ma",
               "somerville brewing company, 15 ward street, somerville ma",
               "mystic brewery, chelsea ma",
               "idle hands craft ales, malden ma",
               "night shift brewing, everett ma",
               "bone up brewing company, everett ma"))

#
# Geocode the addresses
#
dat <- dat %>%
  mutate(location = purrr::map(address, google_geocode, key = MAPS_API_KEY))

dat <- dat %>%
  mutate(latlng = purrr::map(location, function(x) x$results$geometry$location)) %>%
  unnest(latlng)

#
# Builds a distance matrix from a vector of addresses
#
get_google_distance_matrix <- function(addresses, key, mode = "driving") {
  distance_matrix <- matrix(nrow = length(addresses), ncol = length(addresses), 0)

  for(i in 1:length(addresses)) {
    dmat_raw <- google_distance(addresses[i], addresses, mode = mode, units = "imperial", key = key)
    print(dmat_raw)
    dmat <- map(dmat_raw$rows$elements, function(df) df$duration$value) %>%
      unlist() %>%
      matrix(ncol = 1)

    distance_matrix[i,] <- t(dmat)
  }

  return(distance_matrix)
}

distance_matrix_car <- get_google_distance_matrix(dat$address, MAPS_API_KEY, mode = "driving")

#
# Solve the asymmetric traveling salesman problem.
#
get_soln <- function(dat, distance_matrix) {
  tsp <- ATSP(distance_matrix, labels = dat$address)
  soln <- solve_TSP(tsp)

  return(soln)
}

soln_car <- get_soln(dat, distance_matrix_car)

#
# Given a solution, retrieve detailed directions between each point
#
get_directions <- function(soln, dat, distance_matrix, mode = "driving") {
  dat %>%
    mutate(order = as.integer(soln))
  
  dat <- dat[dat$order,]
  
  dat$to <- dat$address[seq_along(dat$address) %% nrow(dat) + 1]

  dat <- dat %>%
    mutate(directions = map2(address, to, function(address, to) { 
      print(paste("FROM:", address, "TO:", to))
      google_directions(address, to, key = MAPS_API_KEY, mode = mode)
    }))

  dat <- dat %>%
    mutate(directions_points = map(directions, function(x) decode_pl(x$routes$overview_polyline$points)))

  return(dat)
}

#
# Estime price to take a Lyft between all locations in the tour
#
get_lyft_estimate <- function(soln, dat, token) {
  dat$order <- as.integer(soln)
  dat <- dat[dat$order,]
  
  # Make Aeronaut first
  aeronaut_index <- which(dat$address == "aeuronaut brewery, somerville ma")
  dat <- dat[c(aeronaut_index:nrow(dat), 1:(aeronaut_index - 1)),]
  
  dat$to_lat <- dat$lat[seq_along(dat$lat) %% nrow(dat) + 1]
  dat$to_lng <- dat$lng[seq_along(dat$lng) %% nrow(dat) + 1]
  
  url <- paste0("https://api.lyft.com/v1/cost")
  
  dat <- dat %>%
    mutate(resp = pmap(list(start_lat = lat, start_lng = lng, end_lat = to_lat, end_lng = to_lng), function(start_lat, start_lng, end_lat, end_lng) {
      query <- list(ride_type="lyft", start_lat = start_lat, start_lng  = start_lng, end_lat = end_lat, end_lng = end_lng)
      resp <- GET(url, add_headers("Authorization" = paste0("Bearer ", token)),
          query = query)
      content(resp)
    }))
  
  dat <- dat %>% mutate(price_min = map(resp, function(resp) resp$cost_estimates[[1]]$estimated_cost_cents_min),
                 price_max = map(resp, function(resp) resp$cost_estimates[[1]]$estimated_cost_cents_max)) %>%
    unnest(price_min, price_max)
  
  c(sum(dat$price_min), sum(dat$price_max))  
}

get_lyft_estimate(soln_car, dat, token = LYFT_API_TOKEN)

#
# Outputs a link to Google Maps directions to every point
#
get_maps_link <- function(soln, dat) {
  dat %>%
    mutate(order = as.integer(soln))
  
  dat <- dat[dat$order,]
  
  # Make Aeronaut first
  aeronaut_index <- which(dat$address == "aeuronaut brewery, somerville ma")
  dat <- dat[c(aeronaut_index:nrow(dat), 1:(aeronaut_index - 1)),]
  
  paste0("https://www.google.com/maps/dir/", paste0(dat$address, collapse = "/"))
}

directions_cars <- get_directions(soln_car, dat, distance_matrix_car, mode = "driving")

#
# Make an interactive map
#

directions_points <- directions_cars %>%
  select(-lat, -lng) %>%
  mutate(directions_points = map(directions_points, rownames_to_column, "point_order")) %>%
  unnest(directions_points)

map <- leaflet() %>%
  addTiles() %>%
  addPolylines(lat = directions_points$lat, lng = directions_points$lon) %>%
  addMarkers(lat = directions_cars$lat, lng = directions_cars$lng, label = directions_cars$address)

map$width <- 600
map$height <- 600

saveWidget(map, "~/herbps10.github.io/public/maps/brewery.html", selfcontained = TRUE)
