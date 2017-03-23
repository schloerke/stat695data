

basketball_year_url <- function(year) {
  str_c("http://www.espn.com/mens-college-basketball/statistics/team/_/stat/scoring/sort/points/year/", year, "/seasontype/2")
}



get_teams <- function(year) {

  ret_links <- list()

  # if find "jcarousel-next-disabled", stop
  cur_url <- basketball_year_url(year)
  while (TRUE) {

    store_name <- cur_url %>%
      str_replace("http://www.espn.com/mens-college-basketball/statistics/team/_/stat/scoring/sort/points/", "") %>%
      str_replace("/seasontype/2", "") %>%
      str_replace_all("/", "_")

    html <- cached_html(cur_url, file.path("teams", store_name))

    team_links <- html %>% html_nodes(".tablehead td:nth-child(2) a") %>% html_attr("href")

    ret_links <- append(ret_links, team_links)

    diabled_next_button <- html %>% html_node(".jcarousel-next-disabled")
    if (!is.na(diabled_next_button)) {
      break
    }
    next_link <- html %>% html_nodes(".mod-footer .controls a") %>% html_attr("href") %>% tail(1)
    cur_url <- next_link
  }

  team_links <- unique(unlist(ret_links))

  team_ids <- str_match(team_links, "_/id/(\\d+)/")[,2]

  data_frame(
    year, team_ids, team_links
  )
}

get_all_team_ids <- function(start = 2002, end = 2017) {
  seq(start, end, by = 1) %>%
    lapply(get_teams) %>%
    bind_rows()
}


basketball_team_year_url <- function(year, id) {
  str_c("http://www.espn.com/mens-college-basketball/team/stats/_/id/", id, "/year/", year)
}

get_team_stats <- function(year, id) {

  team_url <- basketball_team_year_url(year, id)

  store_name <- file.path("team_stats", year, str_c(year, "-", id))
  html <- cached_html(team_url, store_name)

  team_name <- html %>% html_node("#sub-branding b") %>% html_text()

  stats <- html %>% html_nodes(".mod-content table")

  game_statistics <- stats[[1]] %>% html_table()
  game_statistics <- game_statistics[-1:-2, ]
  game_statistics <- game_statistics[- nrow(game_statistics), ]
  colnames(game_statistics) <- c(
    "player", "games_played", "avg_minutes", "avg_points", "avg_rebounds", "avg_assist",
    "avg_steals", "avg_blocks", "avg_turnovers", "avg_field_goal_perc", "avg_free_throw_perc", "avg_three_point_perc"
  )
  game_statistics[-1] <- lapply(game_statistics[-1], as.numeric)
  game_statistics[[1]] <- as.character(game_statistics[[1]])

  season_statistics <- stats[[2]] %>% html_table(fill = TRUE)
  season_statistics <- season_statistics[seq(-1, -1 * which(season_statistics[,1] == "Player")), ]
  season_statistics <- season_statistics[- nrow(season_statistics), ]
  colnames(season_statistics) <- c(
    "player",
    "total_minutes", "field_goal_makes", "field_goal_attemps", "free_throw_makes", "free_throw_attempts",
    "three_point_makes", "three_point_attempts", "total_points",
    "offensive_rebounds", "defensive_rebounds", "total_rebounds",
    "total_assists", "total_turnovers", "total_steals", "total_blocks"
  )
  season_statistics[-1] <- lapply(season_statistics[-1], as.numeric)
  season_statistics[[1]] <- as.character(season_statistics[[1]])

  player_statistics <- merge(game_statistics, season_statistics)

  cbind(year = year, team = team_name, player_statistics, stringsAsFactors = FALSE) %>%
    as_data_frame()

}



#' Get all NCAA data
#'
#' Starting at 2002, and ends with 2017
#'
#' @param start year to start with
#' @param end year to end with
#' @export
get_ncaa <- function(start = 2002, end = 2017) {

  is_parallel <- TRUE
  require(doParallel)
  doParallel::registerDoParallel(4)

  seq(start, end, by = 1) %>%
    lapply(function(year) {
      cache_file <- file.path(cache_dir, "team_processed", str_c(year, ".rds"))


      if (file.exists(cache_file)) {
        ret <- readRDS(cache_file)
        return(ret)
      }

      team_info <- get_teams(year)

      pb <- progress_bar$new(
        format = "[:bar] :year/:team_id :percent eta::eta\n",
        total = nrow(team_info) / ifelse(is_parallel, 4, 1)
      )
      pb$tick(0)

      team_dt <- plyr::llply(
        seq_len(nrow(team_info)),
        function(i) {
          year <- team_info$year[i]
          team_id <- team_info$team_ids[i]
          pb$tick(tokens = list(year = year, team_id = team_id))

          get_team_stats(year, team_id)
        },
        .parallel = is_parallel
      ) %>%
        bind_rows()

      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
      saveRDS(team_dt, cache_file)
      return(team_dt)

    }) ->
  list_info

  list_info %>%
    bind_rows()

}
