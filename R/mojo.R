

make_movie_url <- function(movie_id) {
  str_c("http://www.boxofficemojo.com/movies/?id=", movie_id, ".htm")
}

movie_info <- function(movie_id) {
  if (movie_id %in% c("likefatherlikeson", "mymotherlikeswomen", "freestyle", "likeforlikes")) {
    return(
      data_frame(
        img_loc = NA,
        release_date = NA,
        genres = NA,
        mpaa = NA
      )
    )
  }

  movie_url <- make_movie_url(movie_id)
  first_letter_movie_id <- movie_id %>% str_sub(1, 2)
  html <- cached_html(movie_url, file.path("mojo_movie_id", first_letter_movie_id, movie_id))
  html <- html %>% html_node("#main")


  title <- html %>% html_nodes("font:nth-child(2) b") %>% html_text()
  release_info <- html %>% html_nodes("center td") %>% html_text()
  img_loc <- html %>% html_nodes("#body td > a > img") %>% html_attr("src")
  img_loc <- img_loc[1]

  release_date <- release_info[3] %>% str_replace_all("Release Date: |,", "") %>% mdy()
  genres <- release_info[4] %>% str_replace("Genre: ", "") %>% str_trim()
  mpaa <- release_info[6] %>% str_replace("MPAA Rating: ", "") %>% str_trim()

  ans <- data_frame(
    # movie_id = movie_id,
    img_loc = img_loc,
    release_date = release_date,
    genres = genres,
    mpaa = mpaa
  )
  # print(ans)
  ans
}


# http://www.boxofficemojo.com/weekend/chart/?view=&yr=1982&wknd=01&p=.htm
make_ratings_url <- function(year, week) {
  year <- as.numeric(year)
  week <- as.numeric(week)
  cur_year <- Sys.time() %>% format("%Y") %>% as.numeric()
  cur_week <- Sys.time() %>% format("%V") %>% as.numeric()
  if (year < 1982 || (year > cur_year) || (year == cur_year && week > cur_week) ) {
    stop("only can make urls from 1982/01 to ", cur_year, "/", cur_week)
  }
  str_c(
    "http://www.boxofficemojo.com/weekend/chart/?view=&p=.htm",
    "&yr=", year,
    "&wknd=", str_pad(week, 2, "0", side = "left")
  )
}
# make_ratings_url(1982, 1)


extract_mojo_info <- function(year = 1982, week = 1) {

  cache_file <- file.path(cache_dir, "mojo_processed", str_c(year, "_", week, ".rds"))
  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }
  # print(c(year, week))
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)

  html <- cached_html(make_ratings_url(year, week), file.path("mojo_weekend", str_c(year, "_", week)))
  html <- html %>% html_node("#main")

  content <- html %>% html_nodes("#body > table")

  # extract the ratings table
  ratings_table <- content[[3]] %>%
    html_node("tr:nth-child(2) table")

  # extract the ratings table as data
  ratings_dt <- ratings_table %>%
    html_table(header = TRUE)
  # clean up the ratings table
  colnames(ratings_dt) <- c(
    "rank_this_week",
    "rank_last_week",
    "title",
    "studio",
    "weekend_gross",
    "weekend_gross_perc_change",
    "theater_count",
    "theater_change",
    "average",
    "total_gross",
    "budget",
    "week_num"
  )
  # remove last row of "totals"
  ratings_dt <- ratings_dt[- nrow(ratings_dt), ]
  # make the budget in millions

  as_num <- function(x) {
    suppressWarnings(as.numeric(x))
  }
  sym_num <- function(x) {
    x %>%
      str_replace_all("\\$|,|\\+|-|%", "") %>%
      as_num()
  }
  ratings_dt %>%
    mutate(
      rank_this_week = 1:nrow(ratings_dt),
      rank_last_week = as_num(rank_this_week),
      weekend_gross = sym_num(weekend_gross),
      weekend_gross_perc_change = sym_num(weekend_gross_perc_change),
      theater_count = as_num(theater_count),
      theater_change = sym_num(theater_change),
      average = sym_num(average),
      total_gross = sym_num(total_gross),
      budget = sym_num(budget) * 1000000,
      week_num = as_num(week_num)
    ) ->
  ratings_dt

  movie_id_vals <- ratings_table %>%
    html_nodes("tr td:nth-child(3) a") %>%
    html_attr("href") %>%
    magrittr::extract(-1) %>%
    str_replace("/movies/\\?id=", "") %>%
    str_replace(".htm$", "") %>%
    str_replace("%C2%A0", "%A0")

  ratings_dt %>%
    mutate(
      movie_id = movie_id_vals
    ) ->
  ratings_dt

  # get the date / time
  time_string <- content[[2]] %>%
    html_node("td:nth-child(1) font b") %>%
    html_text()

  if (str_detect(time_string, "^[a-zA-Z]+ \\d+-[a-zA-Z]+ \\d+, \\d+$")) {
    time_info <- str_match(time_string, "^([a-zA-Z]+) (\\d+)-([a-zA-Z]+) (\\d+), (\\d+)$")
    time_info <- c(time_info[,-1])
  } else {
    time_info <- str_match(time_string, "^([a-zA-Z]+) (\\d+)-(\\d+), (\\d+)$")
    time_info <- c(time_info[,-1])
    time_info <- c(time_info[1:2], time_info[1], time_info[3:4])
  }

  ratings_dt %>%
    mutate(
      start_date = mdy(str_c(time_info[1], time_info[2], time_info[5], sep = " ")),
      end_date = mdy(str_c(time_info[3], time_info[4], time_info[5], sep = " ")),
      week = week,
      year = year
    ) ->
  ratings_dt

  ratings_dt %>%
    mutate(movie_information = lapply(movie_id, movie_info)) %>%
    unnest(movie_information) ->
  ratings_dt

  saveRDS(ratings_dt, cache_file)

  ratings_dt
}



#' Make Mojo boxoffice data
#'
#' make mojo boxoffice data. Example url: http://www.boxofficemojo.com/weekend/chart/?view=&yr=2016&wknd=02&p=.htm
#'
#' @export
create_mojo <- function() {

  dates <- expand.grid(year = 1982:2016, week = 1:53) %>%
    arrange(year, week)

  is_parallel <- TRUE
  require(doParallel)
  registerDoParallel(4)
  pb <- progress_bar$new(
    format = "[:bar] :year/:week :percent eta::eta\n",
    total = nrow(dates) / ifelse(is_parallel, 4, 1)
  )
  pb$tick(0)
  # all_ratings_list <- lapply(seq_len(60), function(i) {
  all_ratings_list <- plyr::llply(seq_len(nrow(dates)), function(i) {
    date_info <- dates[i, ]
    year <- date_info[[1]]
    week <- date_info[[2]]
    pb$tick(tokens = list(year = year, week = week))
    extract_mojo_info(year, week)
  }, .parallel = is_parallel)

  all_ratings <- bind_rows(all_ratings_list) %>% as_tibble()
}
