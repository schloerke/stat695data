if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "average", "budget", "genres", "img_loc", "movie_id", "movie_information",
    "mpaa", "rank_this_week", "studio", "theater_change", "theater_count", "title",
    "total_gross", "week", "week_num", "weekend_gross", "weekend_gross_perc_change", "year"
  ))
}



#' @import dplyr stringr rvest tidyr progress tibble xml2
#' @importFrom magrittr %>%
#' @importFrom utils tail
NULL
