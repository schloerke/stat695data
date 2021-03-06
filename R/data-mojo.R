#' Weekend Movie Boxoffice Performance
#'
#' This data contains weekend movie boxoffice performance scraped from the the website \href{www.boxofficemojo.com}{www.boxofficemojo.com}.  The urls used were similar to \href{http://www.boxofficemojo.com/weekend/chart/?view=&yr=2016&wknd=51&p=.htm}{http://www.boxofficemojo.com/weekend/chart/?view=&yr=2016&wknd=51&p=.htm}.
#'
#' Data accessed on March 23, 2017
#'
#' @details \itemize{
#'   \item year year of movie weekend. Ranges from 1982 to 2017
#'   \item week week of movie weekend. Ranges from 1 to 53
#'   \item rank_this_week numeric ranking of weeekend gross
#'   \item rank_last_week prior week ranking of weekend gross
#'   \item title title of movie
#'   \item studio studio of movie
#'   \item weekend_gross weekend gross received by movie
#'   \item weekend_gross_perc_change weekend gross percent change from prior week
#'   \item theater_count number of theaters displaying movie
#'   \item theater_change percent change of number of theaters displaying movie compared to prior week
#'   \item average average amount of money made at each theater
#'   \item total_gross total amount of money made per movie
#'   \item budget total budge per movie
#'   \item week_num number of weekends a movie has been showing
#'   \item movie_id id for a movie
#'   \item start_date start date of movie
#'   \item end_date end date of movie
#'   \item img_loc location of movie image
#'   \item release_date release date of movie
#'   \item genres genres of movies
#'   \item mpaa MPAA rating of a movie
#' }
#'
#' @docType data
#' @keywords datasets
#' @name mojo
#' @usage data(mojo)
#' @format A data frame with 116,663 rows and 21 variables
#'
NULL
