

cache_dir <- file.path("..", "_html")

cached_html <- function(html_url, save_name) {

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  html_file_path <- file.path(cache_dir, str_c(save_name, ".html"))
  if (file.exists(html_file_path)) {
    html <- read_html(html_file_path)
    return(html)
  }
  print(html_url)
  dir.create(dirname(html_file_path), recursive = TRUE, showWarnings = FALSE)
  html <- read_html(html_url)
  xml2::write_html(html, html_file_path)
  return(html)
}
