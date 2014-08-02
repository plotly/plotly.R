# Functions that USERS will possibly want access to.


PLOTLY_DIR <- file.path(path.expand("~"), ".plotly")
CREDENTIALS_FILE <- file.path(PLOTLY_DIR, ".credentials")
# PLOT_OPTIONS_FILE <- file.path(PLOTLY_DIR, ".plot_options")
# THEMES_FILE <- file.path(PLOTLY_DIR, ".themes")


#' Create Plotly credentials file if nonexistent
ensure_local_plotly_files_exist <- function() {
  if (!file.exists(PLOTLY_DIR)) {
    dir.create(PLOTLY_DIR)
  }
  for (filename in c(CREDENTIALS_FILE)) {  # , PLOT_OPTIONS_FILE, THEMES_FILE
    if (!file.exists(filename)) {
      file.create(filename)
    }
  }
}


### Credentials Tools ###

#' Read Plotly credentials file (which is a JSON)
#' @param args Character vector of keys you are looking up
#' @return List of keyword-value pairs (credentials)
#' @examples
#' \dontrun{
#' get_credentials_file(c("username", "api_key"))
#' }
get_credentials_file <- function(args=c()) {
  require(RJSONIO)
  ensure_local_plotly_files_exist()
  if (file.info(CREDENTIALS_FILE)$size) {
    credentials_data <- fromJSON(CREDENTIALS_FILE)
    if (!is.null(args)) {
      credentials_data <- credentials_data[args]
    }
  } else {
    credentials_data <- NULL
  }
  return(as.list(credentials_data))
}


#' Read and print Plotly credentials file, wrapping get_credentials_file()
#' @param args Character vector of keys you are looking up
#' @return List of keyword-value pairs (credentials)
#' @export
show_credentials_file <- function(args=c()) {
  print("Your credentials file:")
  print(get_credentials_file(args))
}


#' Set the keyword-value pairs in Plotly credentials file
#' @param username plotly username
#' @param api_key plotly API key
#' @param stream_ids stream ids
#' @return List of keyword-value pairs (credentials)
#' @export
#' @examples
#' \dontrun{
#' set_credentials_file("username", "api_key", list("foo", "bar))
#' }
set_credentials_file <- function(username="", api_key="",
                                 stream_ids=list("", "")) {
  credentials_data <- show_credentials_file()
  new_credentials <- list()
  if (username != "") {
    new_credentials$username <- username
  } else {
    new_credentials$username <- credentials_data$username
  }
  if (api_key != "") {
    new_credentials$api_key <- api_key
  } else {
    new_credentials$api_key <- credentials_data$api_key
  }
  if (stream_ids[[1]] != "") {
    new_credentials$stream_ids <- stream_ids
  } else {
    new_credentials$stream_ids <- credentials_data$stream_ids
  }
  writeLines(toJSON(new_credentials), CREDENTIALS_FILE)
  print("Now,")
  show_credentials_file()
}
