# Functions that USERS will possibly want access to.


PLOTLY_DIR <- file.path(path.expand("~"), ".plotly")
CREDENTIALS_FILE <- file.path(PLOTLY_DIR, ".credentials")
CONFIG_FILE <- file.path(PLOTLY_DIR, ".config")
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


### Config Tools ###

#' Read Plotly config file (which is a JSON) and create one if nonexistent
#' @param args Character vector of keys you are looking up
#' @return List of keyword-value pairs (config)
#' @examples
#' \dontrun{
#' get_config_file(c("plotly_domain", "plotly_streaming_domain"))
#' }
get_config_file <- function(args=c()) {
  config_data <- list()
  if (!file.exists(CONFIG_FILE)) {
    file.create(CONFIG_FILE)
  }
  if (file.info(CONFIG_FILE)$size) {
    config_data <- fromJSON(CONFIG_FILE)
    if (!is.null(args)) {
      config_data <- config_data[args]
    }
  } else {
    config_data <- NULL
  }
  return(as.list(config_data))
}


#' Read and print Plotly config file, wrapping get_credentials_file()
#' @param args Character vector of keys you are looking up
#' @return List of keyword-value pairs (credentials)
#' @export
show_config_file <- function(args=c()) {
  print("Your config file:")
  print(get_config_file(args))
}


#' Set keyword-value pairs in Plotly config file
#' @param plotly_domain plotly domain
#' @param plotly_streaming_domain plotly streaming domain
#' @return List of keyword-value pairs (config)
#' @export
#' @examples
#' \dontrun{
#' set_config_file("https://kitty.plot.ly", "stream.kitty.plot.ly")
#' }
set_config_file <- function(plotly_domain="", plotly_streaming_domain="") {
  config_data <- show_config_file()
  new_config <- list()
  if (plotly_domain != "") {
    new_config$plotly_domain <- plotly_domain
  } else {
    new_config$plotly_domain <- "https://plot.ly"
  }
  if (plotly_streaming_domain != "") {
    new_config$plotly_streaming_domain <- plotly_streaming_domain
  } else {
    new_config$plotly_streaming_domain <- "stream.plot.ly"
  }
  writeLines(toJSON(new_config), CONFIG_FILE)
  print("Now,")
  show_config_file()
}
