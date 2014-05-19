# Functions that USERS will possibly want access to.

PLOTLY_DIR <- file.path("~/.plotly")
CREDENTIALS_FILE <- file.path(PLOTLY_DIR, ".credentials")
# PLOT_OPTIONS_FILE <- file.path(PLOTLY_DIR, ".plot_options")
# THEMES_FILE <- file.path(PLOTLY_DIR, ".themes")

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


set_credentials_file <- function(username="", api_key="", stream_ids=c()) {
  
  ensure_local_plotly_files_exist()
  
}
