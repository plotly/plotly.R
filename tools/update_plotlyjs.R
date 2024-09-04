# get zip URL to latest plotly.js release
x <- httr::RETRY(
  verb = "GET",
  url = 'https://api.github.com/repos/plotly/plotly.js/releases/latest',
  times = 5,
  terminate_on = c(400, 401, 403, 404),
  terminate_on_success = TRUE
)
zip <- httr::content(x)$zipball_url

# remember where to copy over assets
pkg_dir <- rprojroot::find_package_root_file()
lib_dir <- rprojroot::find_package_root_file("inst/htmlwidgets/lib/plotlyjs")
patches <- list.files(
  rprojroot::find_package_root_file("tools/patches"), 
  full.names = TRUE
)

# download, patch, and build plotly.js in temp dir
tmpdir <- tempfile()
dir.create(tmpdir)

withr::with_dir(tmpdir, {
  # download source
  utils::download.file(zip, "plotly-js.zip")
  utils::unzip("plotly-js.zip", exdir = "plotly-js")
  
  withr::with_dir(
    dir("plotly-js", full.names = TRUE), {
      
      # apply patches
      for (patch in patches) {
        tryCatch({
            message(sprintf("Applying %s", basename(patch)))
            system(sprintf("git apply %s", patch))
          },
          error = function(e) quit(save = "no", status = 1)
        )
      }
      
      # build just the main bundle
      # (also builds schema and locales)
      system("npm update")
      system("npm install")
      system("npm run bundle")
      
      # copy assets to R package source
      file.copy(
        "dist/plotly.min.js", 
        file.path(lib_dir, "plotly-latest.min.js"), 
        overwrite = TRUE
      )
      file.copy(
        "LICENSE", 
        file.path(lib_dir, "LICENSE"), 
        overwrite = TRUE
      )
      
      locales <- Sys.glob("dist/plotly-locale-*.js")
      file.copy(
        locales,
        file.path(lib_dir, "locales", sub("^plotly-locale-", "", basename(locales))),
        overwrite = TRUE
      )
      # update plot schema and (partial) bundles
      Schema <- jsonlite::fromJSON("dist/plot-schema.json")
      bundleTraceMap <-
        paste0(readLines("tasks/util/constants.js"), collapse = "\n") |>
        stringr::str_extract(pattern = "(?<=\\b(const|var) partialBundleTraces = )\\{[^}]+\\}") |>
        yaml::read_yaml(text = _)
      
      withr::with_dir(
        pkg_dir, usethis::use_data(
          Schema,
          bundleTraceMap,
          internal = TRUE,
          overwrite = TRUE,
          compress = "xz",
          version = 3L
        )
      )
      
      # plotly.js used to bundle a typedarray polyfill to support older browsers,
      # but v2 drops support for them, so it no longer includes this polyfill
      # Hopefully, we can continue to get away with using the pre-v2 polyfill
      # to support shinytest/phantomjs
      #download.file(
      #  "dist/extras/typedarray.min.js",
      #  "inst/htmlwidgets/lib/typedarray/typedarray.min.js"
      #)
      
      message("Update plotlyMainBundle()'s version with ", basename(zip))
  })
})
