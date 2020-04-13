## local creates a new, empty environment.
## This avoids polluting the global environment with
## the object r. The following sets the CRAN mirror
## so it doesn't need to be manually selected with
## each use of install.packages
local({
  r = getOption("repos")             
  r["CRAN"] = "https://cran.rstudio.com/"
  options(repos = r)
})

## interactive sessions automatically require
## the devtools package
if (interactive()) {
  suppressMessages(require(devtools))
}

## TODO: Load imgct for interactive sessions automatically?