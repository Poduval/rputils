# ==== Initialization ====
library(devtools)

# ==== Meta information ====
pkg_name <- "rp"
pkg_path <- file.path("release", pkg_name)
pkg_version <- "0.0.1"
pkg_author <- person("Rakesh", "Poduval",
                     email = "poduval.rakesh@icloud.com",
                     role = c("aut", "cre"))

RScripts <- c("01-functions-data.R", "02-functions-vectors.R")
RFiles <- file.path("R", RScripts)
RData <- "data"

# ==== Package setup ====

# clear the directory (to restart package building) ====
unlink(pkg_path, force = TRUE, recursive = TRUE)

# Create the package directory ====
create_package(
  pkg_path,
  fields = list(
    Title = "rp package",
    Description = "My personal collection of custom functions",
    Version = pkg_version,
    Date = Sys.Date(),
    `Authors@R` = pkg_author,
    License = "GPL-2",
    Encoding = "UTF-8",
    Suggests = "",
    Imports =  c("stats")),
  rstudio = FALSE,
  roxygen = TRUE,
  check_name = TRUE,
  open = FALSE)

# Copy the R scripts into sub directory R ====
file.copy(RFiles, file.path(pkg_path, "R"))

# Add release notes
if (!dir.exists(file.path(pkg_path, "inst")))
  dir.create(file.path(pkg_path, "inst"), recursive = T)

file.copy("NEWS.md", file.path(pkg_path, "inst/NEWS"), overwrite = T)

# Create Rd files (help files) from roxygen comments ====
document(pkg_path)

# Build the package (generates rp.tar.gz) ====
build(pkg_path)

# check package ====
check(pkg_path, check_dir = pkg_path, document = FALSE, manual = TRUE)

# install ====
install(pkg_path)

# create win-binary (generates rp.zip for windows only) ====
# build(pkg_path, binary = TRUE)

