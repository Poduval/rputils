# ==== Initialization ====
library(devtools)

# ==== Meta information ====
pkg_name <- "rp"
pkg_version <- "0.0.1"
pkg_author <- person("Rakesh", "Poduval",
                     email = "poduval.rakesh@icloud.com",
                     role = c("aut", "cre"))

RScripts <- c("explore.R")
RFiles <- file.path("R", RScripts)
RData <- "data"

# ==== Package setup ====

# clear the directory (to restart package building) ====
unlink(pkg_name, force = TRUE, recursive = TRUE)

# Create the package directory ====
create_package(
  pkg_name,
  fields = list(
    Title = "rp package",
    Description = "My personal collection of custom functions",
    Version = pkg_version,
    Date = Sys.Date(),
    `Authors@R` = pkg_author,
    License = "GPL-3",
    Encoding = "UTF-8",
    Suggests = "",
    Imports =  c("stats")),
  rstudio = FALSE,
  roxygen = TRUE,
  check_name = TRUE,
  open = FALSE)

# Copy the R scripts into sub directory R ====
file.copy(RFiles, file.path(pkg_name, "R"))

# Create Rd files (help files) from roxygen comments ====
document(pkg_name)

# Build the package (generates rp.tar.gz) ====
build(pkg_name)

# check package ====
check(pkg_name, check_dir = ".", document = FALSE, manual = TRUE)

if (FALSE) # The two lines above in one
  check(pkg_name, manual = TRUE)

# install ====
install(pkg_name)

# create win-binary (generates toypackage.zip) ====
build(pkg_name, binary = TRUE)

