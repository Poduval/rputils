
# ==== Reference ====
# http://r-pkgs.had.co.nz/

# ==== Initialization ====
library(devtools)

# ==== Meta information ====
pkg_name <- "rp"
pkg_author <- person("Rakesh", "Poduval",
                     email = "poduval.rakesh@icloud.com",
                     role = c("aut", "cre"))

pkg_version <- "0.0.1"
pkg_path <- file.path(pkg_name)

dir.create(pkg_path, showWarnings = F, recursive = T)

RScripts <- c("explore.R")
RFiles <- file.path("R", RScripts)
RData <- "data"

# ==== Package setup ====

# clear the directory (to restart package building) ====
unlink(pkg_path, force = TRUE, recursive = TRUE)

# Create the package directory ====

create_package(pkg_path,
       # use_description_defaults(package = pkg_name, roxygen = TRUE, fields = list()),
       fields = list(
         Title = "rp package",
         Description = "My personal collection of custom functions",
         Version = pkg_version,
         Date = Sys.Date(),
         Author = pkg_author,
         Maintainer = pkg_author,
         License = "GPL-3",
         Encoding = "UTF-8",
         Depends =  "R (>= 3.4.0)",
         Suggests = "",
         Imports =  ""),
       rstudio = FALSE,
       roxygen = TRUE,
       check_name = TRUE)

# Copy the R scripts into sub directory R ====
file.copy(RFiles, file.path(pkg_path, "R"))

# Create Rd files (help files) from roxygen comments ====

unlink(file.path(pkg_path, "NAMESPACE")) # delete NAMESPACE file
document(pkg_path) # MUST be run to create NAMESPACE file

# Build the package (generates toypackage.tar.gz) ====
build(pkg_path)

# check package ====
check(pkg_path, check_dir = ".", document = FALSE, manual = TRUE)

if (FALSE) # The two lines above in one
  check(pkg_path, manual = TRUE)

# install ====
install(pkg_path)

# create win-binary (generates toypackage.zip) ====
build(pkg_path, binary = TRUE)

# package files:
# toypackage.tar.gz = bundled package
# toypackage.zip    = binary pacakge
# -> see http://r-pkgs.had.co.nz/package.html #package

