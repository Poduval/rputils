
# Initialize ====
library(usethis)
library(devtools)

# Files to be ignored for packaging ====

use_build_ignore(c("^_make_package.R$", "[.]Rproj$", "^release$",
                   "^backlog$", "^logo.png$"), escape = FALSE)

# Update description file ====
use_description(
  fields = list(
    package = "rputils",
    Title = "Some useful utility function - PRIVATE",
    Version = "0.0.1",
    Description = "This is a personal collection of functions for day to day R operations.",
    `Authors@R` = "person('Rakesh', 'Poduval', email = 'poduval.rakesh@icloud.com', role = c('aut', 'cre', 'cph'))",
    Depends = "R (>= 3.6.3)",
    LazyData = TRUE
  ),
  roxygen = TRUE
)

use_package("stats", "Imports")
use_package("utils", "Imports")
use_package("diffr", "imports")
use_package("openxlsx", "imports")
use_package("rstudioapi", "imports")

use_gpl_license(2)
use_github_links() # use this if this project is on github

# Datasets ====
# use_data()

# Documentation ====
use_logo("logo.png")
# use_vignette()

# Tests ====
# use_testthat()
# use_revdep()
# use_cran_comments()
use_github_action("check-standard")
use_github_action("test-coverage")
use_github_action("pkgdown")

# Packaging ====

document()
# test()
# build_vignettes()
check(manual = FALSE, cran = FALSE, quiet = FALSE)
build(quiet = FALSE, binary = FALSE, path = "release/")
install()

# install.packages("release/rputils_0.0.1.tar.gz", repos = NULL)
# devtools::install_github("Poduval/rputils")

if (FALSE) {
  # RUN IN A FRESH SESSION

  packageVersion(pkg = "rputils")
  packageDescription(pkg = "rputils")
  news(package = "rputils")

  library(rputils)
  describe(iris)
}
