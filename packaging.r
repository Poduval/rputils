# ==== Initialization ====
library(devtools)

# _ignoring package un related files ====
use_build_ignore(c('packaging.R', 'help', 'release',
                   "renv", 'renv.lock'))

document()

build(path = "release/", quiet = FALSE,
      binary = FALSE, vignettes = TRUE)

check(quiet = TRUE)

install()

# *** RUN THIS SCRIPT IN A FRESH R SESSION ***

packageDescription("rp")
news(package = "rp")
summary(rp:::as_rp(iris))

library("rp")
df <- as_rp(CO2)
df
summary(df)
