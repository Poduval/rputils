
# ==== Introduction: The search path ====
search()
filter
find("filter")
stats::filter

library(dplyr)
search()
find("filter")
filter   # dplyr::filter
str(stats::filter)

detach("package:dplyr")
find("filter")

# If there may be any doubt, use ::
# But don't overload your code unnecessarily

# http://r-pkgs.had.co.nz/namespace.html#search-path


# ==== Use of functions within function ====

x <- rnorm(100)
sd(x)
View(sd)  # essentially sqrt(var(x))
sqrt(var(x))

find("sd")
find("var")

var <- function(x, na.rm = FALSE) 1e4 # obviously nonsense
sd(x)

sd <- function (x, na.rm = FALSE){  # same as stats::sd
  sqrt(var(if (is.vector(x) || is.factor(x)) x else as.double(x),
      na.rm = na.rm))
}

sd(x)

find("sd")   # two versions of sd()
sd(x)        # same as: globalenv()$sd(x)
stats::sd(x)

environment(sd)
environment(stats::sd)  # functions from namespace:stats have priority


# ==== Environment ====
cutelist <- list(a = 1, a = 2)
cuteenv <- as.environment(cutelist)

# Differences between list and environment (Slide!)
# 1. Every name in an environment is unique
cutelist
cuteenv
ls(cuteenv) # alias: objects()
as.list(cuteenv)
# 2. The names in an environment are not ordered
cutelist[[1]]  # extracting an element
cutelist[1]    # subsetting a list
cuteenv[1]  # error
cuteenv[[1]]  # error
# but:
cuteenv[["a"]]
cuteenv$a
  # caution: '$' uses partial matching
  # list(ab=1)$a # same as: list(ab=1)[["a", exact = F]]
# 3. An environment has a parent
#  -> see example below
# 4. Environments have reference semantics
myletters <- rep(LETTERS, length.out = 1e6)
object.size(myletters)
letterslist <- list(letters = myletters)
object.size(letterslist)
lettersenv <- new.env()
lettersenv$letters <- myletters
identical(as.list(lettersenv), letterslist)
object.size(lettersenv)

# environment's parent
myenv <- new.env()
myenv$x <- 2*x
evalq(sd(x), myenv)  # evaluate the expression sd(x) in environment myenv
parent.env(myenv)

parent.env(myenv) <- getNamespace("stats")
evalq(sd(x), myenv)

parent.env(myenv) <- globalenv()
evalq(sd(x), myenv)


# ==== Namespaces ====
library(jsonlite)
search()

# Namespace exports
ls("package:jsonlite")  # ls() and objects() are identical
exports <- ls("package:jsonlite", all.names = T)
exports
# Functions in package
ns <- ls(getNamespace("jsonlite"), all = TRUE)
ns

setdiff(exports, ns)  # some very special objects that are not of interest to us
# A package's namespace contains both the exported and unexported functions
setdiff(ns, exports)  # unexported functions, "hidden" in namespace

# Accessing an unexported object in a package's namespace
jsonlite:::collapse
find("collapse")

attach(getNamespace("jsonlite"))   # DON'T DO THAT!
search()
collapse
find("collapse")
detach('getNamespace("jsonlite")')

# searching a function, including namespaces
getAnywhere("collapse")

dplyr::collapse
jsonlite:::collapse


# Namespace imports
getNamespaceImports("dplyr")
# http://adv-r.had.co.nz/Environments.html#function-envs
# http://r-pkgs.had.co.nz/namespace.html

# NAMESPACE file
system.file(package = "dplyr") # directory of local package installation
dir(system.file(package = "dplyr"))

myNSfile <- file.path(system.file(package = "dplyr"), "NAMESPACE")
file.exists(myNSfile)
file.show(myNSfile)
writeLines(readLines(myNSfile))

