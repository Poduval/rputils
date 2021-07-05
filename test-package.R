# *** RUN THIS SCRIPT IN A FRESH R SESSION ***

# ==== Application ====

# Use the package ====
library("rp")

news(package = "rp")

df <- as_rp(iris)
df
summary(df)


df <- as_rp(CO2)
df
summary(df)
