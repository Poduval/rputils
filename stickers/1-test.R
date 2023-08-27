
# Initialization ====

if (FALSE) install.packages("hexSticker")
library(hexSticker)

# Examples ====

## From R plots ====
library(ggplot2)

sticker_feed <- ggplot(aes(x = mpg, y = wt), data = mtcars) + 
  geom_point() + theme_void() + theme_transparent()

sticker_feed

sticker_icon <- sticker(
  sticker_feed, 
  package = "ggplot2", 
  p_size = 20, s_x = 1, s_y = .75, 
  s_width = 1.3, s_height = 1,
  filename = "stickers/ggplot2.png")

sticker_icon

## From system files ====

sticker_feed <- system.file("figures/cat.png", package = "hexSticker")
sticker_feed

sticker_icon <- sticker(
  sticker_feed, 
  package = "cat", 
  p_size = 20, s_x = 1, s_y = .75, s_width =.6,
  filename = "stickers/cat.png")

sticker_icon

## From local files ====

sticker_feed <- "pictures/rakesh-immo.jpg"
sticker_feed

sticker_icon <- sticker(
  sticker_feed, 
  s_x = 1, s_y = .75, 
  s_width = 0.4, s_height = 0.4,
  p_size = 20, p_color = "#FFFFFF", p_family = "Aller_Rg", p_fontface = "plain",
  h_size = 1.2, h_fill = "#1881C2", h_color = "#87B13F",
  spotlight = FALSE,
  dpi = 300,
  package = "rakesh", 
  filename = "stickers/rakesh.png")

sticker_icon

