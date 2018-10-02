library(spirograph)
library(tidyverse)
library(gganimate)
library(ggforce)
library(plotrix)
library(IDPmisc)
t <- seq(0, 2 * pi, length.out = 361)
fixed_radius <- 3
cycling_radius <- 1
pen_radius <- 2
# X and Y coordinates
x <- (fixed_radius - cycling_radius) * cos(t) +
  pen_radius * cos(t * (fixed_radius - cycling_radius) / cycling_radius )
y <- (fixed_radius - cycling_radius) * sin(t) -
  pen_radius * sin(t * (fixed_radius - cycling_radius) / cycling_radius )

max_t <- t

d <- crossing(t, max_t) %>%
  filter(t <= max_t) %>%
  mutate(x = (fixed_radius - cycling_radius) * cos(t) +
           pen_radius * cos(t * (fixed_radius - cycling_radius) / cycling_radius ),
         y = (fixed_radius - cycling_radius) * sin(t) -
           pen_radius * sin(t * (fixed_radius - cycling_radius) / cycling_radius ),
         inner_x = (fixed_radius - cycling_radius) * cos(max_t),
         inner_y = (fixed_radius - cycling_radius) * sin(max_t)) %>%
  arrange(max_t, t)

cnt <- 0
for (mt in max_t) {
  cnt = cnt + 1
  svg(paste0("spiro",cnt + 1000,".png"),width = 8, height = 8, bg = "white")
    dd <- filter(d, max_t == mt)
    md <- filter(dd, t == mt)
    plot.new()
    par(pty = "s", mar = rep(0, 4))
    xrange <- range(d[, "x"], na.rm = TRUE)
    yrange <- range(d[, "y"], na.rm = TRUE)
    lb <- min(xrange[1], yrange[1])
    ub <- max(xrange[2], yrange[2])
    bounds <- c(lb, ub)
    graphics::plot.window(bounds, bounds, asp = 1)
    lines(y ~ x, data = dd, col = "red")
    draw.circle(md$inner_x, md$inner_y, cycling_radius, border = "blue")
    draw.circle(0, 0, fixed_radius, border = "black")
    Arrows(md$inner_x, md$inner_y, md$x, md$y, sh.col = "blue", open = F)
    points(inner_y ~ inner_x, data = md, col=  "blue", pch = 16, cex = 2)

  dev.off()
}

library(animation)
ani.options(interval = 0.02)
im.convert("spiro*.svg", output = "animationsvg.gif", clean = FALSE)
 # im.convert("spiro*.png", output = "animation.gif", clean = FALSE)


 t <- seq(0, 2 * pi, length.out = 361)
 fixed_radius <- 3
 cycling_radius <- -1
 pen_radius <- 2
 # X and Y coordinates
 x <- (fixed_radius - cycling_radius) * cos(t) +
   pen_radius * cos(t * (fixed_radius - cycling_radius) / cycling_radius )
 y <- (fixed_radius - cycling_radius) * sin(t) -
   pen_radius * sin(t * (fixed_radius - cycling_radius) / cycling_radius )

 max_t <- t

 d <- crossing(t, max_t) %>%
   filter(t <= max_t) %>%
   mutate(x = (fixed_radius - cycling_radius) * cos(t) +
            pen_radius * cos(t * (fixed_radius - cycling_radius) / cycling_radius ),
          y = (fixed_radius - cycling_radius) * sin(t) -
            pen_radius * sin(t * (fixed_radius - cycling_radius) / cycling_radius ),
          inner_x = (fixed_radius - cycling_radius) * cos(max_t),
          inner_y = (fixed_radius - cycling_radius) * sin(max_t)) %>%
   arrange(max_t, t)

 cnt <- 0
 for (mt in max_t) {
   cnt = cnt + 1
   svg(paste0("spiro",cnt + 1000,".svg"),width = 8, height = 8, bg = "white")
   dd <- filter(d, max_t == mt)
   md <- filter(dd, t == mt)
   plot.new()
   par(pty = "s", mar = rep(0, 4))
   xrange <- range(d[, "x"], na.rm = TRUE)
   yrange <- range(d[, "y"], na.rm = TRUE)
   lb <- min(xrange[1], yrange[1])
   ub <- max(xrange[2], yrange[2])
   bounds <- c(lb, ub)
   graphics::plot.window(bounds, bounds, asp = 1)
   lines(y ~ x, data = dd, col = "red")
   draw.circle(md$inner_x, md$inner_y, cycling_radius, border = "blue")
   draw.circle(0, 0, fixed_radius, border = "black")
   Arrows(md$inner_x, md$inner_y, md$x, md$y, sh.col = "blue", open = F)
   points(inner_y ~ inner_x, data = md, col=  "blue", pch = 16, cex = 2)

   dev.off()
 }

 library(animation)
 ani.options(interval = 0.02)
 im.convert("spiro*.svg", output = "animationsvg.gif", clean = FALSE)


