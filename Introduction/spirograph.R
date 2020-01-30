## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
remake <- FALSE
library(spiro)
library(pander)
library(magrittr)
library(ggplot2)
library(dplyr)
library(purrr)

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("remotes")

## ---- eval=FALSE---------------------------------------------------------
#  remotes::install_github("wjschne/spiro")

## ----simple_spiro, eval = remake-----------------------------------------
#  library(spiro)
#  library(magrittr)
#  
#  spiro(
#    fixed_radius = 7,
#    cycling_radius = 6,
#    pen_radius = 2,
#    draw_fills = F,
#    file = "simple_spiro.svg"
#  )
#  

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("simple_spiro.svg")

## ----viridis_weave, eval=remake------------------------------------------
#  spiro(
#    fixed_radius = 1231,
#    cycling_radius = 529,
#    pen_radius = 1233,
#    color_groups = 67,
#    color_cycles = 59,
#    windings = 96,
#    points_per_polygon = 50,
#    file = "viridis_weave.svg"
#  ) %>%
#    add_background(color = "black")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("viridis_weave.svg")

## ----cork, eval=remake---------------------------------------------------
#  spiro(
#    fixed_radius = 800,
#    cycling_radius = 677,
#    pen_radius = 100,
#    color_groups = 10,
#    color_cycles = 61,
#    windings = 677 * 0.5,
#    transparency = 1,
#    start_angle = 0,
#    points_per_polygon = 30,
#    colors = scico::scico(n = 10, palette = "cork"),
#    end_at_beginning = F,
#    draw_fills = F,
#    file = "cork.svg"
#  ) %>%
#    add_background_gradient(
#      colors = c("black", "black", "gray40"))

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("cork.svg")

## ----purple_midnight, eval=remake----------------------------------------
#  spiro(
#    file = "purple_midnight.svg",
#    fixed_radius = 800,
#    cycling_radius = 751,
#    pen_radius = 40,
#    color_groups = 4,
#    color_cycles = 2,
#    points_per_polygon = 5000,
#    colors = c(
#      "midnightblue",
#      "white",
#      "purple4",
#      "white")) %>%
#    add_lines(
#      colors = "black",
#      line_width = 0.15) %>%
#    add_background_gradient(
#      colors = c("black",
#                 "purple4",
#                 "black",
#                 "midnightblue",
#                 "black",
#                 "gray20"),
#      stops = c(0,0.25,0.63,0.67,0.70,1))
#  

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("purple_midnight.svg")

## ----royalfire_weave, eval=remake----------------------------------------
#  spiro(
#    fixed_radius = 359,
#    cycling_radius = 261,
#    pen_radius = 40,
#    color_groups = 36,
#    color_cycles = 36,
#    draw_fills = F,
#    points_per_polygon = 10,
#    line_width = 3.5,
#    file = "royalfire_weave.svg",
#    colors = c(
#      scales::div_gradient_pal(
#        low = "royalblue4",
#        mid = "black",
#        high = "firebrick4")(seq(0, 1, length.out = 18)),
#      scales::div_gradient_pal(
#        low = "royalblue",
#        mid = "white",
#        high = "firebrick")(seq(0, 1, length.out = 18))
#      )
#    ) %>%
#    add_background(color = "gray10")
#  

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("royalfire_weave.svg")

## ----spiralstar, eval=remake---------------------------------------------
#  k <- 36
#  files <- paste0("s", 1:k, ".svg")
#  pen_radii <- seq(3.8, 1.5, length.out = k)
#  alphas <- rep_len(c(0.85, rep(0.2, 4)), k)
#  colors <- rep_len(scico::scico(6, palette = "devon"), k) %>%
#             scales::alpha(alpha = alphas)
#  
#  tibble::tibble(
#    file = files,
#    pen_radius = pen_radii,
#    colors = colors) %>%
#    purrr::pmap_chr(
#      spiro,
#      fixed_radius = 7,
#      cycling_radius = 4,
#      rotation = -pi / 10,
#      points_per_polygon = 500,
#      draw_fills = T,
#      xlim = c(-7, 7),
#      ylim = c(-7, 7)) %>%
#    image_merge(
#      output = "spiralstar.svg",
#      delete_input = TRUE) %>%
#    add_lines(colors = c(rep(NA,k - 1), "gray")) %>%
#    image_rotate(degrees = (1:k / 2.5)) %>%
#    add_background_gradient(
#      colors = c(
#        "#FFFFFF",
#        "#26588E",
#        "#E5E3F9",
#        "#283568",
#        "#C8C3F3"),
#      radius = 1,
#      rounding = 1,
#      stops = c(0.42,0.93,0.96,0.97,1))

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("spiralstar.svg")

## ----oslo_aster, eval=remake---------------------------------------------
#  n <- 10
#  oslo_colors <- scico::scico(
#    n = n,
#    palette = "oslo",
#    alpha = 0.9) %>%
#    rev()
#  
#  spiro(
#    file = "oslo_aster.svg",
#    rotation = pi / 6,
#    points_per_polygon = 100
#    ) %>%
#    image_merge(output = "oslo_aster.svg", copies = n) %>%
#    add_fills(colors = oslo_colors) %>%
#    image_scale(scale = sqrt(0.75 ^ (seq(0, n - 1)))) %>%
#    image_spin(rpm = 1:n + 1) %>%
#    add_background(color = "black", rounding = 1)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("oslo_aster.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("animationsvg.gif")

## ----hypotrochoid, eval=remake-------------------------------------------
#  spiro(
#    fixed_radius = 3,
#    cycling_radius = 1,
#    pen_radius = 2,
#    file = "hypotrochoid.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("hypotrochoid.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("negativeradius.gif")

## ----epitrochoid, eval=remake--------------------------------------------
#  spiro(
#    fixed_radius = 3,
#    cycling_radius = -1,
#    pen_radius = 2,
#    file = "epitrochoid.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("epitrochoid.svg")

## ----root3a, eval=remake-------------------------------------------------
#  spiro(
#    fixed_radius = sqrt(3),
#    cycling_radius = 1,
#    windings = 14.7,
#    file = "root3a.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("root3a.svg")

## ----root3b, eval=remake-------------------------------------------------
#  spiro(
#    fixed_radius = sqrt(3),
#    cycling_radius = 1,
#    windings = 35.495,
#    file = "root3b.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("root3b.svg")

## ----epitrochoid_lines, eval = remake------------------------------------
#  spiro(
#    fixed_radius = 3,
#    cycling_radius = -1,
#    pen_radius = 2,
#    draw_fills = FALSE,
#    file = "epitrochoid_lines.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("epitrochoid_lines.svg")

## ----epitrochoid_add_lines, eval = remake--------------------------------
#  spiro(
#    fixed_radius = 3,
#    cycling_radius = -1,
#    pen_radius = 2,
#    file = "epitrochoid_add_lines.svg"
#    ) %>%
#    add_lines(
#      colors = "black",
#      line_width = 4)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("epitrochoid_add_lines.svg")

## ----six_points, eval = remake-------------------------------------------
#  spiro(
#    fixed_radius = 6,
#    cycling_radius = 5,
#    pen_radius = 5,
#    color_groups = 2,
#    file = "six_points.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("six_points.svg")

## ----six_points_blueblack, eval = remake---------------------------------
#  spiro(
#    fixed_radius = 6,
#    cycling_radius = 5,
#    pen_radius = 5,
#    color_groups = 2,
#    colors = c("blue", "black"),
#    file = "six_points_blueblack.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("six_points_blueblack.svg")

## ----six_points_rainbow, eval = remake-----------------------------------
#  spiro(
#    fixed_radius = 6,
#    cycling_radius = 5,
#    pen_radius = 5,
#    color_groups = 12,
#    colors = rainbow(12),
#    file = "six_points_rainbow.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("six_points_rainbow.svg")

## ----six_points_rainbow_3, eval = remake---------------------------------
#  spiro(
#    fixed_radius = 6,
#    cycling_radius = 5,
#    pen_radius = 5,
#    color_groups = 12,
#    colors = rainbow(12),
#    color_cycles = 3,
#    points_per_polygon = 80,
#    file = "six_points_rainbow_3.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("six_points_rainbow_3.svg")

## ----blurry_checkers, eval = remake--------------------------------------
#  spiro(
#    fixed_radius = 121,
#    cycling_radius = 100,
#    pen_radius = 13,
#    color_groups = 5,
#    color_cycles = 1,
#    points_per_polygon = 1000,
#    transparency = .5,
#    file = "blurry_checkers.svg"
#    ) %>%
#    add_background(color = "black")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("blurry_checkers.svg")

## ----blurry_circles, eval = remake---------------------------------------
#  spiro(
#    fixed_radius = 121,
#    cycling_radius = 100,
#    pen_radius = 13,
#    color_groups = 5,
#    color_cycles = 200,
#    points_per_polygon = 20,
#    transparency = 0.5,
#    file = "blurry_circles.svg"
#    ) %>%
#    add_background(color = "black")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("blurry_circles.svg")

## ----rainbowspikes, eval = remake----------------------------------------
#  rainbow_colors <- hsv(
#      h = seq(1 / 16, 1, length.out = 16),
#      s = 0.7,
#      v = 0.7)
#  
#  spiro(
#    fixed_radius = 16,
#    cycling_radius = 5,
#    pen_radius = 5,
#    file = "rainbowspikes.svg",
#    color_groups = 16,
#    color_cycles = 2,
#    points_per_polygon = 50,
#    colors = rainbow_colors
#    ) %>%
#    add_background(color = "gray80")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("rainbowspikes.svg")

## ----rainbowspikes_rounded, eval = remake--------------------------------
#  spiro(
#    fixed_radius = 16,
#    cycling_radius = 5,
#    pen_radius = 5,
#    file = "rainbowspikes_rounded.svg",
#    color_groups = 16,
#    color_cycles = 2,
#    points_per_polygon = 50,
#    colors = rainbow_colors
#    ) %>%
#    add_background(color = "gray80", rounding = 0.1)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("rainbowspikes_rounded.svg")

## ----blue_gem, eval=remake-----------------------------------------------
#  spiro(
#    fixed_radius = 21,
#    cycling_radius = -20,
#    pen_radius = 35,
#    transparency = 1,
#    colors = "black",
#    file = "blue_gem.svg"
#    ) %>%
#    add_lines(colors = "white") %>%
#    add_background_gradient(
#      colors = c(
#        rep(c("lightcyan2", "royalblue4"), 9),
#        rep("white",2)),
#      rounding = 1,
#      radius = 1)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("blue_gem.svg")

## ----rainbowspikesdonut, eval = remake-----------------------------------
#  spiro(
#    fixed_radius = 16,
#    cycling_radius = 5,
#    pen_radius = 5,
#    file = "rainbowspikesdonut.svg",
#    color_groups = 16,
#    color_cycles = 2,
#    points_per_polygon = 50,
#    colors = rainbow_colors,
#    transparency = 0.7
#    ) %>%
#    add_background_gradient(
#      colors = c("white", "black", "black", "white"),
#      stops = c(.27, .34, .93, 1),
#      rounding = 1,
#      radius = 1)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("rainbowspikesdonut.svg")

## ---- eval = remake------------------------------------------------------
#  spiro(
#    fixed_radius = 3,
#    cycling_radius = 1,
#    colors = "royalblue",
#    file = "deltoid_blue.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("deltoid_blue.svg")

## ---- eval = remake------------------------------------------------------
#  spiro(
#    fixed_radius = 3,
#    cycling_radius = 1,
#    rotation = 60 / 180 * pi,
#    colors = "firebrick",
#    file = "deltoid_red.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("deltoid_red.svg")

## ----deltoid_merge, eval = remake----------------------------------------
#  image_merge(
#    input = c("deltoid_blue.svg", "deltoid_red.svg"),
#    output = "redblue.svg")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("redblue.svg")

## ----dodgerblue_snowflake, eval = remake---------------------------------
#  c(spiro(
#      fixed_radius = 17,
#      cycling_radius = 3,
#      colors = "dodgerblue4"),
#    spiro(
#      fixed_radius = 17,
#      cycling_radius = 4,
#      colors = "white"),
#    spiro(
#      fixed_radius = 17,
#      cycling_radius = 5,
#      colors = "dodgerblue3"),
#    spiro(
#      fixed_radius = 17,
#      cycling_radius = 6,
#      colors = "white"),
#    spiro(
#      fixed_radius = 17,
#      cycling_radius = 7,
#      colors = "dodgerblue2"),
#    spiro(
#      fixed_radius = 17,
#      cycling_radius = 8,
#      colors = "white")) %>%
#    image_merge(
#      output = "dodgerblue_snowflake.svg",
#      delete_input = TRUE)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("dodgerblue_snowflake.svg")

## ---- eval=FALSE---------------------------------------------------------
#  library(purrr)
#  tibble::tibble(
#    points_per_polygon = 1000,
#    fixed_radius = 17,
#    cycling_radius = 3:8,
#    colors = c(
#      "dodgerblue4", "white",
#      "dodgerblue3", "white",
#      "dodgerblue2", "white")
#    ) %>%
#    pmap_chr(spiro) %>%
#    image_merge(
#      output = "dodgerblue_snowflake.svg",
#      delete_input = TRUE)
#  

## ----heptagon_petals, eval = remake--------------------------------------
#  spiro(
#    fixed_radius = 7,
#    cycling_radius = 1,
#    pen_radius = 3,
#    colors = "darkorchid",
#    file = "heptagon_petals.svg"
#    ) %>%
#    image_scale(scale = 0.4)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("heptagon_petals.svg")

## ----seven_loops, eval = remake------------------------------------------
#  c(
#    spiro(
#      fixed_radius = 7,
#      cycling_radius = -1,
#      pen_radius = 3,
#      colors = "darkorchid",
#    ),
#    spiro(
#      fixed_radius = 7,
#      cycling_radius = 1,
#      pen_radius = 3,
#      colors = "white",
#      ) %>%
#      image_scale(scale = 0.4)
#    ) %>%
#    image_merge(
#      output = "heptagon_petals_merged.svg",
#      delete_input = TRUE)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("heptagon_petals_merged.svg")

## ----hsv_stripes, eval=remake--------------------------------------------
#  hsv_s <- rep(c(1,0),10)  + rep(c(-1,1),10) * seq(0.3,0.8,length.out = 20)
#  
#  spiro(
#    fixed_radius = 6,
#    cycling_radius = -1,
#    pen_radius = 1,
#     rotation = pi / 6,
#    points_per_polygon = 500
#    ) %>%
#    image_merge(
#      output = "hsv_stripes.svg",
#      copies = 20,
#      delete_input = TRUE
#      ) %>%
#    add_fills(
#      colors = hsv(
#        h = rep(seq(0,.90,0.1), 2),
#        v = rep(c(0.4,.7),10),
#        s = hsv_s)
#      ) %>%
#    image_scale(scale = seq(1, 0, length.out = 21)[-21]) %>%
#    image_rotate(degrees = 0:19 * 5) %>%
#    add_background(rounding = 1)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("hsv_stripes.svg")

## ----rainbowsequence, eval = remake--------------------------------------
#  k <- 9
#  spiro(
#    fixed_radius = 8,
#    cycling_radius = 5,
#    pen_radius = 2,
#    draw_fills = F,
#    colors = "black",
#    line_width = 0.1
#    ) %>%
#    image_scale(0.8) %>%
#    image_merge(
#      output = "rainbowsequence.svg",
#      copies = k,
#      delete_input = TRUE
#      )  %>%
#    add_fills(colors = rainbow(k, alpha = .25)) %>%
#    image_shift(
#      x = seq(-1,1,length.out = k) * -60,
#      y = seq(-1,1,length.out = k) * 15)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("rainbowsequence.svg")

## ----purple, eval = remake-----------------------------------------------
#  spiro(
#    colors = "purple",
#    fixed_radius = 16,
#    cycling_radius = 15,
#    pen_radius = 1.5,
#    draw_fills = FALSE,
#    line_width = 4,
#    file = "purple.svg"
#  )

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("purple.svg")

## ----black, eval = remake------------------------------------------------
#  spiro(
#    colors = "black",
#    fixed_radius = 15,
#    cycling_radius = 14,
#    pen_radius = 1.5,
#    draw_fills = FALSE,
#    line_width = 4,
#    file = "black.svg"
#  )

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("black.svg")

## ----purple_cycle, eval = remake-----------------------------------------
#  image_merge(
#    input = c("purple.svg", "black.svg"),
#    output = "purple_cycle.svg") %>%
#    image_spin(rpm = c(0.5,-0.5)) %>%
#    add_background(color = "black")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("purple_cycle.svg")

## ----static, eval = remake-----------------------------------------------
#  spiro(
#    file = "Static.svg",
#    fixed_radius = pi,
#    cycling_radius = sqrt(8),
#    pen_radius = 0.5 * sqrt(8) / pi ,
#    windings = 81,
#    start_angle = 0,
#    points_per_polygon = 10000,
#    transparency = 0.7
#  )

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("Static.svg")

## ----SpinLeftRight, eval = remake----------------------------------------
#  spiro(
#    fixed_radius = pi,
#    cycling_radius = sqrt(8),
#    pen_radius = 0.5 * sqrt(8) / pi ,
#    windings = 81,
#    start_angle = 0,
#    points_per_polygon = 10000,
#    transparency = 0.7
#    ) %>%
#    image_merge(
#      output = "SpinLeftRight.svg",
#      copies = 2,
#      delete_input = TRUE
#      ) %>%
#    image_spin(rpm = c(-0.1,0.1))

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("SpinLeftRight.svg")

## ----off_center, eval = remake-------------------------------------------
#  spiro(
#    fixed_radius = 5,
#    cycling_radius = 3,
#    pen_radius = 3 ,
#    start_angle = 0,
#    rotation = pi / 2,
#    file = "off_center.svg"
#  ) %>%
#    image_spin(
#      rpm = 5,
#      rotation_point = c(0.5, 0.385),
#      add_dot = TRUE
#    )

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("off_center.svg")

## ----pathdot1, eval = remake---------------------------------------------
#  spiro(
#    fixed_radius = 5,
#    cycling_radius = 1,
#    pen_radius = 11 / 3,
#    draw_fills = FALSE,
#    rotation = pi / 10,
#    file = "pathdot1.svg"
#    ) %>%
#    add_pathdot()

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("pathdot1.svg")

## ----pathdot5, eval = remake---------------------------------------------
#  # Make Colors
#  pathcolors <- scales::viridis_pal()(5)[c(4, 5, 1, 2, 3)]
#  
#  spiro(
#    fixed_radius = 5,
#    cycling_radius = 1,
#    pen_radius = 11 / 3,
#    start_angle = pi,
#    color_groups = 5,
#    draw_fills = FALSE,
#    line_width = 10,
#    rotation = pi / 10,
#    file = "pathdot5.svg"
#    ) %>%
#    add_pathdot(
#      colors = pathcolors,
#      duration = 3)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("pathdot5.svg")

## ----pathdot25, eval = remake--------------------------------------------
#  spiro(
#    fixed_radius = 5,
#    cycling_radius = 1,
#    pen_radius = 11 / 3,
#    color_groups = 5,
#    colors = rep("white", 5),
#    start_angle = pi,
#    draw_fills = FALSE,
#    rotation = pi / 10,
#    file = "pathdot25.svg", points_per_polygon = 250
#  ) %>%
#    add_pathdot(duration = 2) %>%
#    add_pathdot(duration = 3) %>%
#    add_pathdot(duration = 4) %>%
#    add_pathdot(duration = 5) %>%
#    add_pathdot(duration = 6)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("pathdot25.svg")

## ----spiro_data, dev="svg", fig.width=7, fig.height=7, out.height='100%', out.width='100%'----
library(ggplot2)
library(dplyr)
d <- spiro(
  fixed_radius = 37,
  cycling_radius = 12 ,
  pen_radius = 31,
  color_groups = 6,
  color_cycles = 6,
  points_per_polygon = 1000,
  savefile = FALSE
)


## ----spiro_ggplot2, dev="svg", fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
d %>%
  mutate(color_id = factor(color_id),
         color_cycle_id = factor(color_cycle_id)) %>%
  ggplot(aes(x, y, color = color_cycle_id)) +
  geom_path(lwd = 1) +
  theme_void() +
  theme(legend.position = "none") +
  coord_equal()


## ----bezier, eval = FALSE------------------------------------------------
#  string_bezier(
#    file = "bezier.svg",
#    color = c("red","green", "blue"),
#    n = 75)

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("bezier.svg")

## ----bezier_sequence, eval = FALSE---------------------------------------
#  string_bezier(
#    file = "bezier_sequence.svg",
#    x = c(-1, 1, 0,-1, 1,-1, 0, 1,-1, 1),
#    y = c( 0, 0, 1, 0, 0, 0,-1, 0, 0, 0),
#    color = c("#006B6B", "#FFFFFF", "black", "black", "black"),
#    n = 200,
#    lwd = 5,
#    ljoin = 1
#  ) %>%
#    add_background(color = "#108998")

## ---- echo = F, fig.width=7, fig.height=7, out.width='100%', out.height='100%'----
knitr::include_graphics("bezier_sequence.svg")

