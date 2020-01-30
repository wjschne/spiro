#' Make spirograph
#'
#' `spiro` creates a spirograph.
#'
#' The `spiro` function by default returns an object of class spiro. This object contains the name of the file that `spiro` has created. Its print method opens the file in your computers default svg viewer. Google Chrome is the viewer on which best results will likely be seen. When used with knitr, object of class spiro will print via knitr::include_graphics()
#' @param fixed_radius The radius of the fixed circle being cycled around. Can be positive or negative. Defaults to 3.
#' @param cycling_radius The radius of the moving circle cycling around the fixed circle. Can be positive or negative. Defaults to 1.
#' @param pen_radius The pen placement inside the moving circle cycling around the fixed circle. Can be positive or negative.
#' @param windings The number of times the the moving circle winds around the fixed circle.
#' @param color_groups Number of groups in group_id variable
#' @param colors Vector of colors for groups. Defaults to a random hsv color when color_groups is 1 and to a random palette from the paletteer package when color_groups > 1.
#' @param transparency Transparency of colors. Ranges from 0 to 1. Default is NA (i.e., leave the colors as specified in the colors argument).
#' @param color_cycles The number of times the color vector  is recycled
#' @param color_sort Arranges all paths of the same color to be written in groups
#' @param draw_fills Draw fills. Default is TRUE
#' @param line_width The width of lines. Only relevant when draw_fills is FALSE.
#' @param origin_x The x coordinate of the center of the fixed circle
#' @param origin_y The y coordinate of the center of the fixed circle
#' @param xlim The limits of the x coordinates. For example, c(-5,5). NULL means that the limits are determined by the data point that has the largest distance from the origin.
#' @param ylim The limits of the y coordinates. For example, c(-5,5). NULL means that the limits are determined by the data point that has the largest distance from the origin.
#' @param rotation The number of radians by which the entire figure should be rotated around the origin of the fixed circle
#' @param start_angle The number of radians on the fixed circle's arc where the moving circle starts cycling.
#' @param points_per_polygon The number of points for each color grouping
#' @param rule Character value specifying the path fill mode: either "evenodd" or "winding". Defaults to "evenodd"
#' @param openfile Open file in default program for .svg format. Defaults to TRUE.
#' @param end_at_beginning Adds the first point to the end of the figure but with the colors of the last color group. Defaults to FALSE.
#' @param savefile Save a file. Defaults to TRUE.
#' @param file Name of the output file. Defaults to "spiro.svg"
#' @param ... parameters passed to the par function
#' @importFrom rlang .data
#' @return object of class spiro, which contains the file name. If savefile = FALSE, will return a tibble with raw data.
#' @examples
#' spiro(
#'   fixed_radius = 3,
#'   cycling_radius = 1,
#'   file = "spiro.svg")
#' @export
spiro <- function(
                  fixed_radius = 3,
                  cycling_radius = 1,
                  pen_radius = cycling_radius,
                  windings = cycling_radius,
                  color_groups = 1,
                  colors = NA,
                  transparency = NA,
                  color_cycles = 1,
                  color_sort = FALSE,
                  draw_fills = TRUE,
                  line_width = 1,
                  origin_x = 0,
                  origin_y = 0,
                  xlim = NULL,
                  ylim = NULL,
                  rotation = 0,
                  start_angle = 0,
                  points_per_polygon = round(abs(1000 * windings), 0),
                  rule = c("evenodd", "winding"),
                  openfile = TRUE,
                  end_at_beginning = F,
                  savefile = TRUE,
                  file = NA,
                  ...) {
  # Checks----

  ## Is color_groups an integer?
  if (color_groups != round(color_groups, 0)) {
    stop("color_groups must be an integer")
  }

  ## Transparency from 0 to 1?
  if (!dplyr::between(transparency, 0, 1) &
    !is.na(transparency)) {
    stop("transparency must be in the range between 0 and 1")
  }

  rule <- match.arg(rule)
  ## rule is evenodd or winding?
  if (rule != "evenodd" &
    rule != "winding") {
    stop("rule must be either 'evenodd' or 'winding'.")
  }

  ## Give unique file name if file is NA
  file <- spiro_name(file)

  ## Check file extension
  if (tools::file_ext(file) == "") {
    file <- paste0(file, ".svg")
  }
  if (tools::file_ext(file) != "svg") {
    stop("file must end with '.svg'")
  }

  # Colors ----

  # Set default colors
  if (all(is.na(colors))) {
    if (color_groups == 1) {
      colors <- grDevices::hsv(stats::runif(1))
    } else {
      p <- paletteer::palettes_c_names %>%
        dplyr::sample_n(1)
      colors <- paletteer::paletteer_c(
        palette = paste0(dplyr::pull(p, .data$package),"::",dplyr::pull(p, .data$palette)),
        n = color_groups)
    }
  }

  # Add transparency to colors, if needed
  if (!is.na(transparency)) {
    colors <- scales::alpha(colors, alpha = transparency)
  }

  # Time parameter
  t <- seq(start_angle,
    start_angle + windings * 2 * pi,
    length.out = points_per_polygon * color_cycles * color_groups
  )

  # X and Y coordinates ----
  x <- (fixed_radius - cycling_radius) * cos(t) +
    pen_radius * cos(t * (fixed_radius - cycling_radius) / cycling_radius)
  y <- (fixed_radius - cycling_radius) * sin(t) -
    pen_radius * sin(t * (fixed_radius - cycling_radius) / cycling_radius)

  # Rotated X and Y coordinates
  xy <- cbind(x, y) %*% matrix(c(
    cos(rotation),
    sin(rotation), -sin(rotation),
    cos(rotation)
  ),
  nrow = 2,
  ncol = 2
  )


  # Tibble to return ----
  d <- tibble::tibble(
    x = xy[, 1] + origin_x,
    y = xy[, 2] + origin_y,
    id = seq(1, length(t)),
    color_cycle_id = rep(1:color_cycles,
      each = points_per_polygon * color_groups
    ),
    color_id = rep(
      rep(1:color_groups,
        each = points_per_polygon
      ),
      color_cycles
    )
  )

  if (color_groups > 1) {
    # Find where color groups transition
    d_transitions <- dplyr::mutate(
      d,
      transition = .data$color_id != dplyr::lag(.data$color_id, default = 1)
    )

    # Insert transition rows
    d_transitions <- dplyr::mutate(
      d_transitions,
      color_id = dplyr::lag(.data$color_id, default = 1),
      color_cycle_id = dplyr::lag(.data$color_cycle_id, default = 1),
      id = dplyr::lag(.data$id, default = 1)
    )

    # Filter
    d_transitions <- dplyr::filter(
      d_transitions,
      .data$transition
    )

    d <- dplyr::arrange(
      dplyr::bind_rows(
        d,
        d_transitions
      ),
      .data$color_cycle_id,
      .data$color_id,
      .data$id
    )
  }

  if (end_at_beginning) {
    d_last <- d[nrow(d), ]
    d_first <- d[1, ]
    d_last$x <- d_first$x
    d_last$y <- d_first$y
    d_last$id <- d_last$id + 1

    d <- dplyr::bind_rows(d, d_last)
  }

  nd <- tidyr::nest(dplyr::group_by(d, .data$color_cycle_id, .data$color_id))
  nd <- dplyr::mutate(nd, col = as.character(colors[.data$color_id]))
  nd <- dplyr::rename(nd, x = .data$data)

  if (color_sort) {
    nd <- dplyr::arrange(nd, .data$col)
  }
  biggest_deviation <-
    max(sqrt((d[, "x"] - origin_x)^2 +
      (d[, 2 ] - origin_y)^2))
  bounds_x <-
    c(
      -biggest_deviation + ifelse(origin_x < 0,
        origin_x,
        0
      ),
      biggest_deviation + ifelse(origin_x > 0,
        origin_x,
        0
      )
    )
  bounds_y <-
    c(
      -biggest_deviation + ifelse(origin_y < 0,
        origin_y,
        0
      ),
      biggest_deviation + ifelse(origin_y > 0,
        origin_y,
        0
      )
    )

  # Set bounds manually
  if (!is.null(xlim)) bounds_x <- xlim
  if (!is.null(ylim)) bounds_y <- ylim

  # Find dimensions of plot
  plot_width <- bounds_x[2] - bounds_x[1]
  plot_height <- bounds_y[2] - bounds_y[1]

  # Save plot?----
  if (savefile) {
    # Make plot have 10 as the largest dimension and scale the other
    grDevices::svg(
      filename = file,
      width = ifelse(plot_width < plot_height, 10, 10 * plot_width / plot_height),
      height = ifelse(plot_height < plot_width, 10, 10 * plot_height / plot_width),
      bg = NA
    )
    graphics::plot.new()
    # Set margins to 0
    graphics::par(mar = rep(0, 4), ...)

    # Set bounds
    graphics::plot.window(
      xlim = bounds_x,
      ylim = bounds_y,
      asp = 1
    )

    # Draw fills or lines?
    if (draw_fills) {
      purrr::pwalk(
        dplyr::select(dplyr::ungroup(nd), x, col),
        graphics::polypath,
        border = NA,
        rule = rule
      )
    } else {
      purrr::pwalk(
        dplyr::select(dplyr::ungroup(nd), x, col) ,
        graphics::lines,
        lwd = line_width)
    }
    # Finish saving
    grDevices::dev.off()


    # Read saved file as xml
    x1 <- safe_read(file)

    # Set height and width to 100%
    xml2::xml_set_attr(
      x = x1,
      attr = "height",
      value = "100%"
    )
    xml2::xml_set_attr(
      x = x1,
      attr = "width",
      value = "100%"
    )

    # Find children and give first child a name by its parameters
    g <- xml2::xml_children(x1)
    xml2::xml_set_attr(
      x = g[1],
      attr = "id",
      value = paste("spiro", fixed_radius, cycling_radius, pen_radius, sample(1:100000, 1), sep = "_")
    )
    # Write data back to file
    safe_write(x1, file)

    # Open file in default .svg viewer?
    if (openfile) {
      # Give file string the spiro class, which has a print function
      class(file) <- c("spiro", class(file))
      file
    }
  } else {
    # Plot spirogrpah and return raw data----
    graphics::plot.new()
    graphics::par(pty = "s", mar = rep(0, 4), ...)
    xrange <- range(d[, 1], na.rm = TRUE)
    yrange <- range(d[, 2], na.rm = TRUE)
    lb <- min(xrange[1], yrange[1])
    ub <- max(xrange[2], yrange[2])
    bounds <- c(lb, ub)
    graphics::plot.window(bounds, bounds, asp = 1)

    if (draw_fills) {
      purrr::pwalk(
        dplyr::select(dplyr::ungroup(nd), x, col),
        graphics::polypath,
        border = NA,
        rule = rule
      )
    } else {
      purrr::pwalk(dplyr::select(dplyr::ungroup(nd), x, col), graphics::lines, lwd = line_width)
    }
    return(d)
  }
}

#' Spin image. Resulting file best viewed in Google Chrome.
#' @param input File name of .svg file to input
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' @param rpm Number of rotations per minute. Positive numbers spin right. Negative numbers spin left. 0 is stationary.
#' @param rotation_point A vector of 2 numbers ranging from 0 to 1. Default is c(0.5, 0.5)
#' @param add_dot Adds a dot at the rotation point
#' @param dot_color Color of dot at center
#' @param dot_size Size of dot
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @return output name
#' @examples
#' library(spiro)
#' spiro(fixed_radius = 3,
#'       cycling_radius = 1,
#'       file = "image_spin.svg") %>%
#'   image_spin(rpm = 1)
#' @export
image_spin <- function(
                       input,
                       rpm = 1,
                       rotation_point = c(0.5, 0.5),
                       add_dot = FALSE,
                       dot_color = "red",
                       dot_size = 6,
                       openfile = TRUE,
                       output = input) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }

  # Read file
  x1 <- safe_read(input)

  # Get dimensions of viewBox
  viewbox <- xml2::xml_attr(x = x1, attr = "viewBox") %>%
    stringr::str_split(pattern = " ") %>%
    unlist() %>%
    as.numeric()
  width <- viewbox[3]
  height <- viewbox[4]

  # Set rotation center
  rotation_center <- rotation_point * c(width, height)


  duration <- abs(60 / rpm)
  start_degree <- ifelse(sign(rpm) == 1, 0, 360)
  end_degree <- ifelse(sign(rpm) == 1, 360, 0)

  # Find all g elements
  g1 <- xml2::xml_find_all(x1, "//d1:g")

  # Add rotations to all g elements
  xml2::xml_add_child(
    g1,
    .value = paste0(
      'animateTransform attributeType="xml" ',
      'attributeName="transform" ',
      'type="rotate" from="',
      start_degree, " ",
      rotation_center[1], " ",
      rotation_center[2],
      '" to="',
      end_degree, " ",
      rotation_center[1], " ",
      rotation_center[2],
      '" dur="',
      duration,
      's" additive="sum" ',
      'repeatCount="indefinite"'
    )
  )

  # Add dot at center
  if (add_dot) {
    xml2::xml_add_child(
      g1,
      .value = glue::glue(
        'circle fill="{dot_color}"
cx="{rotation_center[1]}"
cy="{rotation_center[2]}"
r = "{dot_size}"'
      )
    )
  }

  # Save file
  safe_write(x1, output)

  # Add spiro class to output

  if (!("spiro" %in% class(output))) {
    class(output) <- c("spiro", class(output))
  }
  if (openfile) output
}

#' Scale image
#' @param input File name of .svg file to input
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' @param scale Ratio by which to scale the image
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @return output name
#' @examples
#' library(spiro)
#' spiro(fixed_radius = 3,
#'       cycling_radius = 1,
#'       file = "image_scale.svg") %>%
#'   image_scale(scale = 0.5)
#' @export
image_scale <- function(
                        input,
                        scale = 1,
                        openfile = TRUE,
                        output = input) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }

  x1 <- safe_read(input)
  g1 <- xml2::xml_find_all(x1, "//d1:g")

  transforms <- xml2::xml_attr(g1, "transform")
  xy <- 360 / scale - 360



  for (i in 1:length(g1)) {
    xml2::xml_set_attr(
      x = g1[i],
      attr = "transform",
      value = paste0(
        ifelse(is.na(transforms[i]), "", transforms[i]),
        " scale(", scale[i], " ", scale[i], ") ",
        "translate(", xy[i], " ", xy[i], ")"
      )
    )
  }


  safe_write(x1, output)
  if (!("spiro" %in% class(output))) class(output) <- c("spiro", class(output))
  if (openfile) output
}

#' shift image
#' @param input File name of .svg file to input
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' @param x Shift x. Can be positive or negative.
#' @param y Shift y. Can be positive or negative.
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @return output name
#' @examples
#' library(spiro)
#' spiro(fixed_radius = 3,
#'       cycling_radius = 1,
#'       file = "image_shift.svg") %>%
#'   image_shift(x = 200, y = 200)
#' @export
image_shift <- function(
                        input,
                        x = 0,
                        y = 0,
                        openfile = TRUE,
                        output = input) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }

  x1 <- safe_read(input)
  g1 <- xml2::xml_find_all(x1, "//d1:g")

  transforms <- xml2::xml_attr(g1, "transform")
  translates <- paste0(
    ifelse(is.na(transforms),
      "", transforms
    ),
    " translate(", x, " ", -y, ")"
  )

  for (i in 1:length(g1)) {
    xml2::xml_set_attr(
      x = g1[i],
      attr = "transform",
      value = translates[i]
    )
  }

  # xml2::xml_set_attr(
  #   x = g1,
  #   attr = "transform",
  #   value = glue::glue(
  #     ifelse(is.na(transforms),"", transforms),
  #     " translate({x} {-y})"))
  safe_write(x1, output)
  if (!("spiro" %in% class(output))) class(output) <- c("spiro", class(output))
  if (openfile) output
}

#' Rotate image.
#'
#' `image_rotate`
#' @param input File name of .svg file to input
#' @param degrees Degrees to rotate image. Default is 90.
#' @param radians radians to rotate image. If specified, radians will override degrees
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' @return output name
#' @examples
#' library(spiro)
#' spiro(fixed_radius = 3,
#'       cycling_radius = 1,
#'       file = "image_rotate.svg") %>%
#'     image_rotate(degrees = 90)
#' @export
image_rotate <- function(
                         input,
                         degrees = 90,
                         radians = NA,
                         openfile = TRUE,
                         output = input) {
  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }

  x1 <- safe_read(input)
  g1 <- xml2::xml_find_all(x1, "//d1:g")

  transforms <- xml2::xml_attr(g1, "transform")

  if (!is.na(radians)) degrees <- radians * 180 / pi

  dg <- rep_len(degrees, length(g1))

  for (i in 1:length(g1)) {
    xml2::xml_set_attr(
      x = g1[i],
      attr = "transform",
      value = paste0(
        ifelse(is.na(transforms[i]), "", transforms[i]),
        " rotate(", dg[i], " 360 360)"
      )
    )
  }

  safe_write(x1, output)
  if (!("spiro" %in% class(output))) class(output) <- c("spiro", class(output))
  if (openfile) output
}

#' Merge images
#' @param input Vector of file names to merge
#' @param output File name of .svg file to output.
#' @param copies The number of copies of the input file to make
#' @param delete_input Delete input file(s)
#' @param openfile Open file in default program for .svg format. Defaults to FALSE
#' @return output name
#' @examples
#' library(spiro)
#' c(spiro(fixed_radius = 6, transparency = 0.5),
#'   spiro(fixed_radius = 3, transparency = 0.5)
#' ) %>%
#'   image_merge(
#'     output = "image_merge.svg",
#'     delete_input = TRUE)
#' @export
image_merge <- function(
                        input,
                        output = NA,
                        copies = 1,
                        delete_input = TRUE,
                        openfile = TRUE) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }

  if (length(input) * copies < 2) stop("The input parameter must be at least 2 files.")

  x1 <- purrr::map(rep(input, copies), safe_read)
  children <- purrr::map(x1, xml2::xml_children)
  g1 <- children[[1]]

  for (i in length(x1):2) xml2::xml_add_sibling(g1, children[[i]])

  ## Give unique output name if output is NA
  output <- spiro_name(output)
  safe_write(x1[[1]], output)

  # Delete input files
  if (delete_input) {
    remove_inputs <- input[!(input %in% output)]
    file.remove(as.character(remove_inputs))
  }
  if (!("spiro" %in% class(output))) class(output) <- c("spiro", class(output))
  if (openfile) output
}

#' Animate image. Resulting file best viewed in Google Chrome.
#' @param input File name of .svg file to input
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' @param attribute Name of attribute. opacity is the default
#' @param values Vector of values to go between. Defaults to c(0,1,0)
#' @param duration Number of seconds for animation to last, defaults to 10 seconds
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @return output name
#' @examples
#' library(spiro)
#' spiro(fixed_radius = 3,
#'       cycling_radius = 1,
#'       file = "image_animate.svg") %>%
#'    image_animate(attribute = "opacity",
#'                  duration = 2,
#'                  values = c(0.3,0.9,0.3))
#' @export
image_animate <- function(
                          input,
                          attribute = "opacity",
                          values = c(0, 1, 0),
                          duration = 10,
                          openfile = TRUE,
                          output = input) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }

  x1 <- safe_read(input)
  g1 <- xml2::xml_find_all(x1, "//d1:g")

  vstring <- paste(values, collapse = "; ")

  xml2::xml_add_child(g1, .value = paste0('animate attributeName="', attribute, '" attributeType="XML"
        values="', vstring, '"
        begin="0s" dur="', duration, 's" repeatCount="indefinite"'))

  safe_write(x1, output)
  if (!("spiro" %in% class(output))) class(output) <- c("spiro", class(output))
  if (openfile) output
}

#' Add background color
#' @param input File name of .svg file to input
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' @param color Vector of background color
#' @param rounding Rounding of background rectangle's corners. The default is 0 (no rounding). A rounding value of 1 creates a circle. If rounding is a vector of 2 values, the x and y rounding parameters are set separately.
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @return output name
#' @examples
#' library(spiro)
#' spiro(
#'   fixed_radius = 3,
#'   cycling_radius = 1,
#'   file = "add_background.svg") %>%
#'   add_background(color = "black")
#' @export
add_background <- function(
                           input,
                           color = "black",
                           rounding = 0,
                           openfile = TRUE,
                           output = input) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }
  x1 <- safe_read(input)
  # Remove previous backgrounds
  old_defs <- xml2::xml_find_all(x = x1, '//d1:defs[@id="spiro_rad_def"]')
  xml2::xml_remove(old_defs)
  old_rect <- xml2::xml_find_all(x = x1, '//d1:rect[@id="spiro_bg"]')
  xml2::xml_remove(old_rect)

  # Set rounding for background
  roundingx <- rounding[1] * 100 / 2
  roundingy <- rounding[1] * 100 / 2
  if (length(rounding) > 1) {
    roundingy <- rounding[2] * 100 / 2
  }


  bg_rgb <- scales::alpha(colour = color)
  bg_rect <- xml2::read_xml(
    x = paste0(
      '<rect id="spiro_bg" ',
      'x="0" y="0" ',
      'rx="', roundingx, '%" ',
      'ry="', roundingy, '%" ',
      'width="100%" height="100%" ',
      'style="fill:',
      bg_rgb,
      ';stroke:none;"/>'
    )
  )
  xml2::xml_add_child(x1, bg_rect, .where = 0)

  safe_write(x1, output)


  if (!("spiro" %in% class(output))) class(output) <- c("spiro", class(output))
  if (openfile) output
}

#' Add background color radial gradients
#' @param input File name of .svg file to input
#' @param colors Vector of gradient colors
#' @param stops Vector of stops for gradient colors. Ranges from 0 to 1.
#' @param center_x x-coordinate for center of gradient. Number between 0 and 1.
#' @param center_y y-coordinate for center of gradient. Number between 0 and 1.
#' @param focus_x x-coordinate for focus of gradient. Number between 0 and 1.
#' @param focus_y y-coordinate for focus of gradient. Number between 0 and 1.
#' @param radius Radius of radial gradient. The default is the square root of 2 so that the gradient extends to the corners of the square. If you want it to extend to the edges, set it to 1.
#' @param rounding Rounding of background rectangle's corners. The default is 0 (no rounding). A rounding value of 1 creates a circle. If rounding is a vector of 2 values, the x and y rounding parameters are set separately.
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' @return output name
#' @examples
#' library(spiro)
#' spiro(
#'    fixed_radius = 3,
#'    cycling_radius = 1,
#'    file = "add_background_gradient.svg"
#'    ) %>%
#'    add_background_gradient(color = c("black","white"))
#' @export
add_background_gradient <- function(
                                    input,
                                    colors = c("white", "black"),
                                    stops = seq(0, 1, length.out = length(colors)),
                                    center_x = 0.5,
                                    center_y = 0.5,
                                    focus_x = 0.5,
                                    focus_y = 0.5,
                                    radius = sqrt(2),
                                    rounding = 0,
                                    openfile = TRUE,
                                    output = input) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }

  x1 <- safe_read(input)

  # Remove previous backgrounds
  old_defs <- xml2::xml_find_all(x = x1, '//d1:defs[@id="spiro_rad_def"]')

  xml2::xml_remove(old_defs)
  old_rect <- xml2::xml_find_all(x = x1, '//d1:rect[@id="spiro_bg"]')
  xml2::xml_remove(old_rect)

  # Set rounding for background
  roundingx <- rounding[1] * 100 / 2
  roundingy <- rounding[1] * 100 / 2
  if (length(rounding) > 1) {
    roundingy <- rounding[2] * 100 / 2
  }

  # Set colors to hex
  colors <- scales::alpha(colors)

  # Make stops
  offsets <- paste0('<stop offset="', 100 * stops, '%" stop-color="', colors, '"/>', collapse = "\n")

  # Make Definitions
  defs <- xml2::read_xml(glue::glue(
    '
<defs id="spiro_rad_def">
  <radialGradient id="backgrad"
    cx="{center_x}"
    cy="{center_y}"
    fx="{focus_x}"
    fy="{focus_y}"
    r="{radius / 2}">
      {offsets}
  </radialGradient>
</defs>
'
  ))

  # Make background rectangle
  bg_rect <- xml2::read_xml(
    glue::glue(
      '<rect id="spiro_bg" ',
      'x="0" y="0" ',
      'width="100%" height="100%" ',
      'rx="{roundingx}%" ry="{roundingy}%" ',
      'fill="url(#backgrad)" ',
      'stroke="none"/>'
    )
  )

  xml2::xml_add_child(x1, bg_rect, .where = 0)
  xml2::xml_add_child(x1, defs, .where = 0)
  safe_write(x1, output)

  if (!("spiro" %in% class(output))) class(output) <- c("spiro", class(output))
  if (openfile) output
}


#' Add color to lines
#' @param input File name of .svg file to input
#' @param colors Vector of background color
#' @param transparency Transparency of colors. Ranges from 0 to 1. Default is NA (i.e., leave the colors as specified in the colors argument).
#' @param line_width Width of lines. Default is 0.5.
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' #' @return output name
#' @examples
#' library(spiro)
#' spiro(
#'   fixed_radius = 3,
#'   cycling_radius = 1,
#'   file = "add_lines.svg"
#'   ) %>%
#'   add_lines(
#'     colors = "firebrick",
#'     line_width = 3)
#' @export
add_lines <- function(
                      input,
                      colors = "black",
                      transparency = NA,
                      line_width = 0.5,
                      openfile = TRUE,
                      output = input) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }

  x1 <- safe_read(input)

  # Set colors to hex
  colors <- scales::alpha(colors)
  if (!is.na(transparency)) colors <- scales::alpha(colors, transparency)
  colors[is.na(colors)] <- "none"

  # Find all paths
  paths <- xml2::xml_find_all(x = x1, "//d1:path")
  styles <- xml2::xml_attr(x = paths, attr = "style")
  styles <- stringr::str_remove(styles, "stroke\\:(.*?);")
  styles <- stringr::str_remove(styles, "stroke-width\\:(.*?);")
  npaths <- length(paths)
  new_styles <- paste0(
    "stroke:",
    colors,
    ";",
    "stroke-width:",
    line_width,
    ";",
    styles
  )
  for (i in 1:npaths) {
    xml2::xml_set_attr(
      x = paths[i],
      attr = "style",
      value = new_styles[i]
    )
  }

  safe_write(x1, output)

  if (!("spiro" %in% class(output))) class(output) <- c("spiro", class(output))
  if (openfile) output
}

#' Add fills to lines
#' @param input File name of .svg file to input
#' @param colors Vector of background color
#' @param transparency Transparency of colors. Ranges from 0 to 1. Default is NA (i.e., leave the colors as specified in the colors argument).
#' @param rule Character value specifying the path fill mode: either "evenodd" or "winding". Defaults to "evenodd"
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' #' @return output name
#' @examples
#' library(spiro)
#' spiro(
#'  fixed_radius = 3,
#'  cycling_radius = 1,
#'  file = "add_fills.svg"
#'  ) %>%
#'  add_fills(colors = "black")
#' @export
add_fills <- function(
                      input,
                      colors = "black",
                      transparency = NA,
                      rule = c("evenodd", "winding"),
                      openfile = TRUE,
                      output = input) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }

  rule <- match.arg(rule)
  ## rule is evenodd or winding?
  if (rule != "evenodd" &
    rule != "winding") {
    stop("rule must be either 'evenodd' or 'winding'.")
  }

  x1 <- safe_read(input)
  # Set colors to hex
  colors <- scales::alpha(colors)
  if (!is.na(transparency)) colors <- scales::alpha(colors, transparency)
  colors[is.na(colors)] <- "none"

  # Find all paths
  paths <- xml2::xml_find_all(x = x1, "//d1:path")
  styles <- xml2::xml_attr(x = paths, attr = "style")
  styles <- stringr::str_remove(styles, "fill\\:(.*?);")
  styles <- stringr::str_remove(styles, "fill-opacity\\:(.*?);")
  styles <- stringr::str_remove(styles, "fill-rule\\:(.*?);")
  npaths <- length(paths)
  new_styles <- paste0(
    "fill:",
    colors,
    ";",
    "fill-rule:",
    rule,
    ";",
    styles
  )
  for (i in 1:npaths) xml2::xml_set_attr(
      x = paths[i], attr = "style", value = new_styles[i]
    )

  safe_write(x1, output)

  if (!("spiro" %in% class(output))) class(output) <- c("spiro", class(output))
  if (openfile) output
}

#' Add a moving circle to paths. Resulting file best viewed in Google Chrome.
#' @param input File name of .svg file to input
#' @param colors Vector of background color
#' @param transparency Transparency of colors. Ranges from 0 to 1. Default is NA (i.e., leave the colors as specified in the colors argument).
#' @param radius Radius of dot
#' @param duration Number seconds to complete animation
#' @param delay Number of seconds to delay animation
#' @param path_id Set id to path
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' @return output name
#' @examples
#' library(spiro)
#' spiro(
#'  fixed_radius = 3,
#'  cycling_radius = 1,
#'  file = "add_pathdot.svg"
#'  ) %>%
#'  add_pathdot(duration = 3)
#' @export
add_pathdot <- function(
                        input,
                        colors = "red",
                        transparency = NA,
                        radius = 3,
                        duration = 10,
                        delay = 0,
                        path_id = "pathdot",
                        openfile = TRUE,
                        output = input) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }

  x1 <- safe_read(input)
  g <- xml2::xml_find_all(x1, "//d1:g")
  # Set colors to hex
  colors <- scales::alpha(colors)
  if (!is.na(transparency)) colors <- scales::alpha(colors, transparency)

  # Find all paths
  paths <- xml2::xml_find_all(x1, "//d1:path")
  npaths <- length(paths)
  ncolors <- length(colors)
  color_id <- rep_len(1:ncolors, npaths)
  for (i in 1:npaths) {
    xml2::xml_set_attr(
      paths[i],
      "id",
      paste0(path_id, "_", i)
    )

    for (j in duration) {

      circ <- xml2::read_xml(
        glue::glue('<circle id="circle_{path_id}_{i}" r="{radius}" fill="{colors[color_id[i]]}">
    <animateMotion dur="{j}s" repeatCount="indefinite" begin="{delay}s">
      <mpath href="#{path_id}_{i}"/>
                           </animateMotion>
                             </circle>')
      )
      xml2::xml_add_child(g, circ)
    }


  }
  safe_write(x1, output)
  if (!("spiro" %in% class(output))) class(output) <- c("spiro", class(output))
  if (openfile) output
}


#' Spin image. Resulting file best viewed in Google Chrome.
#' @param input File name of .svg file to input
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' @param location a vector of coordinates for the start button
#' @param fill button fill color
#' @param color button text color
#' @param radius button radius
#' @param start_label start button text
#' @param stop_label stop button text
#' @param font_family button text font family
#' @param font_size button text font size
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @return output name
#' @examples
#' library(spiro)
#' spiro(fixed_radius = 3,
#'       cycling_radius = 1,
#'       file = "image_spin.svg") %>%
#'   image_spin(rpm = 1)
#' @export
add_restart <- function(
  input,
  location = c(0.05, 0.95),
  fill = "white",
  color = "black",
  radius = 20,
  start_label = "Start",
  stop_label = "Stop",
  font_family = "inherit",
  font_size = 14,
  openfile = TRUE,
  output = input) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }

  # Read file
  x1 <- safe_read(input)

    # Get dimensions of viewBox
  viewbox <- xml2::xml_attr(x = x1, attr = "viewBox") %>%
    stringr::str_split(pattern = " ") %>%
    unlist() %>%
    as.numeric()
  width <- viewbox[3]
  height <- viewbox[4]

  fn <- stringr::str_remove(as.character(input),"\\.svg")

  start_circle <- paste0(fn,"_start")
  stop_circle <- paste0(fn,"_stop")


  # Find all g elements
  # g1 <- xml2::xml_find_all(x1, "//d1:g")
  at1 <- xml2::xml_find_all(x = x1, "//d1:animateTransform")
  xml2::xml_attr(at1, "begin") <- paste0(start_circle,".click")
  xml2::xml_attr(at1, "end") <- paste0(stop_circle,".click")

  am1 <- xml2::xml_find_all(x = x1, "//d1:animateMotion")
  xml2::xml_attr(am1, "begin") <- paste0(start_circle,".click+",xml2::xml_attr(am1, "begin"))
  xml2::xml_attr(am1, "end") <- paste0(stop_circle,".click")

  a1 <- xml2::xml_find_all(x = x1, "//d1:animate")
  xml2::xml_attr(a1, "begin") <- paste0(start_circle,".click")
  xml2::xml_attr(a1, "end") <- paste0(stop_circle,".click")


  rb_start <- xml2::read_xml(glue::glue('
<g id="{start_circle}" transform="translate({location[1] * width}, {location[2] * height})" style="cursor: pointer;">
<circle fill="{fill}" r="{radius}" />
<text fill="{color}" text-anchor="middle" alignment-baseline="central" style="font-family:{font_family};font-size:{font_size}pt">{start_label}</text>
</g>'))

  xml2::xml_add_child(.where = 0,
                      .x = x1,
                      .value = rb_start)

  rb_stop <- xml2::read_xml(glue::glue('
<g id="{stop_circle}" transform="translate({(1-location[1]) * width}, {location[2] * height})" style="cursor: pointer;">
<circle fill="{fill}" r="{radius}"/>
<text fill="{color}" text-anchor="middle" alignment-baseline="central" style="font-family:{font_family};font-size:{font_size}pt">{stop_label}</text>
</g>'))

  xml2::xml_add_child(.where = 0,
                      .x = x1,
                      .value = rb_stop)
  # Save file
  safe_write(x1, output)

  # Add spiro class to output

  if (!("spiro" %in% class(output))) {
    class(output) <- c("spiro", class(output))
  }
  if (openfile) output
}



#' Custom print function for spiro objects. It opens the .svg file with your default svg viewer with the pander::openFileInOS function.
#' @param x Name of the output file.
#' @keywords internal
#' @export
print.spiro <- function(x, ...) {
  pander::openFileInOS(x, ...)
}

#' define a knit_print method for objects of the class spiro. It inserts the .svg file into the document with the knitr::include_graphics function
#' @param x Name of the output file.
#' @keywords internal
#' @export
knit_print.spiro <- function(x, ...) {
  knitr::include_graphics(path = x)
}

#' Make string bezier
#' @param file Name of the output file. Defaults to "bezier.svg"
#' @param x The x coordinates of the 3 points of the curve
#' @param y The y coordinates of the 3 points of the curve
#' @param n The number of segments. Defaults to 20.
#' @param color Vector of colors
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @param ... parameters passed to the segments function
#' @importFrom rlang .data
#' @return file by default or tibble with data if savefile if FALSE
#' @examples
#' library(spiro)
#' string_bezier(
#'    file = "string_bezier.svg",
#'    x = c(0,0,1),
#'    y = c(1,0,0),
#'    color = c("red","blue","black"),
#'    n = 100,
#'    lwd = 0.3
#'    )
#' @export
string_bezier <- function(
                          file = "bezier.svg",
                          x = c(0, 0, 1),
                          y = c(1, 0, 0),
                          n = 20,
                          color = "royalblue",
                          openfile = TRUE,
                          ...) {
  k <- length(x)
  if (k < 3) stop("x must have at least three values (e.g., x = c(0,0,1)).")
  if (length(y) < 3) stop("y must have at least three values (e.g., y = c(1,0,0)).")
  d <- tibble::tibble(
    id = seq(-1, k - 2),
    x0 = x,
    y0 = y,
    x1 = dplyr::lag(x),
    y1 = dplyr::lag(y),
    x2 = dplyr::lag(x, 2),
    y2 = dplyr::lag(y, 2)
  )

  d <- dplyr::filter(d, .data$id > 0)
  makeSegments <- function(id, x0, x1, x2, y0, y1, y2) {
    x_1 <- seq(x0, x1, length.out = n)
    x_2 <- seq(x1, x2, length.out = n)
    y_1 <- seq(y0, y1, length.out = n)
    y_2 <- seq(y1, y2, length.out = n)
    tibble::tibble(
      id = rep(id, n),
      x_1 = x_1,
      x_2 = x_2,
      y_1 = y_1,
      y_2 = y_2
    )
  }
  d_segments <- purrr::pmap_df(d, makeSegments)

  grDevices::svg(
    file = file,
    width = 10,
    height = 10,
    bg = NA
  )
  graphics::plot.new()
  graphics::par(mar = rep(0, 4))

  graphics::plot.window(
    xlim = c(min(x), max(x)),
    ylim = c(min(y), max(y)),
    asp = 1
  )
  graphics::segments(
    x0 = d_segments$x_1,
    y0 = d_segments$y_1,
    x1 = d_segments$x_2,
    y1 = d_segments$y_2,
    col = color,
    ...
  )

  grDevices::dev.off()
  if (openfile) {
    class(file) <- c("spiro", class(file))
    file
  }
}

#' Writes file with a delay to reduce chance of file conflicts
#' @param file Name of file
#' @keywords internal
#' @usage NULL
#' @export
safe_write <- function(x, f) {
  try({
    profvis::pause(0.1)
    xml2::write_xml(x = x, file = f)
    profvis::pause(0.1)
  })
}

#' Reads file with a delay to reduce chance of file conflicts
#' @param file Name of file
#' @return xml for use by xml2 functions
#' @keywords internal
#' @usage NULL
#' @export
safe_read <- function(file) {
  try({
    profvis::pause(0.1)
    xml2::read_xml(x = file)
  })
}


#' checks if file exists and creates unique name if not
#' @param file Name of file
#' @return Name of file
#' @keywords internal
#' @usage NULL
#' @export
spiro_name <- function(file) {
  ## Check if file exists
  if (is.na(file)) {
    spiroplots <- list.files(pattern = "spiro([0-9]+).svg")
    # pull out numeric component of the plot files
    if (length(spiroplots) > 0) {
      nums <- gsub(
        pattern = "spiro|.svg",
        replacement = "",
        x = spiroplots
      ) %>%
        as.numeric()
      last <- max(nums)
      # Create a new file name with an incremented counter.
      file <- paste0(
        "spiro",
        sprintf("%03d", last + 1),
        ".svg"
      )
    } else {
      file <- "spiro001.svg"
    }
  }
  file
}

#' Add a circle to the image
#' @param input File name of .svg file to input
#' @param x x-coordinate of circle center (0 to 1)
#' @param y y-coordinate of circle center (0 to 1)
#' @param r radius (0 to 1)
#' @param color line color
#' @param fill fill color
#' @param line_width width of line
#' @param openfile Open file in default program for .svg format. Defaults to FALSE.
#' @param output File name of .svg file to output. Default is to overwrite the input file.
#' @return output name
#' @export
#' @examples
#' spiro(fixed_radius = 3,
#'       cycling_radius = 1,
#'       file = "add_circle_example.svg",
#'       colors = "red") %>%
#'   add_circle(r = 0.308, fill = "white", color = "white")
add_circle <- function(
  input,
  x = 0.5,
  y = 0.5,
  r = 1,
  color = "black",
  fill = "none",
  line_width = 1,
  openfile = TRUE,
  output = input) {

  # Verify that input file names end with .svg
  if (any(tools::file_ext(input) != "svg")) {
    stop(paste0(
      "All file names must end with '.svg'. ",
      "Input file name(s): ",
      paste0(input, collapse = ",")
    ))
  }
  # read input file
  x1 <- safe_read(input)

  # convert color to hex
  color_rgb <- scales::alpha(colour = color)

  # Make circle svg text
  circle_text <- paste0(glue::glue('<circle cx="{x * 720}" cy="{y * 720}" r="{720 * r / 2}" stroke="{color_rgb}" stroke-width="{line_width}" fill="{fill}" />'), collapse = "")

  # Enclose circles in group
  circle_svg <- xml2::read_xml(glue::glue("<g>{circle_text}</g>"))

  # Add circle svg to file
  xml2::xml_add_child(x1, circle_svg)

  # write output file
  safe_write(x1, output)


  if (!("spiro" %in% class(output))) class(output) <- c("spiro", class(output))
  if (openfile) output
}
