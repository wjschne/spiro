#' Make coordinates for spirograph
#' @param fixed_radius The radius of the fixed circle being cycled around. Can be positive or negative.
#' @param cycling_radius The radius of the moving circle cycling around the fixed circle. Can be positive or negative.
#' @param pen_radius The pen placement inside the moving circle cycling around the fixed circle. Can be positive or negative.
#' @param windings The number of times the the moving circle winds around the fixed circle.
#' @param color_groups = Number of groups in group_id variable
#' @param colors Vector of colors for groups. Defaults to viridis scale.
#' @param transparency Transparency of fills. Ranges from 0 to 1. Defaults to 1.
#' @param color_cycles The number of times the color vector  is recycled
#' @param origin_x The x coordinate of the center of the fixed circle
#' @param origin_y The y coordinate of the center of the fixed circle
#' @param rotation The number of radians by which the entire figure should be rotated around the origin of the fixed circle
#' @param start_angle The number of radians on the fixed circle's arc where the moving circle starts cycling.
#' @param points_per_polygon = The number of points for each color grouping
#' @param rule Character value specifying the path fill mode: either "evenodd" or "winding". Defaults to "evenodd"
#' @param openfile Open file in default program for .svg format. Defaults to TRUE.
#' @param aspect_ratio Aspect ratio of the image. Defaults to 1.
#' @param end_at_beginning Adds the first point to the end of the figure but with the colors of the last color group. Defaults to FALSE.
#' @param savefile Save a file. Defaults to TRUE.
#' @param filename Name of the output file. Defaults to "spiro.svg"
#' @return A tibble with x and y coordinates of the spirograph, row id, and group id
#' @examples
#' spirograph(fixed_radius = 3, cycling_radius = 1)
#' @export
spirograph <- function(
  fixed_radius = 3,
  cycling_radius = 1,
  pen_radius = cycling_radius,
  windings = cycling_radius,
  color_groups = 1,
  colors = scales::viridis_pal()(color_groups),
  transparency = 1,
  color_cycles = 1,
  origin_x = 0,
  origin_y = 0,
  rotation = 0,
  start_angle = 0,
  points_per_polygon = round(abs(1000 * windings),0),
  rule = "evenodd",
  openfile = TRUE,
  aspect_ratio = 1,
  end_at_beginning = F,
  savefile = TRUE,
  filename = "spiro.svg"
  ) {

  # Time parameter
  t <- seq(start_angle,
           start_angle + windings * 2 * pi,
           length.out = points_per_polygon * color_cycles * color_groups)

  # X and Y coordinates
  x <- (fixed_radius - cycling_radius) * cos(t) +
    pen_radius * cos(t * (fixed_radius - cycling_radius) / cycling_radius )
  y <- (fixed_radius - cycling_radius) * sin(t) -
    pen_radius * sin(t * (fixed_radius - cycling_radius) / cycling_radius )

  # Rotated X and Y coordinates
  xy <- cbind(x,y) %*% matrix(c(cos(rotation),
                                sin(rotation),
                                -sin(rotation),
                                cos(rotation)),
                              nrow = 2,
                              ncol = 2)


  # Tibble to return
  d <- tibble::tibble(
    x = xy[,1] + origin_x,
    y = xy[,2] + origin_y,
    id = seq(1, length(t)),
    color_cycle_id = rep(
      1:color_cycles,
      each = points_per_polygon * color_groups),
    color_id = rep(
      rep(1:color_groups,
          each = points_per_polygon),
      color_cycles)
  )


  if (savefile) {
    grDevices::svg(filename = filename,
                   width = 10,
                   height = 10,
                   bg = NA)
    graphics::plot.new()
    graphics::par(pty = "s", mar = rep(0, 4))
    xrange <- range(d[,1], na.rm = TRUE)
    yrange <- range(d[,2], na.rm = TRUE)
    lb <- min(xrange[1],yrange[1])
    ub <- max(xrange[2],yrange[2])
    bounds <- c(lb,ub)
    graphics::plot.window(bounds, bounds, asp = aspect_ratio)

    if (color_groups > 1) {
      d_transitions <- dplyr::mutate(
        d,
        transition = (color_id != dplyr::lag(color_id,
                                             default = 1)))
      d_transitions <- dplyr::mutate(
        d_transitions,
        color_id = dplyr::if_else(transition,
                                  dplyr::lag(color_id, default = 1),
                           color_id),
        color_cycle_id = dplyr::if_else(transition,
                                        dplyr::lag(color_cycle_id,
                                     default = 1),
                                 color_cycle_id),
        id = dplyr::if_else(transition,
                            dplyr::lag(id, default = 1),
                     id))
      d_transitions <- dplyr::filter(d_transitions,
                                     transition)
if (end_at_beginning) {

  d_last <- d_transitions[nrow(d_transitions),]
  d_first <- d_transitions[1, ]
  d_last$x <- d_first$x
  d_last$y <- d_first$y
  d_last$id <- d_last$id + 1

  d <- dplyr::arrange(dplyr::bind_rows(d,
                                       d_transitions,
                                       d_last),
                      color_cycle_id,
                      color_id,
                      id)
} else {
  d <- dplyr::arrange(dplyr::bind_rows(d,
                                       d_transitions),
                      color_cycle_id,
                      color_id,
                      id)
}


          }

    nd <- tidyr::nest(dplyr::group_by(d, color_cycle_id, color_id))
    nd <- dplyr::mutate(nd, col = scales::alpha(colors[color_id],
                                         alpha = transparency))
    nd <- dplyr::rename(nd, x = data)

    purrr::pwalk(dplyr::select(nd, x, col), graphics::polypath,
                 border = NA,
                 rule = rule)



    grDevices::dev.off()
    if (openfile) pander::openFileInOS(filename)
  }


  return(d)
}


