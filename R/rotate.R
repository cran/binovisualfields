#' Rotates a point (x,y) by an angle
#'
#' \code{rotate} calculates the coordinates for a point (x,y) after it rotates by an angle theta (radian)
#' @param xy A vector of length 2 representing the coordinates of a point in cartesian system
#' @param theta A number rotating angle in radian
#' @return Coordinates of the point after the rotation
#' @examples
#' rotate(c(1000, 0), theta=pi/6)
#' @export
rotate <- function(xy, theta = 0) {
    if (length(xy)!=2) stop("'xy' must be a two-element vector.")
    nx <- sum(c(cos(theta), -sin(theta)) * as.vector(xy))
    ny <- sum(c(sin(theta),  cos(theta)) * as.vector(xy))
    return(c(nx, ny))
}
