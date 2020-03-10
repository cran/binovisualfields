#' Calculates the angle of convergence
#'
#' \code{caltheta} calculates the angle of convergence (radians) for left or right eye to fixate at a designated fixation distance.
#' @param fixdist A two element vector of fixation distance in mm in Cartesian coordinates.
#' @param pd A number of pupil distance in mm.
#' @param gender A string of either "male" or "female"
#' @param eye A string specifying either "left" or "right" eye.
#' @return The angle of convergence in radians that respective eye with a pupil distance of \code{pd} rolls to fixate at fixation distance of \code{fixdist}.
#' @examples
#' caltheta(c(600, 0), pd=65, eye="left")
#' caltheta(c(600, 0), gender="male", eye="left")
#' @section Warning:
#' the value of either pd or gender has to be provided
#' the unit of pd and fixdist must be the same and is default to mm.
#' @export
caltheta <- function(fixdist, pd=NULL, gender=NULL, eye = c("left", "right")) {
    if (is.null(c(pd, gender))) stop("the value of either pd or gender has to be provided")
    if (!is.null(pd)) {
        stopifnot(length(pd) == 1,
                  is.numeric(pd))
        }
    if (!is.null(gender)) {
        stopifnot(gender %in% c("male", "female"))
        }
    if (getOption("warn") > 0) {
        stopifnot(is.numeric(fixdist), length(fixdist) == 2,
                  eye %in% c("left", "right"))
    }
    if (is.null(pd)){
        pd <- ifelse(gender== "female", .ddivfEnv$pd_gender[1], .ddivfEnv$pd_gender[2])
    }
    if (eye == "left") {
        theta <- atan2(-pd/2 - fixdist[2], fixdist[1])
        } else {
        theta <- atan2(pd/2 - fixdist[2], fixdist[1])
     }
    return(theta)
}

