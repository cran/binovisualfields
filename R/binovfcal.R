#' Calculates an array of integrated visual fields
#'
#' \code{binovfcal} calculates an array of integrated visual fields given required parameters.
#' @param leftvf A matrix of left monocular visual field
#' @param rghtvf A matrix of right monocular visual field
#' @param pd Pupil distance in mm
#' @param gender A string of either "male" or "female"
#' @param lefttheta A number angle of convergence for the left eye in radian
#' @param righttheta  A number  angle of convergence for the right eye in radian
#' @param distplanes A vector of object distances in mm.
#' @param m_xs Horizontal coordinates for monocular visual field for the 24-2 pattern
#' @param m_ys Vertical coordinates for monocular visual field for the 24-2 pattern
#' @param c_xs Horizontal coordinates for integrated visual field (from -57 to 57 degree with 6 degree spacing)
#' @param db_cutoff cutoff value default to 25 dB above which the simulated threshold value is returned (NA otherwise) when there threshold value is present only for one eye
#' @return An array of binocular visual fields for the distances specified by distplanes vector.
#' @importFrom stats approx
#' @examples
#' rghtvf <- matrix(c(
#' NA, NA, NA, 30, 30, 30, 30, NA, NA, NA,
#' NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
#' NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
#' 30, 30, 30, 30, 30, 30, 30, 30, 30, NA,
#' 30, 30, 30, 30, 30, 30, 30,  0, 30, NA,
#' NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
#' NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
#' NA, NA, NA, 30, 30, 30, 30, NA, NA, NA
#' ), ncol=10, byrow=TRUE)
#' leftvf <- rghtvf[, 10:1]
#' binovfcal(leftvf, rghtvf, -.045, .045, c(1000, 1010), pd=62)
#' @section Warning:
#' the value of either pd or gender has to be provided
#' the unit of pd and fixdist must be the same, default to mm.
#' @export
binovfcal <- function(leftvf, rghtvf,
                      lefttheta, righttheta,
                      distplanes,
                      pd=NULL, gender=NULL,
                      m_xs = seq(-27, 27, length.out = 10),
                      m_ys = seq(21, -21, -6),
                      c_xs = seq(-57, 57, 6),
                      db_cutoff = 25) {
    if (is.null(c(pd, gender))) stop("the value of either pd or gender has to be provided")
    if (!is.null(pd)) {
        stopifnot(length(pd) == 1,
                  is.numeric(pd))
    }
    if (!is.null(gender)) {
        stopifnot(gender %in% c("male", "female"))
    }
    if (getOption("warn") > 0) {
        stopifnot(nrow(leftvf) == 8, ncol(leftvf) == 10,
                  nrow(rghtvf) == 8, ncol(rghtvf) == 10,
                  is.numeric(lefttheta),  length(lefttheta)  == 1,
                  is.numeric(righttheta), length(righttheta) == 1,
                  is.numeric(distplanes))
    }
    if (is.null(pd)){
        pd <- ifelse(gender== "female", .ddivfEnv$pd_gender[1],  .ddivfEnv$pd_gender[2])
    }
    combined <- matrix(NA, nrow = length(m_ys), ncol = length(c_xs))
    combined_array <- replicate(length(distplanes), combined)

    plane <- 1
    for (dp in distplanes) {
        for (y in m_ys) {
            row_index <- (21 - y)/6 + 1
            vf_left <- leftvf[row_index, ]
            vf_rght <- rghtvf[row_index, ]

            combined[row_index, ] <- sapply(c_xs, function(toP) {
                pxy <- c(dp * cos(toP/180 * pi), dp * sin(toP/180 * pi))

                left_angle <- round((atan2(pxy[2] - (-pd/2), pxy[1]) + lefttheta) * 180/pi)
                rght_angle <- round((atan2(pxy[2] - pd/2,    pxy[1]) + righttheta) * 180/pi)

                left_db <- stats::approx(m_xs, vf_left, left_angle)$y
                rght_db <- stats::approx(m_xs, vf_rght, rght_angle)$y
                # print(paste(toP, left_angle, rght_angle, left_db, rght_db))
                if (is.na(left_db)) {
                  if (is.na(rght_db)) {
                    return(NA)
                  } else {
                    if (rght_db >= db_cutoff) {
                      return(rght_db)
                    } else {
                      return(NA)
                    }
                  }
                } else {
                  if (is.na(rght_db)) {
                    if (left_db >= db_cutoff) {
                      return(left_db)
                    } else {
                      return(NA)
                    }
                  } else {
                    return(max(left_db, rght_db))
                  }
                }
            })
        }
        combined_array[, , plane] <- combined
        plane <- plane + 1
    }
    dimnames(combined_array) <- list(m_ys, c_xs, distplanes)
    return(round(combined_array))
}
