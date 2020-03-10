#' Makes a visual field matrix
#'
#' \code{makevf} Makes a visual field matrix from a vector of 54 elements for the 24-2 test results
#' @param vfvector A vector of length 54. The 54 data points for the 24-2 pattern have to be ordered from superior nasal to inferior temporal.
#' @param eye A string of either "left" or "right"
#' @return A matrix of 8 rows and 10 columns
#' @examples
#' vfvector <- rep(35, 54)
#' makevf(vfvector, 'left')
#' @export


makevf <- function(vfvector, eye = c("left", "right")) {
    if (getOption("warn") > 0) {
        stopifnot(length(vfvector) == 54,
        eye %in% c("left", "right"))
    }
    rght_vf_pattern <- matrix(c(3, 4, 3, 2, 6, 2, 1, 8, 1, 0, 9, 1, 0, 9, 1, 1, 8, 1, 2, 6, 2, 3, 4, 3), nrow = 8, byrow = T)
    left_vf_pattern <- rght_vf_pattern[, c(3:1)]

    if (eye == "left") {
        vf_pattern <- left_vf_pattern
    } else {
        vf_pattern <- rght_vf_pattern
    }

    vm <- matrix(NA, ncol = 10, nrow = 8)
    start <- 0
    for (i in 1:nrow(vm)) {
        count <- vf_pattern[i, 2]
        vm[i, (vf_pattern[i, 1] + 1):(vf_pattern[i, 1] + count)] <- vfvector[(start + 1):(start + count)]
        start <- start + count
    }
    vm
}

