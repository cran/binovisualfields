#' Color-codes dB values
#'
#' \code{get_color} returns the color of a visual field location given its dB value (e.g., black = 0dB, white = 35dB)
#' @param db A number of sensitivity threshold in dB
#' @return the color of a visual field location in hcl color space with darker colors corresponding to lower dB values
#' @importFrom grDevices grey
#' @examples
#' get_col(25)
#' @export
get_col <- function(db) {
    if (is.na(db))
        return(NA)
    db <- max(min(db, 35), 0)
    return(grDevices::grey(db/35, 1))
}
