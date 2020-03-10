#' Color-codes dB values
#'
#' \code{get_inv_col} returns the color of a visual field location given its dB value, (e.g., white < 15dB, black > 15dB)
#' @param db A number of sensitivity threshold in dB
#' @return either white or black color for a visual field location
#' @examples
#' get_inv_col(25)
#' @export

get_inv_col <- function(db) {
    if (is.na(db))
        return(NA)
    if (db < 15)
        return("white")
    return("black")
}
