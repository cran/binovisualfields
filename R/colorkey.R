#' Generates a color legend for visual field plots
#'
#' \code{colorkey} generates a color legend for dB values in visual field plots with darker colors corresponding to lower dB values (e.g., black = 0dB, bright yellow >= 35dB)
#' @return  a color legend for dB values in visual field plots
#' @importFrom graphics axis par plot
#' @examples
#' colorkey()
#' @export
colorkey <- function() {
    oldmar <- graphics::par(no.readonly = TRUE)$mar
    on.exit(graphics::par(mar=oldmar))
    graphics::par(mar = c(3, 0, 0, 0))
    # plot(rep(1, 37), col = c(colfunc(35), tail(colfunc(35), 1), tail(colfunc(35), 1)), pch = 19, cex = 2, yaxt = "n")
    graphics::plot(rep(1, 35), col = c(colfunc(35)), pch = 19, cex = 3, yaxt = "n", bty="n")
    graphics::axis(1)
}
