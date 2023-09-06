#' Add together two numbers
#' 
#' @param x A number.
#' @param y A number.
#' @returns A numeric vector.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
    x + y
}

#' Convert hms format to minutes using lubridate
#' 
#' @param hms A time in hms format
#' @returns A numeric vector.
#' @export
#' @examples
#' convert_to_min("11:11:11")
#' convert_to_min("22:00:00")
convert_to_min <- function(hms){
    save <- lubridate::hms(hms)
    save <- lubridate::period_to_seconds(save)
    save <- save/60
    return (save)
}