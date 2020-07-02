# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   06/05/2010
# *

#' All codes copy from library RGraphics. The xscale and yscale were changed to draw more figures in the right and top
#'
#' @param ozRegion an object of class "ozRegion" for library ozRegion
#' 
#' @rdname ozGraphics
makeOzViewports <- function(ozRegion, 
        xlim = ozRegion$rangex,
        ylim = ozRegion$rangey) {
  vpStack(viewport(name="ozlay", layout=grid.layout(1, 1,
                     widths=diff(xlim),
                     heights=diff(ylim), 
                     respect=TRUE)),
          viewport(name="ozvp", layout.pos.row=1, 
                   layout.pos.col=1,
                   xscale=xlim + c( -1, 3 ), 
                   yscale=ylim + c( -3, 3 ), 
                   clip=TRUE))
}

#' All codes copy from library RGraphics.
#'
#' @param ozRegion an object of class "ozRegion" for library ozRegion
#' 
#' @rdname ozGraphics
makeOzLines <- function(ozRegion) {
  numLines <- length(ozRegion$lines)
  lines <- vector("list", numLines)
  index <- 1
  for(i in ozRegion$lines) {
    lines[[index]] <- linesGrob(i$x, i$y, 
                    default.units="native",
                    vp=vpPath("ozlay", "ozvp"), 
                    name=paste("ozlines", index, sep=""))
    index <- index + 1
  }
  do.call("gList", lines)
}

#' All codes copy from library RGraphics.
#'
#' @param ozRegion an object of class "ozRegion" for library ozRegion
#' @param name A character identifier for the grob
#' @param gp A gpar object
#' @param vp A viewport object 
#' 
#' @rdname ozGraphics
#' @export
ozGrob <- function(ozRegion, name=NULL, gp=NULL, vp=NULL, 
        xlim = ozRegion$rangex,
        ylim = ozRegion$rangey ) 
{
      gTree(ozRegion=ozRegion, name=name, gp=gp, vp=vp, 
        childrenvp=makeOzViewports(ozRegion, xlim, ylim), 
        children=makeOzLines(ozRegion), 
        cl="ozGrob")
}

#' All codes copy from library RGraphics. 
#'
#' @param ... For graphics parameters
#' 
#' @rdname ozGraphics
#' @export
grid.ozGrob <- function(...) {
  grid.draw(ozGrob(...))
}
