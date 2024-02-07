#' Print R plot in a scrollable div
#' 
#' `plot_in_div` allows a user to scroll through an html div to see a printed
#' plot without the need to scale it to div size.
#' @param cex, magnitude by which figure dimensions will be scaled
#' @param max_size,min_size plot area upper and lower limits in inches
#' @param counter, ???
#' @returns html code containing a scrollable div with the printed plot.
#' @examples none yet
#' @export
#' @importFrom knitr knit knit_expand
#' @importFrom stats runif

plot_in_div <- function(g, fig_height=7, fig_width=12,
  cex = 1,
  max_size = 50,
  min_size = 10, 
  counter = NULL) {
  cat('\n<div class="plot_real_size">\n')
  fig_height <- fig_height * cex
  fig_width <- fig_width * cex

  if(fig_height > max_size){
    fig_height <- max_size
    }else if(fig_height < min_size){
    fig_height <- min_size
  }
  if(fig_width > max_size){
    fig_width <- max_size
    }else if(fig_width < min_size){
    fig_width <- min_size  
  }
  g_deparsed <- paste0(deparse(function() {g}), collapse = '')
  # set.seed(Sys.time())
  if (!is.null(counter)){
    chunk_name <- paste0("sub_chunk_", counter)
    counter <- counter + 1
  } else {
    chunk_name <- paste0("sub_chunk_", floor(stats::runif(1) * 10000))
  }
  sub_chunk <- paste0("\n```{r ", chunk_name, ", fig.height=", 
    fig_height, ", fig.width=", fig_width, ", echo=FALSE}", 
    "\n(", g_deparsed, ")()\n```\n\n\n") 
    # sub_chunk_", floor(runif(1) * 1000000), "
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
  cat('\n</div>\n')
  return(counter)
}


