#' @importFrom ggplot2 ggplot aes_string geom_tile theme_minimal theme 
#' element_blank element_text scale_fill_gradient2 geom_text
#' Function to draw heatmap, filling with provided parameter
#' 
#' `parse_column_names` takes an input data frame and creates a heatmap
#' between two of its columns, filling blocks with provided parameter.
#' @param data Table containing input data.
#' @param x_axis,y_axis Names of columns to use for x and y axes.
#' @param fill A string. Parameter with which the heatmap will be drawn.
#' Default: "Freq", for "frequency".
#' @param text_plot Text to fill heatmap cells
#' @param text_size A numeric. Font size of text_plot.
#' @param text_colour A string. Font colour of text_plot.
#' @param xlabel,ylabel Booleans.
#'   * `TRUE` (the default): Axis wil be labeled in plot.
#'   * `FALSE`:              Axes will NOT be labeled in plot.
#' @param x_angle A numeric. x axis label angle.
#' @param dendro Print dendrogram. Unused.
#' @param transpose A boolean.
#'   * `TRUE`:                Input will be transposed.
#'   * `FALSE` (the default): Input will NOT be transposed.
#' @param col A vector of colour code strings.
#' @param na_col A vector of colour code strings.
#' @returns A heatmap.
#' @examples
#' none yet
#' @export
gg_heatmap <- function(data,
                       x_axis = "x_axis",
                       y_axis= "y_axis",
                       fill = "Freq",
                       text_plot = NULL,
                       text_size = 2.5,
                       text_colour = "black",
                       xlabel = TRUE,
                       ylabel = TRUE,
                       x_angle = 25,
                     #  dendro = NULL,
                       transpose = FALSE,
                       col = c("#0000D5","#FFFFFF","#D50000"),
                       na_col = "grey50"){

    if(transpose){
      data <- t(data)
    }
    if(!all(c(x_axis, y_axis, fill) %in% colnames(data))){
      data <- reshape2::melt(as.matrix(data))
      colnames(data) <- c(x_axis, y_axis, fill)
    }
    str(data)
    pp <- ggplot2::ggplot(data, ggplot2::aes_(x = as.name(x_axis), 
          y = as.name(y_axis), fill = as.name(fill))) +
    ggplot2::geom_tile(show.legend = TRUE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank())+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = x_angle, 
        face = "bold", hjust = 1),
      axis.text.y = ggplot2::element_text(face = "bold")) +
    ggplot2::scale_fill_gradient2(
      low = col[1],
      mid= col[2],
      high = col[3],
      na.value = na_col,
      guide = "colourbar",
      aesthetics = "fill"
    )
    if(!xlabel){
      pp <- pp + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    }
    if(!ylabel){
      pp <- pp + ggplot2::theme(axis.title.y = ggplot2::element_blank())
    }
    if(!is.null(text_plot)){
      pp <- pp + ggplot2::geom_text(ggplot2::aes(label=as.name(text_plot)), 
        colour = text_colour, size = text_size) 
    }
    return(pp)
}



