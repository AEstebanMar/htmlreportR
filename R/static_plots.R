#' Function to draw heatmap, filling with provided parameter
#' 
#' `gg_heatmap` takes a table (preferably in tidy format) and creates a heatmap
#' between two of its columns, filling blocks with provided parameter.
#' @param input_table Table containing input input_table.
#' @param x_axis,y_axis Names of columns to use for x and y axes.
#' @param fill A string. Parameter with which the heatmap will be drawn.
#' Default: "Freq", for "frequency".
#' @param text_plot A string. Name of input_table column with text to
#' fill heatmap cells.
#' @param text_size A numeric. Font size of text_plot.
#' @param text_colour A string. Font colour of text_plot.
#' @param xlabel,ylabel Booleans.
#'   * `TRUE` (the default): Axis wil be labeled in plot.
#'   * `FALSE`:              Axes will NOT be labeled in plot.
#' @param x_angle A numeric. x axis label angle.
#' @param dendro Print dendrogram. Unused.
#' @param col A vector of colour code strings.
#' @param na_col A vector of colour code strings.
#' @returns A heatmap.
#' @examples
#' none yet
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_tile theme_minimal theme 
#' element_blank element_text scale_fill_gradient2 geom_text

gg_heatmap <- function(input_table,
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
                       transform_f = NULL,
                       col = c("#0000D5","#FFFFFF","#D50000"),
                       na_col = "grey50"){

    if(!is.null(transform_f)){
      input_table <- transform_f(input_table)
    }

    if(!all(c(x_axis, y_axis, fill) %in% colnames(input_table))){
      input_table <- reshape2::melt(as.matrix(input_table)) 
      colnames(input_table) <- c(x_axis, y_axis, fill)
    }
    pp <- ggplot2::ggplot(input_table, ggplot2::aes(x = .data[[x_axis]] , 
          y = .data[[y_axis]], fill = .data[[fill]] )) +
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
      pp <- pp + ggplot2::geom_text(mapping = ggplot2::aes(label=.data[[text_plot]]), data = input_table, 
        colour = text_colour, size = text_size) 
    }
    return(pp)
}



