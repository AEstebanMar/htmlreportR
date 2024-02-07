#' Select row from data frame and make it its header
#' 
#' `parse_column_names` takes an input data frame and renames set columns in
#' order to simplify manipulation by other DRAGGER functions.
#' @param df A data frame of N rows.
#' @param row An integer. Index of row to turn into header (default 1).
#' @param update_rows A boolean.
#'   * `TRUE` (the default): data frame row names will be updated (first column
#'                           will be 1, second will be 2). If input df had no
#'							 row names, they are coerced to numerical values.
#' 							 This option ensures they will reflect their new
#' 							 positions.
#'   * `FALSE`:              data frame row names will be left untouched.
#'                           useful if they had previous values.
#' @returns A data frame of N-1 rows, where the selected row is now the header.
#' @examples
#' df <- data.frame(c("Animal", "Fish"), c("Item", "Table"))
#' print(df)
#' header_df <- row_to_header(df, 1)
#' print(header_df)
#' @export

row_to_header <- function(df, row = 1, update_rows = TRUE) {
	if (row < 1 || row > nrow(df)) {
		stop("row is out of bounds")
	}
	colnames(df) <- df[row, ]
	res <- df[-row, ]
	if (update_rows) {
		rownames(res) <- seq(1, nrow(res))
	}
	return(res)
}
