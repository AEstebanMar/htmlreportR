## Estas funciones deben eliminarse cuando cambiemos la estructutra de tablas de los report


#' Select first row from data frame and make it its header
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
build_header <- function(table_data){
	colnames(table_data) <- table_data[1,]
	table_data <- table_data[-1,]
	return(table_data)
}

as_numeric_if <- function(table_data){
#cambiar nombre cancer
	numeric_columns <- grepl("^\\d*\\.?\\d+$", table_data[1,])

	table_data[,numeric_columns] <- lapply(table_data[,numeric_columns], as.numeric)
	return(table_data)
} 
	


# dev_stdout = function (underlying_device = png, ...) {
#     filename = tempfile()
#     underlying_device(filename, ...)
#     filename
# }

# dev_stdout_off = function (filename) {
#     dev.off()
#     on.exit(unlink(filename))
#     fake_stdout = pipe('cat', 'wb')
#     on.exit(close(fake_stdout), add = TRUE)
#     writeBin(readBin(filename, 'raw', file.info(filename)$size), fake_stdout)
# }