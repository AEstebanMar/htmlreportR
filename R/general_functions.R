## Estas funciones deben eliminarse cuando cambiemos la estructutra de tablas de los report



build_header <- function(table_data){
	colnames(table_data) <- table_data[1,]
	table_data <- table_data[-1,]
	return(table_data)
}

as_numeric_if <- function(table_data){
#cambiar nombre cancer
	numeric_columns <- grepl("^\\d*\\.?\\d+$", table_data[1,])

	table_data[,numeric_columns] <- lapply(table_data[,numeric_columns], 
										   function(col) {
										   	if (!is.factor(col)){
										   		as.numeric(col) 
										   	} else {
										   		col
										   	}})
										   	
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