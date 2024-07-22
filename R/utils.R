#' @importFrom xfun base64_uri
#' @noRd
embed_file <- function(file) {
    xfun::base64_uri(file)
}

#' @noRd
split_str <- function(string, split = NULL) {
  if (is.null(split))
    stop("split character must be specifyed")
  if (is.null(string)) return(NULL)  
  if (nchar(string) <= 1) return(NULL)  

  splitted_str <- unlist(strsplit(string, split = split))
  return(splitted_str)
}

#' @noRd
check_numeric_fields <- function(table_data){
    n_columns <- grepl("^-?\\d*\\.?\\d+$", table_data[1,])
    return(n_columns)   
} 

#' @noRd
paste_tag <- function(vec, tag) {
    paste0("<", tag, "> ", vec, " </", tag, ">")
}

#' @noRd
update_options <- function(ref, sub){
        for (opt in names(sub)) {
            ref[opt] <- sub[opt]    
        }
        ref
}