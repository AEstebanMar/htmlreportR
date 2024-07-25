#' @importFrom xfun base64_uri
#' @importFrom mime guess_type
#' @noRd
embed_file <- function(input) {
    if(file.exists(input)) {
        if("gz" %in% strsplit(input, "\\.")[[1]]) {
            size <- file.info(input)$size
            file <- readBin(input, "raw", size)
            rawContent <- memDecompress(file, type = "gzip")
            b64 <- xfun::base64_encode(rawContent)
            type <- mime::guess_type(input)
            b64_uri <- paste0("data:", type, ";base64,", b64)
        }else{
            b64_uri <- xfun::base64_uri(input)
        }
    } else {
        b64_uri <- xfun::base64_encode(input)
    }
    return(b64_uri)
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