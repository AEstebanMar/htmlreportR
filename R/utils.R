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
    n_columns <- unlist(lapply(table_data,
                 function(vector) all(grepl("^-?\\d*\\.?\\d+$", vector))))
    names(n_columns) <- NULL
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

#' replace_paired_mark
#'
#' Recursively replaces paired marks in a string.
#'
#' @importFrom stringr str_match_all str_escape
#' @param string String to edit
#' @param pattern Pattern to recursively replace
#' @param replace Vector containing, in that order, expression that will replace
#' the first paired element and the second paired element
#'
#' @return Modified string.
#' @export
#' @examples
#' example_string <- "** This should be in bold **"
#' pattern <- "(\\*\\*+?)([- \\w]+)(\\*\\*+?)"
#' replace_paired_mark(example_string, pattern, c("<strong>", "</strong>"))

replace_paired_mark <- function(string, pattern, replace) {
    if(grepl(pattern, string, perl = TRUE)) {
    text <- stringr::str_match_all(string, pattern)[[1]]
        for(i in seq(nrow(text))) {
            string <- gsub(pattern = stringr::str_escape(text[i, 1]),
                           x = string, replacement = paste0(replace[1],
                                                        text[i, 3], replace[2]))
        }   
    }
    return(string)
}

#' make_html_list
#'
#' Take an input vector or list and use it to build an html list of specified
#' type
#'
#' @importFrom utils head
#' @param list_content Vector or list of elements with which to build the list
#' @param list_levels Vector or list defining nesting levels. Default NULL
#' @param list_types Vector or list with types to assign to each element ("ul"
#' for unordered or "ol" for ordered).' If not supplied, default_type will
#' determine list type for each element. Default NULL
#' @param default_type List type to default if list_types is undefined, "ul"
#' for unordered or "ol" for ordered. Default "ul"
#'
#' @returns Formatted html list ready to render.
#' @export
#' @examples
#' content <- c("One", "Two", "Three")
#' make_html_list(list_content = content)

make_html_list <- function(list_content, list_levels = NULL, list_types = NULL,
                           default_type = "ul") {
    list_df <- .prepare_standard_triplet_df(list_content = list_content,
                                            list_levels = list_levels,
                                            list_types = list_types,
                                            default_type = default_type)
    html_list <- vector(mode = "list", length = nrow(list_df))
    for(row in seq(nrow(list_df))) {
        current_row <- list_df[row, , drop = FALSE]
        content <- current_row$content
        level <- current_row$level
        type <- current_row$type
        html_content <- paste0("<li>", content, "</li>")
        if(row != 1) {
            last_level <- list_df[row - 1, ]$level
        } else {
            last_level <- 0
            nest_stack <- NULL
        }
        html_tag <- NULL
        if(level != last_level) {
            diff <- level - last_level
            reps <- abs(diff)
            if(diff > 0) {
                nest_stack <- c(nest_stack, type)
                slash <- NULL
            }
            html_tag <- paste0(rep(nest_stack[length(nest_stack)],
                               reps), ">\n")
            if(diff < 0) {
                nest_stack <- head(nest_stack, -reps)
                slash <- "/"
            }
            html_tag <- paste0("<", slash, html_tag, collapse = "")
        }
        html_list[[row]] <- paste0(html_tag, html_content, collapse = "\n")
    }
    html_list <- paste0(html_list, collapse = "\n")
    if(length(nest_stack) > 0) {
        html_final_tag <- paste0("</", rev(nest_stack), ">\n",
                                     collapse = "")
        html_list <- paste(html_list, html_final_tag, sep = "\n")
    }
    return(html_list)
}

#' .prepare_standard_triplet_df
#'
#' Build triplet data frame for make_html_list function from three vectors.
#' User should NEVER have to worry about this function
#'
#' @inheritParams make_html_list
#'
#' @returns Data frame of contents, levels and types, ready for make_html_list.

.prepare_standard_triplet_df <- function(list_content, list_levels = NULL,
                                         list_types = NULL,default_type = "ul"){
    if(is.null(list_levels)) {
        list_levels <- rep(1, length(list_content))
    }
    if(is.null(list_types)) {
        list_types <- rep(default_type, length(list_content))
    }
    standard_triplet_df <- data.frame(content = list_content,
                                      level = list_levels, type = list_types)
    return(standard_triplet_df)
}

#' .add_id_column
#'
#' takes hash_vars, looks for a specific table, and adds its ID in a new
#' column. Private method, use at your own risk.
#'
#' @param id hash_vars ID of table to process
#' @param hash_vars plotter hash_vars slot
#' @inheritParams merge_hashed_tables-htmlReport-method

.add_id_column <- function(id, hash_vars, from_id_name, add_colnames = FALSE) {
    table <- hash_vars[[id]]
    if(add_colnames) {
        table <- row_to_header(table)
    }
    table[from_id_name] <- id
    return(table)
}

#' parse_paths
#'
#' takes a vector of comma-separated paths and parses it so files can be loaded.
#' If any of the paths contain wildcards, they will be expanded.
#'
#' @param string String containing paths to expand.
#' @examples
#' \dontrun{
#'         parse_paths("./*/test*txt")
#'        }
#' @export

parse_paths <- function(string) {
    expanded_paths <- NULL
    if(!is.null(string)) {
        expanded_paths <- list()
        for(path in strsplit(string, ",")[[1]]) {
            if(grepl("\\*", path)) {
                globbed_path <- Sys.glob(path)
            } else {
                globbed_path <- path
            }
            if(length(globbed_path) < 1) {
                warning("Expression \"", path, "\" expanded to 0 files.")
            }
            expanded_paths <- c(expanded_paths, globbed_path)
        }
        expanded_paths <- paste(expanded_paths, collapse = ",")
    }
    return(expanded_paths)
}

#' row_to_header
#'
#' turns a row of the input data frame and turns it into its colnames, and then
#' removes the actual row.
#'
#' @param data_frame Data frame to manipulate.
#' @param row An integer. Row to set as new columns and remove from data frame.
#' @examples
#' row_to_header(head(mtcars))
#' @export

row_to_header <- function(data_frame, row = 1) {
    colnames(data_frame) <- data_frame[row, ]
    data_frame <- data_frame[-row, , drop = FALSE]
    return(data_frame)
}

#' col_to_rownames
#'
#' turns a column of the input data frame and turns it into its rownames, and
#' then removes the actual column.
#'
#' @param data_frame Data frame to manipulate.
#' @param col An integer. Col to set as new rowumns and remove from data frame.
#' @examples
#' col_to_rownames(tail(mtcars))
#' @export

col_to_rownames <- function(data_frame, col = 1) {
    rownames(data_frame) <- data_frame[, col]
    data_frame <- data_frame[, -col, drop = FALSE]
    return(data_frame)
}

.read_table <- function(file, header = FALSE, sep = "\t") {
    message("Reading file ", file)
    return(utils::read.table(file, header = header, sep = sep))
}
