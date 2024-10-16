#! /usr/bin/env Rscript


if( Sys.getenv('HTMLREPORTER_MODE') == 'DEVELOPMENT' ){
  # Obtain this script directory
  full.fpath <- normalizePath(unlist(strsplit(commandArgs()[grep('^--file=', 
                  commandArgs())], '='))[2])
  main_path_script <- dirname(full.fpath)
  root_path <- file.path(main_path_script, '..', '..')
  # Load custom libraries
  devtools::load_all(file.path(root_path))
  source_folder <- file.path(root_path, 'inst')
}else{
  require('htmlreportR')
  root_path <- find.package('htmlreportR')
  source_folder <- file.path(root_path)
}

option_list <- list(
  optparse::make_option(c("-i", "--input"), type="character", default=NULL,
    help="Path to Rmd file."),
  optparse::make_option(c("-o", "--output"), type="character", default=NULL,
    help="Path to output file. If NULL, it will be assumed to be the same
    path, but ending in 'txt'.")
  )

opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))

if(is.null(opt$output)) {
	opt$output <- paste0(tools::file_path_sans_ext(opt$input), ".txt")
}

file <- readLines(opt$input)
item_environment <- "none"
output <- character(0)
for(line in seq(length(file))) {
	current_line <- file[line]
	if(grepl("^\\*(?!\\*)|\n\\*(?!\\*) ", current_line, perl = TRUE)) {
		if(item_environment != "list"){
			current_line <- paste0("<ul>\n", current_line)
			item_environment <- "list"
			}
		current_line <- replace_paired_mark(string = current_line,
																			  pattern = "(\\*)(.*)",
																		    replace = c("<li>", "</li>"))
		} else if(item_environment == "list"){
			item_environment <- "none"
			current_line <- paste0("\n</ul>\n", current_line)
		}
		
	if(grepl("```\\{r", current_line)) {
		item_environment <- "rcode"
		current_line <- replace_paired_mark(string = current_line,
																		pattern = "(```\\{r)(.*)(\\})",
																		replace = c("<!--begin.rcode", ""))
	}
	if(grepl("```", current_line, perl = TRUE) & item_environment == "rcode") {
		current_line <- gsub("```", "end.rcode-->", current_line)
		item_environment <- "none"
	}
	if((grepl("#", current_line) & item_environment != "rcode") | grepl("cat\\(\"#", current_line)) {
		text <- stringr::str_match(current_line, "(#+)(.*)")
		level <- nchar(text[2])
		replace_1 <- paste0("<h", level, ">")
		replace_2 <- paste0("</h", level, ">")
		current_line <- replace_paired_mark(string = current_line, pattern = "(#+)(.*)",
																	  replace = c(replace_1, replace_2))
	}
	current_line <- replace_paired_mark(string = current_line,
																	pattern = paste0("(\\*\\*\\*+?)([-():; \\w]+)",
																		"(\\*\\*\\*+?)"),
																	replace = c("<strong><em>", "</em></strong>"))
	current_line <- replace_paired_mark(string = current_line,
																	pattern = paste0("(\\*\\*+?)([-():; \\w]+)(\\*",
																		"\\*+?)"),
																	replace = c("<strong>", "</strong>"))
	current_line <- replace_paired_mark(string = current_line,
																	pattern = paste0("(\\*+?)([-():; \\w]+)(\\*\\*",
																		"+?)"),
																	replace = c("<em>", "</em>"))
	output <- paste0(output, current_line, "\n")
}

cat(output, file = opt$output)
