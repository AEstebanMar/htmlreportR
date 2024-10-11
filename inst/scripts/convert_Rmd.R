#! /usr/bin/env Rscript

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

for(line in seq(length(file))) {
	if(grepl("```\\{r", file[line])) {
		test <- replace_paired_mark(string = file[line],
																pattern = "(```\\{r)(.*)(\\})",
																replace = c("<!--begin.rcode", ""))
	} else if (grepl("```", file[line])) {
		file[line] <- gsub("```", "end.rcode-->", file[line])
	}
	if(grepl("#", file[line])) {
		text <- stringr::str_match(file[line], "(#+)(.*)")
		level <- nchar(text[2])
		replace_1 <- paste0("<h", level, ">")
		replace_2 <- paste0("</h", level, ">")
		file[line] <- gsub(pattern = text[1], x = text,
							  		   replacement = paste0(replace_1, text[1], replace_2))
	}
	file[line] <- replace_paired_mark(string = file[line],
																		pattern = paste0("(\\*\\*\\*+?)([- \\w",
																		"]+)(\\*\\*\\*+?)"),
																	  replace = c("<strong><em>",
																	  					  "</em></strong>"))
	file[line] <- replace_paired_mark(string = file[line],
																		pattern = paste0("(\\*\\*+?)([- \\w]+)",
																	  "(\\*\\*+?)"),
																	  replace = c("<strong>", "</strong>"))
	file[line] <- replace_paired_mark(string = file[line],
																		pattern = paste0("(\\*+?)([- \\w]+)",
																	  "(\\*\\*+?)"),
																	  replace = c("<em>", "</em>"))
}

writeLines(file, con = opt$output)

