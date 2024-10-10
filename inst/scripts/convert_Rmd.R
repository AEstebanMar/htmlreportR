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
		file[line] <- gsub("```\\{r", "<!--begin.rcode", file[line])
		file[line] <- gsub("}", "", file[line])
	} else if (grepl("```", file[line])) {
		file[line] <- gsub("```", "end.rcode-->", file[line])
	}
	if(grepl("#", file[line])) {
		expr <- regexec("#*", file[line])
		level <- attributes(expr[[1]])$match.length
		text <- gsub("#", "", file[line])
		file[line] <- paste0("<h", level, ">", text, "</h", level, ">")
	}
	if(grepl("\\*\\*", file[line])) {
		text <- gsub("\\*\\*", "", file[line])
		file[line] <- paste0("<strong>", text, "</strong>")
	}
}

writeLines(file, con = opt$output)

