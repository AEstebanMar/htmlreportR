#! /usr/bin/env Rscript

library(optparse)
# library(R.utils)
# library(ggplot2)
# library(rmarkdown)
# library(canvasXpress)
# library(Rmisc)
# library(gridExtra)
# library(plyr)
# library(knitr)

########################################################################
## FUNCTIONS
########################################################################
load_files <-function(file_names, headers, read_table_parameters){
	argument_list_for_read_table <- parse_arguments(read_table_parameters)
	data <- list()
	count = 1
	for (file_path in file_names){
        	if(headers[count] == 't'){
                	header = TRUE
	        }else if(headers[count] == 'f'){
        	        header = FALSE
	        }
		argument_list_for_read_table['file'] = file_path
		argument_list_for_read_table['header'] = header
        	data[[basename(file_path)]] = do.call(read.table, argument_list_for_read_table)
	        count = count + 1
	}
	return(data)
}

parse_arguments <- function( parameter_string){
	argument_list <- list()
	pairs <- unlist(strsplit(parameter_string, ','))
	for (i in 1:length(pairs)) { 
        	members = unlist(strsplit(pairs[i], '='))
		argument_list[members[1]] = eval(parse(text=members[2]))
	}
	return( argument_list)

}

load_files_by_factors <- function(data, column_names, path_column, header ){
	all_data <- list()
	factor_columns <- match(column_names, names(data))
	factor_combinations <- unique(data[column_names])
	for(row in 1:nrow(factor_combinations)){
		combination <- as.vector(t((factor_combinations[row,]))) #extract row AND convert to vector
		check_combination <- data[factor_columns] == combination[col(data[factor_columns])]
		paths <- data[[path_column]][which(apply(check_combination, 1, sum) == length(combination))]
		name_list <- paste(combination, collapse='_')
		files <- list()
		count = 1
		for(file_path in paths){
			files[[count]] <- read.table(file_path, sep="\t", header=header)
			count = count + 1
		}
		all_data[[name_list]] <- files
	}
	return(all_data)
}

########################################################################
## OPTPARSE
########################################################################

option_list <- list(
  make_option(c("-d", "--data"), type="character",
              help="Input path files comma separated"),
  make_option(c("-o","--output"), type="character",
              help="Output report"),
  make_option(c("-H","--headers"), type="character",
              help="Character comma separated using 't' for indicate the presence of header or 'f' when the file lacks of it"),
  make_option(c("-t","--template"), type = "character",
              help="Template file to use in the report rendering process"),
  make_option(c("-r","--read_file_parameters"), type = "character", default="comment.char='',sep=\"\t\"",
              help="Read table parameters. Default 'file_path, sep=\\t'")
)

opt <- parse_args(OptionParser(option_list=option_list))

########################################################################
## MAIN
########################################################################
file_paths <- strsplit(opt$data, ',')[[1]]
headers <- strsplit(opt$headers, ',')[[1]]
absolute_output_path <- R.utils::getAbsolutePath(opt$output)
data <- load_files(file_paths, headers, opt$read_file_parameters)
knitr::opts_chunk$set(echo = FALSE, results="asis", message=FALSE)

knitr::knit(opt$template, output="test.html")

# rmarkdown::render(opt$template, output_file = absolute_output_path)

