#! /usr/bin/env Rscript


option_list <- list(
  optparse::make_option(c("-i", "--input_tables"), type="character", default=NULL,
    help="Tab-separated tables to read as input. Files must by comma-separated.
    	  Tables must have a header"),
    optparse::make_option(c("-t", "--template"), type="character", default=NULL,
    help="Rmarkdown template to render"),
 optparse::make_option(c("-o", "--output_dir"), type="character", default=NULL,
    help="Directory where provided template will be rendered")
  )

opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))

table_list <- strsplit(opt$input_tables, ",")[[1]]
data <- lapply(table_list, read.table, header = FALSE)
