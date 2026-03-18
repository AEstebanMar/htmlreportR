#! /usr/bin/env Rscript

options(warn=1)
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
  optparse::make_option(c("-d", "--data_files"), type = "character", default=NULL,
    help = "Comma separated paths to tabular input files"),
  optparse::make_option(c("-t", "--template"), type = "character", default=NULL,
    help = "Path to template to render"),
  optparse::make_option(c("-o", "--output_file"), type = "character", default=NULL,
    help = "Path to file where output will be written"),
  optparse::make_option(c("--title"), type = "character", default="htmlreportR",
    help = "Title of the report"),
  optparse::make_option(c("-u", "--uncompressed_data"), type = "logical", default = TRUE, action = "store_false", 
    help = "Do not compress final output" ),
  optparse::make_option(c("-c", "--css_files"), type = "character", default=NULL,
    help = "Comma-separated paths to css files to include"),
  optparse::make_option(c("-j", "--js_files"), type = "character", default=NULL,
    help = "Comma-separated paths to js file to inclide"),
  optparse::make_option(c("-C", "--css_cdn"), type = "character", default=NULL,
    help = "Comma-separated URLs to css CDNs to include"),
  optparse::make_option(c("-J", "--js_cnd"), type = "character", default=NULL,
    help = "Comma-separated URLs to javascript CDNs to include"),
  optparse::make_option(c("-m", "--menu"), type = "character", default="contents_list",
    help = "Type of index content. Values: \"contents_list\" (the default) or \"menu\"")
  )

opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))
opt$data_files <- parse_paths(opt$data_files)
opt$js_files <- parse_paths(opt$js_files)
opt$css_files <- parse_paths(opt$css_files)
opt$source_folder <- source_folder
main_htmlreportR(opt)
