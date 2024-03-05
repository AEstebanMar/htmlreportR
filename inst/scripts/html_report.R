#! /usr/bin/env Rscript

options(warn=1)
if( Sys.getenv('HTMLREPORTER_MODE') == 'DEVELOPMENT' ){
  # Obtain this script directory
  full.fpath <- normalizePath(unlist(strsplit(commandArgs()[grep('^--file=', 
                  commandArgs())], '='))[2])

  main_path_script <- dirname(full.fpath)
  root_path <- file.path(main_path_script, '..', '..')
  # Load custom libraries
  devtools::load_all(file.path(root_path, 'R'))

  template_folder <- file.path(root_path, 'inst', 'templates')
}else{
  require('htmlreportR')
}



option_list <- list(
  optparse::make_option(c("-d", "--data_files"), type="character", default=NULL,
    help="Comma sepparated text files with data to use on graphs or tables within report"),
    optparse::make_option(c("-t", "--template"), type="character", default=NULL,
    help="Report template"),
 optparse::make_option(c("-o", "--output_file"), type="character", default=NULL,
    help="HTML file path to render the template"),
 optparse::make_option(c("--title"), type="character", default="htmlreportR",
    help="Title of the html report")
  )

opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))

main_htmlreportR(opt)
