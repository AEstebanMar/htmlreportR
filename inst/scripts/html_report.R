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
  optparse::make_option(c("-d", "--data_files"), type="character", default=NULL,
    help="Comma sepparated text files with data to use on graphs or tables within report"),
  optparse::make_option(c("-t", "--template"), type="character", default=NULL,
    help="Report template"),
 optparse::make_option(c("-o", "--output_file"), type="character", default=NULL,
    help="HTML file path to render the template"),
 optparse::make_option(c("--title"), type="character", default="htmlreportR",
    help="Title of the html report"),
 optparse::make_option(c("-u", "--uncompressed_data"), type = "logical", default = TRUE, action = "store_false", 
    help = "Indicate if figure data must be uncompressed." ),
 optparse::make_option(c("-c", "--css_files"), type="character", default=NULL,
    help="Path to css files that must be included. Use ',' as path separator for each file"),
  optparse::make_option(c("-j", "--js_files"), type="character", default=NULL,
    help="Path to javascript files that must be included. Use ',' as path separator for each file"),
 optparse::make_option(c("-C", "--css_cdn"), type="character", default=NULL,
    help="URL to css CDNs that must be included. Use ',' as path separator for each file"),
 optparse::make_option(c("-J", "--js_cnd"), type="character", default=NULL,
    help="URL to javascript CDNs that must be included. Use ',' as path separator for each file"),
 optparse::make_option(c("-m", "--menu"), type="character", default="contents_list",
    help="Indicate if indexed content must be a contents list (contents_list) or a menu (menu)")
  )

opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))
opt$source_folder <- source_folder
main_htmlreportR(opt)
