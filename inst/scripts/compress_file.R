#! /usr/bin/env Rscript


option_list <- list(
  optparse::make_option(c("-i", "--input"), type="character", default=NULL,
    help="Path to file to compress.")
  )

opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))

size <- file.info(opt$input)$size
file <- readBin(opt$input, "raw", size)
comp <- memCompress(file, "gzip")
writeBin(comp, paste0(opt$input, ".gz"))
