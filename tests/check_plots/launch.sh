#!/usr/bin/env bash

source init_htmlreportR
../../inst/scripts/html_report.R -d data_toc_sample.txt,hap_sample,density_ridgeline_example.txt,x_y.txt,sparse_matrix -t template.txt -u
../../inst/scripts/html_report.R -d data_toc_sample.txt,hap_sample,density_ridgeline_example.txt,x_y.txt,sparse_matrix -t template.txt  -o report_compressed.html
./launch_report_library.R 

