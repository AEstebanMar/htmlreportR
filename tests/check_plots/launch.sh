#!/usr/bin/env bash

source init_htmlreportR
../../inst/scripts/html_report.R -d data_toc_sample.txt,hap_sample,density_ridgeline_example.txt,x_y.txt,x_y_factor.txt,sparse_matrix,barplot1.txt,barplot2.txt,barplot3.txt,barplot4.txt -t template.txt -u -m menu
../../inst/scripts/html_report.R -d data_toc_sample.txt,hap_sample,density_ridgeline_example.txt,x_y.txt,x_y_factor.txt,sparse_matrix,barplot1.txt,barplot2.txt,barplot3.txt,barplot4.txt -t template.txt -o report_compressed.html
./launch_report_library.R 

