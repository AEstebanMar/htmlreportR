#!/usr/bin/env bash

source init_htmlreportR
../../inst/scripts/html_report.R -d data_toc_sample.txt,hap_sample,density_ridgeline_example.txt -t template.txt
#../../inst/scripts/html_report.R -d data_toc_sample.txt -t template.txt -m -o report_menu.html
./launch_report_library.R 

