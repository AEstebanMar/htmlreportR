#!/usr/bin/env bash

source init_htmlreportR
../../inst/scripts/html_report.R -d ./demo_files/\*txt -t template.txt -u -m menu
../../inst/scripts/html_report.R -d ./demo_files/\*txt -t template.txt -o report_compressed.html
./launch_report_library.R 
