#!/usr/bin/env bash


source ~aestebanm/software/inits/init_htmlreportR
html_report.R -d ./demo_files/\*txt -t template.txt -u -m menu -o report.html 2> uncompressed_log

./launch_report_library.R 2> library_log
