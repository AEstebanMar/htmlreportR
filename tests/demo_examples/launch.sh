#!/usr/bin/env bash

source init_htmlreportR
../../inst/scripts/html_report.R -d ./demo_files/\*txt -t template.txt -u -m menu 2> uncompressed_log
../../inst/scripts/html_report.R -d ./demo_files/\*txt -t template.txt -o report_compressed.html 2> compressed_log
./launch_report_library.R 2> library_log
