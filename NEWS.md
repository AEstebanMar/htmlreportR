# htmlreportR 1.0.0

* Initial CRAN submission.

# htmlreportR 1.0.1

* New: Added prettify\_div method to htmlreportR class. Simplifies div style customisation, and comes with some pretty presets.
* New: Added boxplot method to htmlreportR class.
* New: Added reshape method to canvasXpress\_obj class. Reshapes canvas data structure to adapt to long and wide formats.
* New: Added script to convert Rmd templates to html, making them compatible with htmlreportR. Relies on a new function (replace\_paired\_mark) under utils.R
* Fix: Revamped line method in htmlreportR class, did not work as intended. If no additional parameters are provided, it will work as it does originally in CanvasXpress. Now admits smp\_attr
  and xAxis parameter in config. If there is at least one smp\_attr, series specified in xAxis will be used for X axis.

