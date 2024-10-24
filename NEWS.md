# htmlreportR 1.0.0

* Initial CRAN submission.

# htmlreportR 1.0.1

* New: Added prettify\_div method to htmlreportR class. Simplifies div style customisation, and comes with some pretty presets.
* New: Added boxplot method to htmlreportR class.
* New: Added reshape method to canvasXpress\_obj class. Reshapes canvas data structure to adapt to long and wide formats.
* New: Added script to convert Rmd templates to html, making them compatible with htmlreportR. Relies on a new function (replace\_paired\_mark) under utils.R
* New: Overhauled whole basic structure, now more closely matches python version. Usage has changed slightly, now rows and columns meant to be names now should also be counted when specifying indices in templates, as they are now treated as regular fields and rows up until drawing the plot. Allowed implementation of the following feature.
* New: "fields" and "rows" arguments may now be added to calls of plotting methods in templates. They allow to reorder and subset data before plotting without altering the original data.
