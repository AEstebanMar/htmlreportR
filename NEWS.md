# htmlreportR 1.0.0

* Initial CRAN submission.

# htmlreportR 1.0.1

* New: Added prettify\_div method to htmlreportR class. Simplifies div style customisation, and comes with some pretty presets.
* New: Added boxplot method to htmlreportR class.
* New: Added reshape method to canvasXpress\_obj class. Reshapes canvas data structure to adapt to long and wide formats.
* New: Added script to convert Rmd templates to html, making them compatible with htmlreportR. Relies on a new function (replace\_paired\_mark) under utils.R

# htmlreportR 2.0.0

* New: Overhauled whole basic structure, now more closely matches python version. Usage has changed slightly, now rows and columns meant to be names now should also be counted when specifying indices in templates, as they are now treated as regular fields and rows up until drawing the plot. Allowed implementation of the following feature.
* New: "fields" and "rows" arguments may now be added to calls of plotting methods in templates. They allow to reorder and subset data before plotting without altering the original data.
  * Change: setting header to TRUE removes the first row specified by "rows" from the actual data frame, setting it as its name. Same goes for row\_names and the first column specified by "fields". You might need to update var\_attr and smp\_attr in your templates after updating if any of these values is set to TRUE.
* Update: "table" method now behaves like the rest, taking options list as argument instead. You will need to update old calls to this method (simply wrap arguments in a list).
* Update: "extract\_data" method no longer takes data frame as input, accesses it through hash\_vars (reads ID from options list). This method is not public, so it should not have any repercussions in regular usage.
* Update: added make\_html\_list to general functions. Takes three vectors: contents, levels (optional) and types (optional) and creates an html list with them.
* Fix: plotting methods can now handle a smp\_attr field that is encoded in data frame as a factor
* New: heatmap method can now handle two data frames and represent them in the same heatmap (second data frame must be an additional factor of the first, think of it as adding a third dimension to data). Will be represented by point sizes.
* New: added scatter3D method.
* New: added merge\_hashed\_tables method, takes a list of ids and some optional arguments and merges hash\_vars tables corresponding to those ids.
* Update: table method now admits rownames\_col argument in option list, renames rownames column.
