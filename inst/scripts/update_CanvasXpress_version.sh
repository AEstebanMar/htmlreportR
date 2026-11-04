#! /usr/bin/env bash


## Run this to update CvX version in htmlreportR. It is up here
## because this should _never_ be distributed with the package.

source ~soft_bio_267/initializes/init_R

rm ../js/canvasXpress.css
rm ../js/canvasXpress.min.js.gz

wget https://www.canvasxpress.org/dist/canvasXpress.css -O ../js/canvasXpress.css
wget https://www.canvasxpress.org/dist/canvasXpress.min.js -O ../js/canvasXpress.min.js
compress_file.R -i ../js/canvasXpress.min.js
rm ../js/canvasXpress.min.js
git add ../js/canvasXpress.min.js.gz
git add ../js/canvasXpress.css
git commit -m 'Updated CanvasXpress version'
git push origin devel

