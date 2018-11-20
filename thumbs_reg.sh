#!/bin/bash

for i in ./pdfs_reg/*.pdf; do
  file=${i##*[/|\\]}
  base=${file%.*}
  convert $i[0] -flatten -density 300 -quality 90 -crop 892x631  ./thumbs_reg/$base.jpg
done;
