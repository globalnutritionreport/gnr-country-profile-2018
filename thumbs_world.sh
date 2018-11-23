#!/bin/bash

for i in ./pdfs_world/*.pdf; do
  file=${i##*[/|\\]}
  base=${file%.*}
  convert $i[0] -flatten -density 300 -quality 90 -crop 892x631  ./thumbs_world/$base.jpg
done;
