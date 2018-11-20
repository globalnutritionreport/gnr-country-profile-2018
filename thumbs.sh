#!/bin/bash

for i in ./pdfs/*.pdf; do
  file=${i##*[/|\\]}
  base=${file%.*}
  convert $i[0] -flatten -density 300 -quality 90 -crop 892x631  ./thumbs/$base.jpg
done;
