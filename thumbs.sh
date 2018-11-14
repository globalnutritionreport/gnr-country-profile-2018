#!/bin/bash

for i in ./pdfs/*.pdf; do
  file=${i##*[/|\\]}
  base=${file%.*}
  convert $i[0] -flatten -resize 450x800 -density 300 -quality 90 ./thumbs/$base.jpg
done;
