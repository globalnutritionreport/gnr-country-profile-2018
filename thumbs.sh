#!/bin/bash

for i in ./pdfs/*.pdf; do
  file=${i##*[/|\\]}
  base=${file%.*}
  convert $i[0] -flatten ./thumbs/$base.png
done;
