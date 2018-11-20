#!/bin/bash

rm -rf pdfs/*
rm -rf thumbs/*
rm -rf charts/*

Rscript ./data.R && Rscript ./charts.R && python3 render.py && ./thumbs.sh
