#!/bin/bash

rm -rf pdfs/*
rm -rf thumbs/*
rm -rf charts/*

Rscript ./data.R && Rscript ./charts.R && python render.py && ./thumbs.sh
