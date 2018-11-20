#!/bin/bash

rm -rf pdfs_reg/*
rm -rf thumbs_reg/*
rm -rf charts_reg/*

Rscript ./data_reg.R && Rscript ./charts_reg.R && python render_reg.py && ./thumbs_reg.sh
