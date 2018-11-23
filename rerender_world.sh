#!/bin/bash

rm -rf pdfs_world/*
rm -rf thumbs_world/*
rm -rf charts_world/*

Rscript ./data_world.R && Rscript ./charts_world.R && python3 render_world.py && ./thumbs_world.sh
