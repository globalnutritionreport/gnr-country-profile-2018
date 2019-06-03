# GNR Country Profiles

### Set up:

1. Download and install a version of R for your operating system, ensure it's in your PATH: https://cran.r-project.org/ You should have the ability to run `R` and `Rscript rfile.R` from the command line.
2. (Optional for windows) Download and install the correct Rtools version: https://cran.r-project.org/bin/windows/Rtools/
3. (Optional for MacOS) `brew install imagemagick` (for thumbs.sh, should give you `convert` in the command line)
4. `python3 -m virtualenv venv`
5. `source venv/bin/activate`
6. `pip install -r requirements.txt`
7. (If starting from scratch)`mkdir charts pdfs thumbs charts_reg pdfs_reg thumbs_reg charts_world pdfs_world thumbs_reg`

### To run:

```
# Country profiles
./rerender_countries.sh

# Regional profiles
./rerender_regions.sh

# World profile
,/rerender_world.sh
```

# Short explanation

For time, data processing, chart making, and PDF generation were largely copied and pasted between countries, regions, and the global profile. So you can look at a single process to start.

The command `rerender_countries.sh` first runs the data amalgamation script `data.R`. This takes the data contained in `Dataset working directory` as an input, combines it, and outputs `data.csv`. Next, the chart creation script takes `data.csv` as an input, and outputs country-name labeled folders containing numbered PNG chart files in the `charts` directory (primarily via the `ggplot2` R package). Third, `render.py` is run, which takes the `2018_template/template.xml.j2` Jinja template, substitutes in country names where applicable, and passes the resultant XML to a custom PDF generation script built off of the Python library `reportlab`. Within `render.py`, `tables.py` is imported to create the data-driven tables. `tables.py` is also built off of `reportlab`, but the process of creating table templates and swapping out the data with what's contained in `data.csv` was such a large process I wanted to separate it. If all goes well, you should have 194 country profile PDFs within the `pdfs` directory.
