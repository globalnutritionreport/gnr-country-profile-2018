# GNR Country Profiles

### To run:

```
# Country profiles
mkdir charts
mkdir pdfs
mkdir thumbs
Rscript ./data.R && Rscript ./charts.R && python render.py && ./thumbs.sh

# Regional profiles
mkdir charts_reg
mkdir pdfs_reg
mkdir thumbs_reg
Rscript ./data_reg.R && Rscript ./charts_reg.R && python render_reg.py && ./thumbs_reg.sh
```
