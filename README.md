# Carob

The aim of the *Carob project* is to create reproducible workflows to aggregate agronomic from experiments and surveys. These aggregated data sets can then by used for analysis.

Each individual dataset is associated with a single R script. Feel free to improve these scripts, or provide new ones through a pull request. 

## Guidelines

### Contributions 

Please contribute R scripts for additional datasets. More guidelines are forthcoming, but you can start by replicating an existing file (in the scripts folder). 

All data should be downloaded from a URI. Data that does not have a URI should be hosted somewhere. We can  host it on the [carob dataverse](https://dataverse.harvard.edu/dataverse/carob/).

The script file should be `<uri>.R`  where <`uri`> is a normalized URI (see example scripts). 
You should use standard variable names if available, and express all data in standard units. See the `.csv` files in the `terms` folder. 

Style: use base R, and a as few packages as reasonably possible. Avoid the tidyverse dialect. Do not use `%>%`. You can use `|>` but only sparingly, never use more than 2 in one statement.


### Get the data

Versions of the dataset will be made available on the [carob dataverse](https://dataverse.harvard.edu/dataverse/carob/).

Create your own by cloning the repo and running 

```
# install.packages(c("rjson", "readxl")
# remotes::install_github("reagro/agro")
# remotes::install_github("reagro/carobiner")
carobiner::make_carob(path)
```

where `path` is the folder of the cloned repo (e.g. `"d:/github/carob"`)

### Use

if you use the data, run `carobiner::get_citations(data)` to get references for the data set used. 

