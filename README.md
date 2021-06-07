# Carob

<img align="right" width="350" height="350" src="https://github.com/reagro/carob/raw/master/img/carob.png">

The aim of the *Carob project* is to create reproducible workflows that reshape primary agricultural research data from experiments and surveys into a standard format, and to aggregate individual data sets into larger collections that can be used in further research.

We do this by writing an R script for each individual dataset. Feel free to improve these scripts, or provide new ones through a pull request (see below for more info). 


### Get the data

Versions of the dataset will be made available on the [carob dataverse](https://dataverse.harvard.edu/dataverse/carob/).

You can also compile your own version by cloning the repo and running 

```
# install.packages(c("rjson", "readxl")
# remotes::install_github("reagro/agro")
# remotes::install_github("reagro/carobiner")
ff <- carobiner::make_carob(path)
```

where `path` is the folder of the cloned repo (e.g. `"d:/github/carob"`)

### Use

if you use the data, run `carobiner::get_citations(data)` to get references for the data set used. 

### Contribute 

Contributions are welcomed via pull-requests. See these [preliminary guidelines](https://github.com/reagro/carob/wiki/Guidelines).
