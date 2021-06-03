# Carob

The aim of the *Carob project* is to create reproducible workflows to aggregate agronomic from experiments and surveys. These aggregated data sets can then by used for analysis.

Each individual dataset is associated with a single R script. Feel free to improve these scripts, or provide new ones through a pull request. 


### Get the data

Versions of the dataset will be made available on the [carob dataverse](https://dataverse.harvard.edu/dataverse/carob/).

Create your own by cloning the repo and running 

```
# install.packages(c("rjson", "readxl")
# remotes::install_github("reagro/agro")
# remotes::install_github("reagro/carobiner")
ff <- carobiner::make_carob(path)
```

where `path` is the folder of the cloned repo (e.g. `"d:/github/carob"`)

### Use

if you use the data, run `carobiner::get_citations(data)` to get references for the data set used. 

### Contributions 

Contributions are welcomed via pull-requests. See these [preliminary guidelines](https://github.com/reagro/carob/wiki).

