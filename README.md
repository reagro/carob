# Carob

<img align="right" width="350" height="350" src="https://github.com/reagro/carob/raw/master/img/carob.png">

The aim of the *Carob project* is to create reproducible workflows that reshape primary agricultural research data from experiments and surveys into a standard format, and to aggregate individual data sets into larger collections that can be used in further research. 

We do this by writing an R script for each individual dataset. Feel free to improve these scripts, or provide new ones through a pull request (see below for more info). 

It is the process of combining data from multiple sources into a large, central repository called a data warehouse. ETL uses a set of business rules to clean and organize raw data and prepare it for storage, data analytics, and machine learning (ML).

### Get the data

Compiled versions of the dataset can be downloaded from [carob-data.org](http://carob-data.org) and some will eventually be made available on the [carob dataverse](https://dataverse.harvard.edu/dataverse/carob/).

You can also compile your own version by cloning the repo and running 

```
remotes::install_github("reagro/carobiner")
ff <- carobiner::make_carob(path)
```

where `path` is the folder of the cloned repo (e.g. `"d:/github/carob"`)

### Use

if you use the data, run `carobiner::get_citations(data)` to get references for the data set used. 

### Contribute 

Carob is the *Extract, transform, and load* (ETL) framework supported by [CGIAR](https://www.cgiar.org/initiative/excellence-in-agronomy/). Contributions are welcome from anyone, and they can be made via pull-requests. You can also raises issues on this github site. See these [preliminary guidelines](https://github.com/reagro/carob/wiki/Guidelines). A good place to discover new data sets is the [Gardian](https://gardian.bigdata.cgiar.org/) website. 

