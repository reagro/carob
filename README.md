# Carob

<img align="right" width="350" height="350" src="https://github.com/reagro/carob/raw/master/img/carob.png">

*Carob* creates reproducible workflows that standardize primary agricultural research data from experiments and surveys. Standardization includes the use of a common file format, variable names, units and accepted values according to the [terminag](https://github.com/reagro/terminag) standard. Standardized data sets are aggregated into larger collections that can be used in further research. We do this by writing an *R* script for each individual dataset. See the [website](https://carob-data.org) for more information.

Carob is an open access *Extract, Transform, and Load* (ETL) framework supported by [CGIAR](https://www.cgiar.org/initiative/excellence-in-agronomy/) to support predictive analytics (machine learning, artifical intelligence) and other types of data analysis. 

Contributions are welcome from anyone, and they can be made via pull-requests. Feel free to improve these scripts, or provide new ones. See the [Guidelines for contributors] for instructions on how to write a Carob script, and follow the steps described [here](https://carob-data.org/contribute.html). You can also raise [issues](https://github.com/reagro/carob/issues) on this github site. A good place to discover new data sets is the [Gardian](https://gardian.bigdata.cgiar.org/) website or our [to-do list](https://carob-data.org/todo.html). 

### Get the data

Compiled versions of the dataset can be downloaded from [carob-data.org](http://carob-data.org) and some will eventually be made available on the [carob dataverse](https://dataverse.harvard.edu/dataverse/carob/).

You can also compile your own version by cloning the repo and running 

```
remotes::install_github("reagro/carobiner")
ff <- carobiner::make_carob(path)
```

where `path` is the folder of the cloned repo (e.g. `"d:/github/carob"`)

### Use

if you use the aggregated data, you can run `carobiner::get_citations(data)` to get references (citations) to the orginal data sets used. 

