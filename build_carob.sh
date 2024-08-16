#!/bin/bash -l

Rscript --vanilla -e "x <- carobiner::make_carob(path='.', cache=TRUE, split_license=TRUE, report=TRUE)" 

