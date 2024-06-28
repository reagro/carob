#!/bin/bash -l

module load R
Rscript --vanilla -e "x <- carobiner::make_carob(path='.', cache=TRUE)" 
