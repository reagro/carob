@ECHO OFF

rem Rscript.exe --vanilla -e "x <- carobiner::make_carob(path='.', cache=TRUE)"

Rscript.exe --vanilla -e "x <- carobiner::make_carob(path='.', cache=FALSE)"
