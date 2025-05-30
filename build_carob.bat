@ECHO OFF

IF %1.==. (GOTO cache) else (GOTO nocache)

:cache
Rscript.exe --vanilla -e "x <- carobiner::make_carob(path='.', cache=TRUE, split_license=TRUE, report=TRUE, combine=FALSE)" 
GOTO done

:nocache
Rscript.exe --vanilla -e "x <- carobiner::make_carob(path='.', cache=FALSE, split_license=TRUE, report=TRUE, combine=FALSE)"
GOTO done

:done
	