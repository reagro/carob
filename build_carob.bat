@ECHO OFF

IF %1.==. (GOTO cache) else (GOTO nocache)

:cache
Rscript.exe --vanilla -e "x <- carobiner::make_carob(path='.', cache=TRUE, report=TRUE)" 
GOTO done

:nocache
Rscript.exe --vanilla -e "x <- carobiner::make_carob(path='.', cache=FALSE, report=TRUE)"
GOTO done

:done
	