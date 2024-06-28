@ECHO OFF

IF %1.==. (GOTO cache) else (GOTO nocache)

:cache
Rscript.exe --vanilla -e "x <- carobiner::make_carob(path='.', cache=TRUE)" 
GOTO done

:nocache
Rscript.exe --vanilla -e "x <- carobiner::make_carob(path='.', cache=FALSE)"
GOTO done

:done
	