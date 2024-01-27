
fix_date <- function(x) {
	x <- gsub(", ", "-", x)
	x <- gsub(" ", "-", x)
	x <- gsub("/", "-", x)
	for (y in 16:24) {
		x <- gsub(paste0("-", y, "$"), paste0("-20", y), x)
	}
	
	month.num <- paste0("-", formatC(1:12, width=2, flag = "0"), "-")
	for (i in 1:12) {
		x <- gsub(paste0("-", month.abb[i], "-"), month.num[i], x)
	}

	dat <- rep(as.Date(NA), length(x))
	i <- grepl("-", x)
	dat[!i] <- as.Date("1899-12-31") + as.numeric(x[!i])
	dat[i] <- as.Date(x[i], "%d-%m-%Y")
	as.character(dat)
}
