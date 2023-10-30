
carob_script <- function(path) {

"
	Description:
	The genotype x environment interaction (GXE) does see the importance of environmental effect on the adaptation and varietal performance. Therefore, breeders must have a methodology for quantifying and interpreting the GXE interaction and thus help clarify areas where a genotype can be useful.
	New statistical methods allow the identification and recommendation of new clones with specific or broad adaptation. The combination of Geographic Information System (GIS) with an analysis of variance of AMMI models (Additive Main Effects and Multiplicative Interactions), SREG (Sites Regression Model) and PLS (Partial Least Squares Regression) offer a new possibility to predict potential areas for production of these materials. Regional yield trials are networks of experiments by which a set of cultivars is usually assessed to make genotype recommendation.
	
"

uri <-  "doi:10.21223/P3/UTZBYL"
dataset_id <- carobiner::simple_uri(uri)
group <- "lateblight"
## dataset level data 
dset <- data.frame(
   dataset_id = dataset_id,
   group=group,
   uri=uri,
   publication= NA,# 
   data_citation ="Salas, Elisa; Juarez, Henry; Giraldo, Diana; Amoros, Walter; Simon, Reinhard; Bonierbale, Merideth, 2017, Dataset for: Models of analysis stability and definition of environments with GIS, 
   https://doi.org/10.21223/P3/UTZBYL, International Potato Center, V1",
   data_institutions = "IITA",
   carob_contributor="Cedric Ngakou",
   data_type="experiment",
   project=NA,
   carob_date="2023-10-30"
)

## download and read data 
ff <- carobiner::get_data(uri, path, group)
js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
dset$license <- carobiner::get_license(js)

bn <- basename(ff)

# read and process files
lst <- list()
for (i in c(4:9)){
   
   name<- c("REP","INSTN","MTYNA")
   
   Newname<-c("rep","variety","yield")
      r <- readxl::read_excel(ff[bn==bn[i]],sheet=9) |> as.data.frame()
      ri<- r[,name]
      colnames(ri) <- Newname
      ri$k<- i
      lst[[i]] <- ri
}
# append all the data
d <- do.call(rbind, lst)

d$yield<- d$yield*1000 ## in kg/ha
d$yield_part<- "tubers" 
d$crop <- "potato"
#d$pathogen <- NA
## add columns
d$dataset_id <- dataset_id
d$country<- "Peru"

d$adm1<- ifelse(d$k==4,"Lima",
                ifelse(d$k==5,"Junin",
                       ifelse(d$k==6,"Ayacucho",
                              ifelse(d$k==7,"Junin",
                                     ifelse(d$k==8,"Junin","Lima"))))) 
d$adm2<- ifelse(d$k==4,"Lima",
                  ifelse(d$k==5,"Chanchamayo",
                         ifelse(d$k==6,"Huamanga",
                                ifelse(d$k==7,"Huancayo",
                                       ifelse(d$k==8,"Chanchamayo","Lima"))))) 
d$adm3<- ifelse(d$k==4,"La Molina",
                  ifelse(d$k==5,"San Ramon",
                         ifelse(d$k==6,"Chiara",
                                ifelse(d$k==7,"San agustin",
                                       ifelse(d$k==8,"San Ramon","La Molina"))))) 
                                          

d$trial_id <- paste(d$adm1,d$dataset_id,sep = "-")

date<- data.frame(k=c(4,5,6,7,8,9),
                  harvest_date=c("2002-05-01","2002-08-01","2002-11-01","2002-11-01","2003-08-01","2003-09-01"),
                  planting_date=c("2002-08-01","2002-10-01","2003-02-10","2003-01-01","2003-10-01","2003-12-10"))
d<- merge(d,date,by="k")
d$irrigated <- FALSE
d$inoculated <- FALSE
d$is_survey <- FALSE
d$on_farm <- TRUE
### add lon and lat coordinate
geo<- data.frame(adm3=c("La Molina","San Ramon","Chiara","San agustin"),
                 lon=c(-76.948417,-75.356389,-74.206,-75.2449),
                 lat=c(-12.076289,-11.1275,-13.2734,-12.0264))
                 

d<- merge(d,geo,by="adm3")

d$longitude<- d$lon
d$latitude<-  d$lat
d$lon<- d$lat <- d$k <- NULL

##data type
d$rep<- as.integer(d$rep)

# all scripts must end like this
carobiner::write_files(dset, d, path=path)

}