
# border tbd
path<-"D:/OneDrive - CGIAR/EiA/siycarob"
carob_script <- function(path) {
"Description:
Yield gains and associated changes in an early yellow bi-parental maize population following Genomic Selection for Striga resistance and drought tolerance."
				
	uri <- "doi.org/10.25502/szwf-he08"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri = uri,
		data_citation="Badu-Apraku Baffour, R. Asiedu, A.O. Talabi, M.A.B. Fakorede, Y. Fasanmade, M. Gedil, & C. Magorokosho. (2018). Yield gains and associated changes in an early yellow bi-parental maize population following Genomic Selection for Striga resistance and drought tolerance [dataset]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/SZWF-HE08 ",
 	    # publication="doi.org/10.1186/s12870-019-1740-z",
		publication=NA,
		carob_contributor = "Siyabusa Mkuhlani",
		carob_date="2024-17-01",
		data_type = "experiment",
		project="CGIAR Research Program on Maize",
		data_institutions="IITA"
	)

	## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
	dset$license <- carobiner::get_license(js)
	
	#data set1 EARLY Drought.csv"
	f <- ff[basename(ff) == "EARLY Drought.csv"]
	d0 <- read.csv(f)
	d0<-as.data.frame(d0)
	
	d0$country <- "Nigeria"
	d0$adm1[d0$LOC=='IKDS']<-"Ogun"
	d0$adm2[d0$adm1=='Ogun']<-"Ikenne"
	d0$longitude[d0$adm2=='Ikenne']<-3.711
	d0$latitude[d0$adm2=='Ikenne']<-6.872
	d0$dataset_id <- dataset_id
	d0$trial_id<-paste0('genomic_str_Nig_Ogun',d0[1,3])
	d0$planting_date<-paste(d0[1,2],"04","01",sep="-")
	d0$flowering<-d0['DYSK']+d0['ASI']
	d0<-d0[,c(2,6,8,10,11,12,13,14,15,16,17,18,19,20,21,24,25,26,27,28,29,30,31,32)]
	d0$yield_part<-'grain'
	d0$fertilizer_type <- NA
	d0$fertilizer_type<-as.character(d0$fertilizer_type)
	d0$N_fertilizer<-120
	d0$P_fertilizer<-60
	d0$K_fertilizer<-60
	d0$adm3<-NA
	d0$adm3<-as.character(d0$adm3)
	d0$pl_st<-NA
	d0$striga_trial<-TRUE
	d0$striga_infected<-TRUE
	d0$borer_trial<-FALSE
	d0$crop<-'maize'
names(d0)<-c("date", "treatment","rep", "yield","polshed","dy_sk","asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp","e_harv","erot","country","adm1",
            "adm2","longitude","latitude","dataset_id","trial_id","planting_date","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","adm3","pl_st","striga_trial","striga_infected","borer_trial","crop" )
d0$rlper<-round(d0$rlper*100,digits=2)
d0$slper<-round(d0$slper*100,digits=2)
as.character(d0$date)
d0<-d0[,c("date", "treatment","rep", "yield","polshed","dy_sk","asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp","e_harv","erot","country","adm1",
     "adm2","adm3","longitude","latitude","dataset_id","trial_id","planting_date","pl_st","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","striga_trial","striga_infected","borer_trial","crop")]	
as.numeric(d0$yield)
as.numeric(d0$polshed)
as.numeric(d0$dy_sk)
as.numeric(d0$dy_sk)
as.numeric(d0$asi)
as.numeric(d0$pl_ht)
as.numeric(d0$e_ht)
as.numeric(d0$rlper)
as.numeric(d0$slper)
as.numeric(d0$husk)
as.numeric(d0$p_asp)
as.numeric(d0$e_harv)
as.numeric(d0$erot)
d0<-d0[,-1]
d0$anthesis<-as.numeric(unlist(d0[,25]))






#data set2 Early Drought+Heat.csv
f1 <- ff[basename(ff) == "Early Drought+Heat.csv"]
d1 <- read.csv(f1)

d1$country <- "Nigeria"
d1$adm1[d1$LOC=='KDWHS']<-"Kano"
d1$adm2[d1$adm1=='Kano']<-"Garum Mallam"
d1$adm3[d1$adm2=='Garum Mallam']<-"Kadawa"
d1$adm3<-as.character(d1$adm3)
d1$longitude[d1$adm3=='Kadawa']<-8.448
d1$latitude[d1$adm3=='Kadawa']<-11.645
d1$dataset_id <- dataset_id
d1$trial_id<-paste0('genomic_str_Nig_Kadawa',d1[1,3])
d1$planting_date<-paste(d1[1,2],"04","01",sep="-")
d1$flowering<-d1['DYSK']+d1['ASI']
d1<-d1[,c(2,6,7,10,11,12,13,14,15,16,17,18,19,20,21,26,27,28,29,30,31,32,33,34,35)]
d1$yield_part<-'grain'
d1$fertilizer_type <- NA
d1$fertilizer_type<-as.character(d1$fertilizer_type)
d1$N_fertilizer<-120
d1$P_fertilizer<-60
d1$K_fertilizer<-60
d1$pl_st<-NA
d1$striga_trial<-TRUE
d1$striga_infected<-TRUE
d1$borer_trial<-FALSE
d1$crop<-'maize'

names(d1)<-c("date", "treatment","rep", "yield","polshed","dy_sk","asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp","e_harv","erot","country","adm1",
            "adm2","adm3","longitude","latitude","dataset_id","trial_id","planting_date","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","pl_st","striga_trial","striga_infected","borer_trial","crop")

d1$rlper<-round(d1$rlper*100,digits=2)
d1$slper<-round(d1$slper*100,digits=2)
as.character(d0$date)

d1<-d1[,c("date", "treatment","rep", "yield","polshed","dy_sk","asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp","e_harv","erot","country","adm1",
        "adm2","adm3","longitude","latitude","dataset_id","trial_id","planting_date","pl_st","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","striga_trial","striga_infected","borer_trial","crop")]	

as.numeric(d1$yield)
as.numeric(d1$polshed)
as.numeric(d1$dy_sk)
as.numeric(d1$dy_sk)
as.numeric(d1$asi)
as.numeric(d1$pl_ht)
as.numeric(d1$e_ht)
as.numeric(d1$rlper)
as.numeric(d1$slper)
as.numeric(d1$husk)
as.numeric(d1$p_asp)
as.numeric(d1$e_harv)
as.numeric(d1$erot)

d1<-d1[,-1]
as.character(d1$planting_date)
d1$anthesis<-as.numeric(unlist(d1[,25]))






#data set3 Early Heat.csv
f2 <- ff[basename(ff) == "Early Heat.csv"]
d2 <- read.csv(f2)

d2$country <- "Nigeria"
d2$adm1[d2$LOC=='KADAWA']<-"Kano"
d2$adm2[d2$adm1=='Kano']<-"Garum Mallam"
d2$adm3[d2$adm2=='Garum Mallam']<-"Kadawa"
d2$adm3<-as.character(d2$adm3)
d2$longitude[d2$adm3=='Kadawa']<-8.448
d2$latitude[d2$adm3=='Kadawa']<-11.645
d2$dataset_id <- dataset_id
d2$trial_id<-paste0('genomic_str_Nig_Kadawa',d2[1,3])
d2$planting_date<-paste(d2[1,2],"04","01",sep="-")
d2$flowering<-d2['DYSK']+d2['ASI']
d2<-d2[,c(2,6,7,10,11,12,13,14,15,16,17,18,19,20,21,26,27,28,29,30,31,32,33,34,35)]
d2$yield_part<-'grain'
d2$fertilizer_type <- NA
d2$fertilizer_type<-as.character(d2$fertilizer_type)
d2$N_fertilizer<-120
d2$P_fertilizer<-60
d2$K_fertilizer<-60
d2$pl_st<-NA
d2$striga_trial<-TRUE
d2$striga_infected<-TRUE
d2$borer_trial<-FALSE
d2$crop<-'maize'

names(d2)<-c("date","treatment","rep", "yield","polshed","dy_sk","asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp","e_harv","erot","country","adm1",
             "adm2","adm3","longitude","latitude","dataset_id","trial_id","planting_date","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","pl_st","striga_trial","striga_infected","borer_trial","crop")

d2$rlper<-round(d2$rlper*100,digits=2)
d2$slper<-round(d2$slper*100,digits=2)

d2<-d2[,c("date", "treatment","rep", "yield","polshed","dy_sk","asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp","e_harv","erot","country","adm1",
          "adm2","adm3","longitude","latitude","dataset_id","trial_id","planting_date","pl_st","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","striga_trial","striga_infected","borer_trial","crop")]	

as.numeric(d2$yield)
as.numeric(d2$polshed)
as.numeric(d2$dy_sk)
as.numeric(d2$dy_sk)
as.numeric(d2$asi)
as.numeric(d2$pl_ht)
as.numeric(d2$pl_st)
as.numeric(d2$e_ht)
as.numeric(d2$rlper)
as.numeric(d2$slper)
as.numeric(d2$husk)
as.numeric(d2$p_asp)
as.numeric(d2$e_harv)
as.numeric(d2$erot)
as.character(d2$planting_date)
d2<-d2[,-1]
d2$anthesis<-as.numeric(unlist(d2[,25]))
d2$treatment[d2$treatment=="Check 3 - 2015 TZE-Y DT STR Syn C0"]<-'Check_3a'
d2$treatment[d2$treatment=="Check 1 - 2011 TZE-W DT STR Synthetic"]<-'Check_1'
d2$treatment[d2$treatment=="Check 3 - 2015 TZE \x96Y DT STR Syn C0"]<-'Check_3b'




#data set4 #Extra-Early Drought.csv
f3 <- ff[basename(ff) == "Extra-Early Drought.csv"]
d3 <- read.csv(f3)

d3$country <- "Nigeria"
d3$adm1[d3$LOC=='IKDS']<-"Ogun"
d3$adm2[d3$adm1=='Ogun']<-"Ikenne"
d3$longitude[d3$adm2=='Ikenne']<-3.711
d3$latitude[d3$adm2=='Ikenne']<-6.872
d3$dataset_id <- dataset_id
d3$trial_id<-paste0('genomic_str_Nig_Ogun',d3[1,3])
d3$planting_date<-paste(d3[1,2],"04","01",sep="-")
d3$flowering<-d3['DYSK']+d3['ASI']
d3<-d3[,c(2,6,7,9,10,11,12,13,14,15,16,17,18,19,20,23,24,25,26,27,28,29,30,31)]
d3$yield_part<-'grain'
d3$fertilizer_type <- NA
d3$fertilizer_type<-as.character(d3$fertilizer_type)
d3$N_fertilizer<-120
d3$P_fertilizer<-60
d3$K_fertilizer<-60
d3$adm3<-NA
d3$adm3<-as.character(d3$adm3)
d3$pl_st<-NA
d3$striga_trial<-TRUE
d3$striga_infected<-TRUE
d3$borer_trial<-FALSE
d3$crop<-'maize'

names(d3)<-c("date", "treatment","rep", "yield","polshed","dy_sk","asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp","e_harv","erot","country","adm1",
             "adm2","longitude","latitude","dataset_id","trial_id","planting_date","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","adm3","pl_st","striga_trial","striga_infected","borer_trial","crop")

d3$rlper<-round(d3$rlper*100,digits=2)
d3$slper<-round(d3$slper*100,digits=2)

d3<-d3[,c("date", "treatment","rep", "yield","polshed","dy_sk","asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp","e_harv","erot","country","adm1",
          "adm2","adm3","longitude","latitude","dataset_id","trial_id","planting_date","pl_st","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","striga_trial","striga_infected","borer_trial","crop")]	

as.numeric(d3$yield)
as.numeric(d3$polshed)
as.numeric(d3$dy_sk)
as.numeric(d3$dy_sk)
as.numeric(d3$asi)
as.numeric(d3$pl_ht)
as.numeric(d3$e_ht)
as.numeric(d3$rlper)
as.numeric(d3$slper)
as.numeric(d3$husk)
as.numeric(d3$p_asp)
as.numeric(d3$e_harv)
as.numeric(d3$erot)
as.character(d3$planting_date)
d3<-d3[,-1]

d3$anthesis<-as.numeric(unlist(d3[,25]))









#data set 5 #Extra-Early Drought+Heat.csv
f4 <- ff[basename(ff) == "Extra-Early Drought+Heat.csv"]
d4 <- read.csv(f4)

d4$country <- "Nigeria"
d4$adm1[d4$LOC=='KADAWA']<-"Kano"
d4$adm2[d4$adm1=='Kano']<-"Garum Mallam"
d4$adm3[d4$adm2=='Garum Mallam']<-"Kadawa"
d4$adm3<-as.character(d4$adm3)
d4$longitude[d4$adm3=='Kadawa']<-8.448
d4$latitude[d4$adm3=='Kadawa']<-11.645
d4$dataset_id <- dataset_id

d4$trial_id<-paste0('genomic_str_Nig_Kadawa',d4[1,3])
d4$planting_date<-paste(d4[1,2],"04","01",sep="-")
d4$flowering<-d4['DYSK']+d4['ASI']
d4<-d4[,c(2,7,8,10,11,12,13,14,15,16,17,18,19,20,21,26,27,28,29,30,31,32,33,34,35)]

d4$yield_part<-'grain'
d4$fertilizer_type <- NA
d4$fertilizer_type<-as.character(d4$fertilizer_type)
d4$N_fertilizer<-120
d4$P_fertilizer<-60
d4$K_fertilizer<-60
d4$pl_st<-NA
d4$striga_trial<-TRUE
d4$striga_infected<-TRUE
d4$borer_trial<-FALSE
d4$crop<-'maize'

names(d4)<-c("date","treatment","rep", "yield","polshed","dy_sk",
             "asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp",
             "e_harv","erot","country","adm1", "adm2","adm3","longitude",
             "latitude","dataset_id","trial_id","planting_date","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","pl_st","striga_trial","striga_infected","borer_trial","crop")

d4$rlper<-round(d4$rlper*100,digits=2)
d4$slper<-round(d4$slper*100,digits=2)
as.character(d0$date)

d4<-d4[,c("date","treatment","rep", "yield","polshed","dy_sk","asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp","e_harv","erot","country","adm1",
          "adm2","adm3","longitude","latitude","dataset_id","trial_id","planting_date","pl_st","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","striga_trial","striga_infected","borer_trial","crop")]	

as.numeric(d4$yield)
as.numeric(d4$polshed)
as.numeric(d4$dy_sk)
as.numeric(d4$dy_sk)
as.numeric(d4$asi)
as.numeric(d4$pl_ht)
as.numeric(d4$e_ht)
as.numeric(d4$rlper)
as.numeric(d4$slper)
as.numeric(d4$husk)
as.numeric(d4$p_asp)
as.numeric(d4$e_harv)
as.numeric(d4$erot)
as.character(d4$planting_date)
d4<-d4[,-1]

d4$anthesis<-as.numeric(unlist(d4[,25]))









#data set 5 #Extra-Early Heat.csv
f5 <- ff[basename(ff) == "Extra-Early Heat.csv"]
d5 <- read.csv(f5)

d5$country <- "Nigeria"
d5$adm1[d5$LOC=='KADAWA']<-"Kano"
d5$adm2[d5$adm1=='Kano']<-"Garum Mallam"
d5$adm3[d5$adm2=='Garum Mallam']<-"Kadawa"
d5$adm3<-as.character(d5$adm3)
d5$longitude[d5$adm3=='Kadawa']<-8.448
d5$latitude[d5$adm3=='Kadawa']<-11.645
d5$dataset_id <- dataset_id
d5$trial_id<-paste0('genomic_str_Nig_Kadawa',d5[1,3])
d5$planting_date<-paste(d5[1,2],"04","01",sep="-")
d5$flowering<-d5['DYSK']+d5['ASI']
d5<-d5[,c(2,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,26,27,28,29,30,31,32,33,34,35)]
d5$yield_part<-'grain'
d5$fertilizer_type <- NA
d5$fertilizer_type<-as.character(d5$fertilizer_type)
d5$N_fertilizer<-120
d5$P_fertilizer<-60
d5$K_fertilizer<-60
d5$pl_st<-NA
d5$striga_trial<-TRUE
d5$striga_infected<-TRUE
d5$borer_trial<-FALSE
d5$crop<-'maize'

names(d5)<-c("date", "rep","treatment", "yield","pl_st","polshed","dy_sk","asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp","e_harv","erot","country","adm1",
             "adm2","adm3","longitude","latitude","dataset_id","trial_id","planting_date","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","pl_st","striga_trial","striga_infected","borer_trial","crop")

d5$rlper<-round(d5$rlper*100,digits=2)
d5$slper<-round(d5$slper*100,digits=2)
as.character(d0$date)

d5<-d5[,c("date", "treatment","rep", "yield","polshed","dy_sk","asi","pl_ht", "e_ht" , "rlper", "slper", "husk","p_asp","e_harv","erot","country","adm1",
          "adm2","adm3","longitude","latitude","dataset_id","trial_id","planting_date","pl_st","anthesis","yield_part","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","striga_trial","striga_infected","borer_trial","crop")]	

as.numeric(d5$yield)
as.numeric(d5$polshed)
as.numeric(d5$dy_sk)
as.numeric(d5$dy_sk)
as.numeric(d5$asi)
as.numeric(d5$pl_ht)
as.numeric(d5$e_ht)
as.numeric(d5$rlper)
as.numeric(d5$slper)
as.numeric(d5$husk)
as.numeric(d5$p_asp)
as.numeric(d5$e_harv)
as.numeric(d5$erot)
as.character(d5$planting_date)
d5<-d5[,-1]
d5$anthesis<-as.numeric(unlist(d5[,25]))

d<-rbind(d0,d1,d2,d3,d4,d5)
	
carobiner::write_files(dset, d, path=path)
}



	