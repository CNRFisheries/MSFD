setwd("...")
library(tidyverse)
library(readxl)
library(rfishbase)

# target data ####
species_AD=read_excel("data/D3Adriatic_raw.xlsx")
species_AD$a=-999
species_AD$b=-999
species_AD$Lm=-999
species_AD$Linf=-999
species_AD$k=-999
species_AD$t0=-999

# general data ####
ASFIS <-read_csv("data/ASFIS_2020.csv")
head(ASFIS)
Sp_Medits <- read_csv("data/Sp_Medits.csv")
Sp_Medits=Sp_Medits[,c('code', 'scientific_name')]%>%distinct(code,.keep_all = T)
head(Sp_Medits)

# medits ####
medits_tb <- read_csv("data/medits_tb.csv")
medits_tb$code=paste0(medits_tb$genus, medits_tb$species)
medits_tb <- medits_tb%>%left_join(Sp_Medits, by = "code")

# solemon ####
solemon_tb <- read_excel("data/solemon_TB.xlsx")
solemon_tb$code=paste0(solemon_tb$GENUS, solemon_tb$SPECIES)
solemon_tb <-solemon_tb%>%left_join(Sp_Medits, by='code')
solemon_tb=solemon_tb[!is.na(solemon_tb$MONTH),]

# campbiol ####
LFDcampbiol=read_excel('data/C8 C.xlsx')
LFDcampbiol$code=str_remove(toupper(LFDcampbiol$Specie), ' ')
LFDcampbiol=LFDcampbiol%>%
  dplyr::left_join(.,Sp_Medits, by='code')%>%
  dplyr::rename('ScientificName'='scientific_name')

# landings dcf
Landings <- read_excel("data/Landings.xlsx")
spe_name=ASFIS[,c('Scientific_name', 'English_name', '3A_CODE')]
names(spe_name)[3]='SPECIES'
names(spe_name)[1]='ScientificName'
Landings=left_join(Landings, spe_name)

# LANDINGS stecf
STECF=read_excel("data/STECF_AER/2021/STECF 21-08 - EU Fleet Economic and Transversal data_fleet segment.xlsx", 
           sheet = "FS_landings FAO 2017-")
STECF=STECF[STECF$sub_reg%in%c('sa 17', 'sa 18'),]
STECF=STECF[STECF$year%in%2017:2019,]
STECF=STECF[STECF$variable_name=="Live weight of landings",]

# other DCF ####
gp=read_csv("data/gp.csv")
ml=read_csv("data/ml.csv")

# fishbase ####
fb=fb_tbl('species')
fb$SciName=paste(fb$Genus, fb$Species, sep=' ')

slb=fb_tbl("species", "sealifebase")
slb$SciName=paste(slb$Genus, slb$Species, sep=' ')


### survey ####
for(i in 1:nrow(species_AD)){
  
  cat(i)
  # get species
  xspec=species_AD[i,]$ScientificName
  
  # surevys
  xmedit=medits_tb[medits_tb$scientific_name== xspec, ]
  xsolem=solemon_tb[solemon_tb$scientific_name==xspec,]
  xsolem=xsolem[!is.na(xsolem$YEAR),]
  species_AD[i,]$Medits_survey_positive_hauls=nrow(xmedit%>%
                                                     dplyr::filter(ptot>0)%>%
                                                     distinct(year,area,haul_number))
  
  species_AD[i,]$Solemon_survey_positive_hauls=nrow(xsolem%>%
                                                      dplyr::filter(TOTAL_NUMBER_IN_THE_HAUL>0)%>%
                                                      distinct(YEAR,HAUL_NUMBER))
  
  if(species_AD[i,]$Medits_survey_positive_hauls+species_AD[i,]$Solemon_survey_positive_hauls>0){
    
    if(species_AD[i,]$Medits_survey_positive_hauls>species_AD[i,]$Solemon_survey_positive_hauls ){
      species_AD[i,]$Trawl_survey_georeferenced='Medits'
      species_AD[i,]$Trawl_survey_years=nrow(xmedit%>%
                                               dplyr::filter(ptot>0)%>%
                                               distinct(year))
      species_AD[i,]$Trawl_survey_total_hauls=nrow(medits_tb%>%
                                                     distinct(year,area,haul_number))
    }else{
      species_AD[i,]$Trawl_survey_georeferenced='Solemon'
      species_AD[i,]$Trawl_survey_years=nrow(xsolem%>%
                                               dplyr::filter(TOTAL_NUMBER_IN_THE_HAUL>0)%>%
                                               distinct(YEAR))
      species_AD[i,]$Trawl_survey_total_hauls=nrow(solemon_tb%>%
                                                     distinct(YEAR, AREA, HAUL_NUMBER))
    }
    
  }else{
    species_AD[i,]$Trawl_survey_georeferenced=NA
    species_AD[i,]$Trawl_survey_years=NA
    species_AD[i,]$Trawl_survey_total_hauls=NA
  }
  
  # campbiol
  xcb=LFDcampbiol[LFDcampbiol$ScientificName== species_AD[i,]$ScientificName & !is.na(LFDcampbiol$`Numero espanso`),]
  species_AD[i,]$Campbiol_years=length(unique(xcb$Anno))
  species_AD[i,]$Campbiol_number_specimens=sum(xcb[!is.na(xcb$`Numero campionato`),]$`Numero campionato`)
  
  # landings
  species_AD[i,]$Landings_years= nrow(Landings[Landings$ScientificName==species_AD[i,]$ScientificName,]%>%
                                        distinct(ANNO))
  
  # landing proportion
  xstecf=STECF[STECF$species_code==species_AD[i,]$species_code,]
  xstecf=xstecf%>%
    dplyr::mutate(country_code=ifelse(country_code=='ITA', country_code, 'other'))%>%
    dplyr::group_by(country_code)%>%
    dplyr::summarise(value=sum(value))
  
  if(nrow(xstecf[xstecf$country_code=='ITA',])==0){
    
    species_AD[i,]$`Stock non condiviso -principalmente acque italiane (Y/N)`='N'
    
  }else{
    
    species_AD[i,]$`Stock non condiviso -principalmente acque italiane (Y/N)`=ifelse(xstecf[xstecf$country_code=='ITA',]$value/sum(xstecf$value)>0.7,'Y','N')
  }
  

  
  
  
  # DCF gp
  xgp=gp[gp$species==species_AD[i,]$species_code & gp$area%in%c('GSA 17', 'GSA 18') & gp$country=='ITA',]
  
  if(nrow(xgp)>0){
    species_AD[i,]$`Relazione allometrica (Y/N)`='campbiol'
    species_AD[i,]$`Von Bertalanffy (Y/N)`='campbiol'
    
    species_AD[i,]$a= mean(xgp[xgp$a<1,]$a)
    species_AD[i,]$b=mean(xgp[xgp$b<5,]$b)
    species_AD[i,]$Linf=mean(xgp[xgp$vb_linf>0,]$vb_linf)
    species_AD[i,]$k=mean(xgp[xgp$vb_k>0,]$vb_k)
    species_AD[i,]$t0=mean(xgp[xgp$vb_t0>-10,]$vb_t0)
  }
  
  # DCF mat
  xml=ml[ml$species==species_AD[i,]$species_code & ml$area%in%c('GSA 17', 'GSA 18') & ml$country=='ITA', ]
  
  if(nrow(xml)>0){
    species_AD[i,]$`Taglia di maurità - L50 (Y/N)`='campbiol'
    
    xml=xml%>%dplyr::group_by(lengthclass)%>%
      dplyr::summarise(prm=mean(prm))%>%
      dplyr::filter(prm>0.5)%>%
      slice_min(prm)
    species_AD[i,]$Lm=xml$lengthclass
  }
  
  
  # inspect fishbase
  cbinspect=species_AD[i,c('Relazione allometrica (Y/N)',  'Taglia di maurità - L50 (Y/N)' , 'Von Bertalanffy (Y/N)' )]
  
  if(nrow(cbinspect)!=  nrow(na.omit(cbinspect))){
    
    ref_area='Adriatic'
    # detect which database
    if(nrow(fb[fb$SciName==xspec,])>0){
      xdb='fishbase'
    }else if(nrow(slb[slb$SciName==xspec,])>0){
      xdb='sealifebase'
    }
    xspecies=xspec
    
    
    if(is.na(species_AD[i,c('Relazione allometrica (Y/N)')][[1]])){
      
      xpars=poplw(xspecies,
                  server = xdb)
      xpars=xpars[!is.na(xpars$Locality),]
      
      if(nrow(xpars[str_detect(xpars$Locality, ref_area),] )> 0 ){
        xdat=xpars[str_detect(xpars$Locality, ref_area),]
        xdat=xdat[!is.na(xdat$a),]
        xdat=xdat[!is.na(xdat$b),]
        xarea=ref_area
      }else if(nrow(xpars[str_detect(xpars$Locality, "Mediterranean"),])> 0){
        xdat=xpars[str_detect(xpars$Locality, "Mediterranean"),]
        xdat=xdat[!is.na(xdat$a),]
        xdat=xdat[!is.na(xdat$b),]
        xarea="Mediterranean"
      }else{
        xdat=xpars
        xdat=xdat[!is.na(xdat$a),]
        xdat=xdat[!is.na(xdat$b),]
        xarea="Other"
      }
      xdat=xdat[c("a", "b", "Species")]
      
      xreslw=data.frame( Species=xspecies,
                         area=xarea,
                         a=mean(xdat$a),
                         b=mean(xdat$b))
      species_AD[i,]$`Relazione allometrica (Y/N)`=paste(xdb, xreslw$area, sep='_')
      species_AD[i,]$a=xreslw$a
      species_AD[i,]$b=xreslw$b
      
    }
    
    if(is.na(species_AD[i,c('Taglia di maurità - L50 (Y/N)')][[1]])){
      
      xpars=rfishbase::maturity(xspecies,
                                server = xdb)
      xpars=xpars[!is.na(xpars$Locality),]
      
      if(nrow(xpars[str_detect(xpars$Locality, ref_area),] )> 0 ){
        xdat=xpars[str_detect(xpars$Locality, ref_area),]
        xdat=xdat[!is.na(xdat$Lm),]
        xarea=ref_area
      }else if(nrow(xpars[str_detect(xpars$Locality, "Mediterranean"),])> 0){
        xdat=xpars[str_detect(xpars$Locality, "Mediterranean"),]
        xdat=xdat[!is.na(xdat$Lm),]
        xarea="Mediterranean"
      }else{
        xdat=xpars
        xdat=xdat[!is.na(xdat$Lm),]
        xarea="Other"
      }
      
      xresmat=data.frame(xdat%>%
                           #dplyr::group_by(Sex)%>%
                           dplyr::summarise(Lm=mean(Lm))%>%
                           dplyr::mutate(area=xarea))
      
      species_AD[i,]$`Taglia di maurità - L50 (Y/N)`=paste(xdb, xresmat$area, sep='_')
      species_AD[i,]$Lm=xresmat$Lm
      
    }
    
    if(is.na(species_AD[i,c('Von Bertalanffy (Y/N)')][[1]])){
      
      xpars=rfishbase::popgrowth(xspecies,
                                 server = xdb)
      xpars=xpars[!is.na(xpars$Locality),]
      if(nrow(xpars[str_detect(xpars$Locality, ref_area),] )> 0 ){
        xdat=xpars[str_detect(xpars$Locality, ref_area),]
        xdat=xdat[!is.na(xdat$TLinfinity),]
        xarea=ref_area
      }else if(nrow(xpars[str_detect(xpars$Locality, "Mediterranean"),])> 0){
        xdat=xpars[str_detect(xpars$Locality, "Mediterranean"),]
        xdat=xdat[!is.na(xdat$TLinfinity),]
        xarea="Mediterranean"
      }else{
        xdat=xpars
        xdat=xdat[!is.na(xdat$TLinfinity),]
        xarea="Other"
      }
      
      xresVBGF=data.frame(xdat%>%
                            #dplyr::group_by(Sex)%>%
                            dplyr::summarise(Linf=mean(TLinfinity),
                                             k=mean(K),
                                             t0=mean(to))%>%
                            dplyr::mutate(area=xarea))
      
      species_AD[i,]$`Von Bertalanffy (Y/N)`=paste(xdb, xresVBGF$area, sep='_')
      species_AD[i,]$Linf=xresVBGF$Linf
      species_AD[i,]$k=xresVBGF$k
      species_AD[i,]$t0=xresVBGF$t0
      
    }
  }
}

names(species_AD)=NA
write.csv(species_AD, 'results/tabella_compilata_v20222.csv', row.names = F)
