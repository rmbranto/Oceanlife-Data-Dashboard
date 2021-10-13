# rmbranto 2021/08 - original
# rmbranto 2021/10/12 - rewrite

# Demonstrate R programming lanquage 'spocc', 'scrubr' and 'ggplot2' packages to retrieve and combine
# selected invasive species occurrence data from multiple source, see also: 
# https://ocean.si.edu/ocean-life/5-invasive-species-you-should-know 
# https://docs.ropensci.org/spocc/

options(stringsAsFactors = FALSE)

library(spocc)
library(rinat)
library(sparsesvd)
library(scrubr)
library(stringr)
library(plyr)
library(sf)

#load(file='invasives.rda')
#objects()

########################################################################################
########################################################################################
# setup extraction run ...

fName<-'invasives'
fao.loc="geobase/World_Fao_Zones.shp"
eez.loc="geobase/EEZ_Land_v3_202030.shp"
species.list<-read.csv(paste(fName,'_list.csv',sep=''))
qNames=species.list$Names
prov.list=read.csv('prov_list.csv')
pNames=prov.list$prov

limit=9999
limit=9

extract=FALSE
extract=TRUE

########################################################################################
########################################################################################
# extract data ...

if(extract){
do.extract<-function(qName,pNames,limit){
    df1<-data.frame(qName=qName,
                    occ2df(occ(query = qName, 
                               from=pNames[!(pNames %in% c('UNIQUE','DUPS','inat'))], 
                               limit=limit, 
                               has_coords = TRUE)))
    df2<-get_inat_obs(
            query=qName,quality="research",
            geo=TRUE,
            maxresults=limit)    
    df2<-data.frame(qName=qName,
              name=df2$scientific_name,
              longitude=df2$longitude,
              latitude=df2$latitude,
              prov='inat',
              date=df2$datetime,
              key=df2$id)
    return(rbind(df1,df2))
}
df<-NULL
for(qName in qNames){
    cat(qName,'...\n')
    df<-rbind(df,do.extract(qName,pNames,limit))
    }
df.raw<-df
}

cat('\n# df.raw counts by species ...\n\n')
table(df.raw$qName,df.raw$prov)%>%
rbind(.,total=apply(.,2,sum,na.rm=TRUE))%>%
cbind(.,total=apply(.,1,sum,na.rm=TRUE))%>%
print(.,na.print='.')   

########################################################################################
########################################################################################
# scrub data

df<-df.raw
names(df)[1]<-'species'

# fix coordinates

df$latitude<-round(as.numeric(df$latitude),12)
df$longitude<-round(as.numeric(df$longitude),12)

df<-dframe(df) %>%
    coord_impossible() %>%
    coord_incomplete() %>%
    coord_unlikely()

# fix dates ...
df<-df[!is.na(df$date),]
df<-df[as.character(df$date)>'1111-11-11',]

df<-df[df$date<=Sys.Date(),]

df.clean<-df
nrow(df.clean)
range(df.clean$date)

cat('\n# df.clean counts by species & prov ...\n\n')
table(df.clean$species,df.clean$prov)%>%
rbind(.,col.totals=apply(.,2,sum,na.rm=TRUE))%>%
cbind(.,ALL=apply(.,1,sum,na.rm=TRUE))%>%
print(.,na.print='.')   
cat('\n# df.clean occurrrence counts by species & provs and by ALL, UNIQUE & DUPS ...\n\n')
table(df.clean$species,df.clean$prov)%>%
cbind(.,ALL=apply(.,1,sum,na.rm=TRUE))%>%
cbind(.,UNIQUE=table(count(df.clean,c('species','longitude','latitude','date'))$species))%>%
cbind(.,DUPS=.[,'ALL']-.[,'UNIQUE'])%>%
rbind(.,col.totals=apply(.,2,sum,na.rm=TRUE))%>%
print(.,na.print='.')   

########################################################################################
########################################################################################
# create df.UNIQUE ...
cat('\n# df.unique...\n\n')
df.UNIQUE<-count(df.clean,c('species','longitude','latitude','date'))
names(df.UNIQUE)[5]<-'DUPS'
df.UNIQUE$id=seq.int(nrow(df.UNIQUE))
cat('UNIQUE=',nrow(df.UNIQUE),'\n')
head(df.UNIQUE)

#create df.ALL ...
cat('\n# df.ALL...\n\n')
df.ALL=merge(df.clean,df.UNIQUE,by=c(c('species','longitude','latitude','date')))
cat('ALL=',nrow(df.ALL),'\n')
cat('UNIQUE=',nrow(count(df.ALL$id)),'\n')
cat('DUPS=',length(df.ALL$DUPS[df.ALL$DUPS>1])-nrow(count(df.ALL$id[df.ALL$DUPS>1])))
head(df.ALL)

# create df.DUPS ...
cat('\n# df.DUPS...\n\n')
df.DUPS=count(df.ALL[df.ALL$DUP>1,],c('species','longitude','latitude','date','id'))
names(df.DUPS)[6]='DUPS'
df.DUPS=df.DUPS[,c('species','longitude','latitude','date','DUPS','id')]
cat('nrows=',nrow(df.DUPS),'\n')
cat('DUPS=',sum(df.DUPS$DUPS)-nrow(df.DUPS),'\n')
head(df.DUPS)

########################################################################################
########################################################################################
# eez and fao indexing

cat('\n# unique PTS...\n\n')

fao.shp=st_read(fao.loc,quiet=T)
eez.shp=st_read(eez.loc,quiet=T)

prov.pts<-count(df.ALL,c('latitude','longitude'))[,1:2]
nrow(prov.pts)

cat('\n# EEZ...\n\n')

eez.pts<-st_as_sf(prov.pts,coords = c('longitude','latitude'), crs = st_crs(eez.shp))
eez.intersect<-data.frame(st_intersects(eez.pts, eez.shp))
nrow(eez.intersect)

prov.pts$eez='UNK'
for(i in 1:nrow(eez.intersect)){
    prov.pts$eez[eez.intersect$row.id[i]]<-eez.shp$ISO_TER1[eez.intersect$col.id[i]]    
}

cat('\n# FAO...\n\n')

fao.pts<-st_as_sf(prov.pts,coords = c('longitude','latitude'), crs = st_crs(fao.shp))
fao.nearest<-data.frame(st_nearest_feature(fao.pts, fao.shp))
nrow(fao.nearest)

prov.pts$fao='99'
for(i in 1:nrow(fao.nearest)){
    prov.pts$fao[i]<-fao.shp$zone[fao.nearest[i,1]]    
}

cat('\n# df.ALL...\n\n')

df.ALL<-merge(df.ALL,prov.pts,by=c('longitude','latitude'))#[,c(3:9,1:2,10:11)]

cat('nrows= ',nrow(df.ALL),'\n')
head(df.ALL,5)
table(df.ALL$fao)[rev(order(table(df.ALL$fao)))]
table(df.ALL$eez)[rev(order(table(df.ALL$eez)))][1:26]

########################################################################################
########################################################################################
# get taxon.ids for each provider and species 

prov.keys<-data.frame(species=species.list$Names)

for(my.prov in c('ala','bison','gbif','idigbio','inat','obis')){

spec.keys<-NULL

for (my.species in species.list$Names){
    
    an.error.occured <- FALSE
    
    tryCatch(
        
    { 
      if(my.prov=='ala'){
        result=as.character(
        as.ala(occ(query=my.species,from='ala',limit=1))[[1]]$processed$classification$left
        )
      }
      if(my.prov=='bison'){
        result=as.character(
        as.bison(occ(query=my.species,from='bison',limit=1))[[1]]$points$ITIStsn
        )
      }
      if(my.prov=='gbif'){
        result=as.character(
        as.gbif(occ(query=my.species,from='gbif',limit=1, has_coords = TRUE))[[1]][[1]]$hierarchy$key[7]
        )
      }
      if(my.prov=='idigbio'){
        result=as.character(
        as.idigbio(occ(query=my.species,from='idigbio',limit=1, has_coords = TRUE))[[1]]$indexTerms$taxonid
        )
      }
      if(my.prov=='inat'){
        result=as.character(
        as.inat(occ(query=my.species,from='inat',limit=1, has_coords = TRUE))[[1]]$results$taxon$id
        )
      }
      if(my.prov=='obis'){
        result=as.character(
        as.obis(occ(query=my.species,from='obis',limit=1))[[1]]$results$aphiaID
        )
      }
    }

    ,error = function(e) {an.error.occured <<- TRUE}
             
            )

    if(an.error.occured){result<-NA}
    
    spec.keys<<-c(spec.keys,ifelse(is.null(result),NA,result))
}

prov.keys<-cbind(prov.keys,spec.keys)

colnames(prov.keys)[ncol(prov.keys)]<-my.prov 

prov.keys<<-prov.keys

}

prov.keys

########################################################################################
########################################################################################
# save outputs

objects()
save(list=objects(),file=fName )           

save(list=c('df.ALL','species.list','fao.shp','eez.shp','prov.list','prov.keys'),file=paste('short-',fName,sep=''))

