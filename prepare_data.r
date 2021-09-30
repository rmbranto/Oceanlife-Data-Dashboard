# rmbranto 2021/08
# Demonstrate R programming lanquage 'spocc', 'scrubr' and 'ggplot2' packages to retrieve and combine
# selected invasive species occurrence data from multiple source, see also: 
# https://ocean.si.edu/ocean-life/5-invasive-species-you-should-know 
# https://docs.ropensci.org/spocc/

fName<-'invasives.rda'
limit<-9999

options(stringsAsFactors = FALSE)
library(spocc)
library(rinat)
library(sparsesvd)
library(scrubr)
library(stringr)
library(plyr)
library(sf)

extract<-FALSE
pNames<-c('ala','bison','gbif','eBird','idigbio','obis','VertNet')

qNames<-c('Carcinus maenas','Caulerpa taxifolia','Codium fragile','Dreissena polymorpha','Mnemiopsis leidyi','Pterois volitans','Rapana venosa')
cNames<-c('European Green Crab','Killer Algae','Dead Mans Fingers','Zebra Mussel','Sea Walnut','Lion Fish','Veined Rapa Whelk') # invasive
wikidata<-c('Q27779','Q310961','Q2712208','Q752130','Q133630','Q824672','Q139053')
sColors<-c('green','cyan','magenta','red','orange','yellow','dodgerblue')
fImages<-c('https://upload.wikimedia.org/wikipedia/commons/1/17/Carcinus_maenas.jpg',
           'https://upload.wikimedia.org/wikipedia/commons/e/e7/CaulerpaTaxifolia.jpg',
           'https://upload.wikimedia.org/wikipedia/commons/e/ed/Codiumfragile.jpg',
           'https://upload.wikimedia.org/wikipedia/commons/thumb/a/a9/Dreissena_polymorpha3.jpg/220px-Dreissena_polymorpha3.jpg',
           'https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Sea_walnut%2C_Boston_Aquarium.jpg/220px-Sea_walnut%2C_Boston_Aquarium.jpg',
           'https://upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Pterois_volitans_Manado-e_edit.jpg/220px-Pterois_volitans_Manado-e_edit.jpg',
           'https://upload.wikimedia.org/wikipedia/commons/thumb/f/fb/Rapana_Black_Sea_2008_G1.jpg/220px-Rapana_Black_Sea_2008_G1.jpg')

species.style<-data.frame(Names=qNames,cNames=cNames,sColors=sColors,fImages=fImages,stringsAsFactors = FALSE)

write.csv(species.style,'invasives_list.csv')

prov.style<-data.frame(
    prov = c("UNIQUE","DUPS","ala","bison","bold","gbif","idigbio","inat","obis"),
    id=c('UNI','DUP','ala','bis','bol','gbi','idi','ina','obi'),
    color = c("grey", "white", "red", "orange", "yellow", "green", "cyan", "dodgerblue", "magenta"),
    order=c(8,9,1:7))

if(extract){
do.extract<-function(qName=qNames[1],limit=9999){

    df1<-data.frame(qName=qName,
                    occ2df(occ(query = qName, from=pNames, limit=limit, has_coords = TRUE)))

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
    df<-rbind(df,do.extract(qName,limit))
    }

nrow(df)
table(df$qName,df$prov)
df.raw<-df
}

#load(fName) ; nrow(df.raw); names(df.raw)
#table(df.raw$prov,df.raw$qName)

# data scrubbing

df<-df.raw
names(df)[1]<-'species'

# add prov='bold'    
df$prov[substr(df$name,1,4)=='BOLD']<-'bold'

# fix coordinates

df$latitude<-as.numeric(df$latitude)
df$longitude<-as.numeric(df$longitude)

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

# dedup ???

if(FALSE){

    dedup
    
    smalldf<-df.clean[df.clean$species=='Pterois volitans',]
    smalldf<-smalldf[1:1000,]
    NROW(smalldf)
    dp <- dframe(smalldf) %>% dedup()
    NROW(dp)
    attr(dp, "dups")
}

# find UNIQUEs and DUPS

df.agg<-aggregate(OCCS~species+longitude+latitude+prov+date,data=data.frame(df.clean,OCCS=1),sum) 
df.unique<-count(df.agg,c('species','longitude','latitude','date','OCCS'))
names(df.unique)[dim(df.unique)[2]]<-'DUPS'

# save UNIQUE into df.prov

df.agg$DUPS<-1
df.unique$prov<-'UNIQUE'
df.unique<-df.unique[,c(1:3,7,4:6)]
df.prov<-rbind(df.agg,df.unique)
df.prov$year<-as.integer(substr(as.character(df.prov$date),1,4))
df.prov<-df.prov[,c(4,1,8,5,2:3,6:7)]

# save DUPS into df.prov

df.dups<-df.prov[df.prov$prov=='UNIQUE' & df.prov$DUPS>1,]
df.dups$prov<-'DUPS'
df.dups$DUPS<-df.dups$DUPS-1
df.dups$OCCS<-df.dups$DUPS*df.dups$OCCS
df.prov<-rbind(df.prov,df.dups)

xtab<-table(df.prov$species,df.prov$prov)
rbind(xtab,apply(xtab,2,sum))[,c(1:3,5:8,9,4)]
range(df.prov$date)
names(df.prov)
nrow(df.prov)

# eez and fao indexing
prov.pts<-count(df.prov,c('latitude','longitude'))[,1:2]

#eez.shp<-st_read("/home/bobbranton/geoserver/data_dir/eez/EEZ_Land_v3_202030.shp",quiet=TRUE)
eez.pts<-st_as_sf(prov.pts,coords = c('longitude','latitude'), crs = st_crs(eez.shp))
eez.intersect<-data.frame(st_intersects(eez.pts, eez.shp))
nrow(eez.intersect)

prov.pts$eez='UNK'
for(i in 1:nrow(eez.intersect)){
    prov.pts$eez[eez.intersect$row.id[i]]<-eez.shp$ISO_TER1[eez.intersect$col.id[i]]    
}

#fao.shp<-st_read("/home/bobbranton/geoserver/data_dir/fao/World_Fao_Zones.shp",quiet=TRUE)
fao.pts<-st_as_sf(prov.pts,coords = c('longitude','latitude'), crs = st_crs(fao.shp))
fao.intersect<-data.frame(st_intersects(fao.pts, fao.shp))
nrow(fao.intersect)

prov.pts$fao='99'
for(i in 1:nrow(fao.intersect)){
    prov.pts$fao[fao.intersect$row.id[i]]<-fao.shp$zone[fao.intersect$col.id[i]]    
}

df.prov<-merge(df.prov,prov.pts,by=c('longitude','latitude'))[,c(3:8,1:2,9:10)]
nrow(df.prov)
head(df.prov,5)


# get taxon.ids for each provider and species 

prov.keys<-data.frame(species=species.style$Names)

for(my.prov in c('ala','bison','gbif','idigbio','inat','obis')){

spec.keys<-NULL

for (my.species in species.style$Names){
    
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

library(dplyr)
# prepare expanded data
df<-df.prov%>%
    transform(area=str_pad(paste(round(latitude,1),round(longitude,1)),13,'right','.'))%>%
    group_by(species, prov ,year, eez, fao ,area) %>%
    summarize(OCCS = sum(OCCS, na.rm = TRUE))%>%
    transform(data=paste(species,prov,year,eez,fao,area,sep=';'))%>%
    .[,c('data','OCCS')]

fn=tempfile("df.exp")               
write(unlist(rep(df$data,df$OCCS)),fn)
df.exp<-data.frame(read.csv(fn,header=FALSE,sep=';'))
names(df.exp)<-c('species','prov','year','eez','fao','area')

# summary of expanded data ...
names(df.exp)
nrow(df.exp)

table(df.exp$species,df.exp$prov)%>%
rbind(.,total=apply(.,2,sum,na.rm=TRUE))%>%
cbind(.,total=apply(.,1,sum,na.rm=TRUE))%>%
.[,c(1:3,5:8,9,4,10)]

objects()
save(list=objects(),file=fName )           

save(list=c('df.prov','species.style','fao.shp','eez.shp','prov.style','prov.keys','df.exp'),file=paste('short-',fName,sep=''))

