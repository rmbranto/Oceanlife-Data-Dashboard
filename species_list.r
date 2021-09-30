# create metadata for a list of species depending on name of .rda file
# to run ... source("species_lists.r") 

if(fName=='invasives.rda'){

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

species.style<-data.frame(Names=qNames,cNames=cNames,sColors=sColors,wikidata=wikidata,fImages=fImages,stringsAsFactors = FALSE)
}

if(fName=='marineplants.rda'){

qNames<-c('Codium fragile')
cNames<-c('Dead Mans Fingers')
wikidata<-c('Q2712208')
sColors<-c('magenta')
fImages<-c('https://upload.wikimedia.org/wikipedia/commons/e/ed/Codiumfragile.jpg')

species.style<-data.frame(Names=qNames,cNames=cNames,sColors=sColors,wikidata=wikidata,fImages=fImages,stringsAsFactors = FALSE)
}

cat('species list = ',paste(species.style$Names,collapse=', '),'\n')
