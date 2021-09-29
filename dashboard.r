options(stringsAsFactors = FALSE)
library(shiny)
library(ggplot2)
library(maps)
library(sf)
library(stringr)
library(magrittr)
library(dplyr)

world <- map_data("world")
load(file='short-invasives.rda')

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

sNames<-species.style$Names

eezs=unique(df.prov$eez)
eezs=c(eezs[order(eezs)])

faos=unique(df.prov$fao)
faos=c(faos[order(faos)])

######################################################################################     
######################################################################################     

shinyApp(
ui=fluidPage(

# Sidebar layout ----
  sidebarLayout(
    sidebarPanel(

titlePanel('Invasive marine species ...'),

# species
    selectInput("species","select species",sNames,selected=sNames[1]),
# provider
    selectInput("provs","select provider",prov.style$prov,selected='UNIQUE'),
# years
    sliderInput(inputId = "year", label = "year:",
        min = min(df.prov$year), max = max(df.prov$year),
        value = c(min(df.prov$year), max(df.prov$year)),
        round=TRUE, sep='', step=10),        
# zoom
    checkboxInput(inputId = "zoom", label = "zoom to data:", TRUE),
# faos
    selectInput("fao","select fao",faos,multiple=TRUE,NULL),
    checkboxInput(inputId = "fao_labels", label = "fao labels:", TRUE),
# eezs
    selectInput("eez","select eez",eezs,multiple=TRUE,NULL),
    checkboxInput(inputId = "eez_labels", label = "eez labels:", FALSE),
# downloads
#    selectInput("dataset", "select data",
#                  choices = c("summarized", "expanded", "species", "providers")),
#    downloadButton("downloadData", "Download")

    ),

######################################################################################     
######################################################################################     

    # Main panel for displaying outputs ----
    mainPanel(

htmlOutput(outputId= "titles"),
fluidRow(
    column(3,
      htmlOutput(outputId = "picture")
          ),
    column(4,
      plotOutput(outputId = "provPlot", height=150, width=250)
          ),
    column(5,
      plotOutput(outputId = "distPlot", height=150, width=300)
          )
),
    plotOutput(outputId = "distMap"),
    htmlOutput(outputId= "links")
    
))),

######################################################################################     

server<-function(input, output,local=TRUE) {

######################################################################################     
    
  output$titles<-renderText({

    paste('<center><h3><a target="_blank", href="https://docs.ropensci.org/spocc/">spocc</a>:&nbsp;<i>',
          input$species,'</i>&nbsp;',
          '[',species.style$cNames[species.style$Names==input$species],']',
          '</h3></center>',sep='')
      })

######################################################################################     
######################################################################################     

  output$picture<-renderText({
    paste('<center><img src="',
          species.style$fImages[species.style$Names==input$species],'" height="120">',
          '<br><a href="https://www.wikidata.org/wiki/',
          species.style$wikidata[species.style$Names==input$species],
          '"><small>** See Wikidata TAXON INFO **</small></a></center>',
          sep='')
      })
    
######################################################################################     
######################################################################################     

  output$links<-renderText({
    paste(
        '&nbsp;<center><h4><strong>Provider home page links:</strong>&nbsp;',
        paste('<a target="_blank", href="http://www.marinespecies.org/aphia.php?p=taxdetails&id=',
              prov.keys$obis[prov.keys$species==input$species],'">Worms</a>',
              sep=''),
        '&nbsp;-&nbsp;',
        paste('<a target="_blank", href="https://biocache.ala.org.au/occurrences/search?q=',
              str_replace(input$species,' ','%20'),'"#tab_mapView>ala</a>',
              sep=''),
        '&nbsp;-&nbsp;',
        paste('<a target="_blank", href="https://bison.usgs.gov/#home">bison</a>',sep=''),
        '&nbsp;-&nbsp;',
        paste('<a target="_blank", href="https://www.gbif.org/species/',
              prov.keys$gbif[prov.keys$species==input$species],'">gbif</a>',
              sep=''),
        '&nbsp;-&nbsp;',
        paste('<a target="_blank", href="https://www.idigbio.org/portal/search">idigbio</a>',sep=''),
        '&nbsp;-&nbsp;',
        paste('<a target="_blank", href="https://www.inaturalist.org/observations?place_id=any&subview=map&taxon_id=',
              prov.keys$inat[prov.keys$species==input$species],'">inat</a>',
              sep=''),
        '&nbsp;-&nbsp;',
        paste('<a target="_blank", href="https://obis.org/taxon/',
              prov.keys$obis[prov.keys$species==input$species],'">obis</a>',
              sep=''),
        '</h4></center>',
        paste('<br>See: <a target="_blank", href="https://github.com/rmbranto/Oceanlife-Data-Dashboard">https://github.com/rmbranto/Oceanlife-Data-Dashboard</a>',
              sep=''),
        sep='')
      })
    
######################################################################################
######################################################################################
    
output$provPlot <- renderPlot({

my.species=input$species
my.sYear=input$year[1]
my.eYear=input$year[2]
    
df.exp=df.exp[df.exp$species==input$species,]
df1.min.yr<-min(df.exp$year); df1.max.yr<-max(df.exp$year)
df1=merge(x=prov.style[,c(1,3,4)],y=df.exp%>%count(prov),by='prov')    
    
p<-ggplot() + 
    geom_bar(data=df1,aes(x=reorder(prov,order), y=n, fill=prov),stat= "identity",position=position_dodge())+
    scale_fill_manual(name = "prov", values = df1$color, labels = df1$prov)+
    geom_text(data=df1,aes(x=reorder(prov,order),y=n, label=n), position=position_dodge(width=0.9),vjust=-1.0, size=3)

# subset of years
df.exp=df.exp[df.exp$year>=my.sYear & df.exp$year<=my.eYear,]
df2.min.yr<-min(df.exp$year); df2.max.yr<-max(df.exp$year)    
df2=merge(x=prov.style[,c(1,3,4)],y=df.exp%>%count(prov),by='prov')

subset<-FALSE
if((sum(df1$n,na.rm=TRUE))!=(sum(df2$n,na.rm=TRUE))){
    subset<-TRUE
    p<-p+geom_text(data=df2,aes(x=reorder(prov,order), y=n, label=paste('--> ',n,' <--',sep='')),size=3)
}

p+labs(title=paste('by prov (all: ',df1.min.yr,'-',df1.max.yr,
                ifelse(subset,paste(' vs subset: -->',df2.min.yr,'-',df2.max.yr,'<--',sep=''),''),')',sep=''))+
    theme(legend.position = "none")+xlab('prov')+ylab('occurrences')

  })

###############################################################################
###############################################################################
    
output$distPlot <- renderPlot({

my.species=input$species
my.prov=input$provs
my.sYear=input$year[1]
my.eYear=input$year[2]
my.zoom=input$zoom
    
df.exp<-df.exp[df.exp$prov==my.prov & df.exp$species==my.species,]
df.exp=df.exp[df.exp$prov==my.prov,]
y1=df.exp%>%count(year)
    
# prepare area data
df.area=df.exp%>%count(species,prov,year,area)%>%.[,1:4]%>%count(species,prov,year)
df.area=data.frame(data=c(rep(paste(df.area$year,df.area$species,sep=';'),df.area$n)))
df.area=data.frame(species=substr(df.area$data,6,str_length(df.area$data)),year=as.integer(substr(df.area$data,1,4)))
y2=df.area%>%count(year)

conv=round(max(y1$n)/max(y2$n))
    
p<-ggplot() + 
    geom_bar(data=y1, aes(x = year, y = n), stat = "identity", fill = "grey") +
    geom_line(data=y2, aes(x = year, y = n*conv), size = 1, color = "red") + 
    geom_vline(xintercept = my.sYear, colour='red',linetype = "longdash",size=.1) +
    geom_vline(xintercept = my.eYear, colour='red',linetype = "longdash",size=.1) +
    scale_y_continuous(name = "occurrences (bars)", 
    sec.axis = sec_axis(~./conv, name = "areas (line)"))+
    labs(title=paste(input$provs,' by year:'))+
    theme_bw()

if(my.zoom){p<-p+xlim(my.sYear,my.eYear)}
suppressWarnings(print(p))
    })

###############################################################################
###############################################################################

output$distMap <- renderPlot({

my.prov=input$provs
my.spec=input$species
my.sYear=input$year[1]
my.eYear=input$year[2]
if(length(input$fao)==0){my.fao='.'}else{my.fao=input$fao}
if(length(input$eez)==0){my.eez='.'}else{my.eez=input$eez}
my.eezlabel=input$eez_labels
my.faolabel=input$fao_labels
my.zoom=input$zoom
my.pacific=FALSE

if(my.eezlabel){my.eezlabel=3}
if(my.faolabel){my.faolabel=3}
    
df.p=df.prov[df.prov$species==input$species,]
df.s<-df.p[df.p$prov==my.prov & 
           df.p$year>=my.sYear & 
           df.p$year<=my.eYear
           ,]

if(my.fao != '.'){df.s<-df.s[df.s$fao %in% my.fao,]}
if(my.eez != '.'){df.s<-df.s[df.s$eez %in% my.eez,]}
    
xlim = c(-180, 180)
ylim = c(-90, 90)

if(my.zoom){
    ylim <- c(min(df.s$latitude-1), max(df.s$latitude)+1)
    if(my.pacific){
        xlim <- c(min(df.s$longitude+1), max(df.s$longitude)-1)
    }else{
        xlim <- c(min(df.s$longitude-1), max(df.s$longitude)+1)
    }
}    

df.n<-aggregate(OCCS~species,data=df.s, sum); names(df.n)<-c('species','n') # aggregate occ
df.n$area<-''
for (species in unique(df.s$species)){
    x<-df.s[df.s$species==species,]
    df.n[df.n$species==species,'area']<-length(unique(paste(round(x$lon,1),round(x$lat,1))))   
}
df.n<- merge(df.s,df.n,id='species')
df.n$label<- paste(min(df.n$prov),' ',df.n$species,' ',min(df.n$year),'-', max(df.n$year), 
                   '\n OCCS=',df.n$n,' ; areas=',df.n$area, sep='') # format ggplot label    

world <- map_data("world")

suppressWarnings(print( 

ggplot() + 
    geom_sf(data = fao.shp, size = .1, color = "black", fill='slategray1') +
    geom_sf(data = eez.shp, size = .1, color = "blue", fill=NA) +
    geom_polygon(data = world, aes(x=long, y = lat, group = group), size=.05, fill='beige', color=NA) + 
    coord_sf(xlim = xlim, ylim = ylim)+
    geom_point(data=df.p, aes(x = longitude, y = latitude), size = 1.5, color='lightgray')+
    geom_point(data=df.n, aes(x = longitude, y = latitude), color="red", size = .75)+
    scale_colour_gradient(low = "red", high = "yellow", na.value = NA)+
    geom_polygon(data = world, aes(x=long, y = lat, group = group), size=.25, fill=NA, color="darkgray")+
    geom_sf_text(data = fao.shp, aes(label = zone), colour = "black", size=my.faolabel)+
    geom_sf_text(data = eez.shp, aes(label = ISO_TER1 ), colour = "black", size=my.eezlabel)+
    theme_bw(base_size = 9)+
    theme(strip.text = element_text(size=12), legend.position = "none")+
    facet_wrap(~ label,ncol=2)
))  

    })
})
