require(maptools)
require(rgdal)

fixup <- function(usa,alaskaFix,hawaiiFix){
  
  alaska=usa[usa$STATE_NAME=="Alaska",]
  alaska = fix1(alaska,alaskaFix)
  proj4string(alaska) <- proj4string(usa)
  
  hawaii = usa[usa$STATE_NAME=="Hawaii",]
  hawaii = fix1(hawaii,hawaiiFix)
  proj4string(hawaii) <- proj4string(usa)
  
  usa = usa[! usa$STATE_NAME %in% c("Alaska","Hawaii"),]
  usa = rbind(usa,alaska,hawaii)
  
  return(usa)
  
}

fix1 <- function(object,params){
  r=params[1];scale=params[2];shift=params[3:4]
  object = elide(object,rotate=r)
  size = max(apply(bbox(object),1,diff))/scale
  object = elide(object,scale=size)
  object = elide(object,shift=shift)
  object
}

setwd(tempdir())
download.file("https://dl.dropbox.com/s/wl0z5rpygtowqbf/states_21basic.zip?dl=1", 
  "usmapdata.zip", 
  method = "curl")
unzip("usmapdata.zip")
readOGR("states_21basic/", "states")
all_states <- readOGR("states_21basic/", "states")


allstates <- readOGR(dsn = "/Users/highamm/Desktop/TimeSpaceExpo/states_21basic", layer = "states")
str(myshape)
fortify(myshape)

require(ggplot2); require(maptools); require(rgeos); require(mapproj);
allstates <- fortify(allstates, region = "STATE_NAME")
allstates$long
p <- ggplot(data = allstates) + geom_polygon(aes(x=long, y=lat, group = group, fill = as.numeric(as.factor(id))), 
  colour="white", size = 0.25
) + coord_map(projection="azequalarea") + 
  scale_fill_gradient(limits = c(1,50))
p

p + theme(axis.line=element_blank(),
  axis.text.x=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank(),
  panel.background=element_blank(),
  panel.border=element_blank(),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  plot.background=element_blank())

AK <- p %+% subset(allstates, id == "Alaska") + theme(legend.position = "none")
HI <- p %+% subset(allstates, id == "Hawaii") + theme(legend.position = "none")
contiguous <- p %+% subset(allstates, id != "Alaska" & id != "Hawaii")
??grid.newpage
require(grid)
grid.newpage()
vp <- viewport(width = 1, height = 1)
print(contiguous, vp = vp)
subvp1 <- viewport(width = 0.25, height = 0.25, x = 0.18, y = 0.33)
print(AK, vp = subvp1)
subvp2 <- viewport(width = 0.12, height = 0.12, x = 0.32, y = 0.27)
print(HI, vp = subvp2)

allstates <- readOGR(dsn = "/Users/highamm/Desktop/TimeSpaceExpo/states_21basic", layer = "states")
usAEA = spTransform(allstates, CRS("+init=epsg:2163"))
usfix = fixup(usAEA,c(-35,1.5,-2800000,-2600000),c(-35,1,6800000,-1600000))
plot(usfix)

usfixLL = spTransform(usfix,CRS("+init=epsg:4326"))
plot(usfixLL)
str(usfixLL)
fortify.allstate <- fortify(usfixLL)
class(usfixLL)
head(fortify.allstate)
ggplot(data = fortify.allstate, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = winter_max_error_F1, 
    aes(x = longitude, y = latitude, group = NULL))
?geom_polygon
