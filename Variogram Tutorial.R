library(gstat)
library(sp)
library(ggplot2)

theme_set(theme_bw())
theme_update(legend.position='top')

# data
x <- 1:100
y <- 1:100
dat <- expand.grid(x=x,y=y)
dat$z <- 1
coordinates(dat) <- ~x+y
gridded(dat) <- TRUE
g <- gstat(id='z', formula=z~1,
           model=vgm(0.9,'Sph',60,0.1, 
           anis=c(45,0.1)), data=dat, dummy=TRUE,
           beta=0, maxdist=20, nmax=10)
dat <- data.frame(predict(g,newdata=dat,nsim=1))

names(dat)[3] <- 'z'
head(dat)

ggplot(dat, aes(x=x,y=y,fill=z)) + geom_raster() + 
  scale_fill_gradientn(colours=rainbow(7))

coordinates(dat) <- ~x+y
g <- gstat(id='z', formula=z~1,data=dat)

ggplot(expvar,aes(x=dist,y=gamma,size=np)) + geom_point()
expvar <- variogram(g)
head(expvar)

expvar <- variogram(g,width=3,cutoff=30,map=TRUE)
ggplot(data.frame(expvar),aes(x=map.dx,y=map.dy,
                              fill=map.z)) +
  geom_raster() + scale_fill_gradientn(colours=rainbow(7))
