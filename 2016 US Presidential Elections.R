library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(tmap)
library(tmaptools)
# Get a shape file of states in the US
usa.df = readOGR(dsn="cb_2015_us_state_5m", "cb_2015_us_state_5m")
# convert it to Albers equal area
usa.df = spTransform(usa.df, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0
                                 +a=6370997 +b=6370997 +units=m +no_defs"))
usa.df@data$id = rownames(usa.df@data)
#extract rotate, shrink and move Alaska.
alaska <- usa.df[usa.df$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(usa.df)
# extract, rotate & shift hawaii
hawaii <- usa.df[usa.df$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(usa.df)
# remove Hawaii, Alaska and Puerto Rico from the list
usa.df<- usa.df[!usa.df$STATEFP %in% c("02", "15", "72"),]
# Add modified Hawaii, and Alaska to the list
usa.df <- rbind(usa.df, alaska, hawaii)
# Get the data to be plotted
usamap.dat <- read.table("us_2016_election_data.csv", header = T, sep = ",")
usamap.dat$NAME <- tolower(usamap.dat$NAME)
usamap.dat <- usamap.dat[usamap.dat$ID == 1, c("NAME","Wins")]
#change NAME column values to lowercase in the shapefile
usa.df$NAME <- tolower(usa.df$NAME)
#order shapefile based on NAME column
usa.df <- usa.df[order(usa.df@data$NAME),]
#removing the 4 that are not there
usa.df<- usa.df[!usa.df$NAME %in% c("american samoa", "commonwealth of the northern mariana islands", "guam", "united states virgin islands"),]
# Merge the data with the shape file
usa.df <- append_data(usa.df, usamap.dat, key.shp = "NAME", key.data="NAME", ignore.duplicates=TRUE, ignore.na=TRUE)
#Now, plot the resultant dataset. The first line below sets the data file to be mapped,
# tm_fill() sets the data column to use for mapping color values. The "Blues" #palette
#argument is a ColorBrewer palette.
usmap <- tm_shape(usa.df) + tm_fill("Wins", title = "Legend", palette = "-RdBu") + tm_borders(alpha=.5) + tm_layout(title="US Presidential Election Results", title.size = 1, title.position = c("center", "top"), inner.margins=c(.03, 0.1, .03, 0.1), legend.title.size = 1, legend.text.size = 0.6, sepia.intensity = -0.1, saturation = 1.5) + tm_text("STUSPS", size=0.5) + tm_legend(position = c("left","bottom"))
#save static maps created by tmap by using the save_tmap() function:
save_tmap(usmap, filename="usa.jpg")