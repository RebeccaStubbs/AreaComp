######
# Author: Rebecca Stubbs
# Date: March 6, 2017 
# Purpose: Test out strategies and functions for area composition!

# Load libraries
library(MapSuite)
library(sf) #Simple features is the future!

rm(list=ls())# Clear workspace

# Read in block-group shapefile
bg_poly <- st_read("/Users/stubbsrw/Documents/git_code/AreaComp/data/nhgis_shapefile_tl2010_us_blck_grp_2010/US_blck_grp_2010.shp")

# Discover what the projection of the block group file is:
st_crs(bg_poly) # Looks like albers equal area conic, which is great!

# To make things more tractable, let's only play with 1 state, Washington, in King County:
king<-data.table(bg_poly)[STATEFP10=="53"&COUNTYFP10=="033"] # parsing it as a data.table
sf_king<-st_sf(king) # Converting back to simple features
cent<-st_centroid(sf_king)

# Define distances for ring buffers
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
distances<-c(500,1000,2000,3000,5000)

# Set up a series of rings expanding outward based on the distance bands
distances<-sort(distances) # Ensure that your distances are sorted smallest to largest

buffers<-list() # Make a list of buffers
geoms<-list() # Make a list of rings

# Generate buffers for each point
input_geom<-cent
for (d in 1:length(distances)){
  if(d==1){
    # If this is the first buffer, we want to keep it:
    buff<-st_buffer(input_geom,distances[d])
    buffers[[as.character(distances[d])]]<-buff
    # Add to final geoms as data.table
      buff$dist<-as.character(distances[[d]]) # Add field of distance band
      geoms[[as.character(distances[[d]])]]<-buff
  }else{ # If this is the second or later buffers, we only want the "ring"
    buff<-st_buffer(input_geom,distances[d]) # Make buffer
    buffers[[as.character(distances[d])]]<-buff
    diff<-st_erase(buff,buffers[[ as.character(distances[[(d-1)]]) ]]) # Get only ring
    buffers[[as.character(distances[[d-1]])]]<-NULL # Erase buffers with no usefulness
    diff$dist<-paste0(distances[[d-1]],":",distances[[d]]) # Add field of distance band
    geoms[[as.character(distances[[d]])]]<-diff
  }
  if(d==length(distances)){
    rm(buffers,buff,diff) # Get rid of buffers 
  }
}

# Combine all the geometry into 1 
geoms<-rbindlist(geoms)

plot.new()
plot(buff_1k$geometry,col="red")
plot(buff_500$geometry,col="blue")
plot(diff$geometry,col="orange")


a<-st_geometry(sf_king[1,])
b<-st_geometry(sf_king[2,])

plot(st_geometry(sf_king[1:2,]))
plot(st_geometry(a),add=T,col="red")
plot(st_geometry(b),add=T,col="blue")

# test out transforming where the polygon is located:
trans<-st_geometry(a-(st_centroid(a))+st_centroid(b))

plot(trans,add=T,col="orange")
