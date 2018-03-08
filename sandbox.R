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
king<-st_sf(data.table(bg_poly)[STATEFP10=="53"&COUNTYFP10=="033",list(as.character(GISJOIN),geometry)][1:50]) # parsing it as a data.table, returning it to sf

###########################################
# Generate a "Cookie Cutter" by creating 1
# Set of concentric rings for each nb buffer,
# then shift the rings from one location
# to the next, gathering information


#' ConcentricRings
#' Returns a sf polygon object with geometry centered at 0,0 in the coordinate system of the input geometry.

ConcentricBufferRings<-function(input_geom, distances, at_origin=T){
  
  distances<-sort(distances) #ensure that distances are sorted such that smallest->largest
  # Make empty lists for things to be stored later
  buffers<-list() # Make a list of buffers
  rings<-list() # Make a list of rings

  # Define function to get ony differences of shapes when compared to one another,
  # leaving a ring outside the prior buffer
  st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
  
  for (d in 1:length(distances)){
    if(d==1){
      # If this is the first buffer, we want to keep it:
      buff<-st_buffer(input_geom,distances[d])
      buffers[[as.character(distances[d])]]<-buff
      # Add to final geoms as data.table
      buff$dist<-as.character(distances[[d]]) # Add field of distance band
      st_crs(buff)<-st_crs(input_geom)
      rings[[as.character(distances[[d]])]]<-data.table(buff)
    }else{ # If this is the second or later buffers, we only want the "ring"
      buff<-st_buffer(input_geom,distances[d]) # Make buffer
      buffers[[as.character(distances[d])]]<-buff
      diff<-st_erase(buff,buffers[[ as.character(distances[[(d-1)]]) ]]) # Get only ring
      buffers[[as.character(distances[[d-1]])]]<-NULL # Erase buffers with no usefulness
      diff$dist<-paste0(distances[[d-1]],":",distances[[d]]) # Add field of distance band
      rings[[as.character(distances[[d]])]]<-data.table(diff)
    }
    if(d==length(distances)){
      rm(buffers,buff,diff) # Get rid of buffers 
    }
  }
  
  # Create a "Cookie Cutter" template to move around to each point location
  # Combine rings into 1 feature
  rings<-st_sf(rbindlist(rings))
  st_crs(rings)<-st_crs(input_geom)
  
  if(at_origin){
    # Shift to 0,0 coord space
    rings$geometry<-st_geometry(rings)-st_centroid(st_geometry(rings))
  }
  return(rings)
}


#' ShiftCentroid
#' Shifts the centroid of a input sf object such that the centroid of the over-written
#' geometry is now at the centroid of the output location geometry. 
ShiftCentroid<-function(input_geom,output_location_geom,input_at_origin=T){
  
  if(!input_at_origin){ # if it's not already at the origin, shift to 0,0 coord space
    input_geom$geometry<-st_geometry(input_geom)-st_centroid(st_geometry(input_geom))
  }
  # Over-write geometry to "shift" geometry to be centered the new centroid's location
  input_geom$geometry<-st_geometry(input_geom)+st_centroid(st_geometry(output_location_geom))
  # Set CRS
  st_crs(input_geom)<-st_crs(output_location_geom)
  return(input_geom)
}

# Time it by using the cookie-cutter method:
ptm <- proc.time()
cookie_cutter<-ConcentricBufferRings(st_centroid(king[1,]),distances=c(500,1000,2000,3000,5000))
cookies<-list()
for(loc in 1:nrow(king)){
  cookie<-st_intersection(king,ShiftCentroid(cookie_cutter,king[loc,]))
  st_crs(cookie)<-st_crs(cookie_cutter)
  cookie$area<-st_area(cookie)
  cookies[[as.character(loc)]]<-data.table(cookie)[,list(GEOID=V1,dist,area)]
}
cookies<-rbindlist(cookies)
shift_time<-proc.time() - ptm
