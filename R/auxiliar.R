distCoast = function(lon, lat){
  temp = data.frame(lon = lon, lat = lat)
  posiciones = temp[,c("lon", "lat")]
  
  #- Convert VMS data to SpatialPolygons
  spTa              = SpatialPoints(data.frame(posiciones))
  proj4string(spTa) = CRS("+proj=longlat")
  spTa.proj         = spTransform(spTa, CRS("+proj=utm +zone=18 ellips=WGS84"))
  
  #- Read shapefile of Peru
  Peru              = as(PER_ADM0, "SpatialPolygons")
  proj4string(Peru) = CRS("+proj=longlat")
  Peru.proj         = spTransform(Peru, CRS("+proj=utm +zone=18 ellips=WGS84"))
  dists = gDistance(spgeom1 = spTa.proj, spgeom2=Peru.proj,byid=T) #
  distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas
  
  return(distance)}

# -------------------------------------------------------------------------

# Function to identify NAs
VectorInVector = function(pattern, tag){
  lenTag = length(pattern) - 1
  result = NULL
  for(i in seq(length(tag) - lenTag))
  {
    if(isTRUE(identical(tag[seq(i, i + lenTag)], pattern)))
      result = c(result, i)
  }
  return(result)
}


# -------------------------------------------------------------------------

# Function to replace NAs with zeros
AddZeros <- function(vect, nzeros){
  vect <- as.matrix(vect, ncol=1)
  if(!all(is.na(y0)))
  {
    if(nzeros > 0)
    {
      iniIndex = apply(apply(vect, 2, is.na), 2, VectorInVector, pattern = c(TRUE, FALSE))
      finIndex = apply(apply(vect, 2, is.na), 2, VectorInVector, pattern = c(FALSE, TRUE))
      
      if((sum(is.na(vect)) == nrow(vect)) | (sum(!is.na(vect)) == nrow(vect)))
        next
      
      index.fill = which(!is.na(vect))
      index = NULL
      for(k in seq_len(length(iniIndex)))
        index = c(index, seq(iniIndex[k] - nzeros + 1, iniIndex[k]))
      for(k in seq_len(length(finIndex)))
        index = c(index, seq(finIndex[k] + 1, finIndex[k] + nzeros))
      index = index[!index %in% index.fill]
      # index = c(min(iniIndex[[i - 1]]), max(finIndex[[i - 1]]))
      # index = c(seq(index[1] - nzeros + 1, index[1]), seq(index[2], index[2] + nzeros - 1))
      vect[index] = 0
    }
  }
  vect <- as.numeric(vect)
  return(vect)
}



