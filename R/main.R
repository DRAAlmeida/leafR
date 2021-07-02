#' Count number of points in each Z slice
#'
#' @param Z numeric vector. The heights vector.
#' @param maxZ numeric. The maximum height expected in the whole dataset.
#'
#' @return A [`list`][base::list] of point counts in each Z slice of 1 meter
#'
#' @importFrom data.table data.table
#' @importFrom stats aggregate
#' @export
pointsByZSlice = function(Z, maxZ){
  heightSlices = as.integer(Z) # Round down
  zSlice = data.table::data.table(Z=Z, heightSlices=heightSlices) # Create a data.table (Z, slices))
  sliceCount = stats::aggregate(list(V1=Z), list(heightSlices=heightSlices), length) # Count number of returns by slice

  ##############################################
  # Add columns to equalize number of columns
  ##############################################
  colRange = 0:maxZ
  addToList = setdiff(colRange, sliceCount$heightSlices)
  n = length(addToList)
  if (n > 0) {
    bindDt = data.frame(heightSlices = addToList, V1=integer(n))
    sliceCount = rbind(sliceCount, bindDt)
    # Order by height
    sliceCount = sliceCount[order(sliceCount$heightSlices),]
  }

  colNames = as.character(sliceCount$heightSlices)
  colNames[1] = "ground_0_1m"
  colNames[-1] = paste0("pulses_", colNames[-1], "_", sliceCount$heightSlices[-1]+1, "m")
  metrics = list()
  metrics[colNames] = sliceCount$V1

  return(metrics)

} #end function pointsByZSlice

######################################################################################
######################################################################################
######################################################################################
#' Creates a data frame of the 3D voxels information (xyz)
#' with Leaf Area Density values from las file
#'
#' @param normlas.file normalized las file
#' @param grain.size horizontal resolution (suggested 1 meter for lad profiles and 10 meters for LAI maps)
#' @param k coefficient to transform effective LAI to real LAI (k = 1; for effective LAI)
#'
#' @return A [`data.frame`][base::data.frame] of the 3D voxels information (xyz) with Leaf Area Density values
#'
#' @note The values of LAD are not estimated below 1 meter. For the following reasons:
#' ground points influence
#' realtive low sampling
#'
#' @examples
#' # Get the example laz file
#' normlas.file = system.file("extdata", "lidar_example.laz", package="leafR")
#'
#' VOXELS_LAD = lad.voxels(normlas.file,
#'                         grain.size = 2, k=1)
#'
#' @importFrom raster values
#' @importFrom lidR grid_metrics readLAS
#' @importFrom sp coordinates
#' @importFrom stats formula
#' @export
lad.voxels = function(normlas.file, grain.size = 1, k = 1){

  #empty list object that will be fueling with binneds data.frames
  LAD_VOXELS = list()
  Z = NA

  #load normalized las cloud
  .las = lidR::readLAS(normlas.file)

  .las@data$Z[.las@data$Z < 0] = 0

  maxZ = floor(max(.las@data$Z))

  func = formula(paste0("~pointsByZSlice(Z, ", maxZ, ")"))
  t.binneds    = lidR::grid_metrics(.las, func, res = grain.size,
                                 start = c(min(.las@data$X), max(.las@data$Y)))
  t.binneds    = data.frame(sp::coordinates(t.binneds), raster::values(t.binneds))
  names(t.binneds)[1:2] = c("X", "Y")


  #getting the coordinates X and Y
  #t.binneds$X = coordinates(t.binneds)[,1]
  #t.binneds$Y = coordinates(t.binneds)[,2]
  #t.binneds = as.data.frame(t.binneds) #transforming in a data.frame

  #clip product by las files limits
  #t.binneds = t.binneds[t.binneds$X < xmax(.las) &
  #                        t.binneds$X > xmin(.las) &
  #                        t.binneds$Y > ymin(.las) &
  #                        t.binneds$Y < ymax(.las),]


  #select ground returns
  ground.returns = t.binneds[, grep("ground", names(t.binneds))]

  #select columns vegetation above 1m:
  if(nrow(t.binneds) != 1){ #this if is necessary when grain size is the whole plot
    pulses.profile.dz1 = t.binneds[, c(grep("pulses", names(t.binneds)))]
  }else{
    pulses.profile.dz1 = data.frame(matrix(as.numeric(as.character(t.binneds[, c(grep("pulses", names(t.binneds)))])), ncol = length(grep("pulses", names(t.binneds)))))
    names(pulses.profile.dz1) = names(t.binneds)[c(grep("pulses", names(t.binneds)))]
  }

  #invert data.frames for the sky be first
  pulses.profile.dz1 = pulses.profile.dz1[,length(pulses.profile.dz1):1] #invert columns

  #add grounds returns (0-1m)
  pulses.profile.dz1 = cbind(pulses.profile.dz1, ground.returns)
  rm(ground.returns)

  ### total matriz and cumsum.matrix:
  total.pulses.matrix.dz1 = matrix(apply(pulses.profile.dz1, 1, sum), ncol = length(pulses.profile.dz1), nrow = nrow(pulses.profile.dz1))
  cumsum.matrix.dz1 = matrix(apply(pulses.profile.dz1, 1, cumsum), ncol = length(pulses.profile.dz1), nrow = nrow(pulses.profile.dz1), byrow = TRUE)

  rm(pulses.profile.dz1)

  #Pulses out for each voxel
  pulse.out.dz1 = total.pulses.matrix.dz1 - cumsum.matrix.dz1

  #The pulses.out of voxel 1 is the pulses.in of voxel 2 and so on...
  #Therefore, pulse.in is pulse.out without the last line and adding in the
  #first line the total pulses:
  if(nrow(t.binneds) != 1){ #if used when grain size of the whole plot
    pulse.in.dz1 <- cbind(total.pulses.matrix.dz1[,1], pulse.out.dz1[,-c(ncol(pulse.out.dz1))])
  }else{
    pulse.in.dz1 <- c(total.pulses.matrix.dz1[,1], pulse.out.dz1[,-c(ncol(pulse.out.dz1))])
  } #enf if

  rm(total.pulses.matrix.dz1, cumsum.matrix.dz1)

  # MacArthur-Horn eqquation
  # LAD = ln(S_bottom/S_top)*(1/(dz*K))
  #k value for LAD equation
  dz = 1

  LAD.dz1 = log(pulse.in.dz1/pulse.out.dz1) * 1/k * 1/dz

  rm(pulse.in.dz1, pulse.out.dz1)

  # Remove infinite and NaN values
  #Inf ocorre qndo pulses.out eh zero
  #NaN ocorre qndo pulses.in eh zero
  LAD.dz1[is.infinite(LAD.dz1)] <- NA; LAD.dz1[is.nan(LAD.dz1)] <- NA;

  #remove the first 1 meter close to the ground (and the ground too)
  LAD.dz1 = LAD.dz1[, -c(ncol(LAD.dz1))]

  #fuel list object
  LAD_VOXELS[["LAD"]] = LAD.dz1
  LAD_VOXELS[["coordenates"]] = t.binneds[,c("X", "Y")]

  rm(LAD.dz1, t.binneds)

  return(LAD_VOXELS)
}#End function

################################################################
################################################################







################################################################
################################################################
### LAD PROFILE
#' This function calculate the lad profile from the input lad.voxels
#'
#' @param VOXELS_LAD 3D grid of LAD values (output of lad.voxels() function)
#' @param relative produce lad profile by relative total LAI values. Indicate when usinh effective LAI
#'
#' @return A [`data.frame`][base::data.frame] with the calculated Leaf Area Density
#'
#' @examples
#' # Get the example laz file
#' normlas.file = system.file("extdata", "lidar_example.laz", package="leafR")
#'
#' # Calculate LAD from voxelization
#' VOXELS_LAD = lad.voxels(normlas.file,
#'                         grain.size = 2)
#'
#' lad_profile = lad.profile(VOXELS_LAD)
#' plot(lad_profile$height ~ lad_profile$lad, type = "l", ylim = c(0, 40),
#'      ylab = "Canopy height (m)", xlab = "LAD (m2/m3)")
#'
#' # relative LAD PROFILE
#' relative.lad_profile = lad.profile(VOXELS_LAD, relative = TRUE)
#'
#' plot(relative.lad_profile$height ~ relative.lad_profile$lad, type = "l", ylim = c(0, 40),
#'      ylab = "Canopy height (m)", xlab = "LAD (% of LAI)")
#'
#' @export
lad.profile = function(VOXELS_LAD, relative = FALSE){

  if(relative == TRUE){
    t.lad.profile = apply(VOXELS_LAD$LAD, 2, mean, na.rm = TRUE)
    t.lad.profile = t.lad.profile/sum(t.lad.profile)*100
  }else{
    t.lad.profile = apply(VOXELS_LAD$LAD, 2, mean, na.rm = TRUE)
  }

  max_height = ncol(VOXELS_LAD[[1]]) + .5

  t.lad.profile = data.frame(height = seq(1.5, max_height), lad = t.lad.profile[length(t.lad.profile):1])

  return(t.lad.profile)

}#end looping

#use example


################################################################
################################################################





################################################################
################################################################
### LAI
#' calculates the lead area index (LAI)
#'
#' @param lad_profile output of the lad.profile function
#' @param min mix canopy height
#' @param max max canopy height
#'
#' @return A [`numeric`][base::numeric] containing the LAI calculated from the Leaf Area Density
#'
#' @note The use of min and max arguments allowed the estimation of the LAI for different vertical strata
#'
#' @examples
#' # Get the example laz file
#' normlas.file = system.file("extdata", "lidar_example.laz", package="leafR")
#'
#' # Calculate LAD from voxelization
#' VOXELS_LAD = lad.voxels(normlas.file,
#'                         grain.size = 2)
#'
#' # Calculate the LAD profile
#' lad_profile = lad.profile(VOXELS_LAD)
#'
#' lidar.lai = lai(lad_profile); lidar.lai
#' understory.lai = lai(lad_profile, min = 1, max = 5); understory.lai
#'
#' # relative LAD PROFILE
#' relative.lad_profile = lad.profile(VOXELS_LAD, relative = TRUE)
#'
#' #understory relative LAI (% of total LAI)
#' relative.understory.lai = lai(relative.lad_profile, min = 1, max = 5); relative.understory.lai
#'
#' @export
lai = function(lad_profile, min = 1, max = 100){
  lai = sum(lad_profile$lad[(min):(max)], na.rm = TRUE)
  return(lai)
}


################################################################
################################################################



################################################################
################################################################
#' Leaf Area Height Volume metric
#'
#' @description Calculates the leaf area height volume (LAHV) metric
#' as described in Almeida et al. (2019).
#'
#' @param lad_profile output of the lad.profile function
#' @param LAI.weighting boolean, define if LAVH should be weighted by total LAI. default FALSE
#' @param height.weighting boolean, define if LAVH should be weighted by the max height. default FALSE
#'
#' @return A [`numeric`][base::numeric] containing the Leaf Area Heght Volume calculated from the Leaf Area Density profile.
#'
#' @references
#' Almeida, D. R. A., Stark, S. C., Chazdon, R., Nelson, B. W., Cesar, R. G., Meli, P., … Brancalion, P. H. S. (2019). The effectiveness of lidar remote sensing for monitoring forest cover attributes and landscape restoration. Forest Ecology and Management, 438, 34–43. \doi{10.1016/J.FORECO.2019.02.002}
#'
#' @examples
#' # Get the example laz file
#' normlas.file = system.file("extdata", "lidar_example.laz", package="leafR")
#'
#' # Calculate LAD from voxelization
#' VOXELS_LAD = lad.voxels(normlas.file,
#'                         grain.size = 2)
#'
#' # Calculate the LAD profile
#' lad_profile = lad.profile(VOXELS_LAD)
#'
#' LAHV(lad_profile, LAI.weighting = FALSE, height.weighting = FALSE)
#' LAHV(lad_profile, LAI.weighting = TRUE, height.weighting = FALSE)
#' LAHV(lad_profile, LAI.weighting = FALSE, height.weighting = TRUE)
#' LAHV(lad_profile, LAI.weighting = TRUE, height.weighting = TRUE)
#'
#' @export
LAHV = function(lad_profile, LAI.weighting = FALSE, height.weighting = FALSE){

  #LAI.weighting
  if(LAI.weighting){
    LAHV = sum(lad_profile$height*lad_profile$lad)/sum(lad_profile$lad)
  }else{
    LAHV = sum(lad_profile$height*lad_profile$lad)
  } #end if else

  #height.weighting
  if(height.weighting){
    LAHV = LAHV/max(lad_profile$height)
  } #enf if

  return(LAHV)

} #end function


################################################################
################################################################
#' Foliage Height Diversity
#'
#' @description Calculates the foliage height diversity (FHD) metric
#' from abundances considered as per-voxel relative LAD values, as described in MacArthur and MacArthur (1961).
#'
#' @param lad_profile a data.frame including values of relative LAD at height intervals, output of the lad.profile function (use relative = TRUE)
#' @param evenness boolean, defines whether FHD should be based on Shannon's diversity or evenness (Hill 1973).
#' The default FALSE calculates Shannon diversity as the original FHD by MacArthur and MacArthur (1961);
#' the alternative TRUE was recommended by Valbuena et al. (2012), and it calculates Shannon evenness dividing it by the natural logarithm of the number of number of voxels with LAD values above the threshold.
#' @param LAD.threshold numerical (0,1), defines the minimum value of LAD for considering the relative leaf abundance of a voxel in FHD calculation. Defaults to the inverse of the total number of voxels.
#'
#' @return A [`numeric`][base::numeric] containing the Foliage Height Diversity calculated from the Leaf Area Density profile
#'
#' @references
#' Hill M. O. (1973) Diversity and evenness: a unifying notation and its consequences. Ecology. 54: 427–432. \doi{10.2307/1934352}
#'
#' MacArthur R.H., MacArthur J.W. (1961). On bird species diversity. Ecology 42: 594–598. \doi{10.2307/1932254}
#'
#' Valbuena R., Packalen P., Martín-Fernández S., Maltamo M. (2012) Diversity and equitability ordering profiles applied to the study of forest structure. Forest Ecology and Management 276: 185–195. \doi{10.1016/j.foreco.2012.03.036}
#'
#' @examples
#' # Get the example laz file
#' normlas.file = system.file("extdata", "lidar_example.laz", package="leafR")
#'
#' # Calculate LAD from voxelization
#' VOXELS_LAD = lad.voxels(normlas.file,
#'                         grain.size = 2)
#'
#' # Calculate the LAD profile
#' lad_profile = lad.profile(VOXELS_LAD, relative = TRUE)
#'
#' FHD(lad_profile, evenness = FALSE)
#' FHD(lad_profile, evenness = TRUE)
#'
#' @export
FHD = function(lad_profile, evenness = FALSE, LAD.threshold = -1){

  # applying threshold
  if(LAD.threshold == -1) LAD.threshold <- 1 / length(lad_profile$height)
  lad_profile <- lad_profile[lad_profile$lad >= LAD.threshold,]

  lad_profile$lad <- lad_profile$lad / 100

  #calculating FHD
  if(evenness){
    FHD = - sum( lad_profile$lad * log(lad_profile$lad) ) / log( length(lad_profile$height) )
  }else{
    FHD = - sum( lad_profile$lad * log(lad_profile$lad) )
  } #end if else

  return(FHD)

} #end function


################################################################
################################################################
#' Gini-Simpson index of foliage structural diversity
#'
#' @description Calculates the Gini-Simpson (GS) index metric (i.e. complement of Simpson diversity (\eqn{1 - \gamma})
#' from abundances considered as per-voxel relative LAD values.
#'
#' @param lad_profile a data.frame including values of relative LAD at height intervals, output of the lad.profile function (use relative = TRUE)
#' @param evenness boolean, defines whether GS should be based on Simpson's diversity or evenness (Hill 1973).
#' The default FALSE calculates Simpson's diversity (\eqn{\gamma});
#' the alternative TRUE was recommended by Valbuena et al. (2012), and it divides by the number of voxels with LAD values above the threshold, following Smith and Wilson (1996).
#' @param LAD.threshold numerical (0,1), defines the minimum value of LAD for considering the relative leaf abundance of a voxel in GS calculation. Defaults to the inverse of the total number of voxels.
#'
#' @return A [`numeric`][base::numeric] containing the Fini-Simpson index calculated from the Leaf Area Density profile
#'
#' @references
#' Hill M. O. (1973) Diversity and evenness: a unifying notation and its consequences. Ecology. 54: 427–432. \doi{10.2307/1934352}
#'
#' Smith B., and Wilson J.B. (1996). A consumer's guide to evenness indices. Oikos 76: 70–82. \doi{10.2307/3545749}
#'
#' Valbuena R., Packalen P., Martín-Fernández S., Maltamo M. (2012) Diversity and equitability ordering profiles applied to the study of forest structure. Forest Ecology and Management 276: 185–195. \doi{10.1016/j.foreco.2012.03.036}
#'
#' @examples
#' # Get the example laz file
#' normlas.file = system.file("extdata", "lidar_example.laz", package="leafR")
#'
#' # Calculate LAD from voxelization
#' VOXELS_LAD = lad.voxels(normlas.file,
#'                         grain.size = 2)
#'
#' # Calculate the LAD profile
#' lad_profile = lad.profile(VOXELS_LAD, relative = TRUE)
#'
#' GS(lad_profile, evenness = FALSE)
#' GS(lad_profile, evenness = TRUE)
#'
#' @export
GS = function(lad_profile, evenness = FALSE, LAD.threshold = -1){

  # applying threshold
  if(LAD.threshold == -1) LAD.threshold <- 1 / length(lad_profile$height)
  lad_profile <- lad_profile[lad_profile$lad >= LAD.threshold,]

  lad_profile$lad <- lad_profile$lad / 100

  #calculating FHD
  if(evenness){
    GS = ( 1 - sum( lad_profile$lad^2 ) ) / ( 1 - ( 1 / length(lad_profile$height) ) )
  }else{
    GS = 1 - sum( lad_profile$lad^2 )
  } #end if else

  return(GS)

} #end function


################################################################
################################################################
#' Gini coefficient of foliage structural diversity
#'
#' @description Calculates the Gini coefficient (GC) from individual LIDAR returns (i.e. without voxelization),
#' as described for the L-coefficient of variation (equivalent to Gini) in Valbuena et al. (2017).
#'
#' @param normlas.file normalized las file
#' @param threshold numerical, defines the minimum height considered to represent an echo from leaves.
#'
#' @return A [`numeric`][base::numeric] containing the Gini coefficient (GC) calculated from the normalized LAS file
#'
#' @note Valbuena et al. (2012) argues on why Gini is better suited to describe structural complexity the Foliage Height Diversity or the Gini-Simpon index.
#'
#' @references
#' Valbuena R., Packalen P., Martín-Fernández S., Maltamo M. (2012) Diversity and equitability ordering profiles applied to the study of forest structure. Forest Ecology and Management 276: 185–195. \doi{10.1016/j.foreco.2012.03.036}
#' Valbuena R., Maltamo M., Mehtätalo L., Packalen P. (2017) Key structural features of Boreal forests may be detected directly using L-moments from airborne lidar data. Remote Sensing of Environment. 194: 437–446. \doi{10.1016/j.rse.2016.10.024}
#'
#' @examples
#' # Get the example laz file
#' normlas.file = system.file("extdata", "lidar_example.laz", package="leafR")
#'
#' GC(normlas.file, threshold =1)
#'
#' @export
GC = function(normlas.file, threshold = 1){

  #load heights from normalized las cloud
  lidar = lidR::readLAS(normlas.file, select = "z", filter = paste("-drop_z_below", threshold) )

  # calculate Gini
  n <- length(lidar@data$Z)
  x <- sort(lidar@data$Z)
  G <- 2 * sum(x * 1L:n)/sum(x) - (n + 1L)
  GC <- G/(n - 1L)

  return(GC)

} #end function



################################################################
################################################################
### LAI.RASTER
#' Produce a raster map of LAI. The resolution of the raster depends of grain.size choosed on lad.voxel() funtion.
#'
#' @param VOXELS_LAD 3D grid of LAD values (output of lad.voxels() function)
#' @param min mix canopy height
#' @param relative.value LAI map can be made in percentage of a relative lai value (indicate for effective LAI)
#'
#' A Leaf Area Index (LAI) [`RasterLayer`][raster::RasterLayer-class] produced from the LAD voxels output from [lad.voxels()] function.
#'
#' @examples
#' library(raster)
#' # Get the example laz file
#' normlas.file = system.file("extdata", "lidar_example.laz", package="leafR")
#'
#' # Calculate LAD from voxelization
#' # use thicker grain size to avoid voxels
#' # without returns
#' VOXELS_LAD.5 = lad.voxels(normlas.file,
#'                         grain.size = 5, k=1)
#'
#' #Map using absolute values
#' lai_raster = lai.raster(VOXELS_LAD.5)
#' plot(lai_raster)
#'
#' #############################
#' ## RELATIVE LAI Raster
#' ######################
#' # Calculate voxels LAD with finer grain size for
#' # better estimation of LAI
#' VOXELS_LAD = lad.voxels(normlas.file,
#'                         grain.size = 2)
#'
#' # Calculate the LAD profile
#' lad_profile = lad.profile(VOXELS_LAD)
#'
#' #Calculate LAI derived from LAD profile
#' lidar.lai = lai(lad_profile)
#'
#' #Map using relative values (%)
#' relative.lai_raster = lai.raster(VOXELS_LAD.5, relative.value = lidar.lai)
#' plot(relative.lai_raster)
#'
#' @importFrom raster raster
#' @importFrom sp gridded
#' @export
# The use of min and max arguments allowed the estimation of the LAI for different vertical strata
lai.raster = function(VOXELS_LAD, min = 1, relative.value = NULL){
  max = ncol(VOXELS_LAD[[1]])
  VOXELS_LAD$LAD = VOXELS_LAD$LAD[,ncol(VOXELS_LAD$LAD):1]

  if(is.null(relative.value)){
    points = data.frame(x = VOXELS_LAD$coordenates$X, y = VOXELS_LAD$coordenates$Y,
                        z = apply(VOXELS_LAD$LAD[,min:max], 1, sum, na.rm = TRUE))
  }else{
    points = data.frame(x = VOXELS_LAD$coordenates$X, y = VOXELS_LAD$coordenates$Y,
                        z = apply(VOXELS_LAD$LAD[,min:max], 1, sum, na.rm = TRUE)/relative.value*100)

  }

  sp::coordinates(points)=~x+y
  sp::gridded(points) <- TRUE
  raster = raster::raster(points, "z")
  return(raster)

} # end function

################################################################
################################################################



################################################################
################################################################
### K.COEFFICIENT
#' Calculate k coefficient provided a known real LAI and the
#' calculated LAI
#'
#' @param lidar.lai the output from lai() function
#' @param real.lai numeric, known real LAI
#'
#' @return A [`numeric`][base::numeric] with the calculate value for k coefficient for calibrating the real LAI from calculated LAI.
#'
#' @examples
#' normlas.file = system.file("extdata", "lidar_example.laz", package="leafR")
#'
#' # Calculate LAD from voxelization
#' VOXELS_LAD = lad.voxels(normlas.file,
#'                         grain.size = 2)
#'
#' # Calculate the LAD profile
#' lad_profile = lad.profile(VOXELS_LAD)
#'
#' # Calculate LAI derived from LAD profile
#' lidar.lai = lai(lad_profile); lidar.lai
#'
#' # The real LAI was measured in the field work for validation
#' k.coefficient(lidar.lai,  real.lai = 6)
#'
#' @export
k.coefficient = function(lidar.lai, real.lai = 6) {
  k.coefficient = lidar.lai/real.lai
  return(k.coefficient)
}
