
########################################################################
########################################################################
########################################################################
### "myMetrics" for lidR ###############################################
#Function used on lidR to produce VOXELS LAD into lad.voxels() function

#######################


myMetrics = function(Z){
  
  metrics = list(
    
    total = length(Z), #total returns
    
    ground_0_1m =  length(which(Z >= 0 & Z < 1)),
    pulses.1_2m =  length(which(Z >= 1 & Z < 2)),
    pulses.2_3m =  length(which(Z >= 2 & Z < 3)),
    pulses.3_4m =  length(which(Z >= 3 & Z < 4)),
    pulses.4_5m =  length(which(Z >= 4 & Z < 5)),
    pulses.5_6m =  length(which(Z >= 5 & Z < 6)),
    pulses.6_7m =  length(which(Z >= 6 & Z < 7)),
    pulses.7_8m =  length(which(Z >= 7 & Z < 8)),
    pulses.8_9m =  length(which(Z >= 8 & Z < 9)),
    pulses.9_10m = length(which(Z >= 9 & Z < 10)),
    
    pulses.10_11m = length(which(Z >= 10 & Z < 11)),
    pulses.11_12m = length(which(Z >= 11 & Z < 12)),
    pulses.12_13m = length(which(Z >= 12 & Z < 13)),
    pulses.13_14m = length(which(Z >= 13 & Z < 14)),
    pulses.14_15m = length(which(Z >= 14 & Z < 15)),
    pulses.15_16m = length(which(Z >= 15 & Z < 16)),
    pulses.16_17m = length(which(Z >= 16 & Z < 17)),
    pulses.17_18m = length(which(Z >= 17 & Z < 18)),
    pulses.18_19m = length(which(Z >= 18 & Z < 19)),
    pulses.19_20m = length(which(Z >= 19 & Z < 20)),
    
    
    pulses.20_21m = length(which(Z >= 20 & Z < 21)),
    pulses.21_22m = length(which(Z >= 21 & Z < 22)),
    pulses.22_23m = length(which(Z >= 22 & Z < 23)),
    pulses.23_24m = length(which(Z >= 23 & Z < 24)),
    pulses.24_25m = length(which(Z >= 24 & Z < 25)),
    pulses.25_26m = length(which(Z >= 25 & Z < 26)),
    pulses.26_27m = length(which(Z >= 26 & Z < 27)),
    pulses.27_28m = length(which(Z >= 27 & Z < 28)),
    pulses.28_29m = length(which(Z >= 28 & Z < 29)),
    pulses.29_30m = length(which(Z >= 29 & Z < 30)),
    
    pulses.30_31m = length(which(Z >= 30 & Z < 31)),
    pulses.31_32m = length(which(Z >= 31 & Z < 32)),
    pulses.32_33m = length(which(Z >= 32 & Z < 33)),
    pulses.33_34m = length(which(Z >= 33 & Z < 34)),
    pulses.34_35m = length(which(Z >= 34 & Z < 35)),
    pulses.35_36m = length(which(Z >= 35 & Z < 36)),
    pulses.36_37m = length(which(Z >= 36 & Z < 37)),
    pulses.37_38m = length(which(Z >= 37 & Z < 38)),
    pulses.38_39m = length(which(Z >= 38 & Z < 39)),
    pulses.39_40m = length(which(Z >= 39 & Z < 40)),
    
    
    pulses.40_41m = length(which(Z >= 40 & Z < 41)),
    pulses.41_42m = length(which(Z >= 41 & Z < 42)),
    pulses.42_43m = length(which(Z >= 42 & Z < 43)),
    pulses.43_44m = length(which(Z >= 43 & Z < 44)),
    pulses.44_45m = length(which(Z >= 44 & Z < 45)),
    pulses.45_46m = length(which(Z >= 45 & Z < 46)),
    pulses.46_47m = length(which(Z >= 46 & Z < 47)),
    pulses.47_48m = length(which(Z >= 47 & Z < 48)),
    pulses.48_49m = length(which(Z >= 48 & Z < 49)),
    pulses.49_50m = length(which(Z >= 49 & Z < 50)),
    
    
    pulses.50_51m = length(which(Z >= 50 & Z < 51)),
    pulses.51_52m = length(which(Z >= 51 & Z < 52)),
    pulses.52_53m = length(which(Z >= 52 & Z < 53)),
    pulses.53_54m = length(which(Z >= 53 & Z < 54)),
    pulses.54_55m = length(which(Z >= 54 & Z < 55)),
    pulses.55_56m = length(which(Z >= 55 & Z < 56)),
    pulses.56_57m = length(which(Z >= 56 & Z < 57)),
    pulses.57_58m = length(which(Z >= 57 & Z < 58)),
    pulses.58_59m = length(which(Z >= 58 & Z < 59)),
    pulses.59_60m = length(which(Z >= 59 & Z < 60)),
    
    pulses.60_61m = length(which(Z >= 60 & Z < 61)),
    pulses.61_62m = length(which(Z >= 61 & Z < 62)),
    pulses.62_63m = length(which(Z >= 62 & Z < 63)),
    pulses.63_64m = length(which(Z >= 63 & Z < 64)),
    pulses.64_65m = length(which(Z >= 64 & Z < 65)),
    pulses.65_66m = length(which(Z >= 65 & Z < 66)),
    pulses.66_67m = length(which(Z >= 66 & Z < 67)),
    pulses.67_68m = length(which(Z >= 67 & Z < 68)),
    pulses.68_69m = length(which(Z >= 68 & Z < 69)),
    pulses.69_70m = length(which(Z >= 69 & Z < 70)),
    
    
    pulses.70_71m = length(which(Z >= 70 & Z < 71)),
    pulses.71_72m = length(which(Z >= 71 & Z < 72)),
    pulses.72_73m = length(which(Z >= 72 & Z < 73)),
    pulses.73_74m = length(which(Z >= 73 & Z < 74)),
    pulses.74_75m = length(which(Z >= 74 & Z < 75)),
    pulses.75_76m = length(which(Z >= 75 & Z < 76)),
    pulses.76_77m = length(which(Z >= 76 & Z < 77)),
    pulses.77_78m = length(which(Z >= 77 & Z < 78)),
    pulses.78_79m = length(which(Z >= 78 & Z < 79)),
    pulses.79_80m = length(which(Z >= 79 & Z < 80)),
    
    
    pulses.80_81m = length(which(Z >= 80 & Z < 91)),
    pulses.81_82m = length(which(Z >= 81 & Z < 92)),
    pulses.82_83m = length(which(Z >= 82 & Z < 93)),
    pulses.83_84m = length(which(Z >= 83 & Z < 94)),
    pulses.84_85m = length(which(Z >= 84 & Z < 95)),
    pulses.85_86m = length(which(Z >= 85 & Z < 96)),
    pulses.86_87m = length(which(Z >= 86 & Z < 97)),
    pulses.87_88m = length(which(Z >= 87 & Z < 98)),
    pulses.88_89m = length(which(Z >= 88 & Z < 99)),
    pulses.89_90m = length(which(Z >= 89 & Z < 90)),
    
    pulses.90_91m = length(which(Z >= 90 & Z < 91)),
    pulses.91_92m = length(which(Z >= 91 & Z < 92)),
    pulses.92_93m = length(which(Z >= 92 & Z < 93)),
    pulses.93_94m = length(which(Z >= 93 & Z < 94)),
    pulses.94_95m = length(which(Z >= 94 & Z < 95)),
    pulses.95_96m = length(which(Z >= 95 & Z < 96)),
    pulses.96_97m = length(which(Z >= 96 & Z < 97)),
    pulses.97_98m = length(which(Z >= 97 & Z < 98)),
    pulses.98_99m = length(which(Z >= 98 & Z < 99)),
    pulses.99_100m = length(which(Z >= 99 & Z < 100)),
    pulses.100_101m = length(which(Z >= 100 & Z < 101))
    
    
  ) #end list() metrics
  
  return(metrics) 
  
} #end function myMetrics


################################################################
################################################################







######################################################################################
######################################################################################
######################################################################################
# lad.voxels() - This function create a data frame of the 3D voxels information (xyz) 
#with Leaf Area Density values from las file

#Arguments:
# normlas.file : normalized las file
# grain.size : horizontal resolution (suggested 1 meter for lad profiles and 10 meters for LAI maps)
# z.max : max canopy height (choose 30, 60 or 90)
# k : coefficient to transform effective LAI to real LAI (k = 1; for effective LAI)

##OBS: the values of LAD are not estimated below 1 meter. For the following reasons:
# ground points influence
# realtive low sampling

lad.voxels = function(normlas.file, grain.size = 1, z.max = 60, k=1){
  
  ########################################################################
  ########################################################################
  ########################################################################
  
  #empty list object that will be fueling with binneds data.frames
  LAD_VOXELS = list()
  
  #loading packages and function
  require(lidR)
  require(raster)
  
  #load normalized las cloud
  .las = lidR::readLAS(normlas.file)
  #x11();plot(.las)
  
  #VOXELS LAD using lidR and myMetrics function
  t.binneds = lidR::grid_metrics(.las, myMetrics(Z), res = grain.size, 
                                 start = c(min(.las@data$X), max(.las@data$Y)))
  t.binneds = data.frame(raster::coordinates(t.binneds), values(t.binneds))
  names(t.binneds)[1:2] = c("X", "Y")
  
  ## ATENCAO CAIO
  #substituir a funcao grid_metrics pela grid_metrics3d e reformatar esta. Exemplo de uso:
  #lidR::grid_metrics3d(.las, length(Z), res = c(2,1), )
  
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
  ground.returns = t.binneds[, c(grep("ground", names(t.binneds)))]
  
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
  cumsum.matrix.dz1 = matrix(apply(pulses.profile.dz1, 1, cumsum), ncol = length(pulses.profile.dz1), nrow = nrow(pulses.profile.dz1), byrow = T)
  
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
  
  rm(t.las)
  
}#End function 

################################################################
################################################################







################################################################
################################################################
### LAD PROFILE 
# This function calculate the lad profile from the input lad.voxels

# Arguments: 
# VOXELS_LAD : 3D grid of LAD values (output of lad.voxels() function)
# relative : produce lad profile by relative total LAI values. Indicate when usinh effective LAI

# lad.profile functions
lad.profile = function(VOXELS_LAD, relative = F){
  
  if(relative == T){
    t.lad.profile = apply(VOXELS_LAD$LAD, 2, mean, na.rm = T)
    t.lad.profile = t.lad.profile/sum(t.lad.profile)*100
  }else{
  t.lad.profile = apply(VOXELS_LAD$LAD, 2, mean, na.rm = T)
  }
  
  t.lad.profile = data.frame(height = seq(1.5, 100.5), lad = t.lad.profile[length(t.lad.profile):1])
  
  return(t.lad.profile)
  
}#end looping

#use example


################################################################
################################################################





################################################################
################################################################
### LAI
##### lai function

# Argument
# lad_profile : output or lad.profile function
# min: mix canopy height
# max: max canopy height

# The use of min and max arguments allowed the estimation of the LAI for different vertical strata

lai = function(lad_profile, min = 1, max = 100){
    lai = sum(lad_profile$lad[(min):(max)], na.rm = T)
    return(lai)
}


################################################################
################################################################



################################################################
################################################################
# Leaf Area Height Volume metric

#Arguments
# LAVH can be weighted by total LAI or max.height


LAHV = function(lad_profile, LAI.weighting = F, height.weighting = F){
  
  #LAI.weighting
  if(LAI.weighting == T){
    LAHV = sum(lad_profile$height*lad_profile$lad)/sum(lad_profile$lad)
  }else{
    LAHV = sum(lad_profile$height*lad_profile$lad)
  } #end if else
  
  #height.weighting
  if(height.weighting == T){
    LAHV = LAHV/max(lad_profile$height)
    } #enf if 

  return(LAHV)  
  
} #end function
  





################################################################
################################################################
### LAI.RASTER
# Produce a raster map of LAI. The resolution of the raster depends of grain.size choosed on lad.voxel() funtion. 

# Argument
# VOXELS_LAD : 3D grid of LAD values (output of lad.voxels() function)
# min: mix canopy height
# max: max canopy height
# reletive.value : LAI map can be made in percentage of a relative lai value (indicate for effective LAI)

# The use of min and max arguments allowed the estimation of the LAI for different vertical strata

lai.raster = function(VOXELS_LAD, min = 1, max = 100, relative.value = NULL){
  
  require(raster)
  require(sp)
  
  VOXELS_LAD$LAD = VOXELS_LAD$LAD[,ncol(VOXELS_LAD$LAD):1]
  
  if(is.null(relative.value)){
    points = data.frame(x = VOXELS_LAD$coordenates$X, y = VOXELS_LAD$coordenates$Y, 
                        z = apply(VOXELS_LAD$LAD[,min:max], 1, sum, na.rm = T))
  }else{
    points = data.frame(x = VOXELS_LAD$coordenates$X, y = VOXELS_LAD$coordenates$Y, 
                        z = apply(VOXELS_LAD$LAD[,min:max], 1, sum, na.rm = T)/relative.value*100)
 
  }
  
  sp::coordinates(points)=~x+y
  gridded(points) <- TRUE
  raster = raster::raster(points, "z")
  return(raster)
    
} # end function

################################################################
################################################################



################################################################
################################################################
### K.COEFFICIENT

# Calculate k coefficient

k.coefficient = function(lidar.lai, real.lai = 6) {
  k.coefficient = lidar.lai/real.lai
  return(k.coefficient)
}



