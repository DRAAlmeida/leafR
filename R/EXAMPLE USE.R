
#example use

source("package_lidarecology.R")


# Voxels lad
normlas.file = "G:/Meu Drive/DADOS LIDAR/ALS_Ducke_2008/rarefied_plots/LO3_2500_Density50.las"
VOXELS_LAD = lad.voxels(normlas.file,
                        grain.size = 2, z.max = 60, k=0.86)

View(VOXELS_LAD[[1]])

#LAD PROFILE
lad_profile = lad.profile(VOXELS_LAD)

plot(lad_profile$height ~ lad_profile$lad, type = "l", ylim = c(0, 40),
     ylab = "Canopy height (m)", xlab = "LAD (m2/m3)")


#relative LAD PROFILE
relative.lad_profile = lad.profile(VOXELS_LAD, relative = T)

plot(relative.lad_profile$height ~ relative.lad_profile$lad, type = "l", ylim = c(0, 40),
     ylab = "Canopy height (m)", xlab = "LAD (% of LAI)")

#LAI
lidar.lai = lai(lad_profile); lidar.lai
understory.lai = lai(lad_profile, min = 1, max = 5); understory.lai


#understory relative LAI (% of total LAI)
relative.understory.lai = lai(relative.lad_profile, min = 1, max = 5); relative.understory.lai

#leaf area hieght volume metric
LAHV(lad_profile, LAI.weighting = F, height.weighting = F)
LAHV(lad_profile, LAI.weighting = T, height.weighting = F)
LAHV(lad_profile, LAI.weighting = T, height.weighting = T)
LAHV(lad_profile, LAI.weighting = F, height.weighting = T)

### RASTER MAP

# Voxels lad 5 meters
VOXELS_LAD.5 = lad.voxels(normlas.file = "G:/Meu Drive/DADOS LIDAR/ALS_Ducke_2008/rarefied_plots/LO3_2500_Density50.las",
                        grain.size = 5, z.max = 60, k=1)

#map using absolute values
lai_raster = lai.raster(VOXELS_LAD.5)
x11();plot(lai_raster)

#map using relative values (%)
relative.lai_raster = lai.raster(VOXELS_LAD.5, relative.value = lidar.lai)
x11();plot(relative.lai_raster)

#k.coefficient
k.coefficient(lidar.lai,  real.lai = 6)



