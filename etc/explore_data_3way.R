dyn.load('/opt/ohpc/pub/libs/hwloc/lib/libhwloc.so.15')
dyn.load('/opt/ohpc/pub/libs/gnu9/openmpi4/hdf5/1.10.8/lib/libhdf5_hl.so.100')
dyn.load("/opt/ohpc/pub/apps/gdal/3.5.1/lib/libgdal.so.31")


library(dplyr)
library(ggplot2)
library(ncdf4)
library(rrcov3way)

# Prepare ATMO and BIOS data cubes
path1 <- "/Net/Groups/BGI/work_2/EuropeanPHI/countrydatacubes"


nc <- nc_open(paste0(path1, "/level_3_quarter.nc"))


varnames_bios <- c("Day_AQUA_Mxx21x1_061_gapfilled_QCflags_dyn",
                   "Night_AQUA_Mxx21x1_061_gapfilled_QCflags_dyn",
                   "NDWI_band7gapfilled_061_QCdyn",
                   "EVIgapfilled_061_QCdyn",
                   "NDVIgapfilled_061_QCdyn",
                   "NIRvgapfilled_061_QCdyn",
                   "NEE", "ET", "ET_T","GPP",
                   "skt")




path2 <- "/Net/Groups/BGI/work_5/phi-eu/out/eurostat-nc"

tar_read(eurostat_metadata) |> left_join(tar_read(eurostat_datasets)) |> select(code, nut_level, title)


soec_list <- list.files(path2, full.names = T)
  
nc <- nc_open(filename = soec_list[1])

